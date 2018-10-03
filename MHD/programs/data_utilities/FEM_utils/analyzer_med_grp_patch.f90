!analyzer_med_grp_patch.f90
!      module analyzer_med_grp_patch
!
!      subroutine initialize_med_grp_patch
!      subroutine analyze_med_grp_patch
!
!      modified by H. Matsui on Nov., 2006 
!
!
      module analyzer_med_grp_patch
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
      use m_FEM_utils
!
      implicit none
!
      private :: set_med_grp_patch_ctl
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_med_grp_patch
!
      use m_phys_constants
      use m_array_for_send_recv
      use m_FEM_utils
      use input_control_udt_diff
      use nod_phys_send_recv
      use mpi_load_mesh_data
      use parallel_FEM_mesh_init
!
!
      if (my_rank.eq.0) then
        write(*,*) 'diff. udt files'
        write(*,*) 'Input file: mesh data, udt data'
      end if
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 's_input_control_grp_patch'
      call s_input_control_grp_patch(mesh_file_FUTIL, udt_param_FUTIL)
      if (iflag_debug.eq.1) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh                                               &
     &   (mesh_file_FUTIL, nprocs, femmesh_FUTIL, elemesh_FUTIL)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_mesh_initialization'
      call FEM_mesh_initialization                                      &
     &   (femmesh_FUTIL%mesh, femmesh_FUTIL%group, elemesh_FUTIL)
!
      field_FUTIL%num_phys = 1
      call alloc_phys_name_type(field_FUTIL)
      field_FUTIL%num_component(1) = 1
      field_FUTIL%istack_component(1) = 1
      field_FUTIL%phys_name(1) = 'temperature'
!
      call calypso_mpi_barrier
!
      end subroutine initialize_med_grp_patch
!
! ----------------------------------------------------------------------
!
      subroutine analyze_med_grp_patch
!
      use m_ctl_params_4_diff_udt
      use set_parallel_file_name
      use t_control_data_sections
      use t_read_control_arrays
      use t_cross_section
      use set_coefs_of_sections
!
      integer(kind = kint) :: igrp
      integer(kind = kint),  parameter :: id_gname = 11
      character(len=kchara) :: grouping_mesh_list
      type(section_controls), save :: psf_ctls_md
      type(sectioning_module), save :: psf_md
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_med_patch_ele_grp',         &
     &             trim(grouping_mesh_head)
      psf_ctls_md%num_psf_ctl = femmesh_FUTIL%group%ele_grp%num_grp
      call alloc_psf_ctl_stract(psf_ctls_md)
!
      if(my_rank .eq. 0) then
        grouping_mesh_list = add_dat_extension(grouping_mesh_head)
        open(id_gname,file=grouping_mesh_list)
        write(id_gname,'(i16)') femmesh_FUTIL%group%ele_grp%num_grp
        do igrp = 1, femmesh_FUTIL%group%ele_grp%num_grp
          write(id_gname,'(i16,a3,a)') igrp, '   ',                     &
     &              trim(femmesh_FUTIL%group%ele_grp%grp_name(igrp))
        end do
        close(id_gname)
      end if
!
      call set_med_grp_patch_ctl(psf_ctls_md%num_psf_ctl,               &
     &     psf_ctls_md%fname_psf_ctl, psf_ctls_md%psf_ctl_struct)
!
      call SECTIONING_initialize                                        &
     &   (femmesh_FUTIL, elemesh_FUTIL, field_FUTIL,                    &
     &    psf_ctls_md, psf_md)
!
      end subroutine analyze_med_grp_patch
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_med_grp_patch_ctl                                  &
     &         (num_psf_ctl, fname_psf_ctl, psf_ctl_struct)
!
      use m_ctl_params_4_diff_udt
      use set_parallel_file_name
      use t_control_data_4_psf
      use t_read_control_arrays
      use set_coefs_of_sections
!
      integer(kind = kint), intent(inout) :: num_psf_ctl
      character(len = kchara), intent(inout)                            &
     &                         :: fname_psf_ctl(num_psf_ctl)
      type(psf_ctl), intent(inout) :: psf_ctl_struct(num_psf_ctl)
!
      integer(kind = kint) :: igrp
!
!
      do igrp = 1, femmesh_FUTIL%group%ele_grp%num_grp
        fname_psf_ctl(igrp) = 'NO_FILE'
        psf_ctl_struct(igrp)%psf_file_head_ctl%iflag = 1
        psf_ctl_struct(igrp)%psf_file_head_ctl%charavalue               &
     &              = add_int_suffix(igrp, grouping_mesh_head)
!
        psf_ctl_struct(igrp)%psf_output_type_ctl%iflag = 1
        psf_ctl_struct(igrp)%psf_output_type_ctl%charavalue = 'VTD'
!
        psf_ctl_struct(igrp)%section_method_ctl%charavalue = cflag_eq
!
        psf_ctl_struct(igrp)%psf_coefs_ctl%num = 1
        call alloc_control_array_c_r(psf_ctl_struct(igrp)%psf_coefs_ctl)
        psf_ctl_struct(igrp)%psf_coefs_ctl%c_tbl(1) = 'y'
        psf_ctl_struct(igrp)%psf_coefs_ctl%vect(1) = -1.0d0
!
        psf_ctl_struct(igrp)%psf_area_ctl%num = 1
        call alloc_control_array_chara                                  &
     &     (psf_ctl_struct(igrp)%psf_area_ctl)
        psf_ctl_struct(igrp)%psf_area_ctl%c_tbl(1)                      &
     &      = femmesh_FUTIL%group%ele_grp%grp_name(igrp)
!
        psf_ctl_struct(igrp)%psf_out_field_ctl%num = 1
        call alloc_control_array_c2                                     &
     &     (psf_ctl_struct(igrp)%psf_out_field_ctl)
        psf_ctl_struct(igrp)%psf_out_field_ctl%c1_tbl(1)                &
     &      = field_FUTIL%phys_name(1)
        psf_ctl_struct(igrp)%psf_out_field_ctl%c2_tbl(1)                &
     &      = 'scalar'
      end do
!
      end subroutine set_med_grp_patch_ctl
!
! ----------------------------------------------------------------------
!
      end module analyzer_med_grp_patch

