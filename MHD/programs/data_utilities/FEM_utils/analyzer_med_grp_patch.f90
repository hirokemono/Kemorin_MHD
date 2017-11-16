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
      call mpi_input_mesh(mesh_file_FUTIL, nprocs,                      &
     &    femmesh_FUTIL%mesh, femmesh_FUTIL%group,                      &
     &    elemesh_FUTIL%surf%nnod_4_surf,                               &
     &    elemesh_FUTIL%edge%nnod_4_edge)
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
      use m_cross_section
      use m_control_data_sections
      use set_parallel_file_name
      use t_read_control_arrays
      use set_coefs_of_sections
!
      integer(kind = kint) :: igrp
      integer(kind = kint),  parameter :: id_gname = 11
      character(len=kchara) :: grouping_mesh_list
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_med_patch_ele_grp',         &
     &             trim(grouping_mesh_head)
      num_psf_ctl = femmesh_FUTIL%group%ele_grp%num_grp
      call allocate_psf_ctl_stract
!
      if(my_rank .eq. 0) then
        call add_dat_extension(grouping_mesh_head,grouping_mesh_list)
        open(id_gname,file=grouping_mesh_list)
        write(id_gname,'(i16)') femmesh_FUTIL%group%ele_grp%num_grp
        do igrp = 1, femmesh_FUTIL%group%ele_grp%num_grp
          write(id_gname,'(i16,a3,a)') igrp, '   ',                     &
     &              trim(femmesh_FUTIL%group%ele_grp%grp_name(igrp))
        end do
        close(id_gname)
      end if
!
      do igrp = 1, femmesh_FUTIL%group%ele_grp%num_grp
        fname_psf_ctl(igrp) = 'NO_FILE'
        psf_ctl_struct(igrp)%psf_file_head_ctl%iflag = 1
        call add_int_suffix(igrp, grouping_mesh_head,                   &
     &      psf_ctl_struct(igrp)%psf_file_head_ctl%charavalue)
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
      call SECTIONING_initialize                                        &
     &   (femmesh_FUTIL%mesh, femmesh_FUTIL%group, elemesh_FUTIL,       &
     &    field_FUTIL)
!
      end subroutine analyze_med_grp_patch
!
! ----------------------------------------------------------------------
!
      end module analyzer_med_grp_patch

