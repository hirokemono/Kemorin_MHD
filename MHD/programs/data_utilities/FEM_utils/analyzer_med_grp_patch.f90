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
      use m_work_time
      use calypso_mpi
      use t_FEM_utils
      use t_comm_table
      use t_mesh_SR
      use t_elapsed_labels_4_SECTIONS
!
      implicit none
!
!       Structure for time stepping parameters
      type(FEM_utils), save :: FUTIL1
!>      Structure of work area for mesh communications
      type(mesh_SR) :: m_SR4

      type(communication_table), save :: edge_comm_MG
!
      logical, parameter :: flag_detailed1 = .TRUE.
      type(elapsed_labels_4_SECTIONS), save :: elps_SECT1
!
      private :: set_med_grp_patch_ctl
      private :: set_med_grp_patch_psf_def_ctl
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
      use input_control_udt_diff
      use nod_phys_send_recv
      use mpi_load_mesh_data
      use parallel_FEM_mesh_init
      use const_element_comm_tables
!
      if (my_rank.eq.0) then
        write(*,*) 'diff. udt files'
        write(*,*) 'Input file: mesh data, udt data'
      end if
!
      call init_elapse_time_by_TOTAL
      call elpsed_label_4_SECT(flag_detailed1, elps_SECT1, elps1)
!
!     ---------------------
!
      if (iflag_debug.eq.1) write(*,*) 's_input_control_grp_patch'
      call s_input_control_grp_patch                                    &
     &   (FUTIL1%mesh_file, FUTIL1%udt_file)
      if (iflag_debug.eq.1) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(FUTIL1%mesh_file, nprocs, FUTIL1%geofem)
!
!     --------------------- 
!
      if (iflag_debug.eq.1) write(*,*) 'FEM_mesh_initialization'
      call FEM_comm_initialization(FUTIL1%geofem%mesh, m_SR4)
      call FEM_mesh_initialization(FUTIL1%geofem%mesh,                  &
     &    FUTIL1%geofem%group, m_SR4%SR_sig, m_SR4%SR_i)

!
      call const_edge_comm_table                                        &
     &   (FUTIL1%geofem%mesh%node, FUTIL1%geofem%mesh%nod_comm,         &
     &    edge_comm_MG, FUTIL1%geofem%mesh%edge, m_SR4)
!
      FUTIL1%nod_fld%num_phys = 1
      call alloc_phys_name(FUTIL1%nod_fld)
      FUTIL1%nod_fld%num_component(1) = 1
      FUTIL1%nod_fld%istack_component(1) = 1
      FUTIL1%nod_fld%phys_name(1) = 'temperature'
!
      end subroutine initialize_med_grp_patch
!
! ----------------------------------------------------------------------
!
      subroutine analyze_med_grp_patch
!
      use m_elapsed_labels_4_VIZ
      use m_ctl_params_4_diff_udt
      use set_parallel_file_name
      use t_control_data_sections
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
      psf_ctls_md%num_psf_ctl = FUTIL1%geofem%group%ele_grp%num_grp
      call alloc_psf_ctl_stract(psf_ctls_md)
!
      if(my_rank .eq. 0) then
        grouping_mesh_list = add_dat_extension(grouping_mesh_head)
        open(id_gname,file=grouping_mesh_list)
        write(id_gname,'(i16)') FUTIL1%geofem%group%ele_grp%num_grp
        do igrp = 1, FUTIL1%geofem%group%ele_grp%num_grp
          write(id_gname,'(i16,a3,a)') igrp, '   ',                     &
     &              trim(FUTIL1%geofem%group%ele_grp%grp_name(igrp))
        end do
        close(id_gname)
      end if
!
      call set_med_grp_patch_ctl(psf_ctls_md%num_psf_ctl,               &
     &    psf_ctls_md%fname_psf_ctl, psf_ctls_md%psf_ctl_struct)
!
      call SECTIONING_initialize(ione, elps_SECT1%elps_PSF,             &
     &    FUTIL1%geofem, edge_comm_MG, FUTIL1%nod_fld, psf_ctls_md,     &
     &    psf_md, m_SR4%SR_sig, m_SR4%SR_il)
!
      end subroutine analyze_med_grp_patch
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_med_grp_patch_ctl(num_psf_ctl, fname_psf_ctl,      &
     &                                 psf_ctl_struct)
!
      use m_ctl_params_4_diff_udt
      use set_parallel_file_name
      use t_control_data_4_psf
      use t_control_array_character2
      use set_coefs_of_sections
!
      integer(kind = kint), intent(inout) :: num_psf_ctl
      character(len = kchara), intent(inout)                            &
     &                        :: fname_psf_ctl(num_psf_ctl)
      type(psf_ctl), intent(inout) :: psf_ctl_struct(num_psf_ctl)
!
      integer(kind = kint) :: igrp
!
!
      do igrp = 1, FUTIL1%geofem%group%ele_grp%num_grp
        fname_psf_ctl(igrp) = 'NO_FILE'
        psf_ctl_struct(igrp)%psf_file_head_ctl%iflag = 1
        psf_ctl_struct(igrp)%psf_file_head_ctl%charavalue               &
     &              = add_int_suffix(igrp, grouping_mesh_head)
!
        psf_ctl_struct(igrp)%psf_output_type_ctl%iflag = 1
        psf_ctl_struct(igrp)%psf_output_type_ctl%charavalue = 'VTD'
!
        call set_med_grp_patch_psf_def_ctl                              &
     &     (FUTIL1%geofem%group%ele_grp%grp_name(igrp),                 &
     &      psf_ctl_struct(igrp)%psf_def_c)
!
        psf_ctl_struct(igrp)%fld_on_psf_c%field_output_ctl%num = 1
        call alloc_control_array_c2                                     &
     &     (psf_ctl_struct(igrp)%fld_on_psf_c%field_output_ctl)
        psf_ctl_struct(igrp)%fld_on_psf_c%field_output_ctl%c1_tbl(1)    &
     &      = FUTIL1%nod_fld%phys_name(1)
        psf_ctl_struct(igrp)%fld_on_psf_c%field_output_ctl%c2_tbl(1)    &
     &      = 'scalar'
      end do
!
      end subroutine set_med_grp_patch_ctl
!
! ----------------------------------------------------------------------
!
      subroutine set_med_grp_patch_psf_def_ctl(grp_name, psf_def_c)
!
      use m_section_coef_flags
      use t_control_data_4_psf_def
      use set_coefs_of_sections
!
      character(len = kchara), intent(in) :: grp_name
      type(psf_define_ctl), intent(inout) :: psf_def_c
!
!
      psf_def_c%section_method_ctl%charavalue = cflag_eq
!
      psf_def_c%psf_coefs_ctl%num = 1
      call alloc_control_array_c_r(psf_def_c%psf_coefs_ctl)
      psf_def_c%psf_coefs_ctl%c_tbl(1) = 'y'
      psf_def_c%psf_coefs_ctl%vect(1) = -1.0d0
!
      psf_def_c%psf_area_ctl%num = 1
      call alloc_control_array_chara(psf_def_c%psf_area_ctl)
      psf_def_c%psf_area_ctl%c_tbl(1) = grp_name
!
      end subroutine set_med_grp_patch_psf_def_ctl
!
! ----------------------------------------------------------------------
!
      end module analyzer_med_grp_patch

