!>@file   input_control_sph_MHD.f90
!!@brief  module input_control_sph_MHD
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in March, 2015
!
!>@brief  Load mesh and filtering data for MHD simulation
!!
!!@verbatim
!!      subroutine input_control_SPH_mesh                               &
!!     &         (MHD_ctl, sph, comms_sph, sph_grps, rj_fld, nod_fld,   &
!!     &          pwr, SGS_par, dynamic_SPH, MHD_step, MHD_prop,        &
!!     &          WK, mesh, group, ele_mesh)
!!      subroutine input_control_4_SPH_MHD_nosnap                       &
!!     &         (MHD_ctl, sph, comms_sph, sph_grps, rj_fld,            &
!!     &          pwr, SGS_par, dynamic_SPH, MHD_step, MHD_prop, WK)
!!
!!      subroutine input_control_4_SPH_make_init                        &
!! .   &         (MHD_ctl, sph, comms_sph, sph_grps, rj_fld,            &
!!     &          pwr, SGS_par, MHD_step, mesh, group, ele_mesh,        &
!!     &          MHD_prop, WK)
!!      subroutine input_control_SPH_dynamobench                        &
!!     &          (MHD_ctl, sph, comms_sph, sph_grps, rj_fld, nod_fld,  &
!!     &           pwr, SGS_par, MHD_step, MHD_prop, WK)
!!        type(mhd_simulation_control), intent(inout) :: MHD_ctl
!!        type(sph_grids), intent(inout) :: sph
!!        type(sph_comm_tables), intent(inout) :: comms_sph
!!        type(sph_group_data), intent(inout) ::  sph_grps
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(sph_mean_squares), intent(inout) :: pwr
!!        type(SGS_paremeters), intent(inout) :: SGS_par
!!        type(sph_filters_type), intent(inout) :: sph_filters(1)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
!!        type(element_geometry), intent(inout) :: ele_mesh
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(MHD_evolution_param), intent(inout) :: MHD_prop
!!@endverbatim
!
!
      module input_control_sph_MHD
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_control_parameter
      use t_MHD_step_parameter
      use t_SGS_control_parameter
      use t_spheric_parameter
      use t_mesh_data
      use t_phys_data
      use t_spheric_mesh
      use t_group_data
      use t_rms_4_sph_spectr
      use t_file_IO_parameter
      use t_SPH_MHD_file_parameters
      use t_ctl_data_MHD
      use t_sph_trans_arrays_MHD
      use sph_filtering
!
      implicit none
!
      private :: set_control_4_SPH_to_FEM
!
      type(sph_grids), private :: sph_gen
!
!>      Structure for mesh file IO paramters
      type(field_IO_params), save ::  mesh1_file
!>      Structure for spectr file  paramters
      type(field_IO_params), save :: sph_file_param1
!>      Structure of dynamo file parameters for original data
      type(file_params_4_sph_mhd), save :: MHD1_org_files
!
      private :: mesh1_file
      private :: select_make_SPH_mesh
      private :: sph_boundary_IO_control
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine input_control_SPH_mesh                                 &
     &         (MHD_ctl, sph, comms_sph, sph_grps, rj_fld, nod_fld,     &
     &          pwr, SGS_par, dynamic_SPH, MHD_step, MHD_prop,          &
     &          WK, mesh, group, ele_mesh)
!
      use m_error_IDs
!
      use sph_mhd_rst_IO_control
      use set_control_sph_mhd
      use sph_file_IO_select
!
      type(mhd_simulation_control), intent(inout) :: MHD_ctl
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) ::  sph_grps
!
      type(phys_data), intent(inout) :: rj_fld
      type(phys_data), intent(inout) :: nod_fld
      type(sph_mean_squares), intent(inout) :: pwr
      type(SGS_paremeters), intent(inout) :: SGS_par
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
      type(MHD_step_param), intent(inout) :: MHD_step
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(works_4_sph_trans_MHD), intent(inout) :: WK
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_SGS_SPH_MHD'
      call set_control_SGS_SPH_MHD(MHD_ctl%plt, MHD_ctl%org_plt,        &
     &    MHD_ctl%model_ctl, MHD_ctl%ctl_ctl, MHD_ctl%smonitor_ctl,     &
     &    MHD_ctl%nmtr_ctl, MHD_ctl%psph_ctl,                           &
     &    sph_gen, rj_fld, mesh1_file, sph_file_param1, MHD1_org_files, &
     &    sph_fst_IO, pwr, SGS_par, dynamic_SPH%sph_filters, MHD_step,  &
     &    MHD_prop, WK%WK_sph)
!
      call set_control_4_SPH_to_FEM                                     &
     &   (MHD_ctl%psph_ctl%spctl, sph%sph_params, rj_fld, nod_fld)
!
!
      call select_make_SPH_mesh                                         &
     &   (sph, comms_sph, sph_grps,  mesh, group, ele_mesh, mesh1_file)
!
      call sph_boundary_IO_control(MHD_prop)
!
      end subroutine input_control_SPH_mesh
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine input_control_4_SPH_MHD_nosnap                         &
     &         (MHD_ctl, sph, comms_sph, sph_grps, rj_fld,              &
     &          pwr, SGS_par, dynamic_SPH, MHD_step, MHD_prop, WK)
!
      use m_sph_boundary_input_data
      use sph_mhd_rst_IO_control
      use set_control_sph_mhd
      use parallel_load_data_4_sph
!
      type(mhd_simulation_control), intent(inout) :: MHD_ctl
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) ::  sph_grps
!
      type(phys_data), intent(inout) :: rj_fld
      type(sph_mean_squares), intent(inout) :: pwr
      type(SGS_paremeters), intent(inout) :: SGS_par
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
      type(MHD_step_param), intent(inout) :: MHD_step
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(works_4_sph_trans_MHD), intent(inout) :: WK
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_SGS_SPH_MHD'
      call set_control_SGS_SPH_MHD(MHD_ctl%plt, MHD_ctl%org_plt,        &
     &    MHD_ctl%model_ctl, MHD_ctl%ctl_ctl, MHD_ctl%smonitor_ctl,     &
     &    MHD_ctl%nmtr_ctl, MHD_ctl%psph_ctl,                           &
     &    sph_gen, rj_fld, mesh1_file, sph_file_param1, MHD1_org_files, &
     &    sph_fst_IO, pwr, SGS_par, dynamic_SPH%sph_filters, MHD_step,  &
     &    MHD_prop, WK%WK_sph)
!
      if (iflag_debug.eq.1) write(*,*) 'load_para_sph_mesh'
      call load_para_sph_mesh(sph, comms_sph, sph_grps)
!
      call sph_boundary_IO_control(MHD_prop)
!
      end subroutine input_control_4_SPH_MHD_nosnap
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine input_control_4_SPH_make_init                          &
     &         (MHD_ctl, sph, comms_sph, sph_grps, rj_fld,              &
     &          pwr, SGS_par, MHD_step, mesh, group, ele_mesh,          &
     &          MHD_prop, WK)
!
      use sph_mhd_rst_IO_control
      use set_control_sph_mhd
      use parallel_load_data_4_sph
!
      type(mhd_simulation_control), intent(inout) :: MHD_ctl
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) ::  sph_grps
!
      type(phys_data), intent(inout) :: rj_fld
      type(sph_mean_squares), intent(inout) :: pwr
      type(SGS_paremeters), intent(inout) :: SGS_par
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
      type(MHD_step_param), intent(inout) :: MHD_step
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(works_4_sph_trans_MHD), intent(inout) :: WK
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_SPH_MHD'
      call set_control_4_SPH_MHD(MHD_ctl%plt, MHD_ctl%org_plt,          &
     &    MHD_ctl%model_ctl, MHD_ctl%ctl_ctl, MHD_ctl%smonitor_ctl,     &
     &    MHD_ctl%nmtr_ctl, MHD_ctl%psph_ctl,                           &
     &    sph_gen, rj_fld, mesh1_file, sph_file_param1,                 &
     &    MHD1_org_files, sph_fst_IO, pwr, SGS_par, MHD_step,           &
     &    MHD_prop, WK%WK_sph)
!
      call select_make_SPH_mesh                                         &
     &   (sph, comms_sph, sph_grps, mesh, group, ele_mesh, mesh1_file)
!
      end subroutine input_control_4_SPH_make_init
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine input_control_SPH_dynamobench                          &
     &          (MHD_ctl, sph, comms_sph, sph_grps, rj_fld, nod_fld,    &
     &           pwr, SGS_par, MHD_step, MHD_prop, WK)
!
      use sph_mhd_rst_IO_control
      use set_control_sph_mhd
      use set_control_sph_data_MHD
      use parallel_load_data_4_sph
!
      type(mhd_simulation_control), intent(inout) :: MHD_ctl
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) ::  sph_grps
!
      type(phys_data), intent(inout) :: rj_fld
      type(phys_data), intent(inout) :: nod_fld
      type(sph_mean_squares), intent(inout) :: pwr
      type(SGS_paremeters), intent(inout) :: SGS_par
      type(MHD_step_param), intent(inout) :: MHD_step
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(works_4_sph_trans_MHD), intent(inout) :: WK
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_SPH_MHD'
      call set_control_4_SPH_MHD(MHD_ctl%plt, MHD_ctl%org_plt,          &
     &    MHD_ctl%model_ctl, MHD_ctl%ctl_ctl, MHD_ctl%smonitor_ctl,     &
     &    MHD_ctl%nmtr_ctl, MHD_ctl%psph_ctl,                           &
     &    sph_gen, rj_fld, mesh1_file, sph_file_param1,                 &
     &    MHD1_org_files, sph_fst_IO, pwr, SGS_par, MHD_step,           &
     &    MHD_prop, WK%WK_sph)
!
      call set_control_4_SPH_to_FEM                                     &
     &   (MHD_ctl%psph_ctl%spctl, sph%sph_params, rj_fld, nod_fld)
      call set_ctl_params_dynamobench                                   &
     &   (MHD_ctl%model_ctl%fld_ctl%field_ctl,                          &
     &    MHD_ctl%smonitor_ctl%meq_ctl)
!
      if (iflag_debug.eq.1) write(*,*) 'load_para_sph_mesh'
      call load_para_sph_mesh(sph, comms_sph, sph_grps)
!
      end subroutine input_control_SPH_dynamobench
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_control_4_SPH_to_FEM                               &
     &         (spctl, sph_params, rj_fld, nod_fld)
!
      use t_ctl_data_4_sphere_model
!
      use ordering_field_by_viz
      use node_monitor_IO
      use set_controls_4_sph_shell
!
      type(sphere_data_control), intent(in) :: spctl
      type(sph_shell_parameters), intent(inout) :: sph_params
      type(phys_data), intent(inout) :: rj_fld
      type(phys_data), intent(inout) :: nod_fld
!
!
      call set_FEM_mesh_mode_4_SPH(spctl, sph_params%iflag_shell_mode)
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &     write(*,*) 'copy_rj_spec_name_to_nod_fld'
      call copy_field_name_type(rj_fld, nod_fld)
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &     call check_nodal_field_name_type(6, nod_fld)
!
      call count_field_4_monitor                                        &
     &   (rj_fld%num_phys, rj_fld%num_component,                        &
     &    rj_fld%iflag_monitor, num_field_monitor, ntot_comp_monitor)
!
      end subroutine set_control_4_SPH_to_FEM
!
! -----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine select_make_SPH_mesh(sph, comms_sph, sph_grps,         &
     &          mesh, group, ele_mesh, mesh_file)
!
      use m_error_IDs
      use m_spheric_global_ranks
      use parallel_load_data_4_sph
      use parallel_gen_sph_grids
!
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) ::  sph_grps
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
      type(field_IO_params), intent(inout) ::  mesh_file
!
      integer(kind = kint) :: iflag_lc, iflag_gl
!
!
      iflag_lc = 0
      if     (check_exsist_rtp_file(my_rank) .ne. 0                     &
     &  .or. check_exsist_rtm_file(my_rank) .ne. 0                      &
     &  .or. check_exsist_rlm_file(my_rank) .ne. 0                      &
     &  .or. check_exsist_rj_file(my_rank) .ne.  0) iflag_lc = 1
      call MPI_allREDUCE(iflag_lc, iflag_gl, ione, CALYPSO_INTEGER,     &
     &    MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
      if(iflag_gl.eq.0) then
        if (my_rank.eq.0) write(*,*) 'spherical harmonics table exists'
      else if(iflag_make_SPH .eq. 0) then
        call calypso_mpi_abort(ierr_file,                               &
     &     'Set parameters for spherical shell')
      else
        if (my_rank.eq.0) write(*,*) 'Make spherical harmonics table'
        call para_gen_sph_grids(sph_gen)
        call deallocate_gen_mesh_params
      end if
      call calypso_mpi_barrier
!
      if (iflag_debug.eq.1) write(*,*) 'load_para_SPH_and_FEM_mesh'
      call load_para_SPH_and_FEM_mesh                                   &
     &   (sph, comms_sph, sph_grps, mesh, group, ele_mesh, mesh_file)
!
      end subroutine select_make_SPH_mesh
!
! ----------------------------------------------------------------------
!
      subroutine sph_boundary_IO_control(MHD_prop)
!
      use m_sph_boundary_input_data
      use check_read_bc_file
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
!
      integer(kind = kint) :: iflag
!
!
      iflag = check_read_boundary_files(MHD_prop)
      if (iflag .eq. id_no_boundary_file) return
!
      if (iflag_debug.eq.1) write(*,*) 'read_boundary_spectr_file'
      call read_boundary_spectr_file
!
      end subroutine sph_boundary_IO_control
!
! ----------------------------------------------------------------------
!
      end module input_control_sph_MHD
