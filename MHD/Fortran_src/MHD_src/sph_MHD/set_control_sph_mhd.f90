!>@file   set_control_sph_mhd.f90
!!@brief  module set_control_sph_mhd
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Sep., 2009
!
!>@brief Set control data for spherical transform MHD dynamo simulation
!!
!!@verbatim
!!      subroutine set_control_SGS_SPH_MHD(plt, org_plt,                &
!!     &         model_ctl, ctl_ctl, smonitor_ctl, nmtr_ctl, psph_ctl,  &
!!     &         sph_gen, rj_fld, mesh_file, sph_file_param,            &
!!     &         MHD_org_files, sph_fst_IO, pwr, SGS_par, sph_filters,  &
!!     &         MHD_step, MHD_prop, WK_sph, gen_sph)
!!      subroutine set_control_4_SPH_MHD(plt, org_plt,                  &
!!     &          model_ctl, ctl_ctl, smonitor_ctl, nmtr_ctl, psph_ctl, &
!!     &          sph_gen, rj_fld, mesh_file, sph_file_param,           &
!!     &          MHD_org_files, sph_fst_IO, pwr, SGS_par, MHD_step,    &
!!     &          MHD_prop, WK_sph, gen_sph)
!!        type(platform_data_control), intent(in) :: plt
!!        type(platform_data_control), intent(in) :: org_plt
!!        type(mhd_model_control), intent(inout) :: model_ctl
!!        type(mhd_control_control), intent(inout) :: ctl_ctl
!!        type(sph_monitor_control), intent(inout) :: smonitor_ctl
!!        type(node_monitor_control), intent(inout) :: nmtr_ctl
!!        type(parallel_sph_shell_control), intent(inout) :: psph_ctl
!!        type(sph_grids), intent(inout) :: sph_gen
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(field_IO_params), intent(inout) :: mesh_file
!!        type(field_IO_params), intent(inout) :: sph_file_param
!!        type(file_params_4_sph_mhd), intent(inout) :: MHD_org_files
!!        type(field_IO), intent(inout) :: sph_fst_IO
!!        type(sph_mean_squares), intent(inout) :: pwr
!!        type(SGS_paremeters), intent(inout) :: SGS_par
!!        type(sph_filters_type), intent(inout) :: sph_filters(1)
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(fluid_property), intent(inout) :: fl_prop
!!        type(MHD_evolution_param), intent(inout) :: MHD_prop
!!        type(spherical_trns_works), intent(inout) :: WK_sph
!!        type(construct_spherical_grid), intent(inout) :: gen_sph
!!@endverbatim
!
      module set_control_sph_mhd
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_control_parameter
      use t_MHD_step_parameter
      use t_file_IO_parameter
      use t_field_data_IO
      use t_SPH_MHD_file_parameters
      use t_ctl_data_4_platforms
      use t_ctl_data_MHD_model
      use t_ctl_data_MHD_control
      use t_ctl_data_4_sph_monitor
      use t_ctl_data_node_monitor
      use t_ctl_data_gen_sph_shell
      use t_sph_transforms
!
      implicit none
!
      private :: set_control_SPH_MHD_bcs
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_control_SGS_SPH_MHD(plt, org_plt,                  &
     &         model_ctl, ctl_ctl, smonitor_ctl, nmtr_ctl, psph_ctl,    &
     &         sph_gen, rj_fld, mesh_file, sph_file_param,              &
     &         MHD_org_files, sph_fst_IO, pwr, SGS_par, sph_filters,    &
     &         MHD_step, MHD_prop, WK_sph, gen_sph)
!
      use m_spheric_global_ranks
      use m_ucd_data
      use sph_mhd_rms_IO
!
      use t_SGS_control_parameter
      use t_spheric_parameter
      use t_phys_data
      use t_rms_4_sph_spectr
      use t_sph_filtering_data
      use t_const_spherical_grid
!
      use set_control_4_SGS
!
      type(platform_data_control), intent(in) :: plt
      type(platform_data_control), intent(in) :: org_plt
      type(mhd_model_control), intent(inout) :: model_ctl
      type(mhd_control_control), intent(inout) :: ctl_ctl
      type(sph_monitor_control), intent(inout) :: smonitor_ctl
      type(node_monitor_control), intent(inout) :: nmtr_ctl
      type(parallel_sph_shell_control), intent(inout) :: psph_ctl
      type(sph_grids), intent(inout) :: sph_gen
      type(phys_data), intent(inout) :: rj_fld
      type(field_IO_params), intent(inout) :: mesh_file
      type(field_IO_params), intent(inout) :: sph_file_param
      type(file_params_4_sph_mhd), intent(inout) :: MHD_org_files
      type(field_IO), intent(inout) :: sph_fst_IO
      type(sph_mean_squares), intent(inout) :: pwr
      type(SGS_paremeters), intent(inout) :: SGS_par
      type(sph_filters_type), intent(inout) :: sph_filters(1)
      type(MHD_step_param), intent(inout) :: MHD_step
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(construct_spherical_grid), intent(inout) :: gen_sph
!
!
!   set parameters for SGS model
!
      if (iflag_debug.gt.0) write(*,*) 'set_control_SGS_model'
      call set_control_SGS_model(model_ctl%sgs_ctl,                     &
     &    SGS_par%model_p, SGS_par%commute_p, SGS_par%filter_p)
!
      if(SGS_par%model_p%iflag_SGS .ne. id_SGS_none) then
        call set_control_SPH_SGS                                        &
     &     (model_ctl%sgs_ctl%num_sph_filter_ctl,                       &
     &      model_ctl%sgs_ctl%sph_filter_ctl(1), sph_filters(1))
      end if
      if(model_ctl%sgs_ctl%num_sph_filter_ctl .gt. 0) then
        call dealloc_sph_filter_ctl(model_ctl%sgs_ctl)
      end if
!
      call set_control_4_SPH_MHD(plt, org_plt,                          &
     &    model_ctl, ctl_ctl, smonitor_ctl, nmtr_ctl, psph_ctl,         &
     &    sph_gen, rj_fld, mesh_file, sph_file_param, MHD_org_files,    &
     &    sph_fst_IO, pwr, SGS_par, MHD_step, MHD_prop,                 &
     &    WK_sph, gen_sph)
!
      end subroutine set_control_SGS_SPH_MHD
!
! ----------------------------------------------------------------------
!
      subroutine set_control_4_SPH_MHD(plt, org_plt,                    &
     &          model_ctl, ctl_ctl, smonitor_ctl, nmtr_ctl, psph_ctl,   &
     &          sph_gen, rj_fld, mesh_file, sph_file_param,             &
     &          MHD_org_files, sph_fst_IO, pwr, SGS_par, MHD_step,      &
     &          MHD_prop, WK_sph, gen_sph)
!
      use m_spheric_global_ranks
      use m_ucd_data
      use m_flexible_time_step
      use sph_mhd_rms_IO
!
      use t_SGS_control_parameter
      use t_spheric_parameter
      use t_phys_data
      use t_rms_4_sph_spectr
      use t_sph_trans_arrays_MHD
      use t_const_spherical_grid
!
      use gen_sph_grids_modes
      use set_control_platform_data
      use set_ctl_parallel_platform
      use set_control_4_model
      use set_control_sph_data_MHD
      use set_control_4_force
      use set_control_4_normalize
      use set_control_4_time_steps
!
      use set_control_4_pickup_sph
      use set_ctl_gen_shell_grids
!
      type(platform_data_control), intent(in) :: plt
      type(platform_data_control), intent(in) :: org_plt
!
      type(mhd_model_control), intent(inout) :: model_ctl
      type(mhd_control_control), intent(inout) :: ctl_ctl
      type(sph_monitor_control), intent(inout) :: smonitor_ctl
      type(node_monitor_control), intent(inout) :: nmtr_ctl
      type(parallel_sph_shell_control), intent(inout) :: psph_ctl
      type(sph_grids), intent(inout) :: sph_gen
      type(phys_data), intent(inout) :: rj_fld
      type(field_IO_params), intent(inout) :: mesh_file
      type(field_IO_params), intent(inout) :: sph_file_param
      type(file_params_4_sph_mhd), intent(inout) :: MHD_org_files
      type(field_IO), intent(inout) :: sph_fst_IO
      type(SGS_paremeters), intent(inout) :: SGS_par
      type(MHD_step_param), intent(inout) :: MHD_step
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(sph_mean_squares), intent(inout) :: pwr
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(construct_spherical_grid), intent(inout) :: gen_sph
!
      integer(kind = kint) :: ierr
!
!
!   set parameters for data files
!
      call turn_off_debug_flag_by_ctl(my_rank, plt)
      call check_control_num_domains(plt)
      call set_control_smp_def(my_rank, plt)
      call set_control_mesh_def(plt, mesh_file)
      call set_FEM_mesh_switch_4_SPH(plt, iflag_output_mesh)
      call set_control_sph_mesh(plt, mesh_file, sph_file_param)
      call set_control_restart_file_def(plt, sph_fst_IO)
      call set_control_MHD_field_file(plt)
      call set_control_org_sph_files(org_plt, MHD_org_files)
!
      call s_set_control_4_model                                        &
     &    (model_ctl%reft_ctl, model_ctl%refc_ctl,                      &
     &     ctl_ctl%mevo_ctl, model_ctl%evo_ctl, nmtr_ctl, MHD_prop)
!
!   set spherical shell parameters
!
      if(psph_ctl%iflag_sph_shell .gt. 0) then
        if (iflag_debug.gt.0) write(*,*) 'set_control_4_shell_grids'
        call set_control_4_shell_grids                                  &
     &     (nprocs, psph_ctl%spctl, psph_ctl%sdctl, sph_gen,            &
     &      gen_sph, ierr)
      end if
!
!   set forces
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_force'
      call s_set_control_4_force(model_ctl%frc_ctl, model_ctl%g_ctl,    &
     &    model_ctl%cor_ctl, model_ctl%mcv_ctl,                         &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop)
!
!   set parameters for general information
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_sph_data_MHD'
      call s_set_control_sph_data_MHD                                   &
     &   (SGS_par%model_p, MHD_prop, plt,                               &
     &    model_ctl%fld_ctl%field_ctl, ctl_ctl%mevo_ctl,                &
     &    MHD_org_files%rj_file_param, MHD_org_files%rst_file_param,    &
     &    rj_fld, WK_sph)
!
!   set control parameters
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_normalize'
      call s_set_control_4_normalize                                    &
     &   (MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    model_ctl%dless_ctl, model_ctl%eqs_ctl)
!
!   set boundary conditions
!
      call set_control_SPH_MHD_bcs                                      &
     &   (MHD_prop, model_ctl%nbc_ctl, model_ctl%sbc_ctl)
!
!   set control parameters
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_time_steps'
      call s_set_control_4_time_steps(flex_p1, SGS_par, MHD_step,       &
     &    ctl_ctl%mrst_ctl, ctl_ctl%tctl)
!
      call s_set_control_4_crank(ctl_ctl%mevo_ctl,                      &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop)
!
!   set_pickup modes
!
      call set_ctl_params_layered_spectr(smonitor_ctl%lp_ctl, pwr)
      call set_ctl_params_sph_spectr(smonitor_ctl, pwr)
!
      call set_ctl_params_pick_sph(smonitor_ctl%pspec_ctl,              &
     &    pickup_sph_head, pick_list1, pick1)
!
      call set_ctl_params_pick_gauss(smonitor_ctl%g_pwr,                &
     &    gauss_coefs_file_head, gauss_list1, gauss1)
!
      call set_ctl_params_no_heat_Nu(smonitor_ctl%Nusselt_file_prefix,  &
     &    rj_fld, Nu_type1)
!
      end subroutine set_control_4_SPH_MHD
!
! ----------------------------------------------------------------------
!
      subroutine set_control_SPH_MHD_bcs(MHD_prop, nbc_ctl, sbc_ctl)
!
      use t_ctl_data_node_boundary
      use t_ctl_data_surf_boundary
!
      use set_control_4_velo
      use set_control_4_press
      use set_control_4_temp
      use set_control_4_magne
      use set_control_4_composition
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(node_bc_control), intent(inout) :: nbc_ctl
      type(surf_bc_control), intent(inout) :: sbc_ctl
!
!
!   set boundary conditions for temperature
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_temp'
      call s_set_control_4_temp(MHD_prop%ht_prop,                       &
     &    nbc_ctl%node_bc_T_ctl, sbc_ctl%surf_bc_HF_ctl)
!
!   set boundary conditions for velocity
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_velo'
      call s_set_control_4_velo(MHD_prop%fl_prop,                       &
     &    nbc_ctl%node_bc_U_ctl, sbc_ctl%surf_bc_ST_ctl)
!
!  set boundary conditions for pressure
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_press'
      call s_set_control_4_press(MHD_prop%fl_prop,                      &
     &    nbc_ctl%node_bc_P_ctl, sbc_ctl%surf_bc_PN_ctl)
!
!   set boundary conditions for composition variation
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_composition'
      call s_set_control_4_composition(MHD_prop%cp_prop,                &
     &    nbc_ctl%node_bc_C_ctl, sbc_ctl%surf_bc_CF_ctl)
!
!   set boundary_conditons for magnetic field
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_magne'
      call s_set_control_4_magne(MHD_prop%cd_prop,                      &
     &    nbc_ctl%node_bc_B_ctl, sbc_ctl%surf_bc_BN_ctl)
!
      end subroutine set_control_SPH_MHD_bcs
!
! ----------------------------------------------------------------------
!
      end module set_control_sph_mhd
