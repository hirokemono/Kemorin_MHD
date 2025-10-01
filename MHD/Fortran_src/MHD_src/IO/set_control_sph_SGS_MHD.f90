!>@file   set_control_sph_SGS_MHD.f90
!!@brief  module set_control_sph_SGS_MHD
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Sep., 2009
!
!>@brief Set control data for spherical transform MHD dynamo simulation
!!
!!@verbatim
!!      subroutine set_control_4_SPH_SGS_MHD(plt, org_plt,              &
!!     &          model_ctl, smctl_ctl, psph_ctl, sgs_ctl,              &
!!     &          MHD_files, bc_IO, refs, SGS_par, dynamic_SPH,         &
!!     &          MHD_step, MHD_prop, MHD_BC, trans_p, WK, sph_maker)
!!        type(platform_data_control), intent(in) :: plt
!!        type(platform_data_control), intent(in) :: org_plt
!!        type(mhd_model_control), intent(in) :: model_ctl
!!        type(sph_mhd_control_control), intent(in) :: smctl_ctl
!!        type(sph_monitor_control), intent(inout) :: smonitor_ctl
!!        type(node_monitor_control), intent(in) :: nmtr_ctl
!!        type(parallel_sph_shell_control), intent(in) :: psph_ctl
!!        type(clust_filtering_ctl), intent(in) :: crust_filter_ctl
!!        type(SGS_model_control), intent(in) :: sgs_ctl
!!        type(node_monitor_control), intent(in) :: nmtr_ctl
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(MHD_file_IO_params), intent(inout) :: MHD_files
!!        type(boundary_spectra), intent(inout) :: bc_IO
!!        type(radial_reference_field), intent(inout) :: refs
!!        type(SGS_paremeters), intent(inout) :: SGS_par
!!        type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(MHD_evolution_param), intent(inout) :: MHD_prop
!!        type(MHD_BC_lists), intent(inout) :: MHD_BC
!!        type(parameters_4_sph_trans), intent(inout) :: trans_p
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!        type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
!!        type(sph_mhd_monitor_data), intent(inout) :: monitor
!!        type(node_monitor_IO), intent(inout) :: nod_mntr
!!@endverbatim
!
      module set_control_sph_SGS_MHD
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_control_parameter
      use t_MHD_step_parameter
      use t_MHD_file_parameter
      use t_field_data_IO
      use t_ctl_data_4_platforms
      use t_ctl_data_4_FEM_mesh
      use t_ctl_data_MHD_model
      use t_ctl_data_SPH_MHD_control
      use t_ctl_data_4_sph_monitor
      use t_ctl_data_node_monitor
      use t_ctl_data_gen_sph_shell
      use t_ctl_data_SGS_model
      use t_ctl_data_crust_filter
      use t_sph_grid_maker_in_sim
      use t_bc_data_list
      use t_flex_delta_t_data
      use t_field_on_circle
      use t_radial_reference_field
      use t_sph_grid_maker_in_sim
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_control_4_SPH_SGS_MHD(plt, org_plt,                &
     &          model_ctl, smctl_ctl, psph_ctl, sgs_ctl,                &
     &          MHD_files, bc_IO, refs, SGS_par, dynamic_SPH,           &
     &          MHD_step, MHD_prop, MHD_BC, trans_p, WK, sph_maker)
!
      use t_SGS_control_parameter
      use t_spheric_parameter
      use t_rms_4_sph_spectr
      use t_sph_filtering
      use t_sph_trans_arrays_MHD
      use t_const_spherical_grid
      use t_sph_boundary_input_data
      use t_ctl_params_gen_sph_shell
      use t_sph_trans_arrays_MHD
      use t_radial_reference_field
!
      use set_control_4_SGS
      use set_control_SGS_commute
      use set_control_sph_data_MHD
      use set_control_sph_mhd
      use set_control_sph_filter
      use set_field_data_w_SGS
!
      type(platform_data_control), intent(in) :: plt
      type(platform_data_control), intent(in) :: org_plt
!
      type(mhd_model_control), intent(in) :: model_ctl
      type(sph_mhd_control_control), intent(in) :: smctl_ctl
      type(parallel_sph_shell_control), intent(in) :: psph_ctl
      type(SGS_model_control), intent(in) :: sgs_ctl
!
      type(MHD_file_IO_params), intent(inout) :: MHD_files
      type(boundary_spectra), intent(inout) :: bc_IO
      type(radial_reference_field), intent(inout) :: refs
      type(SGS_paremeters), intent(inout) :: SGS_par
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
      type(MHD_step_param), intent(inout) :: MHD_step
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(MHD_BC_lists), intent(inout) :: MHD_BC
      type(parameters_4_sph_trans), intent(inout) :: trans_p
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(sph_grid_maker_in_sim), intent(inout) :: sph_maker
!
!   set parameters for SGS model
!
      if (iflag_debug.gt.0) write(*,*) 'set_control_SGS_model'
      call set_control_SGS_model                                        &
     &   (sgs_ctl, SGS_par%model_p, SGS_par%filter_p,                   &
     &    MHD_files%Csim_file_IO, SGS_par%i_step_sgs_coefs)
      call s_set_control_SGS_commute                                    &
     &   (sgs_ctl, SGS_par%model_p, SGS_par%commute_p,                  &
     &    MHD_files%Cdiff_file_IO)
!
      call set_control_SPH_SGS_filters                                  &
     &   (sgs_ctl, SGS_par%model_p, dynamic_SPH)
!
!   set parameters for data files
!
      call set_control_4_SPH_MHD(plt, org_plt,                          &
     &    model_ctl, smctl_ctl, psph_ctl, MHD_files, bc_IO,             &
     &    refs, MHD_step, MHD_prop, MHD_BC, trans_p, WK, sph_maker)
!
      end subroutine set_control_4_SPH_SGS_MHD
!
! ----------------------------------------------------------------------
!
      end module set_control_sph_SGS_MHD
