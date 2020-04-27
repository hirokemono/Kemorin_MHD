!>@file   lead_fields_SPH_SGS_MHD.f90
!!@brief  module lead_fields_SPH_SGS_MHD
!!
!!@author H. Matsui
!!@date Programmed in Aug, 2007
!
!>@brief  Evaluate pressure and energy fluxes for snapshots
!!
!!@verbatim
!!      subroutine lead_fields_4_SPH_SGS_MHD(SGS_par, monitor,          &
!!     &          r_2nd, MHD_prop, sph_MHD_bc, trans_p, ipol_LES,       &
!!     &          sph_MHD_mat, WK, WK_LES, dynamic_SPH, SPH_MHD)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(sph_mhd_monitor_data), intent(in) :: monitor
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(SGS_model_addresses), intent(in) :: ipol_LES
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!        type(works_4_sph_trans_SGS_MHD), intent(inout) :: WK_LES
!!        type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!!        type(MHD_radial_matrices), intent(inout) :: sph_MHD_mat
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!@endverbatim
!
      module lead_fields_SPH_SGS_MHD
!
      use m_precision
      use m_machine_parameter
!
      use t_control_parameter
      use t_SGS_control_parameter
      use t_SPH_mesh_field_data
      use t_fdm_coefs
      use t_sph_trans_arrays_MHD
      use t_sph_trans_arrays_SGS_MHD
      use t_sph_matrices
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_sph_transforms
      use t_boundary_data_sph_MHD
      use t_radial_matrices_sph_MHD
      use t_sph_filtering
      use t_phys_address
      use t_SGS_model_addresses
!
      implicit none
!
      private :: pressure_SGS_SPH_MHD
      private :: enegy_fluxes_SPH_SGS_MHD, lead_SGS_terms_4_SPH
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine lead_fields_4_SPH_SGS_MHD(SGS_par, monitor,            &
     &          r_2nd, MHD_prop, sph_MHD_bc, trans_p, ipol_LES,         &
     &          sph_MHD_mat, WK, WK_LES, dynamic_SPH, SPH_MHD)
!
      use t_sph_mhd_monitor_data_IO
      use sph_transforms_4_MHD
      use cal_energy_flux_rtp
      use swap_phi_order_4_sph_trans
      use lead_fields_4_sph_mhd
      use self_buoyancy_w_filter_sph
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(sph_mhd_monitor_data), intent(in) :: monitor
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(SGS_model_addresses), intent(in) :: ipol_LES
!
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(works_4_sph_trans_SGS_MHD), intent(inout) :: WK_LES
      type(MHD_radial_matrices), intent(inout) :: sph_MHD_mat
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!
!
      call cal_self_buoyancy_sph_SGS_MHD                                &
     &   (SPH_MHD%sph, trans_p%leg, SPH_MHD%ipol, ipol_LES,             &
     &    MHD_prop, sph_MHD_bc%sph_bc_U, SPH_MHD%fld)
!
      if(MHD_prop%fl_prop%iflag_scheme .gt. id_no_evolution) then
        call pressure_SGS_SPH_MHD                                       &
     &     (SGS_par%model_p, SPH_MHD%sph, MHD_prop, sph_MHD_bc,         &
     &      r_2nd, trans_p%leg, sph_MHD_mat%band_p_poisson,             &
     &      SPH_MHD%ipol, ipol_LES, SPH_MHD%fld)
      end if
!
!
      call lead_fields_by_sph_trans(SPH_MHD%sph, SPH_MHD%comms,         &
     &    MHD_prop, trans_p, WK%trns_MHD, SPH_MHD%fld)
      call gradients_of_vectors_sph                                     &
     &   (SPH_MHD%sph, SPH_MHD%comms, r_2nd, sph_MHD_bc, trans_p,       &
     &    SPH_MHD%ipol, WK%trns_MHD, WK%trns_tmp, WK%WK_sph,            &
     &    SPH_MHD%fld)
!
      call lead_SGS_terms_4_SPH                                         &
     &   (SGS_par%model_p, SPH_MHD%sph, SPH_MHD%comms, trans_p,         &
     &    WK_LES%trns_Csim, WK_LES%trns_SGS, WK_LES%trns_SGS_snap,      &
     &    dynamic_SPH, SPH_MHD%fld)
!
      call enegy_fluxes_SPH_SGS_MHD(monitor%ltr_crust,                  &
     &    SGS_par%model_p, SPH_MHD%sph, SPH_MHD%comms,                  &
     &    r_2nd, MHD_prop, sph_MHD_bc, trans_p,                         &
     &    SPH_MHD%ipol, ipol_LES, WK%trns_MHD, WK_LES%trns_SGS,         &
     &    WK%trns_snap, WK_LES%trns_SGS_snap, WK%WK_sph, SPH_MHD%fld)
!
      end subroutine lead_fields_4_SPH_SGS_MHD
!
! ----------------------------------------------------------------------
!
      subroutine pressure_SGS_SPH_MHD(SGS_param, sph, MHD_prop,         &
     &          sph_MHD_bc, r_2nd, leg, band_p_poisson,                 &
     &          ipol, ipol_LES, rj_fld)
!
      use cal_sol_sph_fluid_crank
!
      use cal_sph_field_by_rotation
      use const_radial_forces_on_bc
      use cal_div_of_forces
      use cal_div_of_SGS_forces
      use const_sph_radial_grad
      use cal_sph_rotation_of_SGS
      use self_buoyancy_w_filter_sph
      use self_buoyancy_w_fil_on_sph
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(sph_grids), intent(in) :: sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(legendre_4_sph_trans), intent(in) :: leg
      type(band_matrices_type), intent(in) :: band_p_poisson
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (iflag_debug.eq.1) write(*,*) 'cal_div_of_forces_sph_2'
      call cal_div_of_forces_sph_2                                      &
     &   (sph%sph_rj, r_2nd, MHD_prop, sph_MHD_bc,                      &
     &    leg%g_sph_rj, ipol, rj_fld)
      call cal_div_buoyancy_w_fil_sph_2(sph, r_2nd, leg, MHD_prop,      &
     &    sph_MHD_bc%sph_bc_U, ipol_LES, rj_fld)
!
!
      call r_buoyancy_on_sphere_w_filter                                &
     &   (sph_MHD_bc%sph_bc_U%kr_in,  sph%sph_rj, ipol, ipol_LES,       &
     &    MHD_prop%fl_prop, MHD_prop%ref_param_T, MHD_prop%ref_param_C, &
     &    rj_fld)
      call r_buoyancy_on_sphere_w_filter                                &
     &   (sph_MHD_bc%sph_bc_U%kr_out, sph%sph_rj, ipol, ipol_LES,       &
     &    MHD_prop%fl_prop, MHD_prop%ref_param_T, MHD_prop%ref_param_C, &
     &    rj_fld)
!
      call s_const_radial_forces_on_bc(sph%sph_rj, leg%g_sph_rj,        &
     &    MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U, MHD_prop%ref_param_T,  &
     &    MHD_prop%ref_param_C, ipol, rj_fld)
!
      call sum_div_of_forces                                            &
     &   (MHD_prop%fl_prop, ipol%base, ipol%div_forces, rj_fld)
      call add_div_of_filtered_buoyancies(MHD_prop%fl_prop,             &
     &    ipol%base, ipol_LES%div_frc_by_filter, rj_fld)
!
!   ----  Add divegence of SGS terms
      if(SGS_param%iflag_SGS .gt. id_SGS_none) then
        call cal_div_of_SGS_forces_sph_2                                &
     &     (sph%sph_rj, r_2nd, sph_MHD_bc, leg%g_sph_rj,                &
     &      ipol%div_forces, ipol_LES%SGS_term, ipol_LES%div_SGS,       &
     &      rj_fld)
        call sum_div_of_SGS_forces(ipol%base, ipol_LES%div_SGS, rj_fld)
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sol_pressure_by_div_v'
      call cal_sol_pressure_by_div_v(sph%sph_rj, sph_MHD_bc%sph_bc_U,   &
     &    band_p_poisson, ipol, rj_fld)
!
      if(ipol%forces%i_press_grad .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'const_pressure_gradient'
        call const_pressure_gradient                                    &
     &     (sph%sph_rj, r_2nd, sph_MHD_bc%sph_bc_U,                     &
     &      leg%g_sph_rj, MHD_prop%fl_prop%coef_press,                  &
     &      ipol%base%i_press, ipol%forces%i_press_grad, rj_fld)
      end if
!
      end subroutine pressure_SGS_SPH_MHD
!
! ----------------------------------------------------------------------
!
      subroutine enegy_fluxes_SPH_SGS_MHD(ltr_crust, SGS_param,         &
     &          sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc,            &
     &          trans_p, ipol, ipol_LES, trns_MHD, trns_SGS,            &
     &          trns_snap, trns_SGS_snap, WK_sph, rj_fld)
!
      use sph_transforms_snapshot
      use lead_fields_4_sph_mhd
      use cal_SGS_terms_sph_MHD
      use cal_SGS_buo_flux_sph_MHD
      use cal_energy_flux_w_SGS_rtp
      use cal_force_with_SGS_rj
!
      integer(kind = kint), intent(in) :: ltr_crust
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
!
      type(address_4_sph_trans), intent(in) :: trns_MHD
      type(SGS_address_sph_trans), intent(in) :: trns_SGS
      type(address_4_sph_trans), intent(inout) :: trns_snap
      type(SGS_address_sph_trans), intent(inout) :: trns_SGS_snap
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(phys_data), intent(inout) :: rj_fld
!
!
      call cal_sph_enegy_fluxes                                         &
     &   (ltr_crust, sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc,       &
     &    trans_p, ipol, trns_MHD, trns_snap, WK_sph, rj_fld)
!
      if (iflag_debug.eq.1) write(*,*) 's_cal_force_with_SGS_rj'
      call s_cal_force_with_SGS_rj                                      &
     &   (ipol%forces, ipol_LES%SGS_term, ipol_LES%frc_w_SGS, rj_fld)
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &                          'backward transform for SGS snapshot'
      call sph_back_trans_snapshot_MHD(sph, comms_sph, trans_p,         &
     &    rj_fld, trns_SGS_snap%backward, WK_sph)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_filterd_buo_flux_rtp'
      call cal_filterd_buo_flux_rtp(sph%sph_rtp, MHD_prop%fl_prop,      &
     &    trns_snap%b_trns%base, trns_SGS_snap%b_trns%filter_fld,       &
     &    trns_SGS_snap%f_trns_LES%eflux_by_filter,                     &
     &    trns_SGS_snap%backward, trns_SGS_snap%forward)
!
!      Work of SGS terms
      if(SGS_param%iflag_SGS .gt. id_SGS_none) then
        if (iflag_debug.eq.1) write(*,*) 'SGS_fluxes_for_snapshot'
        call SGS_fluxes_for_snapshot                                    &
     &     (sph%sph_rtp, MHD_prop%fl_prop, trns_MHD%b_trns%base,        &
     &      trns_SGS%f_trns_LES%SGS_term,                               &
     &      trns_SGS_snap%b_trns_LES%SGS_term,                          &
     &      trns_SGS_snap%f_trns_LES%SGS_ene_flux,                      &
     &      trns_MHD%backward, trns_SGS%forward,                        &
     &      trns_SGS_snap%backward, trns_SGS_snap%forward)
      end if
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &                          'forward transform for snapshot'
      call sph_forward_trans_snapshot_MHD(sph, comms_sph, trans_p,      &
     &    trns_snap%forward, WK_sph, rj_fld)
      if (iflag_debug.eq.1) write(*,*)                                  &
     &                          'forward transform for SGS snapshot'
      call sph_forward_trans_snapshot_MHD(sph, comms_sph, trans_p,      &
     &    trns_SGS_snap%forward, WK_sph, rj_fld)
!
      end subroutine enegy_fluxes_SPH_SGS_MHD
!
! ----------------------------------------------------------------------
!
      subroutine lead_SGS_terms_4_SPH                                   &
     &         (SGS_param, sph, comms_sph, trans_p,                     &
     &          trns_Csim, trns_SGS, trns_SGS_snap,                     &
     &          dynamic_SPH, rj_fld)
!
      use sph_transforms_4_SGS
      use swap_phi_order_4_sph_trans
      use copy_Csim_4_sph_MHD
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(SGS_address_sph_trans), intent(in) :: trns_Csim
!
      type(SGS_address_sph_trans), intent(inout) :: trns_SGS
      type(SGS_address_sph_trans), intent(inout) :: trns_SGS_snap
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(SGS_param%iflag_SGS .eq. id_SGS_none) return
      if (iflag_debug.eq.1) write(*,*) 'swap_phi_from_MHD_trans'
      call swap_phi_from_MHD_trans                                      &
     &   (sph%sph_rtp, trns_SGS%backward, trns_SGS%forward)
!
      if (iflag_debug.eq.1) write(*,*) 'sph_pole_trans_SGS_MHD'
      call sph_pole_trans_SGS_MHD                                       &
     &   (sph, comms_sph, trans_p, rj_fld, trns_SGS%backward)
!
      if(SGS_param%iflag_dynamic .gt. id_SGS_none) then
        if(iflag_debug.eq.1) write(*,*) 'copy_model_coefs_4_sph_snap'
        call copy_model_coefs_4_sph_snap                                &
     &     (sph%sph_rtp, dynamic_SPH%sph_d_grp,                         &
     &      dynamic_SPH%iak_sgs_term, trns_Csim%f_trns_LES%Csim,        &
     &      dynamic_SPH%wk_sgs, trns_SGS_snap%forward)
      end if
!
      end subroutine lead_SGS_terms_4_SPH
!
! ----------------------------------------------------------------------
!
      end module lead_fields_SPH_SGS_MHD
