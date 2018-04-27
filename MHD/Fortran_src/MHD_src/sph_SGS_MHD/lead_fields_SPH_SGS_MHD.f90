!>@file   lead_fields_SPH_SGS_MHD.f90
!!@brief  module lead_fields_SPH_SGS_MHD
!!
!!@author H. Matsui
!!@date Programmed in Aug, 2007
!
!>@brief  Evaluate pressure and energy fluxes for snapshots
!!
!!@verbatim
!!      subroutine lead_fields_4_SPH_SGS_MHD                            &
!!     &         (SGS_par, r_2nd, MHD_prop, sph_MHD_bc, trans_p,        &
!!     &          sph_MHD_mat, WK, dynamic_SPH, SPH_MHD)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
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
      use t_addresses_sph_transform
      use t_sph_trans_arrays_MHD
      use t_sph_matrices
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_sph_transforms
      use t_boundary_data_sph_MHD
      use t_radial_matrices_sph_MHD
      use t_sph_filtering
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
      subroutine lead_fields_4_SPH_SGS_MHD                              &
     &         (SGS_par, r_2nd, MHD_prop, sph_MHD_bc, trans_p,          &
     &          sph_MHD_mat, WK, dynamic_SPH, SPH_MHD)
!
      use sph_transforms_4_MHD
      use cal_buoyancies_sph_MHD
      use cal_energy_flux_rtp
      use swap_phi_order_4_sph_trans
      use lead_fields_4_sph_mhd
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(parameters_4_sph_trans), intent(in) :: trans_p
!
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
      type(MHD_radial_matrices), intent(inout) :: sph_MHD_mat
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!
!
      call sel_buoyancies_sph_MHD                                       &
     &   (SPH_MHD%sph%sph_rj, trans_p%leg, SPH_MHD%ipol,                &
     &    MHD_prop%fl_prop, MHD_prop%ref_param_T, MHD_prop%ref_param_C, &
     &    sph_MHD_bc%sph_bc_U, SPH_MHD%fld)
!
      if(MHD_prop%fl_prop%iflag_scheme .gt. id_no_evolution) then
        call pressure_SGS_SPH_MHD                                       &
     &     (SGS_par%model_p, SPH_MHD%sph, MHD_prop, sph_MHD_bc, r_2nd,  &
     &      trans_p%leg, sph_MHD_mat%band_p_poisson, SPH_MHD%ipol,      &
     &      SPH_MHD%fld)
      end if
!
!
      call lead_fields_by_sph_trans(SPH_MHD%sph, SPH_MHD%comms,         &
     &    MHD_prop, trans_p, WK, SPH_MHD%fld)
!
      call lead_SGS_terms_4_SPH                                         &
     &   (SGS_par%model_p, SPH_MHD%sph, SPH_MHD%comms,                  &
     &    trans_p, WK, dynamic_SPH, SPH_MHD%fld)
!
      call gradients_of_vectors_sph                                     &
     &   (SPH_MHD%sph, SPH_MHD%comms, r_2nd, sph_MHD_bc, trans_p,       &
     &    SPH_MHD%ipol, WK%trns_MHD, WK%trns_tmp, WK%WK_sph,            &
     &    SPH_MHD%fld)
      call enegy_fluxes_SPH_SGS_MHD                                     &
     &   (SGS_par%model_p, SPH_MHD%sph, SPH_MHD%comms,                  &
     &    r_2nd, MHD_prop, sph_MHD_bc, trans_p, SPH_MHD%ipol,           &
     &    WK%trns_MHD, WK%trns_SGS, WK%trns_snap, WK%WK_sph,            &
     &    SPH_MHD%fld)
!
      end subroutine lead_fields_4_SPH_SGS_MHD
!
! ----------------------------------------------------------------------
!
      subroutine pressure_SGS_SPH_MHD(SGS_param, sph, MHD_prop,         &
     &          sph_MHD_bc, r_2nd, leg, band_p_poisson, ipol, rj_fld)
!
      use cal_sol_sph_fluid_crank
!
      use cal_sph_field_by_rotation
      use const_radial_forces_on_bc
      use cal_div_of_forces
      use const_sph_radial_grad
      use cal_sph_rotation_of_SGS
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(sph_grids), intent(in) :: sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(legendre_4_sph_trans), intent(in) :: leg
      type(band_matrices_type), intent(in) :: band_p_poisson
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (iflag_debug.eq.1) write(*,*) 'cal_div_of_forces_sph_2'
      call cal_div_of_forces_sph_2                                      &
     &   (sph%sph_rj, r_2nd, MHD_prop, sph_MHD_bc,                      &
     &    leg%g_sph_rj, ipol, rj_fld)
!
!   ----  Lead SGS terms
      if(SGS_param%iflag_SGS .gt. id_SGS_none) then
        call cal_div_of_SGS_forces_sph_2                                &
     &     (sph%sph_rj, r_2nd, sph_MHD_bc, leg%g_sph_rj, ipol, rj_fld)
        call sum_div_of_SGS_forces(MHD_prop%fl_prop, ipol, rj_fld)
      end if
!
      call s_const_radial_forces_on_bc(sph%sph_rj, leg%g_sph_rj,        &
     &    MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U, MHD_prop%ref_param_T,  &
     &    MHD_prop%ref_param_C, ipol, rj_fld)
!
      call sum_div_of_forces(MHD_prop%fl_prop, ipol, rj_fld)
!
!   ----  Lead SGS terms
      if(SGS_param%iflag_SGS .gt. id_SGS_none) then
        call cal_div_of_SGS_forces_sph_2                                &
     &     (sph%sph_rj, r_2nd, sph_MHD_bc, leg%g_sph_rj, ipol, rj_fld)
        call sum_div_of_SGS_forces(MHD_prop%fl_prop, ipol, rj_fld)
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sol_pressure_by_div_v'
      call cal_sol_pressure_by_div_v(sph%sph_rj, sph_MHD_bc%sph_bc_U,   &
     &    band_p_poisson, ipol, rj_fld)
!
      if(ipol%i_press_grad .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'const_pressure_gradient'
        call const_pressure_gradient                                    &
     &     (sph%sph_rj, r_2nd, sph_MHD_bc%sph_bc_U,                     &
     &      leg%g_sph_rj, MHD_prop%fl_prop%coef_press,                  &
     &      ipol%i_press, ipol%i_press_grad, rj_fld)
      end if
!
      end subroutine pressure_SGS_SPH_MHD
!
! ----------------------------------------------------------------------
!
      subroutine enegy_fluxes_SPH_SGS_MHD                               &
     &         (SGS_param, sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc, &
     &          trans_p, ipol, trns_MHD, trns_SGS, trns_snap,           &
     &          WK_sph, rj_fld)
!
      use sph_transforms_snapshot
      use lead_fields_4_sph_mhd
      use cal_SGS_terms_sph_MHD
      use cal_SGS_buo_flux_sph_MHD
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
!
      type(address_4_sph_trans), intent(in) :: trns_MHD
      type(address_4_sph_trans), intent(in) :: trns_SGS
      type(address_4_sph_trans), intent(inout) :: trns_snap
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(phys_data), intent(inout) :: rj_fld
!
!
      call cal_sph_enegy_fluxes                                         &
     &   (sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc, trans_p,         &
     &    ipol, trns_MHD, trns_snap,  WK_sph, rj_fld)
!
!      Work of SGS terms
      if(SGS_param%iflag_SGS .gt. id_SGS_none) then
        if (iflag_debug.eq.1) write(*,*) 'SGS_fluxes_for_snapshot'
        call SGS_fluxes_for_snapshot                                    &
     &     (sph%sph_rtp, MHD_prop%fl_prop, trns_MHD%b_trns,             &
     &      trns_SGS%f_trns, trns_snap%b_trns, trns_snap%f_trns,        &
     &      trns_MHD%backward%ncomp, trns_SGS%forward%ncomp,            &
     &      trns_snap%backward%ncomp, trns_snap%forward%ncomp,          &
     &      trns_MHD%backward%fld_rtp, trns_SGS%forward%fld_rtp,        &
     &      trns_snap%backward%fld_rtp, trns_snap%forward%fld_rtp)
      end if
!
      if (iflag_debug.eq.1) write(*,*)                                  &
     &                          'sph_forward_trans_snapshot_MHD'
      call sph_forward_trans_snapshot_MHD                               &
     &   (sph, comms_sph, trans_p, trns_snap%forward, WK_sph, rj_fld)
      call calypso_mpi_barrier
!
      end subroutine enegy_fluxes_SPH_SGS_MHD
!
! ----------------------------------------------------------------------
!
      subroutine lead_SGS_terms_4_SPH(SGS_param, sph, comms_sph,        &
     &          trans_p, WK, dynamic_SPH, rj_fld)
!
      use sph_transforms_4_SGS
      use swap_phi_order_4_sph_trans
      use copy_Csim_4_sph_MHD
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
!
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(SGS_param%iflag_SGS .eq. id_SGS_none) return
      if (iflag_debug.eq.1) write(*,*) 'swap_phi_from_MHD_trans'
      call swap_phi_from_MHD_trans(sph%sph_rtp, WK%trns_SGS)
!
      if (iflag_debug.eq.1) write(*,*) 'sph_pole_trans_SGS_MHD'
      call sph_pole_trans_SGS_MHD                                       &
     &   (sph, comms_sph, trans_p, rj_fld, WK%trns_SGS)
!
      if(SGS_param%iflag_dynamic .gt. id_SGS_none) then
        if(iflag_debug.eq.1) write(*,*) 'copy_model_coefs_4_sph_snap'
        call copy_model_coefs_4_sph_snap                                &
     &     (sph%sph_rtp, dynamic_SPH%sph_d_grp, dynamic_SPH%ifld_sgs,   &
     &      dynamic_SPH%wk_sgs, WK%trns_snap)
      end if
!
      end subroutine lead_SGS_terms_4_SPH
!
! ----------------------------------------------------------------------
!
      end module lead_fields_SPH_SGS_MHD
