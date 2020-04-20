!>@file   dynamic_model_sph_MHD.f90
!!@brief  module dynamic_model_sph_MHD
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Evaluate nonlinear terms in spherical coordinate grid
!!
!!@verbatim
!!      subroutine const_model_coefs_4_sph(SGS_param, sph_rtp,          &
!!     &          fg_trns_SGS, bw_trns_wSGS, bd_trns_dSGS,              &
!!     &          trns_f_SIMI, trns_b_wide, trns_b_dble, dynamic_SPH)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(SGS_term_address), intent(in) :: fg_trns_SGS
!!        type(SGS_term_address), intent(in) :: bw_trns_wSGS
!!        type(SGS_term_address), intent(in) :: bd_trns_dSGS
!!        type(address_each_sph_trans), intent(in) :: trns_f_SIMI
!!        type(address_each_sph_trans), intent(in) :: trns_b_wide
!!        type(address_each_sph_trans), intent(in) :: trns_b_dble
!!        type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!!
!!      subroutine const_dynamic_SGS_4_buo_sph(stab_weight, sph_rtp,    &
!!     &          fl_prop, trns_MHD, trns_SGS, trns_Csim, dynamic_SPH)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(address_4_sph_trans), intent(in) :: trns_MHD
!!        type(address_4_sph_trans), intent(in) :: trns_SGS
!!        type(address_4_sph_trans), intent(inout) :: trns_Csim
!!        type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!!@endverbatim
!
      module dynamic_model_sph_MHD
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
      use m_phys_constants
!
      use t_SGS_control_parameter
      use t_spheric_rtp_data
      use t_groups_sph_dynamic
      use t_SGS_term_labels
      use t_SGS_enegy_flux_labels
      use t_phys_address
      use t_addresses_sph_transform
      use t_ele_info_4_dynamic
      use t_addresses_sph_transform
      use t_SGS_model_coefs
      use t_sph_filtering
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_model_coefs_4_sph(SGS_param, sph_rtp,            &
     &          fg_trns_SGS, bw_trns_wSGS, bd_trns_dSGS,                &
     &          trns_f_SIMI, trns_b_wide, trns_b_dble, dynamic_SPH)
!
      use zonal_lsq_4_model_coefs
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(SGS_term_address), intent(in) :: fg_trns_SGS
      type(SGS_term_address), intent(in) :: bw_trns_wSGS
      type(SGS_term_address), intent(in) :: bd_trns_dSGS
      type(address_each_sph_trans), intent(in) :: trns_f_SIMI
      type(address_each_sph_trans), intent(in) :: trns_b_wide
      type(address_each_sph_trans), intent(in) :: trns_b_dble
!
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!
!
      if(dynamic_SPH%ifld_sgs%SGS_term%i_SGS_m_flux .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'cal_dynamic_SGS_4_sph_MHD MF'
        call cal_dynamic_SGS_4_sph_MHD                                  &
     &     (sph_rtp, dynamic_SPH%sph_d_grp, SGS_param%stab_weight,      &
     &      n_vector, dynamic_SPH%ifld_sgs%SGS_term%i_SGS_m_flux,                  &
     &      trns_f_SIMI%fld_rtp(1,fg_trns_SGS%i_SGS_inertia),           &
     &      trns_b_wide%fld_rtp(1,bw_trns_wSGS%i_SGS_inertia),          &
     &      trns_b_dble%fld_rtp(1,bd_trns_dSGS%i_SGS_inertia),          &
     &      dynamic_SPH%wk_sgs)
      end if
!
      if(dynamic_SPH%ifld_sgs%SGS_term%i_SGS_Lorentz .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'cal_dynamic_SGS_4_sph_MHD LZ'
        call cal_dynamic_SGS_4_sph_MHD                                  &
     &     (sph_rtp, dynamic_SPH%sph_d_grp, SGS_param%stab_weight,      &
     &      n_vector, dynamic_SPH%ifld_sgs%SGS_term%i_SGS_Lorentz,      &
     &      trns_f_SIMI%fld_rtp(1,fg_trns_SGS%i_SGS_Lorentz),           &
     &      trns_b_wide%fld_rtp(1,bw_trns_wSGS%i_SGS_Lorentz),          &
     &      trns_b_dble%fld_rtp(1,bd_trns_dSGS%i_SGS_Lorentz),          &
     &      dynamic_SPH%wk_sgs)
      end if
!
      if(dynamic_SPH%ifld_sgs%SGS_term%i_SGS_induction .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'cal_dynamic_SGS_4_sph_MHD ID'
        call cal_dynamic_SGS_4_sph_MHD                                  &
     &     (sph_rtp, dynamic_SPH%sph_d_grp, SGS_param%stab_weight,      &
     &      n_vector, dynamic_SPH%ifld_sgs%SGS_term%i_SGS_induction,    &
     &      trns_f_SIMI%fld_rtp(1,fg_trns_SGS%i_SGS_vp_induct),         &
     &      trns_b_wide%fld_rtp(1,bw_trns_wSGS%i_SGS_vp_induct),        &
     &      trns_b_dble%fld_rtp(1,bd_trns_dSGS%i_SGS_vp_induct),        &
     &      dynamic_SPH%wk_sgs)
      end if
!
      if(dynamic_SPH%ifld_sgs%i_heat_flux .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'cal_dynamic_SGS_4_sph_MHD HF'
        call cal_dynamic_SGS_4_sph_MHD                                  &
     &     (sph_rtp, dynamic_SPH%sph_d_grp, SGS_param%stab_weight,      &
     &      n_vector, dynamic_SPH%ifld_sgs%i_heat_flux,                 &
     &      trns_f_SIMI%fld_rtp(1,fg_trns_SGS%i_SGS_h_flux),            &
     &      trns_b_wide%fld_rtp(1,bw_trns_wSGS%i_SGS_h_flux),           &
     &      trns_b_dble%fld_rtp(1,bd_trns_dSGS%i_SGS_h_flux),           &
     &      dynamic_SPH%wk_sgs)
      end if
!
      if(dynamic_SPH%ifld_sgs%i_comp_flux .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'cal_dynamic_SGS_4_sph_MHD CF'
        call cal_dynamic_SGS_4_sph_MHD                                  &
     &     (sph_rtp, dynamic_SPH%sph_d_grp, SGS_param%stab_weight,      &
     &      n_vector, dynamic_SPH%ifld_sgs%i_comp_flux,                 &
     &      trns_f_SIMI%fld_rtp(1,fg_trns_SGS%i_SGS_c_flux),            &
     &      trns_b_wide%fld_rtp(1,bw_trns_wSGS%i_SGS_c_flux),           &
     &      trns_b_dble%fld_rtp(1,bd_trns_dSGS%i_SGS_c_flux),           &
     &      dynamic_SPH%wk_sgs)
      end if
!
      end subroutine const_model_coefs_4_sph
!
! ----------------------------------------------------------------------
!
      subroutine const_dynamic_SGS_4_buo_sph(stab_weight, sph_rtp,      &
     &          fl_prop, trns_MHD, trns_SGS, trns_Csim, dynamic_SPH)
!
      use cal_SGS_buo_flux_sph_MHD
!
      real(kind = kreal), intent(in) :: stab_weight
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(fluid_property), intent(in) :: fl_prop
      type(address_4_sph_trans), intent(in) :: trns_MHD
      type(address_4_sph_trans), intent(in) :: trns_SGS
!
      type(address_4_sph_trans), intent(inout) :: trns_Csim
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!
!
      call SGS_fluxes_for_buo_coefs                                     &
     &   (sph_rtp, fl_prop, trns_MHD%b_trns%base,                       &
     &    trns_SGS%f_trns%SGS_term, trns_Csim%f_trns%SGS_ene_flux,      &
     &    trns_MHD%backward, trns_SGS%forward, trns_Csim%forward)
!
      call cal_SGS_buo_coefs_sph_MHD(stab_weight, sph_rtp,              &
     &    trns_Csim%f_trns%SGS_ene_flux, trns_Csim%forward,             &
     &    dynamic_SPH)
!
      end subroutine const_dynamic_SGS_4_buo_sph 
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_SGS_buo_coefs_sph_MHD(stab_weight, sph_rtp,        &
     &          f_trns_sef, trns_Csim_fwd, dynamic_SPH)
!
      use SGS_buo_coefs_sph_MHD
!
      real(kind = kreal), intent(in) :: stab_weight
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(SGS_ene_flux_address), intent(in) :: f_trns_sef
      type(address_each_sph_trans), intent(in) :: trns_Csim_fwd
!
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!
!
      if(dynamic_SPH%ifld_sgs%SGS_term%i_SGS_buoyancy .gt. 0) then
        call cal_SGS_buo_coef_sph_MHD                                   &
     &     (sph_rtp, dynamic_SPH%sph_d_grp, stab_weight,                &
     &      trns_Csim_fwd%fld_rtp, trns_Csim_fwd%ncomp,                 &
     &      f_trns_sef%i_reynolds_wk, f_trns_sef%i_SGS_buo_wk,          &
     &      dynamic_SPH%ifld_sgs%SGS_term%i_SGS_buoyancy,               &
     &      dynamic_SPH%icomp_sgs%SGS_term%i_SGS_buoyancy, dynamic_SPH%wk_sgs)
      end if
!
      if(dynamic_SPH%ifld_sgs%SGS_term%i_SGS_comp_buo .gt. 0) then
        call cal_SGS_buo_coef_sph_MHD                                   &
     &     (sph_rtp, dynamic_SPH%sph_d_grp, stab_weight,                &
     &      trns_Csim_fwd%fld_rtp, trns_Csim_fwd%ncomp,                 &
     &      f_trns_sef%i_reynolds_wk, f_trns_sef%i_SGS_comp_buo_wk,     &
     &      dynamic_SPH%ifld_sgs%SGS_term%i_SGS_comp_buo,               &
     &      dynamic_SPH%icomp_sgs%SGS_term%i_SGS_comp_buo,              &
     &      dynamic_SPH%wk_sgs)
      end if
!
      end subroutine cal_SGS_buo_coefs_sph_MHD 
!
! ----------------------------------------------------------------------
!
      end module dynamic_model_sph_MHD
