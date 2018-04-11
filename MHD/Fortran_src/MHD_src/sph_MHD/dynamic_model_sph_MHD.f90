!>@file   dynamic_model_sph_MHD.f90
!!@brief  module dynamic_model_sph_MHD
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Evaluate nonlinear terms in spherical coordinate grid
!!
!!@verbatim
!!      subroutine prod_fixed_sph_SGS_Csim                              &
!!     &         (SGS_param, sph_rtp, ifld_sgs, trns_SGS)
!!      subroutine const_model_coefs_4_sph(istep_dynamic,               &
!!     &          SGS_param, sph_rtp, trns_SGS, dynamic_SPH)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(address_4_sph_trans), intent(inout) :: trns_SGS
!!        type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!!
!!      subroutine const_dynamic_SGS_4_buo_sph                          &
!!     &         (iflag_SGS_buo_usage, stab_weight, sph_rtp,            &
!!     &          fl_prop, trns_MHD, trns_snap, trns_SGS, dynamic_SPH)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(address_4_sph_trans), intent(in) :: trns_MHD
!!        type(address_4_sph_trans), intent(inout) :: trns_snap
!!        type(address_4_sph_trans), intent(inout) :: trns_SGS
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
      use t_phys_address
      use t_addresses_sph_transform
      use t_ele_info_4_dynamic
      use t_addresses_sph_transform
      use t_SGS_model_coefs
      use t_sph_filtering
!
      implicit none
!
      private :: cal_dynamic_SGS_4_sph_MHD
      private :: sel_product_model_coefs
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine prod_fixed_sph_SGS_Csim                                &
     &         (SGS_param, sph_rtp, ifld_sgs, trns_SGS)
!
      use prod_SGS_model_coefs_sph
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(SGS_terms_address), intent(in) :: ifld_sgs
!
      type(address_4_sph_trans), intent(inout) :: trns_SGS
!
!
      if(ifld_sgs%i_mom_flux .gt. 0) then
        call product_fixed_model_coefs(SGS_param%SGS_mf_factor,         &
     &      sph_rtp, trns_SGS%f_trns%i_SGS_inertia, n_vector,           &
     &      trns_SGS%ncomp_rtp_2_rj, trns_SGS%frc_rtp)
      end if
!
      if(ifld_sgs%i_lorentz .gt. 0) then
        call product_fixed_model_coefs(SGS_param%SGS_mawell_factor,     &
     &      sph_rtp, trns_SGS%f_trns%i_SGS_Lorentz, n_vector,           &
     &      trns_SGS%ncomp_rtp_2_rj, trns_SGS%frc_rtp)
      end if
!
      if(ifld_sgs%i_induction .gt. 0) then
        call product_fixed_model_coefs(SGS_param%SGS_uxb_factor,        &
     &      sph_rtp, trns_SGS%f_trns%i_SGS_vp_induct, n_vector,         &
     &      trns_SGS%ncomp_rtp_2_rj, trns_SGS%frc_rtp)
      end if
!
      if(ifld_sgs%i_heat_flux .gt. 0) then
        call product_fixed_model_coefs(SGS_param%SGS_hf_factor,         &
     &      sph_rtp, trns_SGS%f_trns%i_SGS_h_flux, n_vector,            &
     &      trns_SGS%ncomp_rtp_2_rj, trns_SGS%frc_rtp)
      end if
!
      if(ifld_sgs%i_comp_flux .gt. 0) then
        call product_fixed_model_coefs(SGS_param%SGS_cf_factor,         &
     &      sph_rtp, trns_SGS%f_trns%i_SGS_c_flux, n_vector,            &
     &      trns_SGS%ncomp_rtp_2_rj, trns_SGS%frc_rtp)
      end if
!
      end subroutine prod_fixed_sph_SGS_Csim
!
! ----------------------------------------------------------------------
!
      subroutine const_model_coefs_4_sph(istep_dynamic,                 &
     &          SGS_param, sph_rtp, trns_SGS, dynamic_SPH)
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_rtp_grid), intent(in) :: sph_rtp
      integer(kind = kint), intent(in) :: istep_dynamic
!
      type(address_4_sph_trans), intent(inout) :: trns_SGS
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!
!
      if(dynamic_SPH%ifld_sgs%i_mom_flux .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'cal_dynamic_SGS_4_sph_MHD MF'
        call cal_dynamic_SGS_4_sph_MHD                                  &
     &     (SGS_param%SGS_mf_factor, sph_rtp, dynamic_SPH%sph_d_grp,    &
     &      istep_dynamic, SGS_param%stab_weight,                       &
     &      n_vector, trns_SGS%f_trns%i_SGS_inertia,                    &
     &      trns_SGS%b_trns%i_wide_SGS_inertia,                         &
     &      dynamic_SPH%ifld_sgs%i_mom_flux,                            &
     &      dynamic_SPH%icomp_sgs%i_mom_flux,                           &
     &      dynamic_SPH%wk_sgs, trns_SGS)
      end if
!
      if(dynamic_SPH%ifld_sgs%i_lorentz .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'cal_dynamic_SGS_4_sph_MHD LZ'
        call cal_dynamic_SGS_4_sph_MHD                                  &
     &    (SGS_param%SGS_mawell_factor, sph_rtp, dynamic_SPH%sph_d_grp, &
     &     istep_dynamic, SGS_param%stab_weight,                        &
     &     n_vector, trns_SGS%f_trns%i_SGS_Lorentz,                     &
     &     trns_SGS%b_trns%i_wide_SGS_Lorentz,                          &
     &     dynamic_SPH%ifld_sgs%i_lorentz,                              &
     &     dynamic_SPH%icomp_sgs%i_lorentz,                             &
     &     dynamic_SPH%wk_sgs, trns_SGS)
      end if
!
      if(dynamic_SPH%ifld_sgs%i_induction .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'cal_dynamic_SGS_4_sph_MHD ID'
        call cal_dynamic_SGS_4_sph_MHD                                  &
     &     (SGS_param%SGS_uxb_factor, sph_rtp, dynamic_SPH%sph_d_grp,   &
     &      istep_dynamic, SGS_param%stab_weight,                       &
     &      n_vector, trns_SGS%f_trns%i_SGS_vp_induct,                  &
     &      trns_SGS%b_trns%i_wide_SGS_vp_induct,                       &
     &      dynamic_SPH%ifld_sgs%i_induction,                           &
     &      dynamic_SPH%icomp_sgs%i_induction,                          &
     &      dynamic_SPH%wk_sgs, trns_SGS)
      end if
!
      if(dynamic_SPH%ifld_sgs%i_heat_flux .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'cal_dynamic_SGS_4_sph_MHD HF'
        call cal_dynamic_SGS_4_sph_MHD                                  &
     &     (SGS_param%SGS_hf_factor, sph_rtp, dynamic_SPH%sph_d_grp,    &
     &     istep_dynamic, SGS_param%stab_weight,                        &
     &      n_vector, trns_SGS%f_trns%i_SGS_h_flux,                     &
     &      trns_SGS%b_trns%i_wide_SGS_h_flux,                          &
     &      dynamic_SPH%ifld_sgs%i_heat_flux,                           &
     &      dynamic_SPH%icomp_sgs%i_heat_flux,                          &
     &      dynamic_SPH%wk_sgs, trns_SGS)
      end if
!
      if(dynamic_SPH%ifld_sgs%i_comp_flux .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'cal_dynamic_SGS_4_sph_MHD CF'
        call cal_dynamic_SGS_4_sph_MHD                                  &
     &     (SGS_param%SGS_cf_factor, sph_rtp, dynamic_SPH%sph_d_grp,    &
     &      istep_dynamic, SGS_param%stab_weight,                       &
     &      n_vector, trns_SGS%f_trns%i_SGS_c_flux,                     &
     &      trns_SGS%b_trns%i_wide_SGS_c_flux,                          &
     &      dynamic_SPH%ifld_sgs%i_comp_flux,                           &
     &      dynamic_SPH%icomp_sgs%i_comp_flux,                          &
     &      dynamic_SPH%wk_sgs, trns_SGS)
      end if
!
      end subroutine const_model_coefs_4_sph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_dynamic_SGS_4_sph_MHD                              &
     &         (const_Csim, sph_rtp, sph_d_grp, istep_dynamic,          &
     &          stab_weight, numdir, irtp_sgs, irtp_wide,               &
     &          ifld_sgs, icomp_sgs, wk_sgs, trns_SGS)
!
      use m_FFT_selector
      use zonal_lsq_4_model_coefs
      use prod_SGS_model_coefs_sph
      use cal_sph_model_coefs
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
      real(kind = kreal), intent(in) :: stab_weight
      integer(kind = kint), intent(in) :: istep_dynamic
      integer(kind = kint), intent(in) :: numdir
!
      integer(kind = kint), intent(in) :: irtp_sgs, irtp_wide
      integer(kind = kint), intent(in) :: ifld_sgs, icomp_sgs
      real(kind = kreal), intent(in) :: const_Csim
!
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(address_4_sph_trans), intent(inout) :: trns_SGS
!
!
      if(istep_dynamic .eq. 0) then
        if(iflag_debug .gt. 0)                                          &
           write(*,*) 'sel_int_zonal_4_model_coefs', istep_dynamic
        call sel_int_zonal_4_model_coefs(sph_rtp, sph_d_grp, numdir,    &
     &    trns_SGS%frc_rtp(1,irtp_sgs), trns_SGS%fld_rtp(1,irtp_wide),  &
     &    wk_sgs%comp_coef(1,icomp_sgs), wk_sgs%comp_clip(1,icomp_sgs))
!
        if(iflag_debug .gt. 0)                                          &
        call sel_sph_model_coefs(numdir, sph_d_grp%ngrp_dynamic,        &
     &      stab_weight, ifld_sgs, icomp_sgs,                           &
     &      wk_sgs%num_kinds, wk_sgs%ntot_comp, wk_sgs%comp_coef,       &
     &      wk_sgs%comp_clip, wk_sgs%fld_coef)
      end if
!
      call sel_product_model_coefs(const_Csim, sph_rtp, sph_d_grp,      &
     &    numdir, irtp_sgs, ifld_sgs, wk_sgs, trns_SGS)
!
      end subroutine cal_dynamic_SGS_4_sph_MHD
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine const_dynamic_SGS_4_buo_sph                            &
     &         (iflag_SGS_buo_usage, stab_weight, sph_rtp,              &
     &          fl_prop, trns_MHD, trns_snap, trns_SGS, dynamic_SPH)
!
      use SGS_buo_coefs_sph_MHD
      use cal_SGS_buo_flux_sph_MHD
!
      integer(kind = kint), intent(in) :: iflag_SGS_buo_usage
      real(kind = kreal), intent(in) :: stab_weight
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(fluid_property), intent(in) :: fl_prop
      type(address_4_sph_trans), intent(in) :: trns_MHD
!
      type(address_4_sph_trans), intent(inout) :: trns_snap
      type(address_4_sph_trans), intent(inout) :: trns_SGS
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!
!
      call SGS_fluxes_for_buo_coefs(sph_rtp, fl_prop,                   &
     &    trns_MHD%b_trns, trns_SGS%f_trns, trns_snap%f_trns,           &
     &    trns_MHD%ncomp_rj_2_rtp, trns_SGS%ncomp_rtp_2_rj,             &
     &    trns_snap%ncomp_rtp_2_rj, trns_MHD%fld_rtp, trns_SGS%frc_rtp, &
     &    trns_snap%frc_rtp)
!
      if(dynamic_SPH%ifld_sgs%i_buoyancy .gt. 0) then
        call cal_SGS_buo_coefs_sph_MHD                                  &
     &     (sph_rtp, dynamic_SPH%sph_d_grp, stab_weight,                &
     &      trns_snap%frc_rtp, trns_snap%ncomp_rtp_2_rj,                &
     &      trns_snap%f_trns%i_reynolds_wk,                             &
     &      trns_snap%f_trns%i_SGS_buo_wk,                              &
     &      dynamic_SPH%ifld_sgs%i_buoyancy,                            &
     &      dynamic_SPH%icomp_sgs%i_buoyancy, dynamic_SPH%wk_sgs)
      end if
!
      if(dynamic_SPH%ifld_sgs%i_comp_buoyancy .gt. 0) then
        call cal_SGS_buo_coefs_sph_MHD                                  &
     &     (sph_rtp, dynamic_SPH%sph_d_grp, stab_weight,                &
     &      trns_snap%frc_rtp, trns_snap%ncomp_rtp_2_rj,                &
     &      trns_snap%f_trns%i_reynolds_wk,                             &
     &      trns_snap%f_trns%i_SGS_comp_buo_wk,                         &
     &      dynamic_SPH%ifld_sgs%i_comp_buoyancy,                       &
     &      dynamic_SPH%icomp_sgs%i_comp_buoyancy, dynamic_SPH%wk_sgs)
      end if
!
      if(iflag_SGS_buo_usage .eq. id_use_zonal) then
        write(*,*) 'prod_SGS_buoyancy_to_Reynolds'
        call prod_SGS_buoyancy_to_Reynolds                              &
     &     (sph_rtp, dynamic_SPH%sph_d_grp,                             &
     &      trns_SGS%f_trns, dynamic_SPH%ifld_sgs, dynamic_SPH%wk_sgs,  &
     &      trns_SGS%ncomp_rtp_2_rj, trns_SGS%frc_rtp)
      end if
!
      end subroutine const_dynamic_SGS_4_buo_sph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sel_product_model_coefs                                &
     &         (const_Csim, sph_rtp, sph_d_grp, numdir,                 &
     &          irtp_sgs, ifld_sgs, wk_sgs, trns_SGS)
!
      use m_FFT_selector
      use prod_SGS_model_coefs_sph
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
      integer(kind = kint), intent(in) :: numdir
!
      integer(kind = kint), intent(in) :: irtp_sgs
      integer(kind = kint), intent(in) :: ifld_sgs
      real(kind = kreal), intent(in) :: const_Csim
!
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(address_4_sph_trans), intent(inout) :: trns_SGS
!
!
      if(iflag_FFT .eq. iflag_FFTW) then
        call product_model_coefs_pin(const_Csim, ifld_sgs,              &
     &     sph_rtp, sph_d_grp, wk_sgs%num_kinds, wk_sgs%fld_coef,       &
     &     irtp_sgs, numdir, trns_SGS%ncomp_rtp_2_rj, trns_SGS%frc_rtp)
      else
        call product_model_coefs_pout(const_Csim, ifld_sgs,             &
     &     sph_rtp, sph_d_grp, wk_sgs%num_kinds, wk_sgs%fld_coef,       &
     &     irtp_sgs, numdir, trns_SGS%ncomp_rtp_2_rj, trns_SGS%frc_rtp)
      end if
!
      end subroutine sel_product_model_coefs
!
! ----------------------------------------------------------------------
!
      end module dynamic_model_sph_MHD
