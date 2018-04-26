!>@file   product_model_coefs_sph.f90
!!@brief  module product_model_coefs_sph
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Evaluate nonlinear terms in spherical coordinate grid
!!
!!@verbatim
!!      subroutine prod_fixed_sph_SGS_Csim                              &
!!     &         (SGS_param, sph_rtp, ifld_sgs, trns_SGS)
!!      subroutine product_model_coefs_4_sph,                           &
!!     &         (SGS_param, sph_rtp, trns_SGS, dynamic_SPH)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(address_4_sph_trans), intent(inout) :: trns_SGS
!!        type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!!@endverbatim
!
      module product_model_coefs_sph
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
     &      trns_SGS%forward%ncomp, trns_SGS%frc_rtp)
      end if
!
      if(ifld_sgs%i_lorentz .gt. 0) then
        call product_fixed_model_coefs(SGS_param%SGS_mawell_factor,     &
     &      sph_rtp, trns_SGS%f_trns%i_SGS_Lorentz, n_vector,           &
     &      trns_SGS%forward%ncomp, trns_SGS%frc_rtp)
      end if
!
      if(ifld_sgs%i_induction .gt. 0) then
        call product_fixed_model_coefs(SGS_param%SGS_uxb_factor,        &
     &      sph_rtp, trns_SGS%f_trns%i_SGS_vp_induct, n_vector,         &
     &      trns_SGS%forward%ncomp, trns_SGS%frc_rtp)
      end if
!
      if(ifld_sgs%i_heat_flux .gt. 0) then
        call product_fixed_model_coefs(SGS_param%SGS_hf_factor,         &
     &      sph_rtp, trns_SGS%f_trns%i_SGS_h_flux, n_vector,            &
     &      trns_SGS%forward%ncomp, trns_SGS%frc_rtp)
      end if
!
      if(ifld_sgs%i_comp_flux .gt. 0) then
        call product_fixed_model_coefs(SGS_param%SGS_cf_factor,         &
     &      sph_rtp, trns_SGS%f_trns%i_SGS_c_flux, n_vector,            &
     &      trns_SGS%forward%ncomp, trns_SGS%frc_rtp)
      end if
!
      end subroutine prod_fixed_sph_SGS_Csim
!
! ----------------------------------------------------------------------
!
      subroutine product_model_coefs_4_sph                              &
     &         (SGS_param, sph_rtp, trns_SGS, dynamic_SPH)
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
      type(address_4_sph_trans), intent(inout) :: trns_SGS
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!
!
      if(dynamic_SPH%ifld_sgs%i_mom_flux .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'sel_product_model_coefs MF'
        call sel_product_model_coefs                                    &
     &     (SGS_param%SGS_mf_factor, sph_rtp, dynamic_SPH%sph_d_grp,    &
     &      n_vector, trns_SGS%f_trns%i_SGS_inertia,                    &
     &      dynamic_SPH%ifld_sgs%i_mom_flux,                            &
     &      dynamic_SPH%wk_sgs, trns_SGS)
      end if
!
      if(dynamic_SPH%ifld_sgs%i_lorentz .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'sel_product_model_coefs LZ'
        call sel_product_model_coefs                                    &
     &    (SGS_param%SGS_mawell_factor, sph_rtp, dynamic_SPH%sph_d_grp, &
     &     n_vector, trns_SGS%f_trns%i_SGS_Lorentz,                     &
     &     dynamic_SPH%ifld_sgs%i_lorentz,                              &
     &     dynamic_SPH%wk_sgs, trns_SGS)
      end if
!
      if(dynamic_SPH%ifld_sgs%i_induction .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'sel_product_model_coefs ID'
        call sel_product_model_coefs                                    &
     &     (SGS_param%SGS_uxb_factor, sph_rtp, dynamic_SPH%sph_d_grp,   &
     &      n_vector, trns_SGS%f_trns%i_SGS_vp_induct,                  &
     &      dynamic_SPH%ifld_sgs%i_induction,                           &
     &      dynamic_SPH%wk_sgs, trns_SGS)
      end if
!
      if(dynamic_SPH%ifld_sgs%i_heat_flux .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'sel_product_model_coefs HF'
        call sel_product_model_coefs                                    &
     &     (SGS_param%SGS_hf_factor, sph_rtp, dynamic_SPH%sph_d_grp,    &
     &      n_vector, trns_SGS%f_trns%i_SGS_h_flux,                     &
     &      dynamic_SPH%ifld_sgs%i_heat_flux,                           &
     &      dynamic_SPH%wk_sgs, trns_SGS)
      end if
!
      if(dynamic_SPH%ifld_sgs%i_comp_flux .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'sel_product_model_coefs CF'
        call sel_product_model_coefs                                    &
     &     (SGS_param%SGS_cf_factor, sph_rtp, dynamic_SPH%sph_d_grp,    &
     &      n_vector, trns_SGS%f_trns%i_SGS_c_flux,                     &
     &      dynamic_SPH%ifld_sgs%i_comp_flux,                           &
     &      dynamic_SPH%wk_sgs, trns_SGS)
      end if
!
      end subroutine product_model_coefs_4_sph
!
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
     &     irtp_sgs, numdir, trns_SGS%forward%ncomp, trns_SGS%frc_rtp)
      else
        call product_model_coefs_pout(const_Csim, ifld_sgs,             &
     &     sph_rtp, sph_d_grp, wk_sgs%num_kinds, wk_sgs%fld_coef,       &
     &     irtp_sgs, numdir, trns_SGS%forward%ncomp, trns_SGS%frc_rtp)
      end if
!
      end subroutine sel_product_model_coefs
!
! ----------------------------------------------------------------------
!
      end module product_model_coefs_sph
