!>@file   dynamic_model_sph_MHD.f90
!!@brief  module dynamic_model_sph_MHD
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Evaluate nonlinear terms in spherical coordinate grid
!!
!!@verbatim
!!      subroutine const_model_coefs_4_sph                              &
!!     &         (sph_rtp, ifld_sgs, icomp_sgs, wk_sgs, trns_SGS)
!!      subroutine copy_Csim_buo_4_sph_trans                            &
!!     &         (sph_rtp, ifld_sgs,  wk_sgs, trns_SGS)
!!      subroutine copy_model_coefs_4_sph_snap                          &
!!     &         (sph_rtp, ifld_sgs,  wk_sgs, trns_snap)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(phys_address), intent(in) :: bg_trns, fg_trns
!!        type(SGS_terms_address), intent(in) :: ifld_sgs, icomp_sgs
!!        type(dynamic_model_data), intent(inout) :: wk_sgs
!!        type(address_4_sph_trans), intent(inout) :: trns_SGS
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
      use t_spheric_rtp_data
      use t_phys_address
      use t_addresses_sph_transform
      use t_ele_info_4_dynamic
      use t_addresses_sph_transform
      use t_SGS_model_coefs
!
      implicit none
!
      private :: cal_dynamic_SGS_4_sph_MHD, set_model_coefs_sph_snap
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_model_coefs_4_sph                                &
     &         (sph_rtp, ifld_sgs, icomp_sgs, wk_sgs, trns_SGS)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(SGS_terms_address), intent(in) :: ifld_sgs, icomp_sgs
!
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(address_4_sph_trans), intent(inout) :: trns_SGS
!
!
      if(ifld_sgs%i_mom_flux .gt. 0) then
        call cal_dynamic_SGS_4_sph_MHD(sph_rtp, n_vector,               &
     &      trns_SGS%f_trns%i_SGS_inertia,                              &
     &      trns_SGS%b_trns%i_wide_SGS_inertia,                         &
     &      ifld_sgs%i_mom_flux, icomp_sgs%i_mom_flux,                  &
     &      wk_sgs, trns_SGS)
      end if
!
      if(ifld_sgs%i_lorentz .gt. 0) then
        call cal_dynamic_SGS_4_sph_MHD(sph_rtp, n_vector,               &
     &      trns_SGS%f_trns%i_SGS_Lorentz,                              &
     &      trns_SGS%b_trns%i_wide_SGS_Lorentz,                         &
     &      ifld_sgs%i_lorentz, icomp_sgs%i_lorentz,                    &
     &      wk_sgs, trns_SGS)
      end if
!
      if(ifld_sgs%i_induction .gt. 0) then
        call cal_dynamic_SGS_4_sph_MHD(sph_rtp, n_vector,               &
     &      trns_SGS%f_trns%i_SGS_vp_induct,                            &
     &      trns_SGS%b_trns%i_wide_SGS_vp_induct,                       &
     &      ifld_sgs%i_induction, icomp_sgs%i_induction,                &
     &      wk_sgs, trns_SGS)
      end if
!
      if(ifld_sgs%i_heat_flux .gt. 0) then
        call cal_dynamic_SGS_4_sph_MHD(sph_rtp, n_vector,               &
     &      trns_SGS%f_trns%i_SGS_h_flux,                               &
     &      trns_SGS%b_trns%i_wide_SGS_h_flux,                          &
     &      ifld_sgs%i_heat_flux, icomp_sgs%i_heat_flux,                &
     &      wk_sgs, trns_SGS)
      end if
!
      if(ifld_sgs%i_comp_flux .gt. 0) then
        call cal_dynamic_SGS_4_sph_MHD(sph_rtp, n_vector,               &
     &      trns_SGS%f_trns%i_SGS_c_flux,                               &
     &      trns_SGS%b_trns%i_wide_SGS_c_flux,                          &
     &      ifld_sgs%i_comp_flux, icomp_sgs%i_comp_flux,                &
     &      wk_sgs, trns_SGS)
      end if
!
      end subroutine const_model_coefs_4_sph
!
! ----------------------------------------------------------------------
!
      subroutine copy_Csim_buo_4_sph_trans                              &
     &         (sph_rtp, ifld_sgs,  wk_sgs, trns_SGS)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(SGS_terms_address), intent(in) :: ifld_sgs
!
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(address_4_sph_trans), intent(inout) :: trns_SGS
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'ifld_sgs%i_buoyancy',                               &
     &        ifld_sgs%i_buoyancy, trns_SGS%f_trns%i_Csim_SGS_buoyancy
        write(*,*) 'ifld_sgs%i_comp_buoyancy',                          &
     &        ifld_sgs%i_comp_buoyancy,                                 &
     &        trns_SGS%f_trns%i_Csim_SGS_comp_buo
      end if
!
      if(ifld_sgs%i_buoyancy .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp,                          &
     &      trns_SGS%f_trns%i_Csim_SGS_buoyancy,                        &
     &      ifld_sgs%i_buoyancy, wk_sgs, trns_SGS)
      end if
!
      if(ifld_sgs%i_comp_buoyancy .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp,                          &
     &      trns_SGS%f_trns%i_Csim_SGS_comp_buo,                        &
     &      ifld_sgs%i_comp_buoyancy, wk_sgs, trns_SGS)
      end if
!
      end subroutine copy_Csim_buo_4_sph_trans
!
! ----------------------------------------------------------------------
!
      subroutine copy_model_coefs_4_sph_snap                            &
     &         (sph_rtp, ifld_sgs,  wk_sgs, trns_snap)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(SGS_terms_address), intent(in) :: ifld_sgs
!
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(address_4_sph_trans), intent(inout) :: trns_snap
!
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'ifld_sgs%i_mom_flux',                               &
     &        ifld_sgs%i_mom_flux, trns_snap%f_trns%i_Csim_SGS_m_flux
        write(*,*) 'ifld_sgs%i_lorentz',                                &
     &        ifld_sgs%i_lorentz, trns_snap%f_trns%i_Csim_SGS_Lorentz
        write(*,*) 'ifld_sgs%i_induction', ifld_sgs%i_induction,        &
     &        trns_snap%f_trns%i_Csim_SGS_induction
        write(*,*) 'ifld_sgs%i_heat_flux',                              &
     &        ifld_sgs%i_heat_flux, trns_snap%f_trns%i_Csim_SGS_h_flux
        write(*,*) 'ifld_sgs%i_comp_flux',                              &
     &        ifld_sgs%i_comp_flux, trns_snap%f_trns%i_Csim_SGS_c_flux
        write(*,*) 'ifld_sgs%i_buoyancy',                               &
     &        ifld_sgs%i_buoyancy, trns_snap%f_trns%i_Csim_SGS_buoyancy
        write(*,*) 'ifld_sgs%i_comp_buoyancy',                          &
     &        ifld_sgs%i_comp_buoyancy,                                 &
     &        trns_snap%f_trns%i_Csim_SGS_comp_buo
      end if
!
      if(ifld_sgs%i_mom_flux .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp,                          &
     &      trns_snap%f_trns%i_Csim_SGS_m_flux, ifld_sgs%i_mom_flux,    &
     &      wk_sgs, trns_snap)
      end if
!
      if(ifld_sgs%i_lorentz .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp,                          &
     &      trns_snap%f_trns%i_Csim_SGS_Lorentz, ifld_sgs%i_lorentz,    &
     &      wk_sgs, trns_snap)
      end if
!
      if(ifld_sgs%i_induction .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp,                          &
     &     trns_snap%f_trns%i_Csim_SGS_induction, ifld_sgs%i_induction, &
     &     wk_sgs, trns_snap)
      end if
!
      if(ifld_sgs%i_heat_flux .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp,                          &
     &      trns_snap%f_trns%i_Csim_SGS_h_flux, ifld_sgs%i_heat_flux,   &
     &      wk_sgs, trns_snap)
      end if
!
      if(ifld_sgs%i_comp_flux .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp,                          &
     &      trns_snap%f_trns%i_Csim_SGS_c_flux, ifld_sgs%i_comp_flux,   &
     &      wk_sgs, trns_snap)
      end if
!
!
      if(ifld_sgs%i_buoyancy .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp,                          &
     &      trns_snap%f_trns%i_Csim_SGS_buoyancy,                       &
     &      ifld_sgs%i_buoyancy, wk_sgs, trns_snap)
      end if
!
      if(ifld_sgs%i_comp_buoyancy .gt. 0) then
        call set_model_coefs_sph_snap(sph_rtp,                          &
     &      trns_snap%f_trns%i_Csim_SGS_comp_buo,                       &
     &      ifld_sgs%i_comp_buoyancy, wk_sgs, trns_snap)
      end if
!
      end subroutine copy_model_coefs_4_sph_snap
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_dynamic_SGS_4_sph_MHD(sph_rtp, numdir,             &
     &          irtp_sgs, irtp_wide, ifld_sgs, icomp_sgs,               &
     &          wk_sgs, trns_SGS)
!
      use m_FFT_selector
      use zonal_lsq_4_model_coefs
      use prod_SGS_model_coefs_sph
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      integer(kind = kint), intent(in) :: numdir
!
      integer(kind = kint), intent(in) :: irtp_sgs, irtp_wide
      integer(kind = kint), intent(in) :: ifld_sgs, icomp_sgs
!
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(address_4_sph_trans), intent(inout) :: trns_SGS
!
      integer(kind = kint) :: nnod_med
!
!
      nnod_med = sph_rtp%nidx_rtp(1) * sph_rtp%nidx_rtp(2)
      call sel_int_zonal_for_model_coefs                                &
     &   (numdir, sph_rtp%nnod_rtp, nnod_med, sph_rtp%nidx_rtp(3),      &
     &    trns_SGS%frc_rtp(1,irtp_sgs), trns_SGS%fld_rtp(1,irtp_wide),  &
     &    wk_sgs%comp_coef(1,icomp_sgs), wk_sgs%comp_clip(1,icomp_sgs), &
     &    wk_sgs%fld_coef(1,ifld_sgs))
!
!$omp parallel
      if(iflag_FFT .eq. iflag_FFTW) then
        call product_model_coefs_pin                                    &
     &   (numdir, sph_rtp%nnod_rtp, nnod_med, sph_rtp%nidx_rtp(3),      &
     &    wk_sgs%fld_coef(1,ifld_sgs), trns_SGS%frc_rtp(1,irtp_sgs))
      else
        call product_model_coefs_pout                                   &
     &   (numdir, sph_rtp%nnod_rtp, nnod_med, sph_rtp%nidx_rtp(3),      &
     &    wk_sgs%fld_coef(1,ifld_sgs), trns_SGS%frc_rtp(1,irtp_sgs))
      end if
!$omp end parallel
!
      end subroutine cal_dynamic_SGS_4_sph_MHD
!
! ----------------------------------------------------------------------
!
      subroutine set_model_coefs_sph_snap                               &
     &         (sph_rtp, irtp_sgs, ifld_sgs, wk_sgs, trns_SGS)
!
      use prod_SGS_model_coefs_sph
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
      integer(kind = kint), intent(in) :: irtp_sgs
      integer(kind = kint), intent(in) :: ifld_sgs
!
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(address_4_sph_trans), intent(inout) :: trns_SGS
!
      integer(kind = kint) :: nnod_med
!
!
!$omp parallel workshare
      trns_SGS%frc_rtp(1:sph_rtp%nnod_rtp,irtp_sgs) = one
!$omp end parallel workshare
!
      nnod_med = sph_rtp%nidx_rtp(1) * sph_rtp%nidx_rtp(2)
!$omp parallel
      call product_model_coefs_pout                                     &
     &   (ione, sph_rtp%nnod_rtp, nnod_med, sph_rtp%nidx_rtp(3),        &
     &    wk_sgs%fld_coef(1,ifld_sgs), trns_SGS%frc_rtp(1,irtp_sgs))
!$omp end parallel
!
      end subroutine set_model_coefs_sph_snap
!
! ----------------------------------------------------------------------
!
      end module dynamic_model_sph_MHD
