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
      use m_control_parameter
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
      private :: cal_model_coefs_sph_MHD
      private :: set_model_coefs_sph_snap
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
        write(*,*) 'cal_model_coefs_sph_MHD i_mom_flux'
        call cal_model_coefs_sph_MHD(sph_rtp, n_vector,                 &
     &      trns_SGS%f_trns%i_SGS_inertia,                              &
     &      trns_SGS%b_trns%i_wide_SGS_inertia,                         &
     &      ifld_sgs%i_mom_flux, icomp_sgs%i_mom_flux,                  &
     &      wk_sgs, trns_SGS)
      end if
          call calypso_mpi_barrier
!
      if(ifld_sgs%i_lorentz .gt. 0) then
        write(*,*) 'cal_model_coefs_sph_MHD i_lorentz'
        call cal_model_coefs_sph_MHD(sph_rtp, n_vector,                 &
     &      trns_SGS%f_trns%i_SGS_Lorentz,                              &
     &      trns_SGS%b_trns%i_wide_SGS_Lorentz,                         &
     &      ifld_sgs%i_lorentz, icomp_sgs%i_lorentz,                    &
     &      wk_sgs, trns_SGS)
      end if
          call calypso_mpi_barrier
!
      if(ifld_sgs%i_induction .gt. 0) then
        write(*,*) 'cal_model_coefs_sph_MHD i_induction'
        call cal_model_coefs_sph_MHD(sph_rtp, n_vector,                 &
     &      trns_SGS%f_trns%i_SGS_vp_induct,                            &
     &      trns_SGS%b_trns%i_wide_SGS_vp_induct,                       &
     &      ifld_sgs%i_induction, icomp_sgs%i_induction,                &
     &      wk_sgs, trns_SGS)
      end if
          call calypso_mpi_barrier
!
      if(ifld_sgs%i_heat_flux .gt. 0) then
        write(*,*) 'cal_model_coefs_sph_MHD i_heat_flux'
        call cal_model_coefs_sph_MHD(sph_rtp, n_vector,                 &
     &      trns_SGS%f_trns%i_SGS_h_flux,                               &
     &      trns_SGS%b_trns%i_wide_SGS_h_flux,                          &
     &      ifld_sgs%i_heat_flux, icomp_sgs%i_heat_flux,                &
     &      wk_sgs, trns_SGS)
      end if
          call calypso_mpi_barrier
!
      if(ifld_sgs%i_comp_flux .gt. 0) then
        write(*,*) 'cal_model_coefs_sph_MHD i_comp_flux'
        call cal_model_coefs_sph_MHD(sph_rtp, n_vector,                 &
     &      trns_SGS%f_trns%i_SGS_c_flux,                               &
     &       trns_SGS%b_trns%i_wide_SGS_c_flux,                         &
     &      ifld_sgs%i_comp_flux, icomp_sgs%i_comp_flux,                &
     &      wk_sgs, trns_SGS)
      end if
          call calypso_mpi_barrier
!
      end subroutine const_model_coefs_4_sph
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
      end subroutine copy_model_coefs_4_sph_snap
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_model_coefs_sph_MHD(sph_rtp, numdir,               &
     &          irtp_sgs, irtp_wide, ifld_sgs, icomp_sgs,               &
     &          wk_sgs, trns_SGS)
!
      use zonal_lsq_4_model_coefs
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
     &   (numdir, sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                   &
     &    trns_SGS%frc_rtp(1,irtp_sgs), trns_SGS%fld_rtp(1,irtp_wide),  &
     &    wk_sgs%comp_coef(1,icomp_sgs), wk_sgs%comp_clip(1,icomp_sgs))
          call calypso_mpi_barrier
!
        write(*,*) 'cal_sph_model_coefs', nnod_med,  &
     &            size(wk_sgs%comp_coef,1), size(wk_sgs%fld_coef,1),  &
     &            size(wk_sgs%comp_coef,2), icomp_sgs, &
     &            size(wk_sgs%fld_coef,2), ifld_sgs
      call cal_sph_model_coefs(numdir, nnod_med,                        &
     &    wk_sgs%comp_coef(1,icomp_sgs), wk_sgs%comp_clip(1,icomp_sgs), &
     &    wk_sgs%fld_coef(1,ifld_sgs))
          call calypso_mpi_barrier
        write(*,*) 'wk_sgs%fld_coef', wk_sgs%fld_coef(:,ifld_sgs)
!
        write(*,*) 'sel_product_model_coefs'
      call sel_product_model_coefs                                      &
     &   (numdir, sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                   &
     &    wk_sgs%fld_coef(1,ifld_sgs), trns_SGS%frc_rtp(1,irtp_sgs))
          call calypso_mpi_barrier
!
      end subroutine cal_model_coefs_sph_MHD
!
! ----------------------------------------------------------------------
!
      subroutine set_model_coefs_sph_snap                               &
     &         (sph_rtp, irtp_sgs, ifld_sgs, wk_sgs, trns_SGS)
!
      use zonal_lsq_4_model_coefs
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
!
      integer(kind = kint), intent(in) :: irtp_sgs
      integer(kind = kint), intent(in) :: ifld_sgs
!
      type(dynamic_model_data), intent(inout) :: wk_sgs
      type(address_4_sph_trans), intent(inout) :: trns_SGS
!
!
!$omp parallel workshare
      trns_SGS%frc_rtp(1:sph_rtp%nnod_rtp,irtp_sgs) = one
!$omp end parallel workshare
!
      call sel_product_model_coefs                                      &
     &   (ione, sph_rtp%nnod_rtp, sph_rtp%nidx_rtp,                     &
     &    wk_sgs%fld_coef(1,ifld_sgs), trns_SGS%frc_rtp(1,irtp_sgs))
!
      end subroutine set_model_coefs_sph_snap
!
! ----------------------------------------------------------------------
!
      end module dynamic_model_sph_MHD
