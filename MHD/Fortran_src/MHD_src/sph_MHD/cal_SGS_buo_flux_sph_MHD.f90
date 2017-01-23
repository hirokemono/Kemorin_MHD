!>@file   cal_SGS_buo_flux_sph_MHD.f90
!!@brief  module cal_SGS_buo_flux_sph_MHD
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Evaluate nonlinear terms in spherical coordinate grid
!!
!!@verbatim
!!      subroutine SGS_fluxes_for_buo_coefs                             &
!!     &         (sph_rtp, nnod_med, b_trns, fg_trns, fs_trns,          &
!!     &          ncomp_rj_2_rtp, nc_SGS_rtp_2_rj, ncomp_snap_rtp_2_rj, &
!!     &          fld_rtp, fSGS_rtp, frs_rtp)
!!      subroutine SGS_fluxes_for_snapshot                              &
!!     &         (sph_rtp, b_trns, fg_trns, bs_trns, fs_trns,           &
!!     &          ncomp_rj_2_rtp, nc_SGS_rtp_2_rj,                      &
!!     &          ncomp_snap_rj_2_rtp, ncomp_snap_rtp_2_rj,             &
!!     &          fld_rtp, fSGS_rtp, fls_rtp, frs_rtp)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(phys_address), intent(in) :: b_trns
!!        type(phys_address), intent(in) :: fg_trns
!!        type(phys_address), intent(in) :: bs_trns, fs_trns
!!@endverbatim
!
      module cal_SGS_buo_flux_sph_MHD
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_control_parameter
      use m_physical_property
!
      use t_spheric_rtp_data
      use t_phys_address
!
      implicit none
!
      private :: sel_SGS_buoyancy_flux_rtp
      private :: cal_buoyancy_flux_rtp_pin,  cal_buoyancy_flux_rtp_pout
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine SGS_fluxes_for_buo_coefs                               &
     &         (sph_rtp, nnod_med, b_trns, fg_trns, fs_trns,            &
     &          ncomp_rj_2_rtp, nc_SGS_rtp_2_rj, ncomp_snap_rtp_2_rj,   &
     &          fld_rtp, fSGS_rtp, frs_rtp)
!
      use cal_products_smp
!
      integer(kind = kint), intent(in) :: nnod_med
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(phys_address), intent(in) :: b_trns
      type(phys_address), intent(in) :: fg_trns
      type(phys_address), intent(in) :: fs_trns
!
      integer(kind = kint), intent(in) :: ncomp_rj_2_rtp
      integer(kind = kint), intent(in) :: nc_SGS_rtp_2_rj
      integer(kind = kint), intent(in) :: ncomp_snap_rtp_2_rj
      real(kind = kreal), intent(in)                                    &
     &           :: fld_rtp(sph_rtp%nnod_rtp,ncomp_rj_2_rtp)
      real(kind = kreal), intent(in)                                    &
     &           :: fSGS_rtp(sph_rtp%nnod_rtp,nc_SGS_rtp_2_rj)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: frs_rtp(sph_rtp%nnod_rtp,ncomp_snap_rtp_2_rj)
!
!
!$omp parallel
      call cal_dot_prod_no_coef_smp(sph_rtp%nnod_rtp,                   &
     &    fSGS_rtp(1,fg_trns%i_SGS_inertia),                            &
     &    fld_rtp(1,b_trns%i_velo),                                     &
     &    frs_rtp(1,fs_trns%i_reynolds_wk))
!$omp end parallel
!
      if(iflag_4_gravity .gt. id_turn_OFF) then
        call sel_SGS_buoyancy_flux_rtp                                  &
     &     (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(1), nnod_med,            &
     &      sph_rtp%nidx_rtp(3), sph_rtp%radius_1d_rtp_r,               &
     &      fl_prop1%coef_buo, fSGS_rtp(1,fg_trns%i_SGS_h_flux),        &
     &      frs_rtp(1,fs_trns%i_SGS_buo_wk))
      end if
      if(iflag_4_composit_buo .gt. id_turn_OFF) then
        call sel_SGS_buoyancy_flux_rtp                                  &
     &     (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(1), nnod_med,            &
     &      sph_rtp%nidx_rtp(3), sph_rtp%radius_1d_rtp_r,               &
     &      coef_comp_buo, fSGS_rtp(1,fg_trns%i_SGS_c_flux),            &
     &      frs_rtp(1,fs_trns%i_SGS_comp_buo_wk) )
      end if
!
      end subroutine SGS_fluxes_for_buo_coefs
!
!-----------------------------------------------------------------------
!
      subroutine SGS_fluxes_for_snapshot                                &
     &         (sph_rtp, b_trns, fg_trns, bs_trns, fs_trns,             &
     &          ncomp_rj_2_rtp, nc_SGS_rtp_2_rj,                        &
     &          ncomp_snap_rj_2_rtp, ncomp_snap_rtp_2_rj,               &
     &          fld_rtp, fSGS_rtp, fls_rtp, frs_rtp)
!
      use cal_products_smp
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(phys_address), intent(in) :: b_trns
      type(phys_address), intent(in) :: fg_trns
      type(phys_address), intent(in) :: bs_trns, fs_trns
!
      integer(kind = kint), intent(in) :: ncomp_rj_2_rtp
      integer(kind = kint), intent(in) :: nc_SGS_rtp_2_rj
      integer(kind = kint), intent(in) :: ncomp_snap_rj_2_rtp
      integer(kind = kint), intent(in) :: ncomp_snap_rtp_2_rj
      real(kind = kreal), intent(in)                                    &
     &           :: fld_rtp(sph_rtp%nnod_rtp,ncomp_rj_2_rtp)
      real(kind = kreal), intent(in)                                    &
     &           :: fls_rtp(sph_rtp%nnod_rtp,ncomp_snap_rj_2_rtp)
      real(kind = kreal), intent(in)                                    &
     &           :: fSGS_rtp(sph_rtp%nnod_rtp,nc_SGS_rtp_2_rj)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: frs_rtp(sph_rtp%nnod_rtp,ncomp_snap_rtp_2_rj)
!
      integer(kind = kint) :: nnod_med
!
!
      nnod_med = sph_rtp%nidx_rtp(1) * sph_rtp%nidx_rtp(2)
!$omp parallel
      if(fs_trns%i_reynolds_wk .gt. 0) then
        call cal_dot_prod_no_coef_smp(sph_rtp%nnod_rtp,                 &
     &      fSGS_rtp(1,fg_trns%i_SGS_inertia),                          &
     &      fld_rtp(1,b_trns%i_velo), frs_rtp(1,fs_trns%i_reynolds_wk))
      end if
!
      if(fs_trns%i_SGS_Lor_wk .gt. 0) then
        call cal_dot_prod_no_coef_smp(sph_rtp%nnod_rtp,                 &
     &      fSGS_rtp(1,fg_trns%i_SGS_lorentz),                          &
     &      fld_rtp(1,b_trns%i_velo), frs_rtp(1,fs_trns%i_SGS_Lor_wk))
      end if
!
      if(fs_trns%i_SGS_me_gen .gt. 0) then
        call cal_dot_prod_no_coef_smp(sph_rtp%nnod_rtp,                 &
     &      fls_rtp(1,bs_trns%i_SGS_induction),                         &
     &      fld_rtp(1,b_trns%i_magne),                                  &
     &      frs_rtp(1,fs_trns%i_SGS_me_gen))
      end if
!$omp end parallel
!
!$omp parallel
      if(fs_trns%i_SGS_buo_wk .gt. 0) then
        call cal_buoyancy_flux_rtp_pout                                 &
     &     (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(1), nnod_med,            &
     &      sph_rtp%nidx_rtp(3), sph_rtp%radius_1d_rtp_r,               &
     &      fl_prop1%coef_buo, fSGS_rtp(1,fg_trns%i_SGS_h_flux),        &
     &      frs_rtp(1,fs_trns%i_SGS_buo_wk))
      end if
      if(fs_trns%i_SGS_comp_buo_wk .gt. 0) then
        call cal_buoyancy_flux_rtp_pout                                 &
     &     (sph_rtp%nnod_rtp, sph_rtp%nidx_rtp(1), nnod_med,            &
     &      sph_rtp%nidx_rtp(3), sph_rtp%radius_1d_rtp_r,               &
     &      coef_comp_buo, fSGS_rtp(1,fg_trns%i_SGS_c_flux),            &
     &      frs_rtp(1,fs_trns%i_SGS_comp_buo_wk) )
      end if
!$omp end parallel
!
      end subroutine SGS_fluxes_for_snapshot
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_SGS_buoyancy_flux_rtp                              &
     &        (nnod_rtp, nri, nnod_med, nphi, radius, coef,             &
     &         frc_hf, frc_buo)
!
      use m_FFT_selector
!
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_med, nphi, nri
!
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: radius(nri)
      real(kind = kreal), intent(in) :: frc_hf(nnod_rtp)
      real(kind = kreal), intent(inout) :: frc_buo(nnod_rtp)
!
!
!$omp parallel
      if(iflag_FFT .eq. iflag_FFTW) then
        call cal_buoyancy_flux_rtp_pin                                  &
     &     (nnod_rtp, nri, nnod_med, nphi, radius, coef,                &
     &      frc_hf, frc_buo)
      else
        call cal_buoyancy_flux_rtp_pout                                 &
     &     (nnod_rtp, nri, nnod_med, nphi, radius, coef,                &
     &     frc_hf, frc_buo)
      end if
!$omp end parallel
!
      end subroutine sel_SGS_buoyancy_flux_rtp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_buoyancy_flux_rtp_pin                              &
     &         (nnod_rtp, nri, nnod_med, nphi, radius, coef,            &
     &          frc_hf, frc_buo)
!
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_med, nphi, nri
!
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: radius(nri)
      real(kind = kreal), intent(in) :: frc_hf(nnod_rtp)
      real(kind = kreal), intent(inout) :: frc_buo(nnod_rtp)
!
      integer(kind = kint) :: k, kl, m, i1
!
!
!$omp do private(k,kl,m,i1)
      do kl = 1, nnod_med
        k = mod((kl-1),nri) + 1
        do m = 1, nphi
          i1 = m + (kl-1)*nphi
          frc_buo(i1) = coef * frc_hf(i1) * radius(k)
        end do
      end do
!$omp end do
!
      end subroutine cal_buoyancy_flux_rtp_pin
!
!  ---------------------------------------------------------------------
!
      subroutine cal_buoyancy_flux_rtp_pout                             &
     &         (nnod_rtp, nri, nnod_med, nphi, radius, coef,            &
     &          frc_hf, frc_buo)
!
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_med, nphi, nri
!
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in) :: radius(nri)
      real(kind = kreal), intent(in) :: frc_hf(nnod_rtp)
      real(kind = kreal), intent(inout) :: frc_buo(nnod_rtp)
!
      integer(kind = kint) :: k, kl, m, i1
!
!
      do m = 1, nphi
!$omp do private(k,kl,i1)
        do kl = 1, nnod_med
          k = mod((kl-1),nri) + 1
          i1 = kl + (m-1)*nnod_med
          frc_buo(i1) = coef * frc_hf(i1) * radius(k)
        end do
!$omp end do
      end do
 !
      end subroutine cal_buoyancy_flux_rtp_pout
!
!  ---------------------------------------------------------------------
!
      end module cal_SGS_buo_flux_sph_MHD
