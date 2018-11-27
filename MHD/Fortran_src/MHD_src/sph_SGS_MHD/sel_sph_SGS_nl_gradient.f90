!!@file   sel_sph_SGS_nl_gradient.f90
!!@brief  module sel_sph_SGS_nl_gradient
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in MAy., 2018
!
!>@brief SGS terms by nonlinear gradient model in spherical coordinate
!!
!!@verbatim
!!      subroutine sel_sph_SGS_induct_nl_gradient                       &
!!     &        (sph_rtp, sph_filters, coef,                            &
!!     &         ib_mhd_velo, ib_mhd_magne, ncomp_rj_2_rtp, fld_rtp,    &
!!     &         ib_grad_ux, ib_grad_uy, ib_grad_uz,                    &
!!     &         ib_grad_bx, ib_grad_by, ib_grad_bz,                    &
!!     &         ncomp_sgs_rj_2_rtp, fld_sgs_rtp,                       &
!!     &         if_SGS_idct, ncomp_sgs_rtp_2_rj, frc_sgs_rtp)
!!      subroutine sel_SGS_s_flux_nl_gradient                           &
!!     &        (sph_rtp, sph_filters, coef,                            &
!!     &         ib_mhd_velo, ncomp_rj_2_rtp, fld_rtp,                  &
!!     &         ib_grad_ux, ib_grad_uy, ib_grad_uz, ib_grad_s,         &
!!     &         ncomp_sgs_rj_2_rtp, fld_sgs_rtp,                       &
!!     &         if_SGS_sflux, ncomp_sgs_rtp_2_rj, frc_sgs_rtp)
!!      subroutine sel_sph_SGS_m_flux_nl_gradient                       &
!!     &        (sph_rtp, sph_filters, coef,                            &
!!     &         ib_mhd_velo, ncomp_rj_2_rtp, fld_rtp,                  &
!!     &         ib_grad_ux, ib_grad_uy, ib_grad_uz,                    &
!!     &         ncomp_sgs_rj_2_rtp, fld_sgs_rtp,                       &
!!     &         if_SGS_mflux, ncomp_sgs_rtp_2_rj, frc_sgs_rtp)
!!       type(sph_rtp_grid), intent(in) :: sph_rtp
!!       type(sph_filters_type), intent(in) :: sph_filters
!!@endverbatim
!
      module sel_sph_SGS_nl_gradient
!
      use m_precision
      use t_spheric_rtp_data
      use t_sph_filtering_data
      use t_boundary_params_sph_MHD
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine sel_sph_SGS_induct_nl_gradient                         &
     &        (sph_rtp, sph_filters, coef,                              &
     &         ib_mhd_velo, ib_mhd_magne, ncomp_rj_2_rtp, fld_rtp,      &
     &         ib_grad_ux, ib_grad_uy, ib_grad_uz,                      &
     &         ib_grad_bx, ib_grad_by, ib_grad_bz,                      &
     &         ncomp_sgs_rj_2_rtp, fld_sgs_rtp,                         &
     &         if_SGS_idct, ncomp_sgs_rtp_2_rj, frc_sgs_rtp)
!
      use m_FFT_selector
      use sph_SGS_nl_gradient_pin
      use sph_SGS_nl_gradient_pout
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_filters_type), intent(in) :: sph_filters
      real(kind = kreal), intent(in) :: coef
!
      integer(kind = kint), intent(in) :: ncomp_rj_2_rtp
      integer(kind = kint), intent(in) :: ncomp_sgs_rj_2_rtp
      integer(kind = kint), intent(in) :: ncomp_sgs_rtp_2_rj
      integer(kind = kint), intent(in) :: ib_mhd_velo,  ib_grad_ux
      integer(kind = kint), intent(in) :: ib_grad_uy,   ib_grad_uz
      integer(kind = kint), intent(in) :: ib_mhd_magne, ib_grad_bx
      integer(kind = kint), intent(in) :: ib_grad_by,   ib_grad_bz
      integer(kind = kint), intent(in) :: if_SGS_idct
!
      real(kind = kreal), intent(in)                                    &
     &           :: fld_rtp(sph_rtp%nnod_rtp,ncomp_rj_2_rtp)
      real(kind = kreal), intent(in)                                    &
     &           :: fld_sgs_rtp(sph_rtp%nnod_rtp,ncomp_sgs_rj_2_rtp)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: frc_sgs_rtp(sph_rtp%nnod_rtp,ncomp_sgs_rtp_2_rj)
!
!
      write(*,*) 'frc_sgs_rtp start'
      write(50+my_rank,*) frc_sgs_rtp(1:sph_rtp%nnod_rtp,if_SGS_idct:if_SGS_idct+2)
      write(*,*) 'frc_sgs_rtp end'
!
      if(iflag_FFT .eq. iflag_FFTW) then
        call sph_SGS_induct_nl_gradient_pin                             &
     &    (sph_filters%kr_SGS_in, sph_filters%kr_SGS_out,               &
     &     sph_rtp%nnod_rtp, sph_rtp%nidx_rtp, sph_rtp%radius_1d_rtp_r, &
     &     sph_rtp%sin_theta_1d_rtp, sph_rtp%cos_theta_1d_rtp,          &
     &     coef, sph_filters%radial_2nd_moment,                         &
     &     sph_filters%theta_2nd_moment, sph_filters%phi_2nd_moment,    &
     &     fld_rtp(1,ib_mhd_velo), fld_sgs_rtp(1,ib_grad_ux),           &
     &     fld_sgs_rtp(1,ib_grad_uy), fld_sgs_rtp(1,ib_grad_uz),        &
     &     fld_rtp(1,ib_mhd_magne),   fld_sgs_rtp(1,ib_grad_bx),        &
     &     fld_sgs_rtp(1,ib_grad_by), fld_sgs_rtp(1,ib_grad_bz),        &
     &     frc_sgs_rtp(1,if_SGS_idct))
      else
        call sph_SGS_induct_nl_gradient_pout                            &
     &    (sph_filters%kr_SGS_in, sph_filters%kr_SGS_out,               &
     &     sph_rtp%nnod_rtp, sph_rtp%nidx_rtp, sph_rtp%radius_1d_rtp_r, &
     &     sph_rtp%sin_theta_1d_rtp, sph_rtp%cos_theta_1d_rtp,          &
     &     coef, sph_filters%radial_2nd_moment,                         &
     &     sph_filters%theta_2nd_moment, sph_filters%phi_2nd_moment,    &
     &     fld_rtp(1,ib_mhd_velo), fld_sgs_rtp(1,ib_grad_ux),           &
     &     fld_sgs_rtp(1,ib_grad_uy), fld_sgs_rtp(1,ib_grad_uz),        &
     &     fld_rtp(1,ib_mhd_magne),   fld_sgs_rtp(1,ib_grad_bx),        &
     &     fld_sgs_rtp(1,ib_grad_by), fld_sgs_rtp(1,ib_grad_bz),        &
     &     frc_sgs_rtp(1,if_SGS_idct))
      end if
!
      end subroutine sel_sph_SGS_induct_nl_gradient
!
!  ---------------------------------------------------------------------
!
      subroutine sel_SGS_s_flux_nl_gradient                             &
     &        (sph_rtp, sph_filters, coef,                              &
     &         ib_mhd_velo, ncomp_rj_2_rtp, fld_rtp,                    &
     &         ib_grad_ux, ib_grad_uy, ib_grad_uz, ib_grad_s,           &
     &         ncomp_sgs_rj_2_rtp, fld_sgs_rtp,                         &
     &         if_SGS_sflux, ncomp_sgs_rtp_2_rj, frc_sgs_rtp)
!
      use m_FFT_selector
      use sph_SGS_nl_gradient_pin
      use sph_SGS_nl_gradient_pout
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_filters_type), intent(in) :: sph_filters
      real(kind = kreal), intent(in) :: coef
!
      integer(kind = kint), intent(in) :: ncomp_rj_2_rtp
      integer(kind = kint), intent(in) :: ncomp_sgs_rj_2_rtp
      integer(kind = kint), intent(in) :: ncomp_sgs_rtp_2_rj
      integer(kind = kint), intent(in) :: ib_mhd_velo, ib_grad_ux
      integer(kind = kint), intent(in) :: ib_grad_uy,  ib_grad_uz
      integer(kind = kint), intent(in) :: ib_grad_s
      integer(kind = kint), intent(in) :: if_SGS_sflux
!
      real(kind = kreal), intent(in)                                    &
     &           :: fld_rtp(sph_rtp%nnod_rtp,ncomp_rj_2_rtp)
      real(kind = kreal), intent(in)                                    &
     &           :: fld_sgs_rtp(sph_rtp%nnod_rtp,ncomp_sgs_rj_2_rtp)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: frc_sgs_rtp(sph_rtp%nnod_rtp,ncomp_sgs_rtp_2_rj)
!
!
      if(iflag_FFT .eq. iflag_FFTW) then
        call sph_SGS_s_flux_nl_gradient_pin                             &
     &    (sph_filters%kr_SGS_in, sph_filters%kr_SGS_out,               &
     &     sph_rtp%nnod_rtp, sph_rtp%nidx_rtp, sph_rtp%radius_1d_rtp_r, &
     &     sph_rtp%sin_theta_1d_rtp, sph_rtp%cos_theta_1d_rtp,          &
     &     coef, sph_filters%radial_2nd_moment,                         &
     &     sph_filters%theta_2nd_moment, sph_filters%phi_2nd_moment,    &
     &     fld_rtp(1,ib_mhd_velo), fld_sgs_rtp(1,ib_grad_ux),           &
     &     fld_sgs_rtp(1,ib_grad_uy), fld_sgs_rtp(1,ib_grad_uz),        &
     &     fld_sgs_rtp(1,ib_grad_s),  frc_sgs_rtp(1,if_SGS_sflux))
      else
        call sph_SGS_s_flux_nl_gradient_pout                            &
     &    (sph_filters%kr_SGS_in, sph_filters%kr_SGS_out,               &
     &     sph_rtp%nnod_rtp, sph_rtp%nidx_rtp, sph_rtp%radius_1d_rtp_r, &
     &     sph_rtp%sin_theta_1d_rtp, sph_rtp%cos_theta_1d_rtp,          &
     &     coef, sph_filters%radial_2nd_moment,                         &
     &     sph_filters%theta_2nd_moment, sph_filters%phi_2nd_moment,    &
     &     fld_rtp(1,ib_mhd_velo), fld_sgs_rtp(1,ib_grad_ux),           &
     &     fld_sgs_rtp(1,ib_grad_uy), fld_sgs_rtp(1,ib_grad_uz),        &
     &     fld_sgs_rtp(1,ib_grad_s),  frc_sgs_rtp(1,if_SGS_sflux))
      end if
!
      end subroutine sel_SGS_s_flux_nl_gradient
!
!  ---------------------------------------------------------------------
!
      subroutine sel_sph_SGS_m_flux_nl_gradient                         &
     &        (sph_rtp, sph_filters, coef,                              &
     &         ib_mhd_velo, ncomp_rj_2_rtp, fld_rtp,                    &
     &         ib_grad_ux, ib_grad_uy, ib_grad_uz,                      &
     &         ncomp_sgs_rj_2_rtp, fld_sgs_rtp,                         &
     &         if_SGS_mflux, ncomp_sgs_rtp_2_rj, frc_sgs_rtp)
!
      use m_FFT_selector
      use sph_SGS_nl_gradient_pin
      use sph_SGS_nl_gradient_pout
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_filters_type), intent(in) :: sph_filters
      real(kind = kreal), intent(in) :: coef
!
      integer(kind = kint), intent(in) :: ncomp_rj_2_rtp
      integer(kind = kint), intent(in) :: ncomp_sgs_rj_2_rtp
      integer(kind = kint), intent(in) :: ncomp_sgs_rtp_2_rj
      integer(kind = kint), intent(in) :: ib_mhd_velo, ib_grad_ux
      integer(kind = kint), intent(in) :: ib_grad_uy,  ib_grad_uz
      integer(kind = kint), intent(in) :: if_SGS_mflux
!
      real(kind = kreal), intent(in)                                    &
     &           :: fld_rtp(sph_rtp%nnod_rtp,ncomp_rj_2_rtp)
      real(kind = kreal), intent(in)                                    &
     &           :: fld_sgs_rtp(sph_rtp%nnod_rtp,ncomp_sgs_rj_2_rtp)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: frc_sgs_rtp(sph_rtp%nnod_rtp,ncomp_sgs_rtp_2_rj)
!
!
      if(iflag_FFT .eq. iflag_FFTW) then
        call sph_SGS_m_flux_nl_gradient_pin                             &
     &    (sph_filters%kr_SGS_in, sph_filters%kr_SGS_out,               &
     &     sph_rtp%nnod_rtp, sph_rtp%nidx_rtp, sph_rtp%radius_1d_rtp_r, &
     &     sph_rtp%sin_theta_1d_rtp, sph_rtp%cos_theta_1d_rtp,          &
     &     coef, sph_filters%radial_2nd_moment,                         &
     &     sph_filters%theta_2nd_moment, sph_filters%phi_2nd_moment,    &
     &     fld_rtp(1,ib_mhd_velo), fld_sgs_rtp(1,ib_grad_ux),           &
     &     fld_sgs_rtp(1,ib_grad_uy), fld_sgs_rtp(1,ib_grad_uz),        &
     &     frc_sgs_rtp(1,if_SGS_mflux))
      else
        call sph_SGS_m_flux_nl_gradient_pout                            &
     &    (sph_filters%kr_SGS_in, sph_filters%kr_SGS_out,               &
     &     sph_rtp%nnod_rtp, sph_rtp%nidx_rtp, sph_rtp%radius_1d_rtp_r, &
     &     sph_rtp%sin_theta_1d_rtp, sph_rtp%cos_theta_1d_rtp,          &
     &     coef, sph_filters%radial_2nd_moment,                         &
     &     sph_filters%theta_2nd_moment, sph_filters%phi_2nd_moment,    &
     &     fld_rtp(1,ib_mhd_velo), fld_sgs_rtp(1,ib_grad_ux),           &
     &     fld_sgs_rtp(1,ib_grad_uy), fld_sgs_rtp(1,ib_grad_uz),        &
     &     frc_sgs_rtp(1,if_SGS_mflux))
      end if
!
      end subroutine sel_sph_SGS_m_flux_nl_gradient
!
!  ---------------------------------------------------------------------
!
      end module sel_sph_SGS_nl_gradient
