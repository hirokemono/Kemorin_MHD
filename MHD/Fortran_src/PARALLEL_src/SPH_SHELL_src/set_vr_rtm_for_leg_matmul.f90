!>@file   set_vr_rtm_for_leg_matmul.f90
!!@brief  module set_vr_rtm_for_leg_matmul
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  forward Legendre transform for testing
!!
!!@verbatim
!!      subroutine set_vr_rtm_vector_matmul(kst, nkr,                   &
!!     &         mp_rlm, mn_rlm, ncomp, irev_sr_rtm, n_WR, WR,          &
!!     &         nvec_kl, symp_r, asmp_t, asmp_p, symn_t, symn_p)
!!      subroutine set_vr_rtm_scalar_matmul(kst, nkr, mp_rlm,           &
!!     &          ncomp, nvector, irev_sr_rtm, n_WR, WR, nscl_lk, symp)
!!
!!      subroutine set_vr_rtm_vector_sym_matmul                         &
!!     &         (kst, nkr, mp_rlm, mn_rlm, nle_rtm, nlo_rtm,           &
!!     &          ncomp, irev_sr_rtm, n_WR, WR,                         &
!!     &          nvec_kl, symp_r, asmp_t, asmp_p,                      &
!!     &          symn_t, symn_p, asmp_r,                               &
!!     &          symp_t, symp_p, asmn_t, asmn_p)
!!      subroutine set_vr_rtm_scalar_sym_matmul                         &
!!     &         (kst, nkr, mp_rlm, nle_rtm, nlo_rtm,                   &
!!     &          ncomp, nvector, irev_sr_rtm, n_WR, WR,                &
!!     &          nscl_lk, symp, asmp)
!!@endverbatim
!!
      module set_vr_rtm_for_leg_matmul
!
      use m_precision
      use m_spheric_parameter
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_vr_rtm_vector_matmul(kst, nkr,                     &
     &         mp_rlm, mn_rlm, ncomp, irev_sr_rtm, n_WR, WR,            &
     &         nvec_kl, symp_r, asmp_t, asmp_p, symn_t, symn_p)
!
      use m_precision
      use m_constants
      use m_spheric_parameter
!
      implicit none
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(in) :: WR(n_WR)
!
      integer(kind = kint), intent(in) :: nvec_kl
      real(kind = kreal), intent(inout) :: symp_r(nvec_kl)
      real(kind = kreal), intent(inout) :: asmp_t(nvec_kl)
      real(kind = kreal), intent(inout) :: asmp_p(nvec_kl)
      real(kind = kreal), intent(inout) :: symn_t(nvec_kl)
      real(kind = kreal), intent(inout) :: symn_p(nvec_kl)
      real(kind = kreal) :: r2_1d_rlm_r
!
!
      integer(kind = kint) :: kr_nd, kk, k_rlm, nd
      integer(kind = kint) :: l_rtm, i_kl, ip_rtm, in_rtm
      integer(kind = kint) :: ip_recv, in_recv
!
!
      do l_rtm = 1, nidx_rtm(2)
        do kk = 1, nkr
          kr_nd = kk + kst
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
          r2_1d_rlm_r = radius_1d_rlm_r(k_rlm) * radius_1d_rlm_r(k_rlm)
!
          ip_rtm = 1 + (l_rtm-1) *  istep_rtm(2)                        &
     &               + (k_rlm-1) *  istep_rtm(1)                        &
     &               + (mp_rlm-1) * istep_rtm(3)
          in_rtm = 1 + (l_rtm-1) *  istep_rtm(2)                        &
     &               + (k_rlm-1) *  istep_rtm(1)                        &
     &               + (mn_rlm-1) * istep_rtm(3)
          ip_recv = 3*nd + (irev_sr_rtm(ip_rtm) - 1) * ncomp
          in_recv = 3*nd + (irev_sr_rtm(in_rtm) - 1) * ncomp
          i_kl = kk + (l_rtm-1) * nkr
!
          symp_r(i_kl) =  WR(ip_recv-2) * r2_1d_rlm_r
!
          asmp_t(i_kl) =  WR(ip_recv-1) * radius_1d_rlm_r(k_rlm)
          asmp_p(i_kl) =  WR(ip_recv  ) * radius_1d_rlm_r(k_rlm)
!
          symn_t(i_kl) =  WR(in_recv-1) * radius_1d_rlm_r(k_rlm)
          symn_p(i_kl) =  WR(in_recv  ) * radius_1d_rlm_r(k_rlm)
        end do
      end do
!
      end subroutine set_vr_rtm_vector_matmul
!
! -----------------------------------------------------------------------
!
      subroutine set_vr_rtm_scalar_matmul(kst, nkr, mp_rlm,             &
     &          ncomp, nvector, irev_sr_rtm, n_WR, WR, nscl_lk, symp)
!
      use m_precision
      use m_constants
      use m_spheric_parameter
!
      implicit none
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: mp_rlm
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(in) :: WR(n_WR)
!
      integer(kind = kint), intent(in) :: nscl_lk
      real(kind = kreal), intent(inout) :: symp(nscl_lk)
!
      integer(kind = kint) :: kr_nd, kk, k_rlm, nd
      integer(kind = kint) :: l_rtm, i_kl
      integer(kind = kint) :: ip_rtm, i_recv
!
!
      do l_rtm = 1, nidx_rtm(2)
        do kk = 1, nkr
          kr_nd = kk + kst
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
!
          ip_rtm = 1 + (l_rtm-1) *  istep_rtm(2)                        &
     &               + (k_rlm-1) *  istep_rtm(1)                        &
     &               + (mp_rlm-1) * istep_rtm(3)
          i_recv = nd + 3*nvector + (irev_sr_rtm(ip_rtm) - 1) * ncomp
          i_kl = kk + (l_rtm-1) * nkr
!
          symp(i_kl) =  WR(i_recv)
        end do
      end do
!
      end subroutine set_vr_rtm_scalar_matmul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_vr_rtm_vector_sym_matmul                           &
     &         (kst, nkr, mp_rlm, mn_rlm, nle_rtm, nlo_rtm,             &
     &          ncomp, irev_sr_rtm, n_WR, WR,                           &
     &          nvec_kl, symp_r, asmp_t, asmp_p,                        &
     &          symn_t, symn_p, asmp_r,                                 &
     &          symp_t, symp_p, asmn_t, asmn_p)
!
      use m_precision
      use m_constants
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_work_4_sph_trans
!
      implicit none
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
      integer(kind = kint), intent(in) :: nle_rtm, nlo_rtm
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(in) :: WR(n_WR)
!
      integer(kind = kint), intent(in) :: nvec_kl
      real(kind = kreal), intent(inout) :: symp_r(nvec_kl)
      real(kind = kreal), intent(inout) :: asmp_t(nvec_kl)
      real(kind = kreal), intent(inout) :: asmp_p(nvec_kl)
      real(kind = kreal), intent(inout) :: symn_t(nvec_kl)
      real(kind = kreal), intent(inout) :: symn_p(nvec_kl)
      real(kind = kreal), intent(inout) :: asmp_r(nvec_kl)
      real(kind = kreal), intent(inout) :: symp_t(nvec_kl)
      real(kind = kreal), intent(inout) :: symp_p(nvec_kl)
      real(kind = kreal), intent(inout) :: asmn_t(nvec_kl)
      real(kind = kreal), intent(inout) :: asmn_p(nvec_kl)
!
!
      integer(kind = kint) :: kr_nd, kk, k_rlm, nd
      integer(kind = kint) :: lp_rtm, ln_rtm, i_kl
      integer(kind = kint) :: ip_rtpm, in_rtpm, ip_rtnm, in_rtnm
      integer(kind = kint) :: ipp_recv, ipn_recv, inp_recv, inn_recv
      real(kind = kreal) :: wp_rtm, asin_rtm, r1_1d_rlm_r, r2_1d_rlm_r
!
!
      do lp_rtm = 1, nlo_rtm
        ln_rtm = nidx_rtm(2) - lp_rtm + 1
        wp_rtm =   weight_rtm(lp_rtm)
        asin_rtm = asin_theta_1d_rtm(lp_rtm)
        do kk = 1, nkr
          kr_nd = kk + kst
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
          r1_1d_rlm_r = radius_1d_rlm_r(k_rlm)
          r2_1d_rlm_r = radius_1d_rlm_r(k_rlm) * radius_1d_rlm_r(k_rlm)
!
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
          in_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
          ipp_recv = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
          ipn_recv = 3*nd + (irev_sr_rtm(ip_rtnm) - 1) * ncomp
          inp_recv = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp
          inn_recv = 3*nd + (irev_sr_rtm(in_rtnm) - 1) * ncomp
!
          i_kl = kk + (lp_rtm-1) * nkr
!
          symp_r(i_kl) = (WR(ipp_recv-2) + WR(ipn_recv-2))              &
     &                  * wp_rtm * r2_1d_rlm_r
          symp_t(i_kl) = (WR(ipp_recv-1) + WR(ipn_recv-1))              &
     &                  * wp_rtm * r1_1d_rlm_r
          symp_p(i_kl) = (WR(ipp_recv  ) + WR(ipn_recv  ))              &
     &                  * wp_rtm * r1_1d_rlm_r
!
          asmp_r(i_kl) = (WR(ipp_recv-2) - WR(ipn_recv-2))              &
     &                  * wp_rtm * r2_1d_rlm_r
          asmp_t(i_kl) = (WR(ipp_recv-1) - WR(ipn_recv-1))              &
     &                  * wp_rtm * r1_1d_rlm_r
          asmp_p(i_kl) = (WR(ipp_recv  ) - WR(ipn_recv  ))              &
     &                  * wp_rtm * r1_1d_rlm_r
!
          symn_t(i_kl) = (WR(inp_recv-1) + WR(inn_recv-1))              &
     &                  * wp_rtm * r1_1d_rlm_r*asin_rtm
          symn_p(i_kl) = (WR(inp_recv  ) + WR(inn_recv  ))              &
     &                  * wp_rtm * r1_1d_rlm_r*asin_rtm
!
          asmn_t(i_kl) = (WR(inp_recv-1) - WR(inn_recv-1))              &
     &                  * wp_rtm * r1_1d_rlm_r*asin_rtm
          asmn_p(i_kl) = (WR(inp_recv  ) - WR(inn_recv  ))              &
     &                  * wp_rtm * r1_1d_rlm_r*asin_rtm
        end do
      end do
!   Equator (if necessary)
      do lp_rtm = nlo_rtm+1, nle_rtm
        wp_rtm = weight_rtm(lp_rtm)
        asin_rtm = asin_theta_1d_rtm(lp_rtm)
        do kk = 1, nkr
          kr_nd = kk + kst
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
          r1_1d_rlm_r = radius_1d_rlm_r(k_rlm)
          r2_1d_rlm_r = radius_1d_rlm_r(k_rlm) * radius_1d_rlm_r(k_rlm)
!
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          in_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mn_rlm-1) * istep_rtm(3)
          ipp_recv = 3*nd + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
          inp_recv = 3*nd + (irev_sr_rtm(in_rtpm) - 1) * ncomp
!
          i_kl = kk + (lp_rtm-1) * nkr
!
          symp_r(i_kl) = WR(ipp_recv-2) * wp_rtm * r2_1d_rlm_r
          symp_t(i_kl) = WR(ipp_recv-1) * wp_rtm * r1_1d_rlm_r
          symp_p(i_kl) = WR(ipp_recv  ) * wp_rtm * r1_1d_rlm_r
!
          symn_t(i_kl) = WR(inp_recv-1) * wp_rtm * r1_1d_rlm_r*asin_rtm
          symn_p(i_kl) = WR(inp_recv  ) * wp_rtm * r1_1d_rlm_r*asin_rtm
        end do
      end do
!
      end subroutine set_vr_rtm_vector_sym_matmul
!
! -----------------------------------------------------------------------
!
      subroutine set_vr_rtm_scalar_sym_matmul                           &
     &         (kst, nkr, mp_rlm, nle_rtm, nlo_rtm,                     &
     &          ncomp, nvector, irev_sr_rtm, n_WR, WR,                  &
     &          nscl_lk, symp, asmp)
!
      use m_precision
      use m_constants
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
!
      implicit none
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: mp_rlm
      integer(kind = kint), intent(in) :: nle_rtm, nlo_rtm
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      real (kind=kreal), intent(in) :: WR(n_WR)
!
      integer(kind = kint), intent(in) :: nscl_lk
      real(kind = kreal), intent(inout) :: symp(nscl_lk)
      real(kind = kreal), intent(inout) :: asmp(nscl_lk)
!
      integer(kind = kint) :: kr_nd, kk, k_rlm, nd
      integer(kind = kint) :: lp_rtm, ln_rtm, i_kl
      integer(kind = kint) :: ip_rtpm, ip_rtnm, ipp_recv, ipn_recv
      real(kind = kreal) :: wp_rtm
!
!
      do lp_rtm = 1, nlo_rtm
        ln_rtm = nidx_rtm(2) - lp_rtm + 1
        wp_rtm = weight_rtm(lp_rtm)
        do kk = 1, nkr
          kr_nd = kk + kst
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          ip_rtnm = 1 + (ln_rtm-1) * istep_rtm(2)                       &
     &                + (k_rlm-1) *  istep_rtm(1)                       &
     &                + (mp_rlm-1) * istep_rtm(3)
          ipp_recv = nd + 3*nvector                                     &
     &                  + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
          ipn_recv = nd + 3*nvector                                     &
     &                  + (irev_sr_rtm(ip_rtnm) - 1) * ncomp
!
          i_kl = kk + (lp_rtm-1) * nkr
!
          symp(i_kl) = (WR(ipp_recv) + WR(ipn_recv)) * wp_rtm
          asmp(i_kl) = (WR(ipp_recv) - WR(ipn_recv)) * wp_rtm
        end do
      end do
!   Equator (if necessary)
      do lp_rtm = nlo_rtm+1, nle_rtm
        wp_rtm = weight_rtm(lp_rtm)
        do kk = 1, nkr
          kr_nd = kk + kst
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
          ip_rtpm = 1 + (lp_rtm-1) * istep_rtm(2)                   &
     &                + (k_rlm-1) *  istep_rtm(1)                   &
     &                + (mp_rlm-1) * istep_rtm(3)
          ipp_recv = nd + 3*nvector                                 &
     &                  + (irev_sr_rtm(ip_rtpm) - 1) * ncomp
          i_kl = kk + (lp_rtm-1) * nkr
!
          symp(i_kl) = WR(ipp_recv) * wp_rtm
        end do
      end do
!
      end subroutine set_vr_rtm_scalar_sym_matmul
!
! -----------------------------------------------------------------------
!
      end module set_vr_rtm_for_leg_matmul
