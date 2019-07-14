!>@file   set_vr_rtm_for_leg_matmul.f90
!!@brief  module set_vr_rtm_for_leg_matmul
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  forward Legendre transform using matrix multi
!!
!!@verbatim
!!      subroutine set_vr_rtm_vector_matmul(nnod_rtm, nidx_rtm,         &
!!     &         istep_rtm, nidx_rlm, asin_theta_1d_rtm, weight_rtm,    &
!!     &         kst, nkr, mp_rlm, mn_rlm, ncomp, irev_sr_rtm, n_WR, WR,&
!!     &         nvec_kl, symp_r, asmp_t, asmp_p, symn_t, symn_p)
!!      subroutine set_vr_rtm_scalar_matmul                             &
!!     &        (nnod_rtm, nidx_rtm, istep_rtm, nidx_rlm, weight_rtm,   &
!!     &         kst, nkr, mp_rlm, ncomp, nvector, irev_sr_rtm,         &
!!     &         n_WR, WR, nscl_lk, symp)
!!@endverbatim
!!
      module set_vr_rtm_for_leg_matmul
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_vr_rtm_vector_matmul(nnod_rtm, nidx_rtm,           &
     &         istep_rtm, nidx_rlm, asin_theta_1d_rtm, weight_rtm,      &
     &         kst, nkr, mp_rlm, mn_rlm, ncomp, irev_sr_rtm, n_WR, WR,  &
     &         nvec_kl, symp_r, asmp_t, asmp_p, symn_t, symn_p)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: istep_rtm(3)
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      real(kind = kreal), intent(in) :: weight_rtm(nidx_rtm(2))
      real(kind = kreal), intent(in) :: asin_theta_1d_rtm(nidx_rtm(2))
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
!
      integer(kind = kint) :: kr_nd, kk, k_rlm, nd
      integer(kind = kint) :: l_rtm, i_kl, ip_rtm, in_rtm
      integer(kind = kint) :: ip_recv, in_recv
      real(kind = kreal) :: wp_rtm, asin_rtm
!
!
      do l_rtm = 1, nidx_rtm(2)
        wp_rtm =   weight_rtm(l_rtm)
        asin_rtm = asin_theta_1d_rtm(l_rtm)
        do kk = 1, nkr
          kr_nd = kk + kst
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
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
          symp_r(i_kl) =  WR(ip_recv-2) * wp_rtm
!
          asmp_t(i_kl) =  WR(ip_recv-1) * wp_rtm
          asmp_p(i_kl) =  WR(ip_recv  ) * wp_rtm
!
          symn_t(i_kl) =  WR(in_recv-1) * wp_rtm*asin_rtm
          symn_p(i_kl) =  WR(in_recv  ) * wp_rtm*asin_rtm
        end do
      end do
!
      end subroutine set_vr_rtm_vector_matmul
!
! -----------------------------------------------------------------------
!
      subroutine set_vr_rtm_scalar_matmul                               &
     &        (nnod_rtm, nidx_rtm, istep_rtm, nidx_rlm, weight_rtm,     &
     &         kst, nkr, mp_rlm, ncomp, nvector, irev_sr_rtm,           &
     &         n_WR, WR, nscl_lk, symp)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: istep_rtm(3)
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      real(kind = kreal), intent(in) :: weight_rtm(nidx_rtm(2))
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
      real(kind = kreal) :: wp_rtm
!
!
      do l_rtm = 1, nidx_rtm(2)
        wp_rtm =   weight_rtm(l_rtm)
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
          symp(i_kl) =  WR(i_recv) * wp_rtm
        end do
      end do
!
      end subroutine set_vr_rtm_scalar_matmul
!
! -----------------------------------------------------------------------
!
      end module set_vr_rtm_for_leg_matmul
