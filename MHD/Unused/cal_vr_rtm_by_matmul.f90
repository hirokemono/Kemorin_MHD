!>@file   cal_vr_rtm_by_matmul.f90
!!@brief  module cal_vr_rtm_by_matmul
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  Backward Legendre transform after mat multi
!!
!!@verbatim
!!      subroutine cal_vr_rtm_vector_matmul(nnod_rtm, nidx_rtm,         &
!!     &        istep_rtm, nidx_rlm, asin_theta_1d_rtm, kst, nkr,       &
!!     &        mp_rlm, mn_rlm, nvec_lk, symp_r, asmp_t, asmp_p,        &
!!     &        symn_t, symn_p, ncomp, irev_sr_rtm, n_WS, WS)
!!      subroutine cal_vr_rtm_scalar_matmul                             &
!!     &         (nnod_rtm, nidx_rtm, istep_rtm, nidx_rlm,              &
!!     &          kst, nkr, mp_rlm, nscl_lk, symp, ncomp, nvector,      &
!!     &          irev_sr_rtm, n_WS, WS)
!!@endverbatim
!!
!
      module cal_vr_rtm_by_matmul
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
      subroutine cal_vr_rtm_vector_matmul(nnod_rtm, nidx_rtm,           &
     &        istep_rtm, nidx_rlm, asin_theta_1d_rtm, kst, nkr,         &
     &        mp_rlm, mn_rlm, nvec_lk, symp_r, asmp_t, asmp_p,          &
     &        symn_t, symn_p, ncomp, irev_sr_rtm, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: istep_rtm(3)
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      real(kind = kreal), intent(in) :: asin_theta_1d_rtm(nidx_rtm(2))
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
      integer(kind = kint), intent(in) :: nvec_lk
      real(kind = kreal), intent(in) ::    symp_r(nvec_lk)
      real(kind = kreal), intent(in) ::    asmp_t(nvec_lk)
      real(kind = kreal), intent(in) ::    asmp_p(nvec_lk)
      real(kind = kreal), intent(inout) :: symn_t(nvec_lk)
      real(kind = kreal), intent(inout) :: symn_p(nvec_lk)
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: kk, kr_nd, k_rlm, nd, l_rtm, i_lk
      integer(kind = kint) :: ip_rtm, in_rtm, ip_send, in_send
!
!
      do kk = 1, nkr
        do l_rtm = 1, nidx_rtm(2)
          i_lk = l_rtm + (kk-1) * nidx_rtm(2)
          symn_t(i_lk) = - symn_t(i_lk) * asin_theta_1d_rtm(l_rtm)
          symn_p(i_lk) = - symn_p(i_lk) * asin_theta_1d_rtm(l_rtm)
        end do
      end do
!
      do kk = 1, nkr
        kr_nd = kk + kst
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
        do l_rtm = 1, nidx_rtm(2)
          i_lk = l_rtm + (kk-1) * nidx_rtm(2)
          ip_rtm = 1 + (l_rtm-1) *  istep_rtm(2)                        &
     &               + (k_rlm-1) *  istep_rtm(1)                        &
     &               + (mp_rlm-1) * istep_rtm(3)
          in_rtm = 1 + (l_rtm-1) *  istep_rtm(2)                        &
     &               + (k_rlm-1) *  istep_rtm(1)                        &
     &               + (mn_rlm-1) * istep_rtm(3)
!
          ip_send = 3*nd + (irev_sr_rtm(ip_rtm) - 1) * ncomp
          in_send = 3*nd + (irev_sr_rtm(in_rtm) - 1) * ncomp
!
          WS(ip_send-2) = WS(ip_send-2) + symp_r(i_lk)
          WS(ip_send-1) = WS(ip_send-1) + asmp_t(i_lk)
          WS(ip_send  ) = WS(ip_send  ) - asmp_p(i_lk)
!
          WS(in_send-1) = WS(in_send-1) + symn_t(i_lk)
          WS(in_send  ) = WS(in_send  ) + symn_p(i_lk)
        end do
      end do
!
      end subroutine cal_vr_rtm_vector_matmul
!
! -----------------------------------------------------------------------
!
      subroutine cal_vr_rtm_scalar_matmul                               &
     &         (nnod_rtm, nidx_rtm, istep_rtm, nidx_rlm,                &
     &          kst, nkr, mp_rlm, nscl_lk, symp, ncomp, nvector,        &
     &          irev_sr_rtm, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rtm
      integer(kind = kint), intent(in) :: nidx_rtm(3)
      integer(kind = kint), intent(in) :: istep_rtm(3)
      integer(kind = kint), intent(in) :: nidx_rlm(2)
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: mp_rlm
      integer(kind = kint), intent(in) :: nscl_lk
      real(kind = kreal), intent(in) :: symp(nscl_lk)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: irev_sr_rtm(nnod_rtm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: kk, kr_nd, k_rlm, nd
      integer(kind = kint) :: l_rtm, i_lk, ip_rtm, ip_send
!
!
      do kk = 1, nkr
        kr_nd = kk + kst
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
        do l_rtm = 1, nidx_rtm(2)
          i_lk = l_rtm + (kk-1) * nidx_rtm(2)
          ip_rtm = 1 + (l_rtm-1) *  istep_rtm(2)                        &
     &               + (k_rlm-1) *  istep_rtm(1)                        &
     &               + (mp_rlm-1) * istep_rtm(3)
          ip_send = nd + 3*nvector + (irev_sr_rtm(ip_rtm) - 1) * ncomp
          WS(ip_send) = WS(ip_send) + symp(i_lk)
        end do
      end do
!
      end subroutine cal_vr_rtm_scalar_matmul
!
! -----------------------------------------------------------------------
!
      end module cal_vr_rtm_by_matmul
