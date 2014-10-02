!>@file   cal_vr_rtm_by_matmul.f90
!!@brief  module cal_vr_rtm_by_matmul
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  forward Legendre transform for testing
!!
!!@verbatim
!!      subroutine cal_vr_rtm_vector_matmul                             &
!!     &       (kst, nkr, mp_rlm, mn_rlm, nvec_lk,                      &
!!     &        symp_r, asmp_t, asmp_p, symn_t, symn_p, ncomp, vr_rtm)
!!      subroutine cal_vr_rtm_scalar_matmul(kst, nkr, mp_rlm,           &
!!     &          nscl_lk, symp, ncomp, nvector, vr_rtm)
!!
!!      subroutine cal_vr_rtm_vector_sym_matmul                         &
!!     &       (kst, nkr, mp_rlm, mn_rlm, nl_rtm, nvec_lk,              &
!!     &        symp_r, asmp_t, asmp_p, symn_t, symn_p,                 &
!!     &        asmp_r, symp_t, symp_p, asmn_t, asmn_p, ncomp, vr_rtm)
!!      subroutine cal_vr_rtm_scalar_sym_matmul(kst, nkr, mp_rlm,       &
!!     &          nl_rtm, nscl_lk, symp, asmp, ncomp, nvector, vr_rtm)
!!@endverbatim
!!
!
      module cal_vr_rtm_by_matmul
!
      use m_precision
      use m_constants
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
      subroutine cal_vr_rtm_vector_matmul                               &
     &       (kst, nkr, mp_rlm, mn_rlm, nvec_lk,                        &
     &        symp_r, asmp_t, asmp_p, symn_t, symn_p, ncomp, vr_rtm)
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
      integer(kind = kint), intent(in) :: nvec_lk
      real(kind = kreal), intent(in) :: symp_r(nvec_lk)
      real(kind = kreal), intent(in) :: asmp_t(nvec_lk)
      real(kind = kreal), intent(in) :: asmp_p(nvec_lk)
      real(kind = kreal), intent(in) :: symn_t(nvec_lk)
      real(kind = kreal), intent(in) :: symn_p(nvec_lk)
!
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(inout) :: vr_rtm(ncomp*nnod_rtm)
!
      integer(kind = kint) :: kk, kr_nd, k_rlm, nd
      integer(kind = kint) :: l_rtm, i_lk, ip_rtm, in_rtm
!
!
      do kk = 1, nkr
        kr_nd = kk + kst
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
        do l_rtm = 1, nidx_rtm(2)
          i_lk = l_rtm + (kk-1) * nidx_rtm(2)
          ip_rtm = 3*nd + ncomp * ((l_rtm-1) *  istep_rtm(2)            &
     &                           + (k_rlm-1) *  istep_rtm(1)            &
     &                           + (mp_rlm-1) * istep_rtm(3))
          in_rtm = 3*nd + ncomp * ((l_rtm-1) *  istep_rtm(2)            &
     &                           + (k_rlm-1) *  istep_rtm(1)            &
     &                           + (mn_rlm-1) * istep_rtm(3))
!
!
          vr_rtm(ip_rtm-2) = vr_rtm(ip_rtm-2) + symp_r(i_lk)
          vr_rtm(ip_rtm-1) = vr_rtm(ip_rtm-1) + asmp_t(i_lk)
          vr_rtm(ip_rtm  ) = vr_rtm(ip_rtm  ) - asmp_p(i_lk)
!
          vr_rtm(in_rtm-1) = vr_rtm(in_rtm-1) + symn_t(i_lk)
          vr_rtm(in_rtm  ) = vr_rtm(in_rtm  ) + symn_p(i_lk)
        end do
      end do
!
      end subroutine cal_vr_rtm_vector_matmul
!
! -----------------------------------------------------------------------
!
      subroutine cal_vr_rtm_scalar_matmul(kst, nkr, mp_rlm,             &
     &          nscl_lk, symp, ncomp, nvector, vr_rtm)
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: mp_rlm
      integer(kind = kint), intent(in) :: nscl_lk
      real(kind = kreal), intent(in) :: symp(nscl_lk)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      real(kind = kreal), intent(inout) :: vr_rtm(ncomp*nnod_rtm)
!
      integer(kind = kint) :: kk, kr_nd, k_rlm, nd
      integer(kind = kint) :: l_rtm, i_lk, ip_rtm
!
!
      do kk = 1, nkr
        kr_nd = kk + kst
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
        do l_rtm = 1, nidx_rtm(2)
          i_lk = l_rtm + (kk-1) * nidx_rtm(2)
          ip_rtm = nd + 3*nvector                                       &
     &                + ncomp * ((l_rtm-1) *  istep_rtm(2)              &
     &                         + (k_rlm-1) *  istep_rtm(1)              &
     &                         + (mp_rlm-1) * istep_rtm(3))
          vr_rtm(ip_rtm) = vr_rtm(ip_rtm) + symp(i_lk)
        end do
      end do
!
      end subroutine cal_vr_rtm_scalar_matmul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_vr_rtm_vector_sym_matmul                           &
     &       (kst, nkr, mp_rlm, mn_rlm, nl_rtm, nvec_lk,                &
     &        symp_r, asmp_t, asmp_p, symn_t, symn_p,                   &
     &        asmp_r, symp_t, symp_p, asmn_t, asmn_p, ncomp, vr_rtm)
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: mp_rlm, mn_rlm
      integer(kind = kint), intent(in) :: nl_rtm
      integer(kind = kint), intent(in) :: nvec_lk
      real(kind = kreal), intent(in) :: symp_r(nvec_lk)
      real(kind = kreal), intent(in) :: asmp_t(nvec_lk)
      real(kind = kreal), intent(in) :: asmp_p(nvec_lk)
      real(kind = kreal), intent(in) :: symn_t(nvec_lk)
      real(kind = kreal), intent(in) :: symn_p(nvec_lk)
      real(kind = kreal), intent(in) :: asmp_r(nvec_lk)
      real(kind = kreal), intent(in) :: symp_t(nvec_lk)
      real(kind = kreal), intent(in) :: symp_p(nvec_lk)
      real(kind = kreal), intent(in) :: asmn_t(nvec_lk)
      real(kind = kreal), intent(in) :: asmn_p(nvec_lk)
!
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(inout) :: vr_rtm(ncomp*nnod_rtm)
!
      integer(kind = kint) :: kk, kr_nd, k_rlm, nd
      integer(kind = kint) :: lp_rtm, ln_rtm, i_lk
      integer(kind = kint) :: ip_rtpm, in_rtpm, ip_rtnm, in_rtnm
!
!
      do kk = 1, nkr
        kr_nd = kk + kst
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
        do lp_rtm = 1, nidx_rtm(2)/2
          ln_rtm =  nidx_rtm(2) - lp_rtm + 1
          i_lk = lp_rtm + (kk-1) * nl_rtm
          ip_rtpm = 3*nd + ncomp*((lp_rtm-1) * istep_rtm(2)             &
     &                          + (k_rlm-1) *  istep_rtm(1)             &
     &                          + (mp_rlm-1) * istep_rtm(3))
          in_rtpm = 3*nd + ncomp*((lp_rtm-1) * istep_rtm(2)             &
     &                          + (k_rlm-1) *  istep_rtm(1)             &
     &                          + (mn_rlm-1) * istep_rtm(3))
          ip_rtnm = 3*nd + ncomp*((ln_rtm-1) * istep_rtm(2)             &
     &                          + (k_rlm-1) *  istep_rtm(1)             &
     &                          + (mp_rlm-1) * istep_rtm(3))
          in_rtnm = 3*nd + ncomp*((ln_rtm-1) * istep_rtm(2)             &
     &                          + (k_rlm-1) *  istep_rtm(1)             &
     &                          + (mn_rlm-1) * istep_rtm(3))
!
          vr_rtm(ip_rtpm-2) = vr_rtm(ip_rtpm-2)                         &
     &                       + symp_r(i_lk) + asmp_r(i_lk)
          vr_rtm(ip_rtpm-1) = vr_rtm(ip_rtpm-1)                         &
     &                       + asmp_t(i_lk) + symp_t(i_lk)
          vr_rtm(ip_rtpm  ) = vr_rtm(ip_rtpm  )                         &
     &                       - asmp_p(i_lk) - symp_p(i_lk)
!
          vr_rtm(in_rtpm-1) = vr_rtm(in_rtpm-1)                         &
     &                       + symn_t(i_lk) + asmn_t(i_lk)
          vr_rtm(in_rtpm  ) = vr_rtm(in_rtpm  )                         &
     &                       + symn_p(i_lk) + asmn_p(i_lk)
!
!
          vr_rtm(ip_rtnm-2) = vr_rtm(ip_rtnm-2)                         &
     &                       + symp_r(i_lk) - asmp_r(i_lk)
          vr_rtm(ip_rtnm-1) = vr_rtm(ip_rtnm-1)                         &
     &                       - asmp_t(i_lk) + symp_t(i_lk)
          vr_rtm(ip_rtnm  ) = vr_rtm(ip_rtnm  )                         &
     &                       + asmp_p(i_lk) - symp_p(i_lk)
!
          vr_rtm(in_rtnm-1) = vr_rtm(in_rtnm-1)                         &
     &                       + symn_t(i_lk) - asmn_t(i_lk)
          vr_rtm(in_rtnm  ) = vr_rtm(in_rtnm  )                         &
     &                       + symn_p(i_lk) - asmn_p(i_lk)
        end do
!
        do lp_rtm = nidx_rtm(2)/2+1, nl_rtm
          ln_rtm =  nidx_rtm(2) - nidx_rtm(2)/2-1 + 1
          i_lk = lp_rtm + (kk-1) * nl_rtm
          ip_rtpm = 3*nd + ncomp*((lp_rtm-1) * istep_rtm(2)             &
     &                          + (k_rlm-1) *  istep_rtm(1)             &
     &                          + (mp_rlm-1) * istep_rtm(3))
          in_rtpm = 3*nd + ncomp*((lp_rtm-1) * istep_rtm(2)             &
     &                          + (k_rlm-1) *  istep_rtm(1)             &
     &                          + (mn_rlm-1) * istep_rtm(3))
!
          vr_rtm(ip_rtpm-2) = vr_rtm(ip_rtpm-2) + symp_r(i_lk)
          vr_rtm(ip_rtpm-1) = vr_rtm(ip_rtpm-1) + symp_t(i_lk)
          vr_rtm(ip_rtpm  ) = vr_rtm(ip_rtpm  ) - symp_p(i_lk)
!
          vr_rtm(in_rtpm-1) = vr_rtm(in_rtpm-1) + symn_t(i_lk)
          vr_rtm(in_rtpm  ) = vr_rtm(in_rtpm  ) + symn_p(i_lk)
        end do
      end do
!
      end subroutine cal_vr_rtm_vector_sym_matmul
!
! -----------------------------------------------------------------------
!
      subroutine cal_vr_rtm_scalar_sym_matmul(kst, nkr, mp_rlm,         &
     &          nl_rtm, nscl_lk, symp, asmp, ncomp, nvector, vr_rtm)
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: mp_rlm
      integer(kind = kint), intent(in) :: nl_rtm
      integer(kind = kint), intent(in) :: nscl_lk
      real(kind = kreal), intent(in) :: symp(nscl_lk)
      real(kind = kreal), intent(in) :: asmp(nscl_lk)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      real(kind = kreal), intent(inout) :: vr_rtm(ncomp*nnod_rtm)
!
      integer(kind = kint) :: kk, kr_nd, k_rlm, nd
      integer(kind = kint) :: lp_rtm, ln_rtm, i_lk
      integer(kind = kint) :: ip_rtpm, ip_rtnm
!
!
      do kk = 1, nkr
        kr_nd = kk + kst
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
        do lp_rtm = 1, nidx_rtm(2)/2
          ln_rtm =  nidx_rtm(2) - lp_rtm + 1
          i_lk = lp_rtm + (kk-1) * nl_rtm
!
          ip_rtpm = nd + 3*nvector                                      &
     &                  + ncomp*((lp_rtm-1) * istep_rtm(2)              &
     &                         + (k_rlm-1) *  istep_rtm(1)              &
     &                         + (mp_rlm-1) * istep_rtm(3))
          ip_rtnm = nd + 3*nvector                                      &
     &                  + ncomp*((ln_rtm-1) *  istep_rtm(2)             &
     &                          + (k_rlm-1) *  istep_rtm(1)             &
     &                          + (mp_rlm-1) * istep_rtm(3))
!
          vr_rtm(ip_rtpm) = vr_rtm(ip_rtpm)                             &
     &                     + symp(i_lk) + asmp(i_lk)
          vr_rtm(ip_rtnm) = vr_rtm(ip_rtnm)                             &
     &                     + symp(i_lk) - asmp(i_lk)
        end do
!
        do lp_rtm = nidx_rtm(2)/2+1, nl_rtm
          i_lk = lp_rtm + (kk-1) * nl_rtm
!
          ip_rtpm = nd + 3*nvector                                      &
     &                  + ncomp*((lp_rtm-1) * istep_rtm(2)              &
     &                         + (k_rlm-1) *  istep_rtm(1)              &
     &                         + (mp_rlm-1) * istep_rtm(3))
          ip_rtnm = nd + 3*nvector                                      &
     &                  + ncomp*((lp_rtm-1) * istep_rtm(2)              &
     &                         + (k_rlm-1) *  istep_rtm(1)              &
     &                         + (mp_rlm-1) * istep_rtm(3))
!
          vr_rtm(ip_rtpm) = vr_rtm(ip_rtpm) + symp(i_lk)
        end do
      end do
!
      end subroutine cal_vr_rtm_scalar_sym_matmul
!
! -----------------------------------------------------------------------
!
      end module cal_vr_rtm_by_matmul
