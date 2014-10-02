!>@file   set_sp_rlm_for_leg_matmul.f90
!!@brief  module set_sp_rlm_for_leg_matmul
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  forward Legendre transform for testing
!!
!!@verbatim
!!      subroutine set_sp_rlm_vector_matmul                             &
!!     &         (kst, nkr, jst, nj_rlm, ncomp, irev_sr_rlm, n_WR, WR,  &
!!     &          nvec_jk, pol_e, dpl_e, tor_e)
!!      subroutine set_sp_rlm_scalar_matmul(kst, nkr, jst, nj_rlm,      &
!!     &          ncomp, nvector, irev_sr_rlm, n_WR, WR, nscl_jk, scl_e)
!!
!!      subroutine set_sp_rlm_vector_sym_matmul                         &
!!     &         (kst, nkr, jst, n_jk_e, n_jk_o,                        &
!!     &          ncomp, irev_sr_rlm, n_WR, WR,                         &
!!     &          nvec_jk, pol_e, dpl_e, tor_e, pol_o, dpl_o, tor_o)
!!      subroutine set_sp_rlm_scalar_sym_matmul                         &
!!     &         (kst, nkr, jst, n_jk_e, n_jk_o,                        &
!!     &          ncomp, nvector, irev_sr_rlm, n_WR, WR,                &
!!     &          nscl_jk, scl_e, scl_o)
!!@endverbatim
!!
      module set_sp_rlm_for_leg_matmul
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
      subroutine set_sp_rlm_vector_matmul                               &
     &         (kst, nkr, jst, nj_rlm, ncomp, irev_sr_rlm, n_WR, WR,    &
     &          nvec_jk, pol_e, dpl_e, tor_e)
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: jst, nj_rlm
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      integer(kind = kint), intent(in) :: nvec_jk
      real(kind = kreal), intent(inout) :: pol_e(nvec_jk)
      real(kind = kreal), intent(inout) :: dpl_e(nvec_jk)
      real(kind = kreal), intent(inout) :: tor_e(nvec_jk)
!
      integer(kind = kint) :: kk, kr_nd, k_rlm, nd
      integer(kind = kint) :: i_rlm, i_recv
      integer(kind = kint) :: jj, i_jk
      real(kind = kreal) :: a2r_1d_rlm_r
!
!
      do kk = 1, nkr
        kr_nd = kk + kst
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
        a2r_1d_rlm_r = a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
        do jj = 1, nj_rlm
          i_rlm = 1 + (jj+jst-1) * istep_rlm(2)                         &
     &              + (k_rlm-1) *  istep_rlm(1)
          i_recv = 3*nd + (irev_sr_rlm(i_rlm) - 1) * ncomp
          i_jk = jj + (kk-1) * nj_rlm
!
          pol_e(i_jk) = WR(i_recv-2) * a2r_1d_rlm_r
          dpl_e(i_jk) = WR(i_recv-1) * a_r_1d_rlm_r(k_rlm)
          tor_e(i_jk) = WR(i_recv  ) * a_r_1d_rlm_r(k_rlm)
        end do
      end do
!
      end subroutine set_sp_rlm_vector_matmul
!
! -----------------------------------------------------------------------
!
      subroutine set_sp_rlm_scalar_matmul(kst, nkr, jst, nj_rlm,        &
     &          ncomp, nvector, irev_sr_rlm, n_WR, WR, nscl_jk, scl_e)
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: jst, nj_rlm
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      integer(kind = kint), intent(in) :: nscl_jk
      real(kind = kreal), intent(inout) :: scl_e(nscl_jk)
!
      integer(kind = kint) :: kk, kr_nd, k_rlm, nd
      integer(kind = kint) :: i_rlm, i_recv
      integer(kind = kint) :: jj, i_jk
!
!
      do kk = 1, nkr
        kr_nd = kk + kst
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
        do jj = 1, nj_rlm
          i_rlm = 1 + (jj+jst-1) * istep_rlm(2)                         &
     &              + (k_rlm-1) *  istep_rlm(1)
          i_recv = nd + 3*nvector + (irev_sr_rlm(i_rlm) - 1) * ncomp
          i_jk = jj + (kk-1) * nj_rlm
!
          scl_e(i_jk) = WR(i_recv)
        end do
      end do
!
      end subroutine set_sp_rlm_scalar_matmul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sp_rlm_vector_sym_matmul                           &
     &         (kst, nkr, jst, n_jk_e, n_jk_o,                          &
     &          ncomp, irev_sr_rlm, n_WR, WR,                           &
     &          nvec_jk, pol_e, dpl_e, tor_e, pol_o, dpl_o, tor_o)
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: jst, n_jk_e, n_jk_o
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      integer(kind = kint), intent(in) :: nvec_jk
      real(kind = kreal), intent(inout) :: pol_e(nvec_jk)
      real(kind = kreal), intent(inout) :: dpl_e(nvec_jk)
      real(kind = kreal), intent(inout) :: tor_e(nvec_jk)
      real(kind = kreal), intent(inout) :: pol_o(nvec_jk)
      real(kind = kreal), intent(inout) :: dpl_o(nvec_jk)
      real(kind = kreal), intent(inout) :: tor_o(nvec_jk)
!
      integer(kind = kint) :: kk, kr_nd, k_rlm, nd
      integer(kind = kint) :: i_rlm, i_recv
      integer(kind = kint) :: jj, i_jk
      real(kind = kreal) :: a2r_1d_rlm_r
!
!
      do kk = 1, nkr
        kr_nd = kk + kst
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
        a2r_1d_rlm_r = a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
!   even l-m
        do jj = 1, n_jk_e
          i_rlm = 1 + (2*jj + jst - 2) * istep_rlm(2)                   &
     &              + (k_rlm-1) *        istep_rlm(1)
          i_recv = 3*nd + (irev_sr_rlm(i_rlm) - 1) * ncomp
!
          i_jk = jj + (kk-1) * n_jk_e
!
          pol_e(i_jk) = WR(i_recv-2) * a2r_1d_rlm_r
          dpl_e(i_jk) = WR(i_recv-1) * a_r_1d_rlm_r(k_rlm)
          tor_e(i_jk) = WR(i_recv  ) * a_r_1d_rlm_r(k_rlm)
        end do
!   odd l-m
        do jj = 1, n_jk_o
          i_rlm = 1 + (2*jj + jst-1) * istep_rlm(2)                     &
     &              + (k_rlm-1) *      istep_rlm(1)
          i_recv = 3*nd + (irev_sr_rlm(i_rlm) - 1) * ncomp
!
          i_jk = jj + (kk-1) * n_jk_o
          pol_o(i_jk) = WR(i_recv-2) * a2r_1d_rlm_r
          dpl_o(i_jk) = WR(i_recv-1) * a_r_1d_rlm_r(k_rlm)
          tor_o(i_jk) = WR(i_recv  ) * a_r_1d_rlm_r(k_rlm)
        end do
      end do
!
      end subroutine set_sp_rlm_vector_sym_matmul
!
! -----------------------------------------------------------------------
!
      subroutine set_sp_rlm_scalar_sym_matmul                           &
     &         (kst, nkr, jst, n_jk_e, n_jk_o,                          &
     &          ncomp, nvector, irev_sr_rlm, n_WR, WR,                  &
     &          nscl_jk, scl_e, scl_o)
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: jst, n_jk_e, n_jk_o
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      integer(kind = kint), intent(in) :: nscl_jk
      real(kind = kreal), intent(inout) :: scl_e(nscl_jk)
      real(kind = kreal), intent(inout) :: scl_o(nscl_jk)
!
      integer(kind = kint) :: kk, kr_nd, k_rlm, nd
      integer(kind = kint) :: i_rlm, i_recv
      integer(kind = kint) :: jj, i_jk
!
!
      do kk = 1, nkr
        kr_nd = kk + kst
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
!   even l-m
        do jj = 1, n_jk_e
          i_rlm = 1 + (2*jj + jst - 2) * istep_rlm(2)                   &
     &              + (k_rlm-1) *        istep_rlm(1)
          i_recv = nd + 3*nvector + (irev_sr_rlm(i_rlm) - 1) * ncomp
!
          i_jk = jj + (kk-1) * n_jk_e
          scl_e(i_jk) = WR(i_recv)
        end do
!   odd l-m
        do jj = 1, n_jk_o
          i_rlm = 1 + (2*jj + jst - 1) * istep_rlm(2)                   &
     &              + (k_rlm-1) *        istep_rlm(1)
          i_recv = nd + 3*nvector + (irev_sr_rlm(i_rlm) - 1) * ncomp
!
          i_jk = jj + (kk-1) * n_jk_o
          scl_o(i_jk) = WR(i_recv)
        end do
      end do
!
      end subroutine set_sp_rlm_scalar_sym_matmul
!
! -----------------------------------------------------------------------
!
      end module set_sp_rlm_for_leg_matmul
