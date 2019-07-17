!>@file   cal_sp_rlm_by_matmul.f90
!!@brief  module cal_sp_rlm_by_matmul
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  forward Legendre transform using matmulti
!!
!!@verbatim
!!      subroutine cal_sp_rlm_vector_matmul(nnod_rlm, nidx_rlm,         &
!!     &         istep_rlm, idx_gl_1d_rlm_j, radius_1d_rlm_r, g_sph_rlm,&
!!     &         kst, nkr, jst, nj_rlm, nvec_jk,                        &
!!     &         pol_e, dpoldt_e, dpoldp_e, dtordt_e, dtordp_e,         &
!!     &         ncomp, irev_sr_rlm, n_WS, WS)
!!      subroutine cal_sp_rlm_scalar_matmul(nnod_rlm, nidx_rlm,         &
!!     &          istep_rlm, g_sph_rlm, kst, nkr, jst, nj_rlm,          &
!!     &          nscl_jk, scl_e, ncomp, nvector, irev_sr_rlm, n_WS, WS)
!!@endverbatim
!!
!
      module cal_sp_rlm_by_matmul
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
      subroutine cal_sp_rlm_vector_matmul(nnod_rlm, nidx_rlm,           &
     &         istep_rlm, idx_gl_1d_rlm_j, radius_1d_rlm_r, g_sph_rlm,  &
     &         kst, nkr, jst, nj_rlm, nvec_jk,                          &
     &         pol_e, dpoldt_e, dpoldp_e, dtordt_e, dtordp_e,           &
     &         ncomp, irev_sr_rlm, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rlm
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      integer(kind = kint), intent(in) :: istep_rlm(2)
      integer(kind = kint), intent(in)                                  &
     &            :: idx_gl_1d_rlm_j(nidx_rlm(2),3)
      real(kind = kreal), intent(in) :: radius_1d_rlm_r(nidx_rlm(1))
      real(kind = kreal), intent(in) :: g_sph_rlm(nidx_rlm(2),17)
!
      integer(kind = kint), intent(in) :: nvec_jk
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: jst, nj_rlm
!
      real(kind = kreal), intent(inout) :: pol_e(nvec_jk)
      real(kind = kreal), intent(inout) :: dpoldt_e(nvec_jk)
      real(kind = kreal), intent(inout) :: dpoldp_e(nvec_jk)
      real(kind = kreal), intent(inout) :: dtordt_e(nvec_jk)
      real(kind = kreal), intent(inout) :: dtordp_e(nvec_jk)
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: kr_nd, kk, nd, k_rlm
      integer(kind = kint) :: i_rlm, i_send, i_kj, j_rlm, jj
      real(kind = kreal) :: g7, gm, r1_1d_rlm_r, r2_1d_rlm_r
!
!
      do jj = 1, nj_rlm
        j_rlm = jj + jst
        g7 = g_sph_rlm(jj+jst,7)
        gm = dble(idx_gl_1d_rlm_j(jj+jst,3))
        do kk = 1, nkr
          i_kj = kk + (jj-1) * nkr
          k_rlm = 1 + mod((kk+kst-1),nidx_rlm(1))
          r1_1d_rlm_r = radius_1d_rlm_r(k_rlm)
          r2_1d_rlm_r = r1_1d_rlm_r * r1_1d_rlm_r
          pol_e(i_kj) =    pol_e(i_kj) *    r2_1d_rlm_r * g7
          dpoldt_e(i_kj) = dpoldt_e(i_kj) * r1_1d_rlm_r * g7
          dpoldp_e(i_kj) = dpoldp_e(i_kj) * r1_1d_rlm_r * g7 * gm
          dtordt_e(i_kj) = dtordt_e(i_kj) * r1_1d_rlm_r * g7
          dtordp_e(i_kj) = dtordp_e(i_kj) * r1_1d_rlm_r * g7 * gm
        end do
      end do
!
      do jj = 1, nj_rlm
        j_rlm = jj + jst
        do kk = 1, nkr
          kr_nd = kk + kst
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
          i_kj = kk + (jj-1) * nkr
          i_rlm = 1 + (j_rlm-1) * istep_rlm(2)                          &
     &              + (k_rlm-1) * istep_rlm(1)
          i_send = 3*nd + (irev_sr_rlm(i_rlm) - 1) * ncomp
!
          WS(i_send-2) = WS(i_send-2) + pol_e(i_kj)
          WS(i_send-1) = WS(i_send-1) - dpoldp_e(i_kj) + dpoldt_e(i_kj)
          WS(i_send  ) = WS(i_send  ) - dtordp_e(i_kj) - dtordt_e(i_kj)
        end do
      end do
!
      end subroutine cal_sp_rlm_vector_matmul
!
! -----------------------------------------------------------------------
!
      subroutine cal_sp_rlm_scalar_matmul(nnod_rlm, nidx_rlm,           &
     &          istep_rlm, g_sph_rlm, kst, nkr, jst, nj_rlm,            &
     &          nscl_jk, scl_e, ncomp, nvector, irev_sr_rlm, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nnod_rlm
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      integer(kind = kint), intent(in) :: istep_rlm(2)
      real(kind = kreal), intent(in) :: g_sph_rlm(nidx_rlm(2),17)
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: jst, nj_rlm
!
      integer(kind = kint), intent(in) :: nscl_jk
      real(kind = kreal), intent(in) :: scl_e(nscl_jk)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: kr_nd, kk, nd, k_rlm
      integer(kind = kint) :: i_rlm, i_send, i_kj, j_rlm, jj
      real(kind = kreal) :: g6
!
!
      do jj = 1, nj_rlm
        j_rlm = jj + jst
        g6 = g_sph_rlm(j_rlm,6)
        do kk = 1, nkr
          kr_nd = kk + kst
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
          i_rlm = 1 + (j_rlm-1) * istep_rlm(2)                          &
     &              + (k_rlm-1) * istep_rlm(1)
          i_send = nd + 3*nvector + (irev_sr_rlm(i_rlm) - 1) * ncomp
          i_kj = kk + (jj-1) * nkr
!
          WS(i_send) = WS(i_send) + scl_e(i_kj) * g6
        end do
      end do
!
      end subroutine cal_sp_rlm_scalar_matmul
!
! -----------------------------------------------------------------------
!
      end module cal_sp_rlm_by_matmul
