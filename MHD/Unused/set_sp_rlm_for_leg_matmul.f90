!>@file   set_sp_rlm_for_leg_matmul.f90
!!@brief  module set_sp_rlm_for_leg_matmul
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  Set spectrum data for backward Legendre transform
!!
!!@verbatim
!!      subroutine set_sp_rlm_vector_matmul(nnod_rlm, nidx_rlm,         &
!!     &          istep_rlm, idx_gl_1d_rlm_j, a_r_1d_rlm_r, g_sph_rlm,  &
!!     &          kst, nkr, jst, nj_rlm, ncomp, irev_sr_rlm, n_WR, WR,  &
!!     &          nvec_jk, pol_e, dpoldt_e, dpoldp_e, dtordt_e, dtordp_e)
!!      subroutine set_sp_rlm_scalar_matmul                             &
!!     &         (nnod_rlm, nidx_rlm, istep_rlm, kst, nkr, jst, nj_rlm, &
!!     &          ncomp, nvector, irev_sr_rlm, n_WR, WR, nscl_jk, scl_e)
!!@endverbatim
!!
      module set_sp_rlm_for_leg_matmul
!
      use m_precision
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_sp_rlm_vector_matmul(nnod_rlm, nidx_rlm,           &
     &          istep_rlm, idx_gl_1d_rlm_j, a_r_1d_rlm_r, g_sph_rlm,    &
     &          kst, nkr, jst, nj_rlm, ncomp, irev_sr_rlm, n_WR, WR,    &
     &          nvec_jk, pol_e, dpoldt_e, dpoldp_e, dtordt_e, dtordp_e)
!
      integer(kind = kint), intent(in) :: nnod_rlm
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      integer(kind = kint), intent(in) :: istep_rlm(2)
      integer(kind = kint), intent(in) :: idx_gl_1d_rlm_j(nidx_rlm(2),3)
      real(kind = kreal), intent(in) :: a_r_1d_rlm_r(nidx_rlm(1))
      real(kind = kreal), intent(in) :: g_sph_rlm(nidx_rlm(2),17)
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: jst, nj_rlm
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real(kind = kreal), intent(in) :: WR(n_WR)
!
      integer(kind = kint), intent(in) :: nvec_jk
      real(kind = kreal), intent(inout) :: pol_e(nvec_jk)
      real(kind = kreal), intent(inout) :: dpoldt_e(nvec_jk)
      real(kind = kreal), intent(inout) :: dpoldp_e(nvec_jk)
      real(kind = kreal), intent(inout) :: dtordt_e(nvec_jk)
      real(kind = kreal), intent(inout) :: dtordp_e(nvec_jk)
!
      integer(kind = kint) :: kk, kr_nd, k_rlm, nd
      integer(kind = kint) :: i_rlm, i_recv
      integer(kind = kint) :: jj, j_rlm, i_jk
      real(kind = kreal) :: a1r_1d_rlm_r, a2r_1d_rlm_r
      real(kind = kreal) :: g3, gm
!
!
      do kk = 1, nkr
        kr_nd = kk + kst
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
        a1r_1d_rlm_r = a_r_1d_rlm_r(k_rlm)
        a2r_1d_rlm_r = a_r_1d_rlm_r(k_rlm)*a_r_1d_rlm_r(k_rlm)
        do jj = 1, nj_rlm
          j_rlm = jj + jst
          g3 = g_sph_rlm(j_rlm,3)
          gm = dble(idx_gl_1d_rlm_j(j_rlm,3))
          i_rlm = 1 + (j_rlm-1) * istep_rlm(2)                          &
     &              + (k_rlm-1) * istep_rlm(1)
          i_recv = 3*nd + (irev_sr_rlm(i_rlm) - 1) * ncomp
          i_jk = jj + (kk-1) * nj_rlm
!
          pol_e(i_jk) =    WR(i_recv-2) * a2r_1d_rlm_r * g3
          dpoldt_e(i_jk) = WR(i_recv-1) * a1r_1d_rlm_r
          dpoldp_e(i_jk) = WR(i_recv-1) * a1r_1d_rlm_r * gm
          dtordt_e(i_jk) = WR(i_recv  ) * a1r_1d_rlm_r
          dtordp_e(i_jk) = WR(i_recv  ) * a1r_1d_rlm_r * gm
        end do
      end do
!
      end subroutine set_sp_rlm_vector_matmul
!
! -----------------------------------------------------------------------
!
      subroutine set_sp_rlm_scalar_matmul                               &
     &         (nnod_rlm, nidx_rlm, istep_rlm, kst, nkr, jst, nj_rlm,   &
     &          ncomp, nvector, irev_sr_rlm, n_WR, WR, nscl_jk, scl_e)
!
      integer(kind = kint), intent(in) :: nnod_rlm
      integer(kind = kint), intent(in) :: nidx_rlm(2)
      integer(kind = kint), intent(in) :: istep_rlm(2)
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: jst, nj_rlm
      integer(kind = kint), intent(in) :: ncomp, nvector
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rlm(nnod_rlm)
      real(kind = kreal), intent(in):: WR(n_WR)
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
!
      end module set_sp_rlm_for_leg_matmul
