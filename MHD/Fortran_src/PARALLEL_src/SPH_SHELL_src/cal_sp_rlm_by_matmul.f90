!>@file   cal_sp_rlm_by_matmul.f90
!!@brief  module cal_sp_rlm_by_matmul
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  forward Legendre transform for testing
!!
!!@verbatim
!!      subroutine cal_sp_rlm_vector_matmul                             &
!!     &         (kst, nkr, jst, nj_rlm, nvec_jk,                       &
!!     &          pol_e, dpoldt_e, dpoldp_e, dtordt_e, dtordp_e,        &
!!     &          ncomp, sp_rlm)
!!      subroutine cal_sp_rlm_scalar_matmul(kst, nkr, jst, nj_rlm,      &
!!     &          nscl_jk, scl_e, ncomp, nvector, sp_rlm)
!!
!!      subroutine cal_sp_rlm_vector_sym_matmul                         &
!!     &         (kst, nkr, jst, n_jk_o, n_jk_e, nvec_jk,               &
!!     &          pol_e, pol_o, dpoldt_e, dpoldp_e, dpoldt_o, dpoldp_o, &
!!     &          dtordt_e, dtordp_e, dtordt_o, dtordp_o, ncomp, sp_rlm)
!!      subroutine cal_sp_rlm_scalar_sym_matmul                         &
!!     &         (kst, nkr, jst, n_jk_o, n_jk_e, nscl_jk, scl_e, scl_o, &
!!     &          ncomp, nvector, sp_rlm)
!!@endverbatim
!!
!
      module cal_sp_rlm_by_matmul
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
      subroutine cal_sp_rlm_vector_matmul                               &
     &         (kst, nkr, jst, nj_rlm, nvec_jk,                         &
     &          pol_e, dpoldt_e, dpoldp_e, dtordt_e, dtordp_e,          &
     &          ncomp, sp_rlm)
!
      integer(kind = kint), intent(in) :: nvec_jk
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: jst, nj_rlm
!
      real(kind = kreal), intent(in) :: pol_e(nvec_jk)
      real(kind = kreal), intent(in) :: dpoldt_e(nvec_jk)
      real(kind = kreal), intent(in) :: dpoldp_e(nvec_jk)
      real(kind = kreal), intent(in) :: dtordt_e(nvec_jk)
      real(kind = kreal), intent(in) :: dtordp_e(nvec_jk)
!
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(inout) :: sp_rlm(ncomp*nnod_rlm)
!
      integer(kind = kint) :: kr_nd, kk, nd, k_rlm
      integer(kind = kint) :: i_rlm, i_kj, j_rlm, jj
!
!
      do jj = 1, nj_rlm
        j_rlm = jj + jst
        do kk = 1, nkr
          kr_nd = kk + kst
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
          i_rlm = 3*nd + ncomp * ((j_rlm-1) * istep_rlm(2)              &
     &                          + (k_rlm-1) * istep_rlm(1))
          i_kj = kk + (jj-1) * nkr
!
          sp_rlm(i_rlm-2) = sp_rlm(i_rlm-2) + pol_e(i_kj)
          sp_rlm(i_rlm-1) = sp_rlm(i_rlm-1)                             &
     &                     - dpoldp_e(i_kj) + dpoldt_e(i_kj)
          sp_rlm(i_rlm  ) = sp_rlm(i_rlm  )                             &
     &                     - dtordp_e(i_kj) - dtordt_e(i_kj)
        end do
      end do
!
      end subroutine cal_sp_rlm_vector_matmul
!
! -----------------------------------------------------------------------
!
      subroutine cal_sp_rlm_scalar_matmul(kst, nkr, jst, nj_rlm,        &
     &          nscl_jk, scl_e, ncomp, nvector, sp_rlm)
!
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: jst, nj_rlm
!
      integer(kind = kint), intent(in) :: nscl_jk
      real(kind = kreal), intent(in) :: scl_e(nscl_jk)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      real(kind = kreal), intent(inout) :: sp_rlm(ncomp*nnod_rlm)
!
      integer(kind = kint) :: kr_nd, kk, nd, k_rlm
      integer(kind = kint) :: i_rlm, i_kj, j_rlm, jj
!
!
      do jj = 1, nj_rlm
        j_rlm = jj + jst
        do kk = 1, nkr
          kr_nd = kk + kst + 3*nvector*nidx_rlm(1)
          k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
          nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
          i_rlm = nd + 3*nvector + ncomp * ((j_rlm-1) * istep_rlm(2)    &
     &                                    + (k_rlm-1) * istep_rlm(1))
          i_kj = kk + (jj-1) * nkr
!
          sp_rlm(i_rlm) = sp_rlm(i_rlm) + scl_e(i_kj)
        end do
      end do
!
      end subroutine cal_sp_rlm_scalar_matmul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sp_rlm_vector_sym_matmul                           &
     &         (kst, nkr, jst, n_jk_o, n_jk_e, nvec_jk,                 &
     &          pol_e, pol_o, dpoldt_e, dpoldp_e, dpoldt_o, dpoldp_o,   &
     &          dtordt_e, dtordp_e, dtordt_o, dtordp_o, ncomp, sp_rlm)
!
      integer(kind = kint), intent(in) :: nvec_jk
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: jst, n_jk_o, n_jk_e
!
      real(kind = kreal), intent(in) :: pol_e(nvec_jk)
      real(kind = kreal), intent(in) :: pol_o(nvec_jk)
      real(kind = kreal), intent(in) :: dpoldt_e(nvec_jk)
      real(kind = kreal), intent(in) :: dpoldp_e(nvec_jk)
      real(kind = kreal), intent(in) :: dpoldt_o(nvec_jk)
      real(kind = kreal), intent(in) :: dpoldp_o(nvec_jk)
      real(kind = kreal), intent(in) :: dtordt_e(nvec_jk)
      real(kind = kreal), intent(in) :: dtordp_e(nvec_jk)
      real(kind = kreal), intent(in) :: dtordt_o(nvec_jk)
      real(kind = kreal), intent(in) :: dtordp_o(nvec_jk)
!
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(inout) :: sp_rlm(ncomp*nnod_rlm)
!
      integer(kind = kint) :: kr_nd, kk, k_rlm
      integer(kind = kint) :: ie_rlm, io_rlm, i_kj
      integer(kind = kint) :: nd, je_rlm, jo_rlm, jj
!
!
      do kk = 1, nkr
        kr_nd = kk + kst
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
!
        do jj = 1, n_jk_o
          i_kj = kk + (jj-1) * nkr
          je_rlm = 2*jj + jst - 1
          jo_rlm = 2*jj + jst
          ie_rlm = 3*nd + ncomp * ((je_rlm-1) * istep_rlm(2)            &
     &                           + (k_rlm-1) *  istep_rlm(1))
          io_rlm = 3*nd + ncomp * ((jo_rlm-1) * istep_rlm(2)            &
     &                           + (k_rlm-1) *  istep_rlm(1))
!
!  even l-m
          sp_rlm(ie_rlm-2) = sp_rlm(ie_rlm-2) + pol_e(i_kj)
          sp_rlm(ie_rlm-1) = sp_rlm(ie_rlm-1)                           &
     &                         - dpoldp_e(i_kj) + dpoldt_e(i_kj)
          sp_rlm(ie_rlm  ) = sp_rlm(ie_rlm  )                           &
     &                         - dtordp_e(i_kj) - dtordt_e(i_kj)
!  odd l-m
          sp_rlm(io_rlm-2) = sp_rlm(io_rlm-2) + pol_o(i_kj)
          sp_rlm(io_rlm-1) = sp_rlm(io_rlm-1)                           &
     &                         + dpoldt_o(i_kj) - dpoldp_o(i_kj)
          sp_rlm(io_rlm  ) = sp_rlm(io_rlm  )                           &
     &                         - dtordp_o(i_kj) - dtordt_o(i_kj)
        end do
        do jj = n_jk_o+1, n_jk_e
          i_kj = kk + (jj-1) * nkr
          je_rlm = 2*jj + jst - 1
          ie_rlm = 3*nd + ncomp * ((je_rlm-1) * istep_rlm(2)            &
     &                           + (k_rlm-1) *  istep_rlm(1))
!
          sp_rlm(ie_rlm-2) = sp_rlm(ie_rlm-2) + pol_e(i_kj)
          sp_rlm(ie_rlm-1) = sp_rlm(ie_rlm-1)                           &
     &                         - dpoldp_e(i_kj) + dpoldt_e(i_kj)
          sp_rlm(ie_rlm  ) = sp_rlm(ie_rlm  )                           &
     &                         - dtordp_e(i_kj) - dtordt_e(i_kj)
        end do
      end do
!
      end subroutine cal_sp_rlm_vector_sym_matmul
!
! -----------------------------------------------------------------------
!
      subroutine cal_sp_rlm_scalar_sym_matmul                           &
     &         (kst, nkr, jst, n_jk_o, n_jk_e, nscl_jk, scl_e, scl_o,   &
     &          ncomp, nvector, sp_rlm)
!
      integer(kind = kint), intent(in) :: nscl_jk
      integer(kind = kint), intent(in) :: kst, nkr
      integer(kind = kint), intent(in) :: jst, n_jk_o, n_jk_e
!
      real(kind = kreal), intent(in) :: scl_e(nscl_jk)
      real(kind = kreal), intent(in) :: scl_o(nscl_jk)
!
      integer(kind = kint), intent(in) :: ncomp, nvector
      real(kind = kreal), intent(inout) :: sp_rlm(ncomp*nnod_rlm)
!
      integer(kind = kint) :: kr_nd, kk, k_rlm
      integer(kind = kint) :: ie_rlm, io_rlm, i_kj
      integer(kind = kint) :: nd, je_rlm, jo_rlm, jj
!
!
      do kk = 1, nkr
        kr_nd = kk + kst
        k_rlm = 1 + mod((kr_nd-1),nidx_rlm(1))
        nd = 1 + (kr_nd - k_rlm) / nidx_rlm(1)
        do jj = 1, n_jk_o
          i_kj = kk + (jj-1) * nkr
!
          je_rlm = 2*jj + jst - 1
          jo_rlm = 2*jj + jst
          ie_rlm = nd + 3*nvector + ncomp * ((je_rlm-1) * istep_rlm(2)  &
     &                                      + (k_rlm-1) * istep_rlm(1))
          io_rlm = nd + 3*nvector + ncomp * ((jo_rlm-1) * istep_rlm(2)  &
     &                                      + (k_rlm-1) * istep_rlm(1))
!
          sp_rlm(ie_rlm) = sp_rlm(ie_rlm) + scl_e(i_kj)
          sp_rlm(io_rlm) = sp_rlm(io_rlm) + scl_o(i_kj)
        end do
!
        do jj = n_jk_o+1, n_jk_e
          i_kj = kk + (jj-1) * nkr
!
          je_rlm = 2*jj + jst - 1
          ie_rlm = nd + 3*nvector + ncomp * ((je_rlm-1) * istep_rlm(2)  &
     &                                      + (k_rlm-1) * istep_rlm(1))
          sp_rlm(ie_rlm) = sp_rlm(ie_rlm) + scl_e(i_kj)
        end do
      end do
!
      end subroutine cal_sp_rlm_scalar_sym_matmul
!
! -----------------------------------------------------------------------
!
      end module cal_sp_rlm_by_matmul
