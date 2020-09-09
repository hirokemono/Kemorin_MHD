!>@file   small_matmul_leg_trans_krin.F90
!!@brief  module small_matmul_leg_trans_krin
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2013
!
!>@brief  small Matrix products for Legendre transforms
!!
!!@verbatim
!!      subroutine matvec_fwd_leg_trans(nkr, n_jk, V_k, P_j, S_kj)
!!        integer(kind = kint), intent(in) :: n_jk, nkr
!!        real(kind = kreal), intent(in) :: V_k(nkr)
!!        real(kind = kreal), intent(in) :: P_j(n_jk)
!!        real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!!
!!      subroutine matmat8_fwd_leg_trans(nkr, n_jk, V_kl, P_lj, S_kj)
!!        integer(kind = kint), intent(in) :: n_jk, nkr
!!        real(kind = kreal), intent(in) :: V_kl(nkr,8)
!!        real(kind = kreal), intent(in) :: P_lj(8,n_jk)
!!        real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!!      subroutine matmat4_fwd_leg_trans(nkr, n_jk, V_kl, P_lj, S_kj)
!!        integer(kind = kint), intent(in) :: n_jk, nkr
!!        real(kind = kreal), intent(in) :: V_kl(nkr,4)
!!        real(kind = kreal), intent(in) :: P_lj(4,n_jk)
!!        real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!!      subroutine matmat2_fwd_leg_trans(nkr, n_jk, V_kl, P_lj, S_kj)
!!        integer(kind = kint), intent(in) :: n_jk, nkr
!!        real(kind = kreal), intent(in) :: V_kl(nkr,2)
!!        real(kind = kreal), intent(in) :: P_lj(2,n_jk)
!!        real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!!
!!
!!      subroutine matvec_bwd_leg_trans(nkr, n_jk, P_j, S_kj, V_k)
!!        integer(kind = kint), intent(in) :: n_jk, nkr
!!        real(kind = kreal), intent(in) :: P_j(n_jk)
!!        real(kind = kreal), intent(in) :: S_kj(nkr, n_jk)
!!        real(kind = kreal), intent(inout) :: V_k(nkr)
!!
!!      subroutine matmat8_bwd_leg_trans(nkr, n_jk, S_kj, P_jl, V_kl)
!!        integer(kind = kint), intent(in) :: n_jk, nkr
!!        real(kind = kreal), intent(in) :: P_jl(n_jk,8)
!!        real(kind = kreal), intent(in) :: S_kj(nkr, n_jk)
!!        real(kind = kreal), intent(inout) :: V_kl(nkr,8)
!!      subroutine matmat4_bwd_leg_trans(nkr, n_jk, S_kj, P_jl, V_kl)
!!        integer(kind = kint), intent(in) :: n_jk, nkr
!!        real(kind = kreal), intent(in) :: P_jl(n_jk,4)
!!        real(kind = kreal), intent(in) :: S_kj(nkr, n_jk)
!!        real(kind = kreal), intent(inout) :: V_kl(nkr,4)
!!      subroutine matmat2_bwd_leg_trans(nkr, n_jk, S_kj, P_jl, V_kl)
!!        integer(kind = kint), intent(in) :: n_jk, nkr
!!        real(kind = kreal), intent(in) :: P_jl(n_jk,2)
!!        real(kind = kreal), intent(in) :: S_kj(nkr, n_jk)
!!        real(kind = kreal), intent(inout) :: V_kl(nkr,2)
!!@endverbatim
!!
      module small_matmul_leg_trans_krin
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
      subroutine matvec_fwd_leg_trans(nkr, n_jk, V_k, P_j, S_kj)
!
      integer(kind = kint), intent(in) :: n_jk, nkr
      real(kind = kreal), intent(in) :: V_k(nkr)
      real(kind = kreal), intent(in) :: P_j(n_jk)
!
      real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!
      integer(kind = kint) :: jj, kk
!
!
      do jj = 1, n_jk
        do kk = 1, nkr
          S_kj(kk,jj) = S_kj(kk,jj) + V_k(kk)*P_j(jj)
        end do
      end do
!
      end subroutine matvec_fwd_leg_trans
!
! ----------------------------------------------------------------------
!
      subroutine matmat8_fwd_leg_trans(nkr, n_jk, V_kl, P_lj, S_kj)
!
      integer(kind = kint), intent(in) :: n_jk, nkr
      real(kind = kreal), intent(in) :: V_kl(nkr,8)
      real(kind = kreal), intent(in) :: P_lj(8,n_jk)
!
      real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!
      integer(kind = kint) :: jj, kk
!
!
      do jj = 1, n_jk
        do kk = 1, nkr
          S_kj(kk,jj) = S_kj(kk,jj)                                     &
     &                 + V_kl(kk,1)*P_lj(1,jj) + V_kl(kk,2)*P_lj(2,jj)  &
     &                 + V_kl(kk,3)*P_lj(3,jj) + V_kl(kk,4)*P_lj(4,jj)  &
     &                 + V_kl(kk,5)*P_lj(5,jj) + V_kl(kk,6)*P_lj(6,jj)  &
     &                 + V_kl(kk,7)*P_lj(7,jj) + V_kl(kk,8)*P_lj(8,jj)
        end do
      end do
!
      end subroutine matmat8_fwd_leg_trans
!
! ----------------------------------------------------------------------
!
      subroutine matmat4_fwd_leg_trans(nkr, n_jk, V_kl, P_lj, S_kj)
!
      integer(kind = kint), intent(in) :: n_jk, nkr
      real(kind = kreal), intent(in) :: V_kl(nkr,4)
      real(kind = kreal), intent(in) :: P_lj(4,n_jk)
!
      real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!
      integer(kind = kint) :: jj, kk
!
!
      do jj = 1, n_jk
        do kk = 1, nkr
          S_kj(kk,jj) = S_kj(kk,jj)                                     &
     &                 + V_kl(kk,1)*P_lj(1,jj) + V_kl(kk,2)*P_lj(2,jj)  &
     &                 + V_kl(kk,3)*P_lj(3,jj) + V_kl(kk,4)*P_lj(4,jj)
        end do
      end do
!
      end subroutine matmat4_fwd_leg_trans
!
! ----------------------------------------------------------------------
!
      subroutine matmat2_fwd_leg_trans(nkr, n_jk, V_kl, P_lj, S_kj)
!
      integer(kind = kint), intent(in) :: n_jk, nkr
      real(kind = kreal), intent(in) :: V_kl(nkr,2)
      real(kind = kreal), intent(in) :: P_lj(2,n_jk)
!
      real(kind = kreal), intent(inout) :: S_kj(nkr,n_jk)
!
      integer(kind = kint) :: jj, kk
!
!
      do jj = 1, n_jk
        do kk = 1, nkr
          S_kj(kk,jj) = S_kj(kk,jj)                                     &
     &                 + V_kl(kk,1)*P_lj(1,jj) + V_kl(kk,2)*P_lj(2,jj)
        end do
      end do
!
      end subroutine matmat2_fwd_leg_trans
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine matvec_bwd_leg_trans(nkr, n_jk, P_j, S_kj, V_k)
!
      integer(kind = kint), intent(in) :: n_jk, nkr
      real(kind = kreal), intent(in) :: P_j(n_jk)
      real(kind = kreal), intent(in) :: S_kj(nkr, n_jk)
!
      real(kind = kreal), intent(inout) :: V_k(nkr)
!
      integer(kind = kint) :: jj, kk
!
!
      do jj = 1, n_jk
        do kk = 1, nkr
          V_k(kk) = V_k(kk) + S_kj(kk,jj) * P_j(jj)
        end do
      end do
!
      end subroutine matvec_bwd_leg_trans
!
! ----------------------------------------------------------------------
!
      subroutine matmat8_bwd_leg_trans(nkr, n_jk, S_kj, P_jl, V_kl)
!
      integer(kind = kint), intent(in) :: n_jk, nkr
      real(kind = kreal), intent(in) :: P_jl(n_jk,8)
      real(kind = kreal), intent(in) :: S_kj(nkr, n_jk)
!
      real(kind = kreal), intent(inout) :: V_kl(nkr,8)
!
      integer(kind = kint) :: jj, kk
!
!
      do jj = 1, n_jk
        do kk = 1, nkr
          V_kl(kk,1:8) = V_kl(kk,1:8) + S_kj(kk,jj) * P_jl(jj,1:8)
        end do
      end do
!
      end subroutine matmat8_bwd_leg_trans
!
! ----------------------------------------------------------------------
!
      subroutine matmat4_bwd_leg_trans(nkr, n_jk, S_kj, P_jl, V_kl)
!
      integer(kind = kint), intent(in) :: n_jk, nkr
      real(kind = kreal), intent(in) :: P_jl(n_jk,4)
      real(kind = kreal), intent(in) :: S_kj(nkr, n_jk)
!
      real(kind = kreal), intent(inout) :: V_kl(nkr,4)
!
      integer(kind = kint) :: jj, kk
!
!
      do jj = 1, n_jk
        do kk = 1, nkr
          V_kl(kk,1:4) = V_kl(kk,1:4) + S_kj(kk,jj) * P_jl(jj,1:4)
        end do
      end do
!
      end subroutine matmat4_bwd_leg_trans
!
! ----------------------------------------------------------------------
!
      subroutine matmat2_bwd_leg_trans(nkr, n_jk, S_kj, P_jl, V_kl)
!
      integer(kind = kint), intent(in) :: n_jk, nkr
      real(kind = kreal), intent(in) :: P_jl(n_jk,2)
      real(kind = kreal), intent(in) :: S_kj(nkr, n_jk)
!
      real(kind = kreal), intent(inout) :: V_kl(nkr,2)
!
      integer(kind = kint) :: jj, kk
!
!
      do jj = 1, n_jk
        do kk = 1, nkr
          V_kl(kk,1:2) = V_kl(kk,1:2) + S_kj(kk,jj) * P_jl(jj,1:2)
        end do
      end do
!
      end subroutine matmat2_bwd_leg_trans
!
! ----------------------------------------------------------------------
!
      end module small_matmul_leg_trans_krin
