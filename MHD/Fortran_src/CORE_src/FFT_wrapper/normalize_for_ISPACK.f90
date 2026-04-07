!>@file   normalize_for_ISPACK.f90
!!@brief  module normalize_for_ISPACK
!!
!!@author H. Matsui
!!@date Programmed in 20026
!!
!!
!>@brief  Normalization and data copy for ISPACK
!!
!!@verbatim
!!      subroutine norm_rtp_spectr_from_FXRTFA_smp(ist_smp, nnod_smp,   &
!!     &          Nfft, Mmax_smp, X_ispack, M, X)
!!        integer(kind = kint_gl), intent(in) :: ist_smp, nnod_smp
!!        integer(kind = kint_gl), intent(in) :: M, Nfft, Mmax_smp
!!        real(kind = kreal), intent(in) :: X_ispack(Mmax_smp*Nfft)
!!        real(kind = kreal), intent(inout) :: X(M,Nfft)
!!      subroutine swap_prt_spectr_from_FXRTFA_smp(ist_smp, nnod_smp,   &
!!     &          Nfft, Mmax_smp, X_ispack, M, X)
!!        integer(kind = kint_gl), intent(in) :: ist_smp, nnod_smp
!!        integer(kind = kint_gl), intent(in) :: M, Nfft, Mmax_smp
!!        real(kind = kreal), intent(in) :: X_ispack(Mmax_smp*Nfft)
!!        real(kind = kreal), intent(inout) :: X(Nfft,M)
!!
!!      subroutine copy_rtp_fld_to_FXRTFA_smp(ist_smp, nnod_smp,        &
!!     &          Nfft, M, X, Mmax_smp, X_ispack)
!!        integer(kind = kint_gl), intent(in) :: ist_smp, nnod_smp
!!        integer(kind = kint_gl), intent(in) :: M, Nfft, Mmax_smp
!!        real(kind = kreal), intent(in) :: X(M,Nfft)
!!        real(kind = kreal), intent(inout) :: X_ispack(Mmax_smp*Nfft)
!!      subroutine swap_prt_fld_to_FXRTFA_smp(ist_smp, nnod_smp,        &
!!     &          Nfft, M, X, Mmax_smp, X_ispack)
!!        integer(kind = kint_gl), intent(in) :: ist_smp, nnod_smp
!!        integer(kind = kint_gl), intent(in) :: M, Nfft, Mmax_smp
!!        real(kind = kreal), intent(in) :: X(Nfft,M)
!!        real(kind = kreal), intent(inout) :: X_ispack(Mmax_smp*Nfft)
!!
!!      subroutine norm_rtp_spectr_to_FXRTBA_smp(ist_smp, nnod_smp,     &
!!     &          Nfft, M, X, Mmax_smp, X_ispack)
!!        integer(kind = kint_gl), intent(in) :: ist_smp, nnod_smp
!!        integer(kind = kint_gl), intent(in) :: M, Nfft, Mmax_smp
!!        real(kind = kreal), intent(in) :: X(M,Nfft)
!!        real(kind = kreal), intent(inout) :: X_ispack(Mmax_smp*Nfft)
!!      subroutine swap_prt_spectr_to_FXRTBA_smp(ist_smp, nnod_smp,     &
!!     &          Nfft, Mmax_smp, X_ispack, M, X)
!!        integer(kind = kint_gl), intent(in) :: ist_smp, nnod_smp
!!        integer(kind = kint_gl), intent(in) :: M, Nfft, Mmax_smp
!!        real(kind = kreal), intent(in) :: X(Nfft,M)
!!        real(kind = kreal), intent(inout) :: X_ispack(Mmax_smp*Nfft)
!!
!!      subroutine copy_rtp_fld_from_FXRTBA_smp(ist_smp, nnod_smp,      &
!!     &          Nfft, Mmax_smp, X_ispack, M, X)
!!        integer(kind = kint_gl), intent(in) :: ist_smp, nnod_smp
!!        integer(kind = kint_gl), intent(in) :: M, Nfft, Mmax_smp
!!        real(kind = kreal), intent(in) :: X_ispack(Mmax_smp*Nfft)
!!        real(kind = kreal), intent(inout) :: X(M,Nfft)
!!      subroutine swap_prt_fld_from_FXRTBA_smp(ist_smp, nnod_smp,      &
!!     &          Nfft, Mmax_smp, X_ispack, M, X)
!!        integer(kind = kint_gl), intent(in) :: ist_smp, nnod_smp
!!        integer(kind = kint_gl), intent(in) :: M, Nfft, Mmax_smp
!!        real(kind = kreal), intent(in) :: X_ispack(Mmax_smp*Nfft)
!!        real(kind = kreal), intent(inout) :: X(Nfft,M)
!!@endverbatim
!
      module normalize_for_ISPACK
!
      use m_precision
      use m_constants
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine norm_rtp_spectr_from_FXRTFA_smp(ist_smp, nnod_smp,     &
     &          Nfft, Mmax_smp, X_ispack, M, X)
!
      integer(kind = kint_gl), intent(in) :: ist_smp, nnod_smp
      integer(kind = kint_gl), intent(in) :: M, Nfft, Mmax_smp
      real(kind = kreal), intent(in) :: X_ispack(Mmax_smp*Nfft)
!
      real(kind = kreal), intent(inout) :: X(M,Nfft)
!
      integer(kind = kint_gl) ::  i, j, inum, inod_c
!
!
      do inum = 1, nnod_smp
        j = ist_smp + inum
        inod_c = inum
        X(j,1) = X_ispack(inod_c)
        X(j,2) = X_ispack(inod_c+nnod_smp)
      end do
      do i = 2, Nfft/2
        do inum = 1, nnod_smp
          j = ist_smp + inum
          inod_c = inum + (2*i-2) * nnod_smp
          X(j,2*i-1) =   two * X_ispack(inod_c         )
          X(j,2*i  ) = - two * X_ispack(inod_c+nnod_smp)
        end do
      end do
!
      end subroutine norm_rtp_spectr_from_FXRTFA_smp
!
! ------------------------------------------------------------------
!
      subroutine swap_prt_spectr_from_FXRTFA_smp(ist_smp, nnod_smp,     &
     &          Nfft, Mmax_smp, X_ispack, M, X)
!
      integer(kind = kint_gl), intent(in) :: ist_smp, nnod_smp
      integer(kind = kint_gl), intent(in) :: M, Nfft, Mmax_smp
      real(kind = kreal), intent(in) :: X_ispack(Mmax_smp*Nfft)
!
      real(kind = kreal), intent(inout) :: X(Nfft,M)
!
      integer(kind = kint_gl) ::  i, j, inum, inod_c
!
!
      do inum = 1, nnod_smp
        j = ist_smp + inum
        inod_c = inum
        X(1,j) = X_ispack(inod_c)
        X(2,j) = X_ispack(inod_c+nnod_smp)
        do i = 2, Nfft/2
          inod_c = inum + (2*i-2) * nnod_smp
          X(2*i-1,j) =   two * X_ispack(inod_c         )
          X(2*i,  j) = - two * X_ispack(inod_c+nnod_smp)
        end do
      end do
!
      end subroutine swap_prt_spectr_from_FXRTFA_smp
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_rtp_fld_to_FXRTFA_smp(ist_smp, nnod_smp,          &
     &          Nfft, M, X, Mmax_smp, X_ispack)
!
      integer(kind = kint_gl), intent(in) :: ist_smp, nnod_smp
      integer(kind = kint_gl), intent(in) :: M, Nfft, Mmax_smp
      real(kind = kreal), intent(in) :: X(M,Nfft)
!
      real(kind = kreal), intent(inout) :: X_ispack(Mmax_smp*Nfft)
!
      integer(kind = kint_gl) ::  i, j, inum, inod_c
!
!
      do i = 1, Nfft/2
        do inum = 1, nnod_smp
          j = ist_smp + inum
          inod_c = inum + 2*(i-1) * nnod_smp
          X_ispack(inod_c         ) = X(j,2*i-1)
          X_ispack(inod_c+nnod_smp) = X(j,2*i  )
        end do
      end do
!
      end subroutine copy_rtp_fld_to_FXRTFA_smp
!
! ------------------------------------------------------------------
!
      subroutine swap_prt_fld_to_FXRTFA_smp(ist_smp, nnod_smp,          &
     &          Nfft, M, X, Mmax_smp, X_ispack)
!
      integer(kind = kint_gl), intent(in) :: ist_smp, nnod_smp
      integer(kind = kint_gl), intent(in) :: M, Nfft, Mmax_smp
      real(kind = kreal), intent(in) :: X(Nfft,M)
!
      real(kind = kreal), intent(inout) :: X_ispack(Mmax_smp*Nfft)
!
      integer(kind = kint_gl) ::  i, j, inum, inod_c
!
!
      do i = 1, Nfft/2
        do inum = 1, nnod_smp
          j = ist_smp + inum
          inod_c = inum + 2*(i-1) * nnod_smp
          X_ispack(inod_c         ) = X(2*i-1,j)
          X_ispack(inod_c+nnod_smp) = X(2*i,  j)
        end do
      end do
!
      end subroutine swap_prt_fld_to_FXRTFA_smp
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine norm_rtp_spectr_to_FXRTBA_smp(ist_smp, nnod_smp,       &
     &          Nfft, M, X, Mmax_smp, X_ispack)
!
      integer(kind = kint_gl), intent(in) :: ist_smp, nnod_smp
      integer(kind = kint_gl), intent(in) :: M, Nfft, Mmax_smp
      real(kind = kreal), intent(in) :: X(M,Nfft)
!
      real(kind = kreal), intent(inout) :: X_ispack(Mmax_smp*Nfft)
!
      integer(kind = kint_gl) ::  i, j, inum, inod_c
!
!
      do inum = 1, nnod_smp
        j = ist_smp + inum
        inod_c = inum
        X_ispack(inod_c         ) = X(j,1)
        X_ispack(inod_c+nnod_smp) = X(j,2)
      end do
      do i = 2, Nfft/2
        do inum = 1, nnod_smp
          j = ist_smp + inum
          inod_c = inum + (2*i-2) * nnod_smp
          X_ispack(inod_c         ) =  half * X(j,2*i-1)
          X_ispack(inod_c+nnod_smp) = -half * X(j,2*i  )
        end do
      end do
!
      end subroutine norm_rtp_spectr_to_FXRTBA_smp
!
! ------------------------------------------------------------------
!
      subroutine swap_prt_spectr_to_FXRTBA_smp(ist_smp, nnod_smp,       &
     &          Nfft, Mmax_smp, X_ispack, M, X)
!
      integer(kind = kint_gl), intent(in) :: ist_smp, nnod_smp
      integer(kind = kint_gl), intent(in) :: M, Nfft, Mmax_smp
      real(kind = kreal), intent(in) :: X(Nfft,M)
!
      real(kind = kreal), intent(inout) :: X_ispack(Mmax_smp*Nfft)
!
      integer(kind = kint_gl) ::  i, j, inum, inod_c
!
!
      do inum = 1, nnod_smp
        j = ist_smp + inum
        inod_c = inum
        X_ispack(inod_c         ) = X(1,j)
        X_ispack(inod_c+nnod_smp) = X(2,j)
      end do
      do i = 2, Nfft/2
        do inum = 1, nnod_smp
          j = ist_smp + inum
          inod_c = inum + (2*i-2) * nnod_smp
          X_ispack(inod_c         ) =  half * X(2*i-1,j)
          X_ispack(inod_c+nnod_smp) = -half * X(2*i  ,j)
        end do
      end do
!
      end subroutine swap_prt_spectr_to_FXRTBA_smp
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_rtp_fld_from_FXRTBA_smp(ist_smp, nnod_smp,        &
     &          Nfft, Mmax_smp, X_ispack, M, X)
!
      integer(kind = kint_gl), intent(in) :: ist_smp, nnod_smp
      integer(kind = kint_gl), intent(in) :: M, Nfft, Mmax_smp
      real(kind = kreal), intent(in) :: X_ispack(Mmax_smp*Nfft)
!
      real(kind = kreal), intent(inout) :: X(M,Nfft)
!
      integer(kind = kint_gl) ::  i, j, inum, inod_c
!
!
      do i = 1, Nfft/2
        do inum = 1, nnod_smp
          j = ist_smp + inum
          inod_c = inum + 2*(i-1) * nnod_smp
          X(j,2*i-1) = X_ispack(inod_c         )
          X(j,2*i  ) = X_ispack(inod_c+nnod_smp)
        end do
      end do
!
      end subroutine copy_rtp_fld_from_FXRTBA_smp
!
! ------------------------------------------------------------------
!
      subroutine swap_prt_fld_from_FXRTBA_smp(ist_smp, nnod_smp,        &
     &          Nfft, Mmax_smp, X_ispack, M, X)
!
      integer(kind = kint_gl), intent(in) :: ist_smp, nnod_smp
      integer(kind = kint_gl), intent(in) :: M, Nfft, Mmax_smp
      real(kind = kreal), intent(in) :: X_ispack(Mmax_smp*Nfft)
!
      real(kind = kreal), intent(inout) :: X(Nfft,M)
!
      integer(kind = kint_gl) ::  i, j, inum, inod_c
!
!
      do inum = 1, nnod_smp
        do i = 1, Nfft/2
          j = ist_smp + inum
          inod_c = inum + 2*(i-1) * nnod_smp
          X(2*i-1,j) = X_ispack(inod_c         )
          X(2*i,  j) = X_ispack(inod_c+nnod_smp)
        end do
      end do
!
      end subroutine swap_prt_fld_from_FXRTBA_smp
!
! ------------------------------------------------------------------
!
      end module normalize_for_ISPACK
