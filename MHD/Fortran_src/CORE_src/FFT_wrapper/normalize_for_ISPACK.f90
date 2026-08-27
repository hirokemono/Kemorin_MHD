!>@file   normalize_for_ISPACK.f90
!!@brief  module normalize_for_ISPACK
!!
!!@author H. Matsui
!!@date Programmed in 2026
!!
!!
!>@brief  Normalization and data copy for ISPACK
!!
!!@verbatim
!!      subroutine copy_rtp_fld_to_FXRTFA(Nsmp, Nstacksmp, Mmax_smp,    &
!!     &                                  Nfft, M, X, X_ispack)
!!        integer(kind = kint), intent(in) :: Nsmp
!!        integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
!!        integer(kind = kint_gl), intent(in) :: M, Mmax_smp
!!        integer(kind = kint_gl), intent(in) :: Nfft
!!        real(kind = kreal), intent(in) :: X(M, Nfft)
!!        real(kind = 8), intent(inout) :: X_ispack(Mmax_smp*Nfft,Nsmp)
!!      subroutine norm_rtp_spectr_from_FXRTFA(Nsmp, Nstacksmp,         &
!!     &          Mmax_smp, Nfft, X_ispack, M, X)
!!        integer(kind = kint), intent(in) :: Nsmp
!!        integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
!!        integer(kind = kint_gl), intent(in) :: M, Mmax_smp
!!        integer(kind = kint_gl), intent(in) :: Nfft
!!        real(kind = 8), intent(in) :: X_ispack(Mmax_smp*Nfft,Nsmp)
!!        real(kind = kreal), intent(inout) :: X(M, Nfft)
!!
!!      subroutine norm_rtp_spectr_to_FXRTBA(Nsmp, Nstacksmp, Mmax_smp, &
!!     &                                     Nfft, M, X, X_ispack)
!!        integer(kind = kint), intent(in) :: Nsmp
!!        integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
!!        integer(kind = kint_gl), intent(in) :: Mmax_smp
!!        integer(kind = kint_gl), intent(in) :: M, Nfft
!!        real(kind = kreal), intent(in) :: X(M,Nfft)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: X_ispack(Mmax_smp*Nfft,Nsmp)
!!      subroutine copy_rtp_fld_from_FXRTBA(Nsmp, Nstacksmp, Mmax_smp,  &
!!     &                                    Nfft, X_ispack, M, X)
!!        integer(kind = kint), intent(in) :: Nsmp
!!        integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
!!        integer(kind = kint_gl), intent(in) :: Mmax_smp
!!        integer(kind = kint_gl), intent(in) :: M, Nfft
!!        real(kind = kreal), intent(in) :: X_ispack(Mmax_smp*Nfft,Nsmp)
!!        real(kind = kreal), intent(inout) :: X(M,Nfft)
!!@endverbatim
!
      module normalize_for_ISPACK
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: norm_rtp_spectr_from_FXRTFA_smp
      private :: norm_rtp_spectr_to_FXRTBA_smp
      private :: copy_rtp_fld_to_FXRTFA_smp
      private :: copy_rtp_fld_from_FXRTBA_smp
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine copy_rtp_fld_to_FXRTFA(Nsmp, Nstacksmp, Mmax_smp,      &
     &                                  Nfft, M, X, X_ispack)
!
      integer(kind = kint), intent(in) :: Nsmp
      integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: M, Mmax_smp
      integer(kind = kint_gl), intent(in) :: Nfft
      real(kind = kreal), intent(in) :: X(M, Nfft)
!
      real(kind = 8), intent(inout) :: X_ispack(Mmax_smp*Nfft,Nsmp)
!
      integer(kind = kint_gl) :: ismp, ist, num8
!
!
!$omp parallel do private(ist,num8)
      do ismp = 1, Nsmp
        ist = Nstacksmp(ismp-1)
        num8 = Nstacksmp(ismp) - Nstacksmp(ismp-1)
        if(num8 .le. 0) cycle
!
        call copy_rtp_fld_to_FXRTFA_smp(ist, num8, Nfft, M, X,          &
     &                                  Mmax_smp, X_ispack(1,ismp))
      end do
!
      end subroutine copy_rtp_fld_to_FXRTFA
!
! ------------------------------------------------------------------
!
      subroutine norm_rtp_spectr_from_FXRTFA(Nsmp, Nstacksmp,           &
     &          Mmax_smp, Nfft, X_ispack, M, X)
!
      integer(kind = kint), intent(in) :: Nsmp
      integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: M, Mmax_smp
      integer(kind = kint_gl), intent(in) :: Nfft
      real(kind = 8), intent(in) :: X_ispack(Mmax_smp*Nfft,Nsmp)
!
      real(kind = kreal), intent(inout) :: X(M, Nfft)
!
      integer(kind = kint_gl) :: ismp, ist, num8
!
!
!$omp parallel do private(ist,num8)
      do ismp = 1, Nsmp
        ist = Nstacksmp(ismp-1)
        num8 = Nstacksmp(ismp) - Nstacksmp(ismp-1)
        if(num8 .le. 0) cycle
!
        call norm_rtp_spectr_from_FXRTFA_smp(ist, num8,                 &
     &      Nfft, Mmax_smp, X_ispack(1,ismp), M, X)
      end do
!$omp end parallel do
!
      end subroutine norm_rtp_spectr_from_FXRTFA
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine norm_rtp_spectr_to_FXRTBA(Nsmp, Nstacksmp, Mmax_smp,   &
     &                                     Nfft, M, X, X_ispack)
!
      integer(kind = kint), intent(in) :: Nsmp
      integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: Mmax_smp
      integer(kind = kint_gl), intent(in) :: M, Nfft
      real(kind = kreal), intent(in) :: X(M,Nfft)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: X_ispack(Mmax_smp*Nfft,Nsmp)
!
      integer(kind = kint_gl) :: ismp, ist, num8
!
!
!$omp parallel do private(ist,num8)
      do ismp = 1, Nsmp
        ist = Nstacksmp(ismp-1)
        num8 = Nstacksmp(ismp) - Nstacksmp(ismp-1)
        if(num8 .le. 0) cycle
!
        call norm_rtp_spectr_to_FXRTBA_smp(ist, num8, Nfft, M, X(1,1),  &
     &                                     Mmax_smp, X_ispack(1,ismp))
      end do
!$omp end parallel do
!
      end subroutine norm_rtp_spectr_to_FXRTBA
!
! ------------------------------------------------------------------
!
      subroutine copy_rtp_fld_from_FXRTBA(Nsmp, Nstacksmp, Mmax_smp,    &
     &                                    Nfft, X_ispack, M, X)
!
      integer(kind = kint), intent(in) :: Nsmp
      integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: Mmax_smp
      integer(kind = kint_gl), intent(in) :: M, Nfft
      real(kind = kreal), intent(in) :: X_ispack(Mmax_smp*Nfft,Nsmp)
!
      real(kind = kreal), intent(inout) :: X(M,Nfft)
!
      integer(kind = kint_gl) :: ismp, ist, num8
!
!
!$omp parallel do private(ist,num8)
      do ismp = 1, Nsmp
        ist = Nstacksmp(ismp-1)
        num8 = Nstacksmp(ismp) - Nstacksmp(ismp-1)
        if(num8 .le. 0) cycle
!
        call copy_rtp_fld_from_FXRTBA_smp(ist, num8, Nfft, Mmax_smp,    &
     &                                    X_ispack(1,ismp), M, X)
      end do
!$omp end parallel do
!
      end subroutine copy_rtp_fld_from_FXRTBA
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
      end module normalize_for_ISPACK
