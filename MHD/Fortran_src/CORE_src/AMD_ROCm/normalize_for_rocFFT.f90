!>@file   normalize_for_rocFFT.f90
!!@brief  module normalize_for_rocFFT
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine norm_rtp_from_fwd_rocFFT(Ncomp_r, Nfft_r, X_FFT,     &
!!     &                                    ist_comp, Ncomp, Nfft, X)
!!        integer(kind = kint), intent(in) :: Ncomp_r, Nfft_r
!!        real(kind = kreal), intent(in) :: X_FFT(Ncomp_r*Nfft_r)
!!        integer(kind = kint), intent(in) :: ist_comp, comp, Nfft
!!        real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!!      subroutine norm_prt_from_fwd_rocFFT(Ncomp, NFFT_r, X_FFT,       &
!!     &                                    Nfft, X)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_r
!!        real(kind = kreal), intent(in) :: X_FFT(NFFT_r,Ncomp)
!!        real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
!!
!!      subroutine norm_rtp_to_bwd_rocFFT(ist_comp, Ncomp, Nfft, X,     &
!!     &                                  Ncomp_r, Nfft_r, X_FFT)
!!        integer(kind = kint), intent(in) :: ist_comp, Ncomp, Nfft
!!        real(kind = kreal), intent(in) :: X(Ncomp,Nfft)
!!        integer(kind = kint), intent(in) :: Ncomp_r, Nfft_r
!!        real(kind = kreal), intent(inout) :: X_FFT(Ncomp_r*Nfft_r)
!!      subroutine norm_prt_to_bwd_rocFFT(Ncomp, Nfft, X,               &
!!     &                                  NFFT_r, X_FFT)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_r
!!        real(kind = kreal), intent(in) :: X(Nfft,Ncomp)
!!        real(kind = kreal), intent(inout) :: X_FFT(NFFT_r,Ncomp)
!! ------------------------------------------------------------------
!!@endverbatim
      module normalize_for_rocFFT
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
      subroutine norm_rtp_from_fwd_rocFFT(Ncomp_r, Nfft_r, X_FFT,       &
     &                                    ist_comp, Ncomp, Nfft, X)
!
      integer(kind = kint), intent(in) :: Ncomp_r, Nfft_r
      real(kind = kreal), intent(in) :: X_FFT(Ncomp_r*Nfft_r)
!
      integer(kind = kint), intent(in) :: ist_comp, Ncomp, Nfft
      real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!
      integer(kind = kint) :: i, nd, inum, icomp
!
!
!$omp parallel
!$omp do private(nd,inum,icomp)
      do nd = 1, Ncomp_r
        icomp = nd + ist_comp - 1
        inum =  nd
        X(icomp,1) = X_FFT(2*inum-1)
        inum = nd + (Nfft_r/2-1) * Ncomp_r
        X(icomp,2) = X_FFT(2*inum-1)
      end do
!$omp end do nowait
      do i = 2, Nfft_r/2-1
!$omp do private(nd,inum,icomp)
        do nd = 1, Ncomp_r
          icomp = nd + ist_comp - 1
          inum = nd + (i-1) * Ncomp_r
          X(icomp,2*i-1) =  two * X_FFT(2*inum-1)
          X(icomp,2*i  ) = -two * X_FFT(2*inum  )
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine norm_rtp_from_fwd_rocFFT
!
! ------------------------------------------------------------------
!
      subroutine norm_prt_from_fwd_rocFFT(Ncomp, NFFT_r, X_FFT,         &
     &                                    Nfft, X)
!
      integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_r
      real(kind = kreal), intent(in) :: X_FFT(NFFT_r,Ncomp)
!
      real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
!
      integer(kind = kint) :: i, nd
!
!
!$omp parallel do private(i,nd)
      do nd = 1, Ncomp
        X(1,nd) = real(X_FFT(1,       nd))
        X(2,nd) = real(X_FFT(NFFT_r-1,nd))
        do i = 2, NFFT_r/2-1
          X(2*i-1,nd) =  two * X_FFT(2*i-1,nd)
          X(2*i,  nd) = -two * X_FFT(2*i,  nd)
        end do
      end do
!$omp end parallel do
!
      end subroutine norm_prt_from_fwd_rocFFT
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine norm_rtp_to_bwd_rocFFT(ist_comp, Ncomp, Nfft, X,       &
     &                                  Ncomp_r, Nfft_r, X_FFT)
!
      integer(kind = kint), intent(in) :: ist_comp, Ncomp, Nfft
      real(kind = kreal), intent(in) :: X(Ncomp,Nfft)
!
      integer(kind = kint), intent(in) :: Ncomp_r, Nfft_r
      real(kind = kreal), intent(inout) :: X_FFT(Ncomp_r*Nfft_r)
!
      integer(kind = kint) :: i, nd, inum
!
!$omp parallel
!$omp do private(nd,inum)
      do nd = 1, Ncomp_r
        inum = nd
        X_FFT(2*inum-1) = X(ist_comp+nd-1,1)
        X_FFT(2*inum  ) = zero
      end do
!$omp end do nowait
      do i = 2, Nfft_r/2-1
!$omp do private(nd,inum)
        do nd = 1, Ncomp_r
          inum = nd + (i-1) * Ncomp_r
          X_FFT(2*inum-1) =  half * X(ist_comp+nd-1,2*i-1)
          X_FFT(2*inum  ) = -half * X(ist_comp+nd-1,2*i  )
        end do
!$omp end do nowait
!$omp do private(nd,inum)
        do nd = 1, Ncomp_r
          inum = nd + (Nfft_r/2-1) * Ncomp_r
          X_FFT(2*inum-1) = X(ist_comp+nd-1,2)
          X_FFT(2*inum  ) = zero
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine norm_rtp_to_bwd_rocFFT
!
! ------------------------------------------------------------------
!
      subroutine norm_prt_to_bwd_rocFFT(Ncomp, Nfft, X,                 &
     &                                  NFFT_r, X_FFT)
!
      integer(kind = kint), intent(in) :: Ncomp, Nfft, NFFT_r
      real(kind = kreal), intent(in) :: X(Nfft,Ncomp)
!
      real(kind = kreal), intent(inout) :: X_FFT(NFFT_r,Ncomp)
!
      integer(kind = kint) :: i, nd
!
!$omp parallel do private(i,nd)
      do nd = 1, Ncomp
        X_FFT(1,nd) = X(1,nd)
        X_FFT(2,nd) = zero
        do i = 2, NFFT_r/2-1
          X_FFT(2*i-1,nd) =  half * X(2*i-1,nd)
          X_FFT(2*i,  nd) = -half * X(2*i,  nd)
        end do
        X_FFT(NFFT_r-1,nd) = X(2,nd)
        X_FFT(NFFT_r,  nd) = zero
      end do
!$omp end parallel do
!
      end subroutine norm_prt_to_bwd_rocFFT
!
! ------------------------------------------------------------------
!
      end module normalize_for_rocFFT
