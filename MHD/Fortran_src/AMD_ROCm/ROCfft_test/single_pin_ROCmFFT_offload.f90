!>@file   single_pin_ROCmFFT_offload.f90
!!@brief  module single_pin_ROCmFFT_offload
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! wrapper subroutine for forward Fourier transform by FFTW3
!!      subroutine single_pin_fwd_ROCmFFT(fwd, WK_fwd, n_comp, X,       &
!!     &                                  elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) :: n_comp
!!        type(single_ROCmfft_params), intent(in), target :: fwd
!!        type(single_ROCmfft_work), intent(inout) :: WK_fwd
!!        real(kind = kreal), intent(inout) :: X(fwd%Nfft,n_comp)
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!! ------------------------------------------------------------------
!!
!!   a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!   b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \sin (\frac{2\pi j k}{Nfft})
!!
!!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!    K = Nfft/2....
!!   a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for backward Fourier transform by FFTW3
!!      subroutine single_pin_bwd_ROCmFFT(bwd, WK_bwd, n_comp, X,       &
!!     &                                  elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) :: n_comp
!!        type(single_ROCmfft_params), intent(in), target :: bwd
!!        type(single_ROCmfft_work), intent(inout) :: WK_bwd
!!        real(kind = kreal), intent(inout) :: X(bwd%Nfft,n_comp)
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!! ------------------------------------------------------------------
!!
!!   x_{k} = a_{0} + (-1)^{j} a_{Nfft/2} + sum_{k=1}^{Nfft/2-1}
!!          (a_{k} \cos(2\pijk/Nfft) + b_{k} \sin(2\pijk/Nfft))
!!
!! ------------------------------------------------------------------
!!
!!       i = 1:     a_{0}
!!       i = 2:     a_{Nfft/2}
!!       i = 3:     a_{1}
!!       i = 4:     b_{1}
!!       ...
!!       i = 2*k+1: a_{k}
!!       i = 2*k+2: b_{k}
!!       ...
!!       i = Nfft-1:   a_{Nfft/2-1}
!!       i = Nfft:     b_{Nfft/2-1}
!!
!! ------------------------------------------------------------------
!!@endverbatim
!
      module single_pin_ROCmFFT_offload
!
      use omp_lib
!
      use m_precision
      use m_constants
      use t_single_ROCmFFT_wrapper
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine single_pin_fwd_ROCmFFT(fwd, WK_fwd, n_comp, X,         &
     &                                  elapsed_fft, elapsed_cpy)
!
      integer(kind = kint), intent(in) :: n_comp
      type(single_ROCmfft_params), intent(in), target :: fwd
!
      type(single_ROCmfft_work), intent(inout) :: WK_fwd
      real(kind = kreal), intent(inout) :: X(fwd%Nfft,n_comp)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
      integer(kind = kint) :: nd, i
!
!
      do nd = 1, n_comp
        start = OMP_GET_WTIME()
!$omp parallel workshare
        WK_fwd%X_ROCmFFT(1:WK_fwd%Nfft_r) = 0.0d0
!$omp end parallel workshare
!$omp parallel workshare
        WK_fwd%X_ROCmFFT(1:fwd%Nfft) = X(1:fwd%Nfft,nd)
!$omp end parallel workshare
        elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start

        start = OMP_GET_WTIME()
        call calypso_sgl_fwd_ROCmFFT(fwd,                               &
     &                               WK_fwd%Nfft_r, WK_fwd%X_ROCmFFT,   &
     &                               WK_fwd%Nfft_c, WK_fwd%C_ROCmFFT,   &
     &                               WK_fwd%Nbytes, WK_fwd%data_ptr)
        elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        X(1,nd) = WK_fwd%aNfft * real(WK_fwd%C_ROCmFFT(1))
        X(2,nd) = WK_fwd%aNfft * real(WK_fwd%C_ROCmFFT(WK_fwd%NFFT_c))
!$omp parallel do
        do i = 2, WK_fwd%NFFT_c - 1
          X(2*i-1,nd) =  two * WK_fwd%aNfft * real(WK_fwd%C_ROCmFFT(i))
          X(2*i,  nd) = -two * WK_fwd%aNfft * imag(WK_fwd%C_ROCmFFT(i))
        end do
!$omp end parallel do
        elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
      end do
!
      end subroutine single_pin_fwd_ROCmFFT
!
! ------------------------------------------------------------------
!
      subroutine single_pin_bwd_ROCmFFT(bwd, WK_bwd, n_comp, X,         &
     &                                  elapsed_fft, elapsed_cpy)
!
      integer(kind = kint), intent(in) :: n_comp
      type(single_ROCmfft_params), intent(in), target :: bwd
!
      type(single_ROCmfft_work), intent(inout) :: WK_bwd
      real(kind = kreal), intent(inout) :: X(bwd%Nfft,n_comp)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
      integer(kind = kint) :: i, nd
!
!
!   Backword transform
      do nd = 1, n_comp
        start = OMP_GET_WTIME()
        WK_bwd%C_ROCmFFT(1) = cmplx(X(1,nd), zero, kind(0d0))
!$omp parallel do
        do i = 2, WK_bwd%Nfft_c - 1
          WK_bwd%C_ROCmFFT(i)                                           &
     &         = half * cmplx(X(2*i-1,nd), -X(2*i,nd),kind(0d0))
        end do
!$omp end parallel do
        WK_bwd%C_ROCmFFT(WK_bwd%Nfft_c)                                 &
     &         = cmplx(X(2,nd), zero, kind(0d0))
        elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call calypso_sgl_bwd_ROCmFFT(bwd,                               &
     &                               WK_bwd%Nfft_c, WK_bwd%C_ROCmFFT,   &
     &                               WK_bwd%Nfft_r, WK_bwd%X_ROCmFFT,   &
     &                               WK_bwd%Nbytes, WK_bwd%data_ptr)
        elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
!$omp parallel workshare
        X(1:bwd%Nfft,nd) = WK_bwd%X_ROCmFFT(1:bwd%Nfft)
!$omp end parallel workshare
        elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
      end do
!
      end subroutine single_pin_bwd_ROCmFFT
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine single_pin_fwd_ROCmFFT2(fwd, WK_fwd, n_comp, X,        &
     &                                   elapsed_fft, elapsed_cpy)
!
      integer(kind = kint), intent(in) :: n_comp
      type(single_ROCmfft_params), intent(in), target :: fwd
!
      type(single_ROCmfft_work), intent(inout) :: WK_fwd
      real(kind = kreal), intent(inout) :: X(fwd%Nfft,n_comp)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
      integer(kind = kint) :: nd, i
!
!
      do nd = 1, n_comp
        start = OMP_GET_WTIME()
!$omp parallel workshare
        WK_fwd%X_ROCmFFT(1:WK_fwd%Nfft_r) = 0.0d0
!$omp end parallel workshare
!$omp parallel workshare
        WK_fwd%X_ROCmFFT(1:fwd%Nfft) = X(1:fwd%Nfft,nd)
!$omp end parallel workshare
        elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start

        start = OMP_GET_WTIME()
        call calypso_sgl_fwd_ROCmFFT2(fwd,                              &
     &                               WK_fwd%Nfft_r, WK_fwd%X_ROCmFFT,   &
     &                               WK_fwd%Nbytes, WK_fwd%data_ptr)
        elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        X(1,nd)= WK_fwd%aNfft * real(WK_fwd%X_ROCmFFT(1))
        X(2,nd)= WK_fwd%aNfft * real(WK_fwd%X_ROCmFFT(WK_fwd%NFFT_r-1))
!$omp parallel do
        do i = 2, WK_fwd%Nfft_r/2 - 1
          X(2*i-1,nd) =  two * WK_fwd%aNfft * WK_fwd%X_ROCmFFT(2*i-1)
          X(2*i,  nd) = -two * WK_fwd%aNfft * WK_fwd%X_ROCmFFT(2*i  )
        end do
!$omp end parallel do
        elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
      end do
!
      end subroutine single_pin_fwd_ROCmFFT2
!
! ------------------------------------------------------------------
!
      subroutine single_pin_bwd_ROCmFFT2(bwd, WK_bwd, n_comp, X,        &
     &                                   elapsed_fft, elapsed_cpy)
!
      integer(kind = kint), intent(in) :: n_comp
      type(single_ROCmfft_params), intent(in), target :: bwd
!
      type(single_ROCmfft_work), intent(inout) :: WK_bwd
      real(kind = kreal), intent(inout) :: X(bwd%Nfft,n_comp)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
      integer(kind = kint) :: i, nd
!
!
!   Backword transform
      do nd = 1, n_comp
        start = OMP_GET_WTIME()
        WK_bwd%X_ROCmFFT(1) = X(1,nd)
        WK_bwd%X_ROCmFFT(2) = zero
!$omp parallel do
        do i = 2, WK_bwd%Nfft_r/2 - 1
          WK_bwd%X_ROCmFFT(2*i-1) =  half * X(2*i-1,nd)
          WK_bwd%X_ROCmFFT(2*i  ) = -half * X(2*i,  nd)
        end do
!$omp end parallel do
        WK_bwd%X_ROCmFFT(WK_bwd%Nfft_r-1) = X(2,nd)
        WK_bwd%X_ROCmFFT(WK_bwd%Nfft_r  ) = zero
        elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call calypso_sgl_bwd_ROCmFFT2(bwd,                              &
     &                               WK_bwd%Nfft_r, WK_bwd%X_ROCmFFT,   &
     &                               WK_bwd%Nbytes, WK_bwd%data_ptr)
        elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
!$omp parallel workshare
        X(1:bwd%Nfft,nd) = WK_bwd%X_ROCmFFT(1:bwd%Nfft)
!$omp end parallel workshare
        elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
      end do
!
      end subroutine single_pin_bwd_ROCmFFT2
!
! ------------------------------------------------------------------
!
      end module single_pin_ROCmFFT_offload
