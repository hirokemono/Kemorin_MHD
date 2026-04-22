!>@file   multi_pin_ROCmFFT_offload2.f90
!!@brief  module multi_pin_ROCmFFT_offload2
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! wrapper subroutine for forward Fourier transform by FFTW3
!!      subroutine multi_pin_fwd_ROCmFFT2(fwd, WK_fwd, X,               &
!!     &                                 elapsed_fft, elapsed_cpy)
!!      subroutine multi_pin_fwd_OMP_ROCmFFT(fwd, WK_fwd, X,            &
!!     &                                     elapsed_fft, elapsed_cpy)
!!        type(calypso_ROCmfft_params), intent(in), target :: fwd
!!        type(calypso_ROCmfft_work), intent(inout) :: WK_fwd
!!        real(kind = kreal), intent(inout) :: X(fwd%Nfft,fwd%Ncomp)
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
!!      subroutine multi_pin_bwd_ROCmFFT2(bwd, WK_bwd, X,               &
!!     &                                 elapsed_fft, elapsed_cpy)
!!      subroutine multi_pin_bwd_OMP_ROCmFFT(bwd, WK_bwd, X,            &
!!     &                                     elapsed_fft, elapsed_cpy)
!!        type(calypso_ROCmfft_params), intent(in), target :: bwd
!!        type(calypso_ROCmfft_work), intent(inout) :: WK_bwd
!!        real(kind = kreal), intent(inout) :: X(bwd%Nfft,bwd%Ncomp)
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
      module multi_pin_ROCmFFT_offload2
!
      use omp_lib
!
      use m_precision
      use m_constants
      use t_multi_ROCmFFT_wrapper
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine multi_pin_fwd_ROCmFFT2(fwd, WK_fwd, X,                 &
     &                                 elapsed_fft, elapsed_cpy)
!
      use normalize_for_ROCmFFT
!
      type(calypso_ROCmfft_params), intent(in), target :: fwd
!
      type(calypso_ROCmfft_work), intent(inout) :: WK_fwd
      real(kind = kreal), intent(inout) :: X(fwd%Nfft,fwd%Ncomp)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
      integer(kind = kint) :: nd, i, j
!
!
        start = OMP_GET_WTIME()
!$omp parallel do private(nd,i,j)
        do nd = 1, fwd%Ncomp
          do i = 1, fwd%Nfft
            j = i + (nd-1) * WK_fwd%Nfft_r
            WK_fwd%X_ROCmFFT(j) = X(i,nd)
          end do
          do i = fwd%Nfft+1, WK_fwd%Nfft_r
            j = i + (nd-1) * WK_fwd%Nfft_r
            WK_fwd%X_ROCmFFT(j) = zero
          end do
        end do
!$omp end parallel do
        elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call calypso_forward_ROCmFFT_r2r                                &
     &     (fwd%ROCfft_plan, fwd%ROCfft_wk_info,                        &
     &      fwd%Ncomp, WK_fwd%Nfft_r, WK_fwd%X_ROCmFFT,                 &
     &      fwd%Nbytes, WK_fwd%data_ptr)
        elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call norm_prt_from_fwd_ROCmFFT(int(fwd%Ncomp), WK_fwd%aNfft,    &
     &                            WK_fwd%NFFT_r, WK_fwd%X_ROCmFFT(1),   &
     &                               int(fwd%Nfft), X(1,1))
        elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine multi_pin_fwd_ROCmFFT2
!
! ------------------------------------------------------------------
!
      subroutine multi_pin_bwd_ROCmFFT2(bwd, WK_bwd, X,                 &
     &                                 elapsed_fft, elapsed_cpy)
!
      use normalize_for_ROCmFFT
!
      type(calypso_ROCmfft_params), intent(in), target :: bwd
!
      type(calypso_ROCmfft_work), intent(inout) :: WK_bwd
      real(kind = kreal), intent(inout) :: X(bwd%Nfft,bwd%Ncomp)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
      integer(kind = kint) :: nd, i, j
!
!
        start = OMP_GET_WTIME()
        call norm_prt_to_bwd_ROCmFFT(int(bwd%Ncomp), int(bwd%Nfft),     &
     &                  X(1,1), WK_bwd%Nfft_r, WK_bwd%X_ROCmFFT(1))
        elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call calypso_backward_ROCmFFT_r2r                               &
     &     (bwd%ROCfft_plan, bwd%ROCfft_wk_info,                        &
     &      bwd%Ncomp, WK_bwd%Nfft_r, WK_bwd%X_ROCmFFT(1),              &
     &      bwd%Nbytes, WK_bwd%data_ptr)
        elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
!$omp parallel do private(nd,i,j)
        do nd = 1, bwd%Ncomp
          do i = 1, bwd%Nfft
            j = i + (nd-1) * WK_bwd%Nfft_r
            X(i,nd) = WK_bwd%X_ROCmFFT(j)
          end do
        end do
!$omp end parallel do
        elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine multi_pin_bwd_ROCmFFT2
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine multi_pin_fwd_OMP_ROCmFFT(fwd, WK_fwd, X,              &
     &                                     elapsed_fft, elapsed_cpy)
!
      use normalize_for_ROCmFFT
!
      type(calypso_ROCmfft_params), intent(in), target :: fwd
!
      type(calypso_ROCmfft_work), intent(inout) :: WK_fwd
      real(kind = kreal), intent(inout) :: X(fwd%Nfft,fwd%Ncomp)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
      integer(kind = kint) :: nd, i, j
!
!
        start = OMP_GET_WTIME()
!$omp parallel do private(nd,i,j)
        do nd = 1, fwd%Ncomp
          do i = 1, fwd%Nfft
            j = i + (nd-1) * WK_fwd%Nfft_r
            WK_fwd%X_ROCmFFT(j) = X(i,nd)
          end do
          do i = fwd%Nfft+1, WK_fwd%Nfft_r
            j = i + (nd-1) * WK_fwd%Nfft_r
            WK_fwd%X_ROCmFFT(j) = zero
          end do
        end do
!$omp end parallel do
        elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call calypso_fwd_OpenMP_ROCmFFT                                 &
     &     (fwd%ROCfft_plan, fwd%ROCfft_wk_info,                        &
     &      fwd%Ncomp, WK_fwd%Nfft_r, WK_fwd%X_ROCmFFT)
        elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call norm_prt_from_fwd_ROCmFFT(int(fwd%Ncomp), WK_fwd%aNfft,    &
     &                            WK_fwd%NFFT_r, WK_fwd%X_ROCmFFT(1),   &
     &                               int(fwd%Nfft), X(1,1))
        elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine multi_pin_fwd_OMP_ROCmFFT
!
! ------------------------------------------------------------------
!
      subroutine multi_pin_bwd_OMP_ROCmFFT(bwd, WK_bwd, X,              &
     &                                     elapsed_fft, elapsed_cpy)
!
      use normalize_for_ROCmFFT
!
      type(calypso_ROCmfft_params), intent(in), target :: bwd
!
      type(calypso_ROCmfft_work), intent(inout) :: WK_bwd
      real(kind = kreal), intent(inout) :: X(bwd%Nfft,bwd%Ncomp)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
      integer(kind = kint) :: nd, i, j
!
!
        start = OMP_GET_WTIME()
        call norm_prt_to_bwd_ROCmFFT(int(bwd%Ncomp), int(bwd%Nfft),     &
     &                  X(1,1), WK_bwd%Nfft_r, WK_bwd%X_ROCmFFT(1))
        elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call calypso_bwd_OpenMP_ROCmFFT                                 &
           (bwd%ROCfft_plan, bwd%ROCfft_wk_info,                        &
     &      bwd%Ncomp, WK_bwd%Nfft_r, WK_bwd%X_ROCmFFT(1))
        elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
!$omp parallel do private(nd,i,j)
        do nd = 1, bwd%Ncomp
          do i = 1, bwd%Nfft
            j = i + (nd-1) * WK_bwd%Nfft_r
            X(i,nd) = WK_bwd%X_ROCmFFT(j)
          end do
        end do
!$omp end parallel do
        elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine multi_pin_bwd_OMP_ROCmFFT
!
! ------------------------------------------------------------------
!
      end module multi_pin_ROCmFFT_offload2
