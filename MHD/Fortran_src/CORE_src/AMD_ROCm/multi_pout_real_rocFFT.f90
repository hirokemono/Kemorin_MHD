!>@file   multi_pout_real_rocFFT.f90
!!@brief  module multi_pout_real_rocFFT
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! wrapper subroutine for initialization of rocFFT
!! wrapper subroutine for forward Fourier transform by rocFFT
!!      subroutine multi_pout_fwd_rocFFT_r2r(fwd, WK_fft, X,            &
!!     &                                     elapsed_fft, elapsed_cpy)
!!      subroutine multi_pout_fwd_OMP_rocFFT(fwd, WK_fft, X,            &
!!     &                                     elapsed_fft, elapsed_cpy)
!!        type(calypso_rocFFT_params), intent(in), target :: fwd
!!        type(calypso_rocFFT_work), intent(inout) :: WK_fft
!!        real(kind = kreal), intent(inout) :: X(fwd%Ncomp,fwd%Nfft)
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
!! wrapper subroutine for backward Fourier transform by rocFFT
!!      subroutine multi_pout_bwd_rocFFT_r2r(bwd, WK_fft, X,            &
!!     &                                     elapsed_fft, elapsed_cpy)
!!      subroutine multi_pout_bwd_OMP_rocFFT(bwd, WK_fft, X,            &
!!     &                                     elapsed_fft, elapsed_cpy)
!!        type(calypso_rocFFT_params), intent(in), target :: bwd
!!        type(calypso_rocFFT_work), intent(inout) :: WK_fft
!!        real(kind = kreal), intent(inout) :: X(bwd%Ncomp,bwd%Nfft)
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
      module multi_pout_real_rocFFT
!
      use omp_lib
!
      use m_precision
      use m_constants
      use t_multi_rocFFT_wrapper
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine multi_pout_fwd_rocFFT_r2r(fwd, WK_fft, X,              &
     &                                     elapsed_fft, elapsed_cpy)
!
      use normalize_for_rocFFT
      use calypso_multi_rocFFT
!
      type(calypso_rocFFT_params), intent(in), target :: fwd
!
      type(calypso_rocFFT_work), intent(inout) :: WK_fft
      real(kind = kreal), intent(inout) :: X(fwd%Ncomp,fwd%Nfft)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
      integer(kind = kint) :: i, ist
!
!
        start = OMP_GET_WTIME()
!$omp parallel do private(i,ist)
        do i = 1, fwd%Nfft
          ist = (i-1) * fwd%Ncomp
          WK_fft%X_rocFFT(ist+1:ist+fwd%Ncomp) = X(1:fwd%Ncomp,i)
        end do
!$omp end parallel do
        if(fwd%Nfft .lt. WK_fft%Nfft_r) then
!$omp parallel do private(i,ist)
          do i = fwd%Nfft+1, WK_fft%Nfft_r
            ist = (i-1) * fwd%Ncomp
            WK_fft%X_rocFFT(ist+1:ist+fwd%Ncomp) = 0.0d0
          end do
!$omp end parallel do
        end if
        elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call calypso_forward_rocFFT_r2r                                 &
     &     (fwd%rocFFT_plan, fwd%rocFFT_wk_info, fwd%Ncomp,             &
     &      WK_fft%aNfft, WK_fft%Nfft_r, WK_fft%X_rocFFT(1),            &
     &      fwd%Nbytes, WK_fft%data_ptr)
        elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call norm_rtp_from_fwd_rocFFT                                   &
     &     (int(fwd%Ncomp), int(WK_fft%NFFT_r), WK_fft%X_rocFFT(1),     &
     &      int(fwd%Nfft), X(1,1))
        elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine multi_pout_fwd_rocFFT_r2r
!
! ------------------------------------------------------------------
!
      subroutine multi_pout_bwd_rocFFT_r2r(bwd, WK_fft, X,              &
     &                                     elapsed_fft, elapsed_cpy)
!
      use normalize_for_rocFFT
      use calypso_multi_rocFFT
!
      type(calypso_rocFFT_params), intent(in), target :: bwd
!
      type(calypso_rocFFT_work), intent(inout) :: WK_fft
      real(kind = kreal), intent(inout) :: X(bwd%Ncomp,bwd%Nfft)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
      integer(kind = kint) :: i, ist
!
!
      start = OMP_GET_WTIME()
      call norm_rtp_to_bwd_rocFFT                                       &
     &   (int(bwd%Ncomp), int(bwd%Nfft), X(1,1),                        &
     &    int(WK_fft%Nfft_r), WK_fft%X_rocFFT(1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call calypso_backward_rocFFT_r2r                                  &
     &   (bwd%rocFFT_plan, bwd%rocFFT_wk_info,                          &
     &    bwd%Ncomp, WK_fft%Nfft_r, WK_fft%X_rocFFT(1),                 &
     &    bwd%Nbytes, WK_fft%data_ptr)
      elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
!$omp parallel do private(i,ist)
      do i = 1, bwd%Nfft
        ist = (i-1) * bwd%Ncomp
        X(1:bwd%Ncomp,i) = WK_fft%X_rocFFT(ist+1:ist+bwd%Ncomp)
      end do
!$omp end parallel do
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine multi_pout_bwd_rocFFT_r2r
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine multi_pout_fwd_OMP_rocFFT(fwd, WK_fft, X,              &
     &                                     elapsed_fft, elapsed_cpy)
!
      use normalize_for_rocFFT
      use calypso_multi_rocFFT
!
      type(calypso_rocFFT_params), intent(in), target :: fwd
!
      type(calypso_rocFFT_work), intent(inout) :: WK_fft
      real(kind = kreal), intent(inout) :: X(fwd%Ncomp,fwd%Nfft)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
      integer(kind = kint) :: i, ist
!
!
        start = OMP_GET_WTIME()
!$omp parallel do private(i,ist)
        do i = 1, fwd%Nfft
          ist = (i-1) * fwd%Ncomp
          WK_fft%X_rocFFT(ist+1:ist+fwd%Ncomp) = X(1:fwd%Ncomp,i)
        end do
!$omp end parallel do
        if(fwd%Nfft .lt. WK_fft%Nfft_r) then
!$omp parallel do private(i,ist)
          do i = fwd%Nfft+1, WK_fft%Nfft_r
            ist = (i-1) * fwd%Ncomp
            WK_fft%X_rocFFT(ist+1:ist+fwd%Ncomp) = 0.0d0
          end do
!$omp end parallel do
        end if
        elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call calypso_fwd_OpenMP_rocFFT                                  &
     &     (fwd%rocFFT_plan, fwd%rocFFT_wk_info, fwd%Ncomp,             &
     &      WK_fft%aNfft, WK_fft%Nfft_r, WK_fft%X_rocFFT(1))
        elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call norm_rtp_from_fwd_rocFFT                                   &
     &     (int(fwd%Ncomp), int(WK_fft%NFFT_r), WK_fft%X_rocFFT(1),     &
     &      int(fwd%Nfft), X(1,1))
        elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine multi_pout_fwd_OMP_rocFFT
!
! ------------------------------------------------------------------
!
      subroutine multi_pout_bwd_OMP_rocFFT(bwd, WK_fft, X,              &
     &                                     elapsed_fft, elapsed_cpy)
!
      use normalize_for_rocFFT
      use calypso_multi_rocFFT
!
      type(calypso_rocFFT_params), intent(in), target :: bwd
!
      type(calypso_rocFFT_work), intent(inout) :: WK_fft
      real(kind = kreal), intent(inout) :: X(bwd%Ncomp,bwd%Nfft)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
      integer(kind = kint) :: i, ist
!
!
      start = OMP_GET_WTIME()
      call norm_rtp_to_bwd_rocFFT                                       &
     &   (int(bwd%Ncomp), int(bwd%Nfft),X(1,1),                         &
     &    int(WK_fft%Nfft_r), WK_fft%X_rocFFT(1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call calypso_bwd_OpenMP_rocFFT                                    &
         (bwd%rocFFT_plan, bwd%rocFFT_wk_info,                          &
     &    bwd%Ncomp, WK_fft%Nfft_r, WK_fft%X_rocFFT(1))
      elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
!$omp parallel do private(i,ist)
      do i = 1, bwd%Nfft
        ist = (i-1) * bwd%Ncomp
        X(1:bwd%Ncomp,i) = WK_fft%X_rocFFT(ist+1:ist+bwd%Ncomp)
      end do
!$omp end parallel do
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine multi_pout_bwd_OMP_rocFFT
!
! ------------------------------------------------------------------
!
      end module multi_pout_real_rocFFT
