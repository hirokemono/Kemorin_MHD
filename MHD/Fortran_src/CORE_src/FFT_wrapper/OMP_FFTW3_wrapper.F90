!>@file   OMP_FFTW3_wrapper.F90
!!@brief  module OMP_FFTW3_wrapper
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!
!!      subroutine forward_mul_OMP_FFTW(Ncomp, Nfft, X, WK,             &
!!     &                                elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft
!!        type(working_mul_FFTW), intent(inout) :: WK
!!        real(kind = kreal), intent(inout) :: X(Ncomp, Nfft)
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for forward Fourier transform by FFTW3
!!
!!   a_{k} = \frac{2}{Nfft}
!!          \sum_{j=0}^{Nfft-1} [x_{j} \cos (\frac{2\pi j k}{Nfft})]
!!   b_{k} = \frac{2}{Nfft}
!!          \sum_{j=0}^{Nfft-1} [x_{j} \sin (\frac{2\pi j k}{Nfft})]
!!
!!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!    K = Nfft/2....
!!   a_{k} = \frac{1}{Nfft}
!!          \sum_{j=0}^{Nfft-1} [x_{j} \cos (\frac{2\pi j k}{Nfft})]
!!
!! ------------------------------------------------------------------
!!
!!      subroutine backward_mul_OMP_FFTW(Ncomp, Nfft, X, WK,            &
!!     &                                 elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft
!!        type(working_mul_FFTW), intent(inout) :: WK
!!        real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for backward Fourier transform by FFTW3
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
!!
!!@n @param Nsmp  Number of SMP processors
!!@n @param Nstacksmp(0:Nsmp)   End number for each SMP process
!!@n @param Ncomp           Number of components for Fourier transforms
!!@n @param Nfft        Data length for eadh FFT
!!@n @param X(Ncomp, Nfft)  Data for Fourier transform
!!
!!@n @param plan_forward   FFTW plan for forward transform
!!@n @param plan_backward  FFTW plan for backward transform
!!@n @param aNfft       normalization parameter for FFTW (= 1 / Nfft)
!!@n @param X_FFTW      real data for multiple Fourier transform
!!@n @param C_FFTW      spectrum data for multiple Fourier transform
!
      module OMP_FFTW3_wrapper
!
      use omp_lib
!
      use m_precision
      use m_constants
      use m_fftw_parameters
!
      use t_multi_FFTW_wrapper
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine forward_mul_OMP_FFTW(Ncomp, Nfft, X, WK,               &
     &                                elapsed_fft, elapsed_cpy)
!
      use normalize_for_OMP_FFTW
      use normalize_for_FFTW
!
      integer(kind = kint), intent(in) :: Ncomp, Nfft
!
      type(working_mul_FFTW), intent(inout) :: WK
      real(kind = kreal), intent(inout) :: X(Ncomp, Nfft)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: st
!
!
      st = OMP_GET_WTIME()
      call dfftw_execute_dft_r2c(WK%plan_mul_fwd(1), X(1,1),            &
     &                           WK%C_FFTW_mul(1,1))
      elapsed_fft = elapsed_fft + OMP_GET_WTIME() - st
!
!   normalization
      st = OMP_GET_WTIME()
      call normalize_fwd_OMP_FFTW                                       &
     &   (WK%aNfft, Ncomp, WK%Nfft_c, WK%C_FFTW_mul(1,1))
      call norm_rtp_from_fwd_OMP_FFTW                                   &
     &   (Ncomp, WK%Nfft_c, WK%C_FFTW_mul(1,1), ione, Ncomp, Nfft, X)
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - st
!
      end subroutine forward_mul_OMP_FFTW
!
! ------------------------------------------------------------------
!
      subroutine backward_mul_OMP_FFTW(Ncomp, Nfft, X, WK,              &
     &                                 elapsed_fft, elapsed_cpy)
!
      use normalize_for_OMP_FFTW
!
      integer(kind = kint), intent(in) :: Ncomp, Nfft
!
      type(working_mul_FFTW), intent(inout) :: WK
      real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: st
!
!   normalization
      st = OMP_GET_WTIME()
      call norm_rtp_to_bwd_OMP_FFTW(ione, Ncomp, Nfft, X,               &
     &    Ncomp, WK%Nfft_c, WK%C_FFTW_mul(1,1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - st
!
      st = OMP_GET_WTIME()
      call dfftw_execute_dft_c2r(WK%plan_mul_bwd(1),                    &
     &                           WK%C_FFTW_mul(1,1), X(1,1))
      elapsed_fft = elapsed_fft + OMP_GET_WTIME() - st
!
      end subroutine backward_mul_OMP_FFTW
!
! ------------------------------------------------------------------
!
      end module OMP_FFTW3_wrapper
