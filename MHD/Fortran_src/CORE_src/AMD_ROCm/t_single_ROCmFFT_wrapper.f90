!>@file   t_single_ROCmFFT_wrapper.f90
!!@brief  module t_single_ROCmFFT_wrapper
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  Fourier transform using AMD rocFFT
!!
!!@verbatim
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT by FFTW
!!      subroutine calypso_sgl_rocFFT_init(Nfft, WK_fft)
!!      subroutine calypso_sgl_rocFFT_fin(WK_fft)
!!        integer(c_size_t), intent(in) :: Nfft
!!        type(single_ROCmfft_work), intent(inout), target :: WK_fft
!! ------------------------------------------------------------------
!!
!! ------------------------------------------------------------------
!!
!!
!!   a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!   b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \sin (\frac{2\pi j k}{Nfft})
!!
!!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!    K = Nfft/2....
!!   a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!! ------------------------------------------------------------------
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
!!
      module t_single_ROCmFFT_wrapper
!
      use m_precision
      use m_constants
!
      use iso_c_binding
!
      implicit none
!
      type single_ROCmfft_work
        type(c_ptr) :: rocFFT_fwd_plan = c_null_ptr
        type(c_ptr) :: rocFFT_bwd_plan = c_null_ptr
!
        integer(c_size_t) ::    Nfft =   0
        integer(c_size_t) ::    Nbytes = 0
        integer(kind = kint) :: Nfft_c = 0
        integer(kind = kint) :: Nfft_r = 0
        real(kind = kreal) ::   aNfft = 0.0d0
!
        type(c_ptr) :: data_ptr = c_null_ptr
        real(kind = kreal), allocatable :: X_rocFFT(:)
        complex(kind = kreal), allocatable :: C_rocFFT(:)
      end type single_ROCmfft_work
!
      private :: calypso_sgl_rocFFT_set_size
      private :: calypso_sgl_rocFFT_alloc
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine calypso_sgl_rocFFT_init(Nfft, WK_fft)
!
      use calypso_single_ROCmFFT
!
      integer(c_size_t), intent(in) :: Nfft
      type(single_ROCmfft_work), intent(inout), target :: WK_fft
!
!
      call calypso_sgl_rocFFT_set_size(Nfft, WK_fft)
      call calypso_sgl_rocFFT_alloc(WK_fft)
!
!   Initialize Forward transform
      call calypso_sgl_fwd_rocFFT_init(Nfft, WK_fft%rocFFT_fwd_plan)
!   Initialize Backword transform
      call calypso_sgl_bwd_rocFFT_init(Nfft, WK_fft%rocFFT_bwd_plan)
!
      end subroutine calypso_sgl_rocFFT_init
!
! ------------------------------------------------------------------
!
      subroutine calypso_sgl_rocFFT_fin(WK_fft)
!
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      type(single_ROCmfft_work), intent(inout), target :: WK_fft
!
      call rocfftCheck(rocfft_plan_destroy(WK_fft%rocFFT_bwd_plan))
      call rocfftCheck(rocfft_plan_destroy(WK_fft%rocFFT_fwd_plan))
      call hipCheck(hipFree(WK_fft%data_ptr))
      deallocate(WK_fft%C_rocFFT, WK_fft%X_rocFFT)
!
      end subroutine calypso_sgl_rocFFT_fin
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine calypso_sgl_rocFFT_set_size(Nfft, WK_fft)
!
      integer(c_size_t), intent(in) :: Nfft
      type(single_ROCmfft_work), intent(inout), target :: WK_fft
!
      WK_fft%Nfft =   Nfft
      WK_fft%aNfft =  one / dble(Nfft)
      WK_fft%Nfft_c = Nfft / 2 + 1
      WK_fft%Nfft_r = 2 * WK_fft%Nfft_c
      WK_fft%Nbytes = WK_fft%Nfft_r * kreal
!
      end subroutine calypso_sgl_rocFFT_set_size
!
! ------------------------------------------------------------------
!
      subroutine calypso_sgl_rocFFT_alloc(WK_fft)
!
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      type(single_ROCmfft_work), intent(inout), target :: WK_fft
!
!   Initialize Forward transform
      call hipCheck(hipMalloc(WK_fft%data_ptr, WK_fft%Nbytes))
!
      allocate(WK_fft%X_rocFFT(WK_fft%Nfft_r))
      allocate(WK_fft%C_rocFFT(WK_fft%Nfft_c))
!$omp parallel workshare
      WK_fft%X_rocFFT(1:WK_fft%Nfft_r) = 0.0d0
!$omp end parallel workshare
!$omp parallel workshare
      WK_fft%C_rocFFT(1:WK_fft%Nfft_c) = 0.0d0
!$omp end parallel workshare
!
      end subroutine calypso_sgl_rocFFT_alloc
!
! ------------------------------------------------------------------
!
      end module t_single_ROCmFFT_wrapper
