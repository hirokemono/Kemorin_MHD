!>@file   t_single_ROCmFFT_wrapper.f90
!!@brief  module t_single_ROCmFFT_wrapper
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  Fourier transform using AMD ROCfft
!!
!!@verbatim
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT by FFTW
!!      subroutine calypso_sgl_ROCmFFT_init(Nfft, fwd, bwd, WK_fft)
!!      subroutine calypso_single_ROCmFFT_fin(fwd, bwd, WK_fft)
!!        integer(c_size_t), intent(in) :: Nfft
!!        type(single_ROCmfft_params), intent(inout), target :: fwd
!!        type(single_ROCmfft_params), intent(inout), target :: bwd
!!        type(single_ROCmfft_work), intent(inout), target :: WK_fft
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for forward Fourier transform by FFTW3
!!      subroutine calypso_sgl_fwd_ROCmFFT(fwd, Nfft_r, X_ROCmFFT,      &
!!     &          Nfft_c, C_ROCmFFT, Nbytes, data_ptr)
!!        type(single_ROCmfft_params), intent(in), target :: fwd
!!        integer(kind = kint), intent(in) :: Nfft_r, Nfft_c
!!        integer(c_size_t), intent(in) :: Nbytes
!!        real(kind = kreal), intent(in), target :: X_ROCmFFT(Nfft_r)
!!        complex(kind = kreal), intent(inout),target:: C_ROCmFFT(Nfft_c)
!!        type(c_ptr), intent(inout) :: data_ptr
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
!!
!!      subroutine calypso_sgl_bwd_ROCmFFT(bwd, Nfft_c, C_ROCmFFT,      &
!!     &          Nfft_r, X_ROCmFFT, Nbytes, data_ptr)
!!        type(single_ROCmfft_params), intent(in), target :: bwd
!!        integer(kind = kint), intent(in) :: Nfft_r, Nfft_c
!!        integer(c_size_t), intent(in) :: Nbytes
!!        complex(kind = kreal), intent(in), target:: C_ROCmFFT(Nfft_c)
!!        real(kind = kreal), intent(inout), target :: X_ROCmFFT(Nfft_r)
!!        type(c_ptr), intent(inout) :: data_ptr
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
      module t_single_ROCmFFT_wrapper
!
      use m_precision
      use m_constants
!
      use iso_c_binding
!
      implicit none
!
      type single_ROCmfft_params
        integer(c_size_t) :: Nfft =   0
        type(c_ptr) :: ROCfft_plan = c_null_ptr
      end type single_ROCmfft_params
!
      type single_ROCmfft_work
        real(kind = kreal) ::   aNfft = 0.0d0
        integer(kind = kint) :: Nfft_c = 0
        integer(kind = kint) :: Nfft_r = 0
        integer(c_size_t) ::    Nbytes = 0
!
        type(c_ptr) :: data_ptr = c_null_ptr
        real(kind = kreal), allocatable :: X_ROCmFFT(:)
        complex(kind = kreal), allocatable :: C_ROCmFFT(:)
      end type single_ROCmfft_work
!
      integer(c_size_t), parameter, private :: ione_c =  ione
!
      private :: calypso_sgl_ROCmFFT_set_size
      private :: calypso_sgl_ROCmFFT_alloc
      private :: calypso_sgl_fwd_ROCmFFT_init
      private :: calypso_sgl_bwd_ROCmFFT_init
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine calypso_sgl_ROCmFFT_init(Nfft, fwd, bwd, WK_fft)
!
      integer(c_size_t), intent(in) :: Nfft
      type(single_ROCmfft_params), intent(inout), target :: fwd
      type(single_ROCmfft_params), intent(inout), target :: bwd
      type(single_ROCmfft_work), intent(inout), target :: WK_fft
!
!
      call calypso_sgl_ROCmFFT_set_size(Nfft, fwd, bwd, WK_fft)
      call calypso_sgl_ROCmFFT_alloc(WK_fft)
!
!   Initialize Forward transform
      call calypso_sgl_fwd_ROCmFFT_init(fwd)
!   Initialize Backword transform
      call calypso_sgl_bwd_ROCmFFT_init(bwd)
!
      end subroutine calypso_sgl_ROCmFFT_init
!
! ------------------------------------------------------------------
!
      subroutine calypso_single_ROCmFFT_fin(fwd, bwd, WK_fft)
!
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      type(single_ROCmfft_params), intent(inout), target :: fwd
      type(single_ROCmfft_params), intent(inout), target :: bwd
      type(single_ROCmfft_work), intent(inout), target :: WK_fft
!
      call rocfftCheck(rocfft_plan_destroy(bwd%ROCfft_plan))
      call rocfftCheck(rocfft_plan_destroy(fwd%ROCfft_plan))
      call hipCheck(hipFree(WK_fft%data_ptr))
      deallocate(WK_fft%C_ROCmFFT, WK_fft%X_ROCmFFT)
!
      end subroutine calypso_single_ROCmFFT_fin
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine calypso_sgl_ROCmFFT_set_size(Nfft, fwd, bwd, WK_fft)
!
      integer(c_size_t), intent(in) :: Nfft
      type(single_ROCmfft_params), intent(inout) :: fwd, bwd
      type(single_ROCmfft_work), intent(inout), target :: WK_fft
!
      fwd%Nfft =   Nfft
      bwd%Nfft =   Nfft
      WK_fft%aNfft =  one / dble(Nfft)
      WK_fft%Nfft_c = Nfft / 2 + 1
      WK_fft%Nfft_r = 2 * WK_fft%Nfft_c
      WK_fft%Nbytes = WK_fft%Nfft_r * kreal
!
      end subroutine calypso_sgl_ROCmFFT_set_size
!
! ------------------------------------------------------------------
!
      subroutine calypso_sgl_ROCmFFT_alloc(WK_fft)
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
      allocate(WK_fft%X_ROCmFFT(WK_fft%Nfft_r))
      allocate(WK_fft%C_ROCmFFT(WK_fft%Nfft_c))
!$omp parallel workshare
      WK_fft%X_ROCmFFT(1:WK_fft%Nfft_r) = 0.0d0
!$omp end parallel workshare
!$omp parallel workshare
      WK_fft%C_ROCmFFT(1:WK_fft%Nfft_c) = 0.0d0
!$omp end parallel workshare
!
      end subroutine calypso_sgl_ROCmFFT_alloc
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine calypso_sgl_fwd_ROCmFFT_init(fwd)
!
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      type(single_ROCmfft_params), intent(inout), target :: fwd
!
!
      call rocfftCheck(rocfft_plan_create(fwd%ROCfft_plan,              &
     &                                      rocfft_placement_inplace,   &
     &                            rocfft_transform_type_real_forward,   &
     &                                       rocfft_precision_double,   &
     &                                       ione_c, c_loc(fwd%Nfft),   &
     &                                           ione_c, c_null_ptr))
!
      end subroutine calypso_sgl_fwd_ROCmFFT_init
!
! ------------------------------------------------------------------
!
      subroutine calypso_sgl_bwd_ROCmFFT_init(bwd)
!
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      type(single_ROCmfft_params), intent(inout), target :: bwd
!
!
      call rocfftCheck(rocfft_plan_create(bwd%ROCfft_plan,              &
     &                                      rocfft_placement_inplace,   &
     &                            rocfft_transform_type_real_inverse,   &
     &                                       rocfft_precision_double,   &
     &                                       ione_c, c_loc(bwd%Nfft),   &
     &                                           ione_c, c_null_ptr))
!
      end subroutine calypso_sgl_bwd_ROCmFFT_init
!
! ------------------------------------------------------------------
!
      subroutine calypso_sgl_fwd_ROCmFFT(fwd, Nfft_r, X_ROCmFFT,        &
     &          Nfft_c, C_ROCmFFT, Nbytes, data_ptr)
!
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      type(single_ROCmfft_params), intent(in), target :: fwd
      integer(kind = kint), intent(in) :: Nfft_r, Nfft_c
      integer(c_size_t), intent(in) :: Nbytes
      real(kind = kreal), intent(in), target :: X_ROCmFFT(Nfft_r)
!
      complex(kind = kreal), intent(inout), target :: C_ROCmFFT(Nfft_c)
      type(c_ptr), intent(inout) :: data_ptr
!
!
          call hipCheck(hipMemcpy(data_ptr, c_loc(X_ROCmFFT(1)),        &
     &                            Nbytes, hipMemcpyHostToDevice))
          call rocfftCheck                                              &
     &       (rocfft_execute(fwd%ROCfft_plan, data_ptr,                 &
     &                       c_null_ptr, c_null_ptr))
          call hipCheck(hipDeviceSynchronize())
          call hipCheck(hipMemcpy(c_loc(C_ROCmFFT(1)), data_ptr,        &
     &                  Nbytes, hipMemcpyDeviceToHost))
!
      end subroutine calypso_sgl_fwd_ROCmFFT
!
! ------------------------------------------------------------------
!
      subroutine calypso_sgl_bwd_ROCmFFT(bwd, Nfft_c, C_ROCmFFT,        &
     &          Nfft_r, X_ROCmFFT, Nbytes, data_ptr)
!
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      type(single_ROCmfft_params), intent(in), target :: bwd
      integer(kind = kint), intent(in) :: Nfft_r, Nfft_c
      integer(c_size_t), intent(in) :: Nbytes
      complex(kind = kreal), intent(in), target :: C_ROCmFFT(Nfft_c)
!
      real(kind = kreal), intent(inout), target :: X_ROCmFFT(Nfft_r)
      type(c_ptr), intent(inout) :: data_ptr
!
!
          call hipCheck(hipMemcpy(data_ptr, c_loc(C_ROCmFFT(1)),        &
     &                            Nbytes, hipMemcpyHostToDevice))
          call rocfftCheck(rocfft_execute(bwd%ROCfft_plan, data_ptr,    &
     &                                    c_null_ptr, c_null_ptr))
          call hipCheck(hipDeviceSynchronize())
          call hipCheck(hipMemcpy(c_loc(X_ROCmFFT(1)), data_ptr,        &
     &                            Nbytes, hipMemcpyDeviceToHost))
!
      end subroutine calypso_sgl_bwd_ROCmFFT
!
! ------------------------------------------------------------------
!
      end module t_single_ROCmFFT_wrapper
