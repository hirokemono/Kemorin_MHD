!>@file   calypso_single_ROCmFFT.f90
!!@brief  module calypso_single_ROCmFFT
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2026
!
!>@brief  Single Fourier transform using AMD ROCfft
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine calypso_sgl_fwd_ROCmFFT_init(Nfft, ROCfft_fwd_plan)
!!        integer(c_size_t), intent(in), target :: Nfft
!!        type(c_ptr), intent(inout), target :: ROCfft_fwd_plan
!!   wrapper subroutine for initierize FFT by FFTW
!!      subroutine calypso_sgl_bwd_ROCmFFT_init(Nfft, ROCfft_bwd_plan)
!!        integer(c_size_t), intent(in), target :: Nfft
!!        type(c_ptr), intent(inout), target :: ROCfft_bwd_plan
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for forward Fourier transform by FFTW3
!!      subroutine calypso_sgl_fwd_ROCmFFT_r2c(ROCfft_fwd_plan,         &
!!     &          Nfft_r, X_ROCmFFT, Nfft_c, C_ROCmFFT,                 &
!!     &          Nbytes, data_ptr)
!!        type(c_ptr), intent(in), target :: ROCfft_fwd_plan
!!        integer(kind = kint), intent(in) :: Nfft_r, Nfft_c
!!        integer(c_size_t), intent(in) :: Nbytes
!!        real(kind = kreal), intent(in), target :: X_ROCmFFT(Nfft_r)
!!        complex(kind = kreal), intent(inout),target:: C_ROCmFFT(Nfft_c)
!!        type(c_ptr), intent(inout) :: data_ptr
!!      subroutine calypso_sgl_fwd_ROCmFFT_r2r(ROCfft_fwd_plan,         &
!!     &          Nfft_r, X_ROCmFFT, Nbytes, data_ptr)
!!        type(c_ptr), intent(in), target :: ROCfft_fwd_plan
!!        integer(kind = kint), intent(in) :: Nfft_r
!!        integer(c_size_t), intent(in) :: Nbytes
!!        real(kind = kreal), intent(inout), target :: X_ROCmFFT(Nfft_r)
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
!!      subroutine calypso_sgl_bwd_ROCmFFT_c2r(ROCfft_bwd_plan,         &
!!     &          Nfft_c, C_ROCmFFT, Nfft_r, X_ROCmFFT,                 &
!!     &          Nbytes, data_ptr)
!!        type(c_ptr), intent(in), target :: ROCfft_bwd_plan
!!        integer(kind = kint), intent(in) :: Nfft_r, Nfft_c
!!        integer(c_size_t), intent(in) :: Nbytes
!!        complex(kind = kreal), intent(in), target:: C_ROCmFFT(Nfft_c)
!!        real(kind = kreal), intent(inout), target :: X_ROCmFFT(Nfft_r)
!!        type(c_ptr), intent(inout) :: data_ptr
!!      subroutine calypso_sgl_bwd_ROCmFFT_r2r(ROCfft_bwd_plan,         &
!!     &          Nfft_r, X_ROCmFFT, Nbytes, data_ptr)
!!        type(c_ptr), intent(in), target :: ROCfft_bwd_plan
!!        integer(kind = kint), intent(in) :: Nfft_r
!!        integer(c_size_t), intent(in) :: Nbytes
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
      module calypso_single_ROCmFFT
!
      use m_precision
      use m_constants
!
      use iso_c_binding
!
      implicit none
!
      integer(c_size_t), parameter, private :: ione_c =  ione
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine calypso_sgl_fwd_ROCmFFT_init(Nfft, ROCfft_fwd_plan)
!
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      integer(c_size_t), intent(in), target :: Nfft
      type(c_ptr), intent(inout), target :: ROCfft_fwd_plan
!
!
      call rocfftCheck(rocfft_plan_create(ROCfft_fwd_plan,              &
     &                                      rocfft_placement_inplace,   &
     &                            rocfft_transform_type_real_forward,   &
     &                                       rocfft_precision_double,   &
     &                                           ione_c, c_loc(Nfft),   &
     &                                           ione_c, c_null_ptr))
!
      end subroutine calypso_sgl_fwd_ROCmFFT_init
!
! ------------------------------------------------------------------
!
      subroutine calypso_sgl_bwd_ROCmFFT_init(Nfft, ROCfft_bwd_plan)
!
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      integer(c_size_t), intent(in), target :: Nfft
      type(c_ptr), intent(inout), target :: ROCfft_bwd_plan
!
!
      call rocfftCheck(rocfft_plan_create(ROCfft_bwd_plan,              &
     &                                      rocfft_placement_inplace,   &
     &                            rocfft_transform_type_real_inverse,   &
     &                                       rocfft_precision_double,   &
     &                                           ione_c, c_loc(Nfft),   &
     &                                           ione_c, c_null_ptr))
!
      end subroutine calypso_sgl_bwd_ROCmFFT_init
!
! ------------------------------------------------------------------
!
      subroutine calypso_sgl_fwd_ROCmFFT_r2c(ROCfft_fwd_plan,           &
     &          Nfft_r, X_ROCmFFT, Nfft_c, C_ROCmFFT,                   &
     &          Nbytes, data_ptr)
!
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      type(c_ptr), intent(in), target :: ROCfft_fwd_plan
      integer(kind = kint), intent(in) :: Nfft_r, Nfft_c
      integer(c_size_t), intent(in) :: Nbytes
      real(kind = kreal), intent(in), target :: X_ROCmFFT(Nfft_r)
!
      complex(kind = kreal), intent(inout), target :: C_ROCmFFT(Nfft_c)
      type(c_ptr), intent(inout) :: data_ptr
!
!
      call hipCheck(hipMemcpy(data_ptr, c_loc(X_ROCmFFT(1)),            &
     &                        Nbytes, hipMemcpyHostToDevice))
      call rocfftCheck(rocfft_execute(ROCfft_fwd_plan, data_ptr,        &
     &                                c_null_ptr, c_null_ptr))
      call hipCheck(hipDeviceSynchronize())
      call hipCheck(hipMemcpy(c_loc(C_ROCmFFT(1)), data_ptr,            &
     &                        Nbytes, hipMemcpyDeviceToHost))
!
      end subroutine calypso_sgl_fwd_ROCmFFT_r2c
!
! ------------------------------------------------------------------
!
      subroutine calypso_sgl_bwd_ROCmFFT_c2r(ROCfft_bwd_plan,           &
     &          Nfft_c, C_ROCmFFT, Nfft_r, X_ROCmFFT,                   &
     &          Nbytes, data_ptr)
!
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      type(c_ptr), intent(in), target :: ROCfft_bwd_plan
      integer(kind = kint), intent(in) :: Nfft_r, Nfft_c
      integer(c_size_t), intent(in) :: Nbytes
      complex(kind = kreal), intent(in), target :: C_ROCmFFT(Nfft_c)
!
      real(kind = kreal), intent(inout), target :: X_ROCmFFT(Nfft_r)
      type(c_ptr), intent(inout) :: data_ptr
!
!
      call hipCheck(hipMemcpy(data_ptr, c_loc(C_ROCmFFT(1)),            &
     &                        Nbytes, hipMemcpyHostToDevice))
      call rocfftCheck(rocfft_execute(ROCfft_bwd_plan, data_ptr,        &
     &                                c_null_ptr, c_null_ptr))
      call hipCheck(hipDeviceSynchronize())
      call hipCheck(hipMemcpy(c_loc(X_ROCmFFT(1)), data_ptr,            &
     &                        Nbytes, hipMemcpyDeviceToHost))
!
      end subroutine calypso_sgl_bwd_ROCmFFT_c2r
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine calypso_sgl_fwd_ROCmFFT_r2r(ROCfft_fwd_plan,           &
     &          Nfft_r, X_ROCmFFT, Nbytes, data_ptr)
!
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      type(c_ptr), intent(in), target :: ROCfft_fwd_plan
      integer(kind = kint), intent(in) :: Nfft_r
      integer(c_size_t), intent(in) :: Nbytes
!
      real(kind = kreal), intent(inout), target :: X_ROCmFFT(Nfft_r)
      type(c_ptr), intent(inout) :: data_ptr
!
!
      call hipCheck(hipMemcpy(data_ptr, c_loc(X_ROCmFFT(1)),            &
     &                        Nbytes, hipMemcpyHostToDevice))
      call rocfftCheck(rocfft_execute(ROCfft_fwd_plan, data_ptr,        &
     &                                c_null_ptr, c_null_ptr))
      call hipCheck(hipDeviceSynchronize())
      call hipCheck(hipMemcpy(c_loc(X_ROCmFFT(1)), data_ptr,            &
     &                        Nbytes, hipMemcpyDeviceToHost))
!
      end subroutine calypso_sgl_fwd_ROCmFFT_r2r
!
! ------------------------------------------------------------------
!
      subroutine calypso_sgl_bwd_ROCmFFT_r2r(ROCfft_bwd_plan,           &
     &          Nfft_r, X_ROCmFFT, Nbytes, data_ptr)
!
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      type(c_ptr), intent(in), target :: ROCfft_bwd_plan
      integer(kind = kint), intent(in) :: Nfft_r
      integer(c_size_t), intent(in) :: Nbytes
!
      real(kind = kreal), intent(inout), target :: X_ROCmFFT(Nfft_r)
      type(c_ptr), intent(inout) :: data_ptr
!
!
      call hipCheck(hipMemcpy(data_ptr, c_loc(X_ROCmFFT(1)),            &
     &                        Nbytes, hipMemcpyHostToDevice))
      call rocfftCheck(rocfft_execute(ROCfft_bwd_plan, data_ptr,        &
     &                                c_null_ptr, c_null_ptr))
      call hipCheck(hipDeviceSynchronize())
      call hipCheck(hipMemcpy(c_loc(X_ROCmFFT(1)), data_ptr,            &
     &                        Nbytes, hipMemcpyDeviceToHost))
!
      end subroutine calypso_sgl_bwd_ROCmFFT_r2r
!
! ------------------------------------------------------------------
!
      end module calypso_single_ROCmFFT
