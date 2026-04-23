!>@file   calypso_multi_ROCmFFT.f90
!!@brief  module calypso_multi_ROCmFFT
!!
!!@author H. Matsui
!!@date Programmed in April, 2026
!
!>@brief  Fourier transform using AMD rocFFT
!!
!!@verbatim
!! ------------------------------------------------------------------
!! wrapper subroutine for forward Fourier transform by FFTW3
!!      subroutine calypso_forward_ROCmFFT_r2c(fwd_plan, fwd_wk_info,   &
!!     &          Ncomp, Nfft_r, X_rocFFT, Nfft_c, C_rocFFT,            &
!!     &          Nbytes, data_ptr)
!!        real(kind = kreal), intent(in), target                        &
!!     &                   :: X_rocFFT(Nfft_r*Ncomp)
!!        complex(kind = kreal), intent(inout), target                  &
!!     &                   :: C_rocFFT(Nfft_c*Ncomp)
!!      subroutine calypso_forward_ROCmFFT_r2r(fwd_plan, fwd_wk_info,   &
!!     &          Ncomp, Nfft_r, X_rocFFT, Nbytes, data_ptr)
!!      subroutine calypso_fwd_OpenMP_ROCmFFT(fwd_plan, fwd_wk_info,    &
!!     &                                      Ncomp, Nfft_r, X_rocFFT)
!!        type(c_ptr), intent(in), target :: fwd_plan
!!        type(c_ptr), intent(in), target :: fwd_wk_info
!!        integer(c_size_t), intent(in) :: Ncomp, Nfft_r, Nfft_c
!!        integer(c_size_t), intent(in) :: Nbytes
!!        real(kind = kreal), intent(inout), target                     &
!!     &                   :: X_rocFFT(Nfft_r*Ncomp)
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
!!      subroutine calypso_backward_ROCmFFT_c2r(bwd_plan, bwd_wk_info,  &
!!     &          Ncomp, Nfft_c, C_rocFFT, Nfft_r, X_rocFFT,            &
!!     &          Nbytes, data_ptr)
!!      subroutine calypso_backward_ROCmFFT_r2r(bwd_plan, bwd_wk_info,  &
!!     &          Ncomp, Nfft_r, X_rocFFT, Nbytes, data_ptr)
!!      subroutine calypso_bwd_OpenMP_ROCmFFT(bwd_plan, bwd_wk_info,    &
!!     &                                      Ncomp, Nfft_r, X_rocFFT)
!!        type(c_ptr), intent(in), target :: bwd_plan
!!        type(c_ptr), intent(in), target :: bwd_wk_info
!!        integer(c_size_t), intent(in) :: Ncomp, Nfft_r, Nfft_c
!!        integer(c_size_t), intent(in) :: Nbytes
!!        complex(kind = kreal), intent(in), target                     &
!!     &                   :: C_rocFFT(Nfft_c*Ncomp)
!!        real(kind = kreal), intent(inout), target                     &
!!     &                   :: X_rocFFT(Nfft_r*Ncomp)
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
      module calypso_multi_ROCmFFT
!
      use m_precision
      use m_constants
!
      use iso_c_binding
!
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine calypso_forward_ROCmFFT_r2c(fwd_plan, fwd_wk_info,     &
     &          Ncomp, Nfft_r, X_rocFFT, Nfft_c, C_rocFFT,              &
     &          Nbytes, data_ptr)
!
      type(c_ptr), intent(in), target :: fwd_plan
      type(c_ptr), intent(in), target :: fwd_wk_info
      integer(c_size_t), intent(in) :: Ncomp, Nfft_r, Nfft_c
      integer(c_size_t), intent(in) :: Nbytes
      real(kind = kreal), intent(in), target                            &
     &                   :: X_rocFFT(Nfft_r*Ncomp)
!
      complex(kind = kreal), intent(inout), target                      &
     &                   :: C_rocFFT(Nfft_c*Ncomp)
      type(c_ptr), intent(inout) :: data_ptr
!
!
      call hipCheck(hipMemcpy(data_ptr, c_loc(X_rocFFT(1)),             &
     &                        Nbytes, hipMemcpyHostToDevice))
      call rocfftCheck(rocfft_execute(fwd_plan, data_ptr,               &
     &                                c_null_ptr, fwd_wk_info))
      call hipCheck(hipDeviceSynchronize())
      call hipCheck(hipMemcpy(c_loc(C_rocFFT(1)), data_ptr,             &
     &                        Nbytes, hipMemcpyDeviceToHost))
!
      end subroutine calypso_forward_ROCmFFT_r2c
!
! ------------------------------------------------------------------
!
      subroutine calypso_backward_ROCmFFT_c2r(bwd_plan, bwd_wk_info,    &
     &          Ncomp, Nfft_c, C_rocFFT, Nfft_r, X_rocFFT,              &
     &          Nbytes, data_ptr)
!
      type(c_ptr), intent(in), target :: bwd_plan
      type(c_ptr), intent(in), target :: bwd_wk_info
      integer(c_size_t), intent(in) :: Ncomp, Nfft_r, Nfft_c
      integer(c_size_t), intent(in) :: Nbytes
      complex(kind = kreal), intent(in), target                         &
     &                   :: C_rocFFT(Nfft_c*Ncomp)
!
      real(kind = kreal), intent(inout), target                         &
     &                   :: X_rocFFT(Nfft_r*Ncomp)
      type(c_ptr), intent(inout) :: data_ptr
!
!
      call hipCheck(hipMemcpy(data_ptr, c_loc(C_rocFFT(1)),             &
     &                        Nbytes, hipMemcpyHostToDevice))
      call rocfftCheck(rocfft_execute(bwd_plan, data_ptr,               &
     &                                c_null_ptr, bwd_wk_info))
      call hipCheck(hipDeviceSynchronize())
      call hipCheck(hipMemcpy(c_loc(X_rocFFT(1)), data_ptr,             &
     &                        Nbytes, hipMemcpyDeviceToHost))
!
      end subroutine calypso_backward_ROCmFFT_c2r
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine calypso_forward_ROCmFFT_r2r(fwd_plan, fwd_wk_info,     &
     &          Ncomp, Nfft_r, X_rocFFT, Nbytes, data_ptr)
!
      type(c_ptr), intent(in), target :: fwd_plan
      type(c_ptr), intent(in), target :: fwd_wk_info
      integer(c_size_t), intent(in) :: Ncomp, Nfft_r
      integer(c_size_t), intent(in) :: Nbytes
!
      real(kind = kreal), intent(inout), target                         &
     &                   :: X_rocFFT(Nfft_r*Ncomp)
      type(c_ptr), intent(inout) :: data_ptr
!
!
      call hipCheck(hipMemcpy(data_ptr, c_loc(X_rocFFT(1)),             &
     &                        Nbytes, hipMemcpyHostToDevice))
      call rocfftCheck(rocfft_execute(fwd_plan, data_ptr,               &
     &                                c_null_ptr, fwd_wk_info))
      call hipCheck(hipDeviceSynchronize())
      call hipCheck(hipMemcpy(c_loc(X_rocFFT(1)), data_ptr,             &
     &                        Nbytes, hipMemcpyDeviceToHost))
!
      end subroutine calypso_forward_ROCmFFT_r2r
!
! ------------------------------------------------------------------
!
      subroutine calypso_backward_ROCmFFT_r2r(bwd_plan, bwd_wk_info,    &
     &          Ncomp, Nfft_r, X_rocFFT, Nbytes, data_ptr)
!
      type(c_ptr), intent(in), target :: bwd_plan
      type(c_ptr), intent(in), target :: bwd_wk_info
      integer(c_size_t), intent(in) :: Ncomp, Nfft_r
      integer(c_size_t), intent(in) :: Nbytes
!
      real(kind = kreal), intent(inout), target                         &
     &                   :: X_rocFFT(Nfft_r*Ncomp)
      type(c_ptr), intent(inout) :: data_ptr
!
!
      call hipCheck(hipMemcpy(data_ptr, c_loc(X_rocFFT(1)),             &
     &                        Nbytes, hipMemcpyHostToDevice))
      call rocfftCheck(rocfft_execute(bwd_plan, data_ptr,               &
     &                                c_null_ptr, bwd_wk_info))
      call hipCheck(hipDeviceSynchronize())
      call hipCheck(hipMemcpy(c_loc(X_rocFFT(1)), data_ptr,             &
     &                        Nbytes, hipMemcpyDeviceToHost))
!
      end subroutine calypso_backward_ROCmFFT_r2r
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine calypso_fwd_OpenMP_ROCmFFT(fwd_plan, fwd_wk_info,      &
     &                                      Ncomp, Nfft_r, X_rocFFT)
!
      type(c_ptr), intent(in), target :: fwd_plan
      type(c_ptr), intent(in), target :: fwd_wk_info
      integer(c_size_t), intent(in) :: Ncomp, Nfft_r
!
      real(kind = kreal), intent(inout), target                         &
     &                   :: X_rocFFT(Nfft_r*Ncomp)
!
!$OMP target enter data map(to:X_rocFFT)
!$OMP target data use_device_addr(X_rocFFT)
!      call rocblasCheck(rocblas_set_pointer_mode(rocblas_handle, 0))
      call rocfftCheck(rocfft_execute(fwd%rocFFT_plan,                  &
     &    c_loc(X_rocFFT(1)), c_null_ptr, fwd%rocFFT_wk_info))
      call hipCheck(hipDeviceSynchronize())
!$OMP end target data
!$OMP target update from(X_rocFFT)
!$OMP target exit data map(delete:X_rocFFT)
!
      end subroutine calypso_fwd_OpenMP_ROCmFFT
!
! ------------------------------------------------------------------
!
      subroutine calypso_bwd_OpenMP_ROCmFFT(bwd_plan, bwd_wk_info,      &
     &                                      Ncomp, Nfft_r, X_rocFFT)
!
      type(c_ptr), intent(in), target :: bwd_plan
      type(c_ptr), intent(in), target :: bwd_wk_info
      integer(c_size_t), intent(in) :: Ncomp, Nfft_r
!
      real(kind = kreal), intent(inout), target                         &
     &                   :: X_rocFFT(Nfft_r*Ncomp)
!
!$OMP target enter data map(to:X_rocFFT)
!$OMP target data use_device_addr(X_rocFFT)
!      call rocblasCheck(rocblas_set_pointer_mode(rocblas_handle, 0))
      call rocfftCheck(rocfft_execute(bwd_plan, c_loc(X_rocFFT(1)),     &
     &                                c_null_ptr, bwd_wk_info))
      call hipCheck(hipDeviceSynchronize())
!$OMP end target data
!$OMP target update from(X_rocFFT)
!$OMP target exit data map(delete:X_rocFFT)
!
      end subroutine calypso_bwd_OpenMP_ROCmFFT
!
! ------------------------------------------------------------------
!
      end module calypso_multi_ROCmFFT
