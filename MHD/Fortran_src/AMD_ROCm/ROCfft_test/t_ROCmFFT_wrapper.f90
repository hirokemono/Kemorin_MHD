!>@file   t_ROCmFFT_wrapper.f90
!!@brief  module t_ROCmFFT_wrapper
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  Fourier transform using AMD ROCfft
!!
!!@verbatim
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT by FFTW
!!      subroutine calypso_pin_ROCmFFT_init(Nfft_r, Nbytes,             &
!!     &                                    l_real, fwd, bwd)
!!      subroutine calypso_pin_ROCmFFT_fin(fwd, bwd)
!!        integer(c_size_t), intent(in) :: Nfft_r
!!        integer(c_size_t), intent(in) :: Nbytes
!!        integer(c_size_t), intent(in), target :: l_real(3)
!!        type(calypso_ROCmfft_params), intent(inout), target :: fwd
!!        type(calypso_ROCmfft_params), intent(inout), target :: bwd
!! ------------------------------------------------------------------
!!
!!      subroutine calypso_pin_fwd_ROCmFFT(fwd, n_field,                &
!!     &          Nfft_r, X_ROCmFFT, Nfft_c, C_ROCmFFT,                 &
!!     &          Nbytes, data_ptr)
!!        type(calypso_ROCmfft_params), intent(in), target :: fwd
!!        integer(kind = kint) :: n_field
!!        integer(c_size_t), intent(in) :: Nfft_r, Nfft_c
!!        integer(c_size_t), intent(in) :: Nbytes
!!        real(kind = kreal), intent(in), target                        &
!!     &                   :: X_ROCmFFT(Nfft_r,n_field)
!!        complex(kind = kreal), intent(inout), target                  &
!!     &                   :: C_ROCmFFT(Nfft_c,n_field)
!!        type(c_ptr), intent(inout) :: data_ptr
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for forward Fourier transform by FFTW3
!!
!!   a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!   b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!    K = Nfft/2....
!!   a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!! ------------------------------------------------------------------
!!
!!      subroutine calypso_pin_bwd_ROCmFFT(bwd, n_field,                &
!!     &          Nfft_c, C_ROCmFFT, Nfft_r, X_ROCmFFT,                 &
!!     &          Nbytes, data_ptr)
!!        type(calypso_ROCmfft_params), intent(in), target :: bwd
!!        integer(kind = kint) :: n_field
!!        integer(c_size_t), intent(in) :: Nfft_r, Nfft_c
!!        integer(c_size_t), intent(in) :: Nbytes
!!        complex(kind = kreal), intent(in), target                     &
!!     &                   :: C_ROCmFFT(Nfft_c,n_field)
!!        real(kind = kreal), intent(inout), target                     &
!!     &                   :: X_ROCmFFT(Nfft_r,n_field)
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
      module t_ROCmFFT_wrapper
!
      use m_precision
      use m_constants
!
      use iso_c_binding
!
      implicit none
!
      type calypso_ROCmfft_params
        type(c_ptr) :: data_ptr =          c_null_ptr
!
        type(c_ptr) :: ROCfft_plan =        c_null_ptr
        type(c_ptr) :: ROCfft_description = c_null_ptr
!
        integer(c_size_t) :: ROCfft_wk_buf_size = 0
        type(c_ptr) :: ROCfft_work_buffer = c_null_ptr
        type(c_ptr) :: ROCfft_work_info =   c_null_ptr
!
        type(c_ptr) :: ROCfft_wk_info =   c_null_ptr
        type(c_ptr) :: ROCfft_wk_buffer = c_null_ptr
!
        type(c_ptr) :: in_offsets =  c_null_ptr
        type(c_ptr) :: out_offsets = c_null_ptr
        integer(c_size_t) :: in_strides_size
        integer(c_size_t) :: in_strides(3) =  (/0, 0, 0/)
        integer(c_size_t) :: in_distance =      0
        integer(c_size_t) :: out_strides_size = ithree
        integer(c_size_t) :: out_strides(3) = (/0, 0, 0/)
        integer(c_size_t) :: out_distance =     0
        type(c_ptr) :: strides_nullpo = c_null_ptr
      end type calypso_ROCmfft_params
!
      integer(c_size_t), parameter, private :: ione_c =  ione
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine calypso_pin_ROCmFFT_init(Nfft_r, Nbytes,               &
     &                                    l_real, fwd, bwd)
!
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      integer(c_size_t), intent(in) :: Nfft_r
      integer(c_size_t), intent(in) :: Nbytes
      integer(c_size_t), intent(in), target :: l_real(3)
      type(calypso_ROCmfft_params), intent(inout), target :: fwd
      type(calypso_ROCmfft_params), intent(inout), target :: bwd
!
!   Initialize Forward transform
      fwd%in_strides_size =   1
      fwd%in_strides(1) =     1
      fwd%in_distance =       Nfft_r
      fwd%out_strides_size =  0
      fwd%out_distance =      0
!
      call hipCheck(hipMalloc(fwd%data_ptr, Nbytes))
      call rocfftCheck                                                  &
     &   (rocfft_plan_description_create(fwd%ROCfft_description))
      call rocfftCheck(rocfft_plan_description_set_data_layout          &
     &                                      (fwd%ROCfft_description,    &
     &                                       rocfft_array_type_real,    &
     &                                      rocfft_array_type_unset,    &
     &                                               fwd%in_offsets,    &
     &                                              fwd%out_offsets,    &
     &                                          fwd%in_strides_size,    &
     &                                     c_loc(fwd%in_strides(1)),    &
     &                                              fwd%in_distance,    &
     &                                         fwd%out_strides_size,    &
     &                                           fwd%strides_nullpo,    &
     &                                            fwd%out_distance))
      call rocfftCheck(rocfft_plan_create(fwd%ROCfft_plan,              &
                                          rocfft_placement_inplace,     &
                                  rocfft_transform_type_real_forward,   &
                                          rocfft_precision_double,      &
                                          ione_c, c_loc(l_real(1)),     &
                                  l_real(2), fwd%ROCfft_description))
!
      call rocfftCheck                                                  &
     &   (rocfft_plan_get_work_buffer_size(fwd%ROCfft_plan,             &
     &                                     fwd%ROCfft_wk_buf_size))
      call rocfftCheck                                                  &
     &   (rocfft_execution_info_create(fwd%ROCfft_work_info))
      if(fwd%ROCfft_wk_buf_size > 0) then
        call hipCheck(hipMalloc(fwd%ROCfft_work_buffer,                 &
     &                          fwd%ROCfft_wk_buf_size))
        call rocfftCheck(rocfft_execution_info_set_work_buffer          &
     &                                        (fwd%ROCfft_work_info,    &
     &                                         fwd%ROCfft_work_buffer,  &
     &                                         fwd%ROCfft_wk_buf_size))
      end if
!
!   Initialize Backword transform
      bwd%in_strides_size =  0
      bwd%in_distance =      0
      bwd%out_strides_size = 1
      bwd%out_strides(1) =   1
      bwd%out_distance =     Nfft_r
!
      call hipCheck(hipMalloc(bwd%data_ptr, Nbytes))
      call rocfftCheck                                                  &
     &   (rocfft_plan_description_create(bwd%ROCfft_description))
      call rocfftCheck(rocfft_plan_description_set_data_layout          &
     &                                      (bwd%ROCfft_description,    &
     &                                      rocfft_array_type_unset,    &
     &                                       rocfft_array_type_real,    &
     &                                               bwd%in_offsets,    &
     &                                              bwd%out_offsets,    &
     &                                          bwd%in_strides_size,    &
     &                                           bwd%strides_nullpo,    &
     &                                              bwd%in_distance,    &
     &                                         bwd%out_strides_size,    &
     &                                    c_loc(bwd%out_strides(1)),    &
     &                                            bwd%out_distance))
!
      call rocfftCheck(rocfft_plan_create(bwd%ROCfft_plan,              &
                                          rocfft_placement_inplace,     &
                                  rocfft_transform_type_real_inverse,   &
                                          rocfft_precision_double,      &
                                          ione_c, c_loc(l_real(1)),     &
                                  l_real(2), bwd%ROCfft_description))
!
      call rocfftCheck                                                  &
     &   (rocfft_plan_get_work_buffer_size(bwd%ROCfft_plan,             &
     &                                     bwd%ROCfft_wk_buf_size))
      write(*,*) 'bwd%ROCfft_wk_buf_size', bwd%ROCfft_wk_buf_size
      call rocfftCheck                                                  &
     &   (rocfft_execution_info_create(bwd%ROCfft_work_info))
      if(bwd%ROCfft_wk_buf_size > 0) then
        call hipCheck(hipMalloc(bwd%ROCfft_work_buffer,                 &
     &                          bwd%ROCfft_wk_buf_size))
        call rocfftCheck(rocfft_execution_info_set_work_buffer          &
     &                                       (bwd%ROCfft_work_info,     &
     &                                        bwd%ROCfft_work_buffer,   &
     &                                        bwd%ROCfft_wk_buf_size))
      end if
!
      end subroutine calypso_pin_ROCmFFT_init
!
! ------------------------------------------------------------------
!
      subroutine calypso_pin_ROCmFFT_fin(fwd, bwd)
!
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      type(calypso_ROCmfft_params), intent(inout), target :: fwd
      type(calypso_ROCmfft_params), intent(inout), target :: bwd
!
      call rocfftCheck                                                  &
     &   (rocfft_execution_info_destroy(fwd%ROCfft_work_info))
      call rocfftCheck                                                  &
     &   (rocfft_execution_info_destroy(bwd%ROCfft_work_info))
      if(fwd%ROCfft_wk_buf_size > 0) then
        call hipCheck(hipFree(fwd%ROCfft_work_buffer))
      end if
      if(bwd%ROCfft_wk_buf_size > 0) then
        call hipCheck(hipFree(bwd%ROCfft_work_buffer))
      end if
      call rocfftCheck                                                  &
     &   (rocfft_plan_description_destroy(bwd%ROCfft_description))
      call rocfftCheck                                                  &
     &   (rocfft_plan_description_destroy(fwd%ROCfft_description))
      call rocfftCheck(rocfft_plan_destroy(bwd%ROCfft_plan))
      call rocfftCheck(rocfft_plan_destroy(fwd%ROCfft_plan))
      call hipCheck(hipFree(bwd%data_ptr))
      call hipCheck(hipFree(fwd%data_ptr))
!
      end subroutine calypso_pin_ROCmFFT_fin
!
! ------------------------------------------------------------------
!
      subroutine calypso_pin_fwd_ROCmFFT(fwd, n_field,                  &
     &          Nfft_r, X_ROCmFFT, Nfft_c, C_ROCmFFT,                   &
     &          Nbytes, data_ptr)
!
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      type(calypso_ROCmfft_params), intent(in), target :: fwd
      integer(kind = kint) :: n_field
      integer(c_size_t), intent(in) :: Nfft_r, Nfft_c
      integer(c_size_t), intent(in) :: Nbytes
      real(kind = kreal), intent(in), target                            &
     &                   :: X_ROCmFFT(Nfft_r,n_field)
!
      complex(kind = kreal), intent(inout), target                      &
     &                   :: C_ROCmFFT(Nfft_c,n_field)
      type(c_ptr), intent(inout) :: data_ptr
!
!
      call hipCheck(hipMemcpy(data_ptr, c_loc(X_ROCmFFT(1,1)),          &
     &                        Nbytes, hipMemcpyHostToDevice))
      call rocfftCheck(rocfft_execute(fwd%ROCfft_plan, data_ptr,        &
     &                        c_null_ptr, fwd%ROCfft_work_info))
      call hipCheck(hipDeviceSynchronize())
      call hipCheck(hipMemcpy(c_loc(C_ROCmFFT(1,1)), data_ptr,          &
     &                        Nbytes, hipMemcpyDeviceToHost))
!
      end subroutine calypso_pin_fwd_ROCmFFT
!
! ------------------------------------------------------------------
!
      subroutine calypso_pin_bwd_ROCmFFT(bwd, n_field,                  &
     &          Nfft_c, C_ROCmFFT, Nfft_r, X_ROCmFFT,                   &
     &          Nbytes, data_ptr)
!
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      type(calypso_ROCmfft_params), intent(in), target :: bwd
      integer(kind = kint) :: n_field
      integer(c_size_t), intent(in) :: Nfft_r, Nfft_c
      integer(c_size_t), intent(in) :: Nbytes
      complex(kind = kreal), intent(in), target                         &
     &                   :: C_ROCmFFT(Nfft_c,n_field)
!
      real(kind = kreal), intent(inout), target                         &
     &                   :: X_ROCmFFT(Nfft_r,n_field)
      type(c_ptr), intent(inout) :: data_ptr
!
!
        call hipCheck(hipMemcpy(data_ptr, c_loc(C_ROCmFFT(1,1)),        &
     &                          Nbytes, hipMemcpyHostToDevice))
        call rocfftCheck(rocfft_execute(bwd%ROCfft_plan, data_ptr,      &
     &                          c_null_ptr, bwd%ROCfft_work_info))
        call hipCheck(hipDeviceSynchronize())
        call hipCheck(hipMemcpy(c_loc(X_ROCmFFT(1,1)), data_ptr,        &
     &                          Nbytes, hipMemcpyDeviceToHost))
!
      end subroutine calypso_pin_bwd_ROCmFFT
!
! ------------------------------------------------------------------
!
      end module t_ROCmFFT_wrapper
