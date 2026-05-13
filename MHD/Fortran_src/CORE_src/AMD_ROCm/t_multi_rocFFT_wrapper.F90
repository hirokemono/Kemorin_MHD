!>@file   t_multi_rocFFT_wrapper.F90
!!@brief  module t_multi_rocFFT_wrapper
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2026
!
!>@brief  Fourier transform using AMD rocFFT
!!
!!@verbatim
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT by FFTW
!!      subroutine calypso_rocFFT_set_size(Ncomp_fwd, Ncomp_bwd, Nfft,  &
!!     &                                   fwd, bwd, WK_fft)
!!        integer(kind = kint), intent(in) :: Ncomp_fwd, Ncomp_bwd
!!        integer(kind = kint), intent(in) :: Nfft
!!        type(calypso_rocFFT_params), intent(inout) :: fwd, bwd
!!        type(calypso_rocFFT_work), intent(inout), target :: WK_fft
!!      subroutine calypso_fwd_rocFFT_init(fwd)
!!      subroutine calypso_bwd_rocFFT_init(bwd)
!!      subroutine calypso_rocFFT_fin(fwd, bwd)
!!        type(calypso_rocFFT_params), intent(inout), target :: fwd
!!        type(calypso_rocFFT_params), intent(inout), target :: bwd
!!      subroutine calypso_rocFFT_alloc(fwd, bwd, WK_fft)
!!        type(calypso_rocFFT_params), intent(in) :: fwd, bwd
!!        type(calypso_rocFFT_work), intent(inout), target :: WK_fft
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
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for backward Fourier transform
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
      module t_multi_rocFFT_wrapper
!
      use m_precision
      use m_constants
!
      use iso_c_binding
!
      implicit none
!
      type calypso_rocFFT_params
        integer(c_size_t) ::    Nfft =   0
        integer(c_size_t) ::    Ncomp =  0
        integer(c_size_t) ::    Nbytes = 0
!
        type(c_ptr) :: rocFFT_plan =        c_null_ptr
        type(c_ptr) :: rocFFT_description = c_null_ptr
!
        integer(c_size_t) :: rocFFT_wk_buf_size = 0
        type(c_ptr) :: rocFFT_wk_buffer = c_null_ptr
        type(c_ptr) :: rocFFT_wk_info =   c_null_ptr
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
      end type calypso_rocFFT_params
!
      type calypso_rocFFT_work
        real(kind = kreal) :: aNfft = 0.0d0
        integer(c_size_t) :: Nfft_c = 0
        integer(c_size_t) :: Nfft_r = 0
!
        type(c_ptr) :: data_ptr = c_null_ptr
        real(kind = kreal), allocatable :: X_rocFFT(:)
        complex(kind = kreal), allocatable :: C_rocFFT(:)
      end type calypso_rocFFT_work
!
      integer(c_size_t), parameter, private :: ione_c =  ione
!
#ifdef _AMD_ROCM_
      private :: calypso_each_rocFFT_fin
#endif
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine calypso_rocFFT_set_size(Ncomp_fwd, Ncomp_bwd, Nfft,    &
     &                                   fwd, bwd, WK_fft)
!
      integer(kind = kint), intent(in) :: Ncomp_fwd, Ncomp_bwd
      integer(kind = kint), intent(in) :: Nfft
      type(calypso_rocFFT_params), intent(inout) :: fwd, bwd
      type(calypso_rocFFT_work), intent(inout), target :: WK_fft
!
      fwd%Ncomp =  Ncomp_fwd
      fwd%Nfft =   Nfft
      bwd%Ncomp =  Ncomp_bwd
      bwd%Nfft =   Nfft
      WK_fft%aNfft =  one / dble(Nfft)
      WK_fft%Nfft_c = Nfft / 2 + 1
      WK_fft%Nfft_r = 2 * WK_fft%Nfft_c
!
      fwd%Nbytes = WK_fft%Nfft_r * fwd%Ncomp * kreal
      bwd%Nbytes = WK_fft%Nfft_r * bwd%Ncomp * kreal
!
      end subroutine calypso_rocFFT_set_size
!
! ------------------------------------------------------------------
#ifdef _AMD_ROCM_
! ----------------------------------------------------------------------
!
      subroutine calypso_rocFFT_alloc(fwd, bwd, WK_fft)
!
      use hipfort
      use hipfort_check
!
      type(calypso_rocFFT_params), intent(in) :: fwd, bwd
      type(calypso_rocFFT_work), intent(inout), target :: WK_fft
!
      integer(c_size_t) :: max_size
!
!
      max_size = max(fwd%Ncomp, bwd%Ncomp)
      allocate(WK_fft%X_rocFFT(max_size*WK_fft%Nfft_r))
      allocate(WK_fft%C_rocFFT(max_size*WK_fft%Nfft_c))
!$omp parallel workshare
      WK_fft%X_rocFFT(1:max_size*WK_fft%Nfft_r) = 0.0d0
!$omp end parallel workshare
!$omp parallel workshare
      WK_fft%C_rocFFT(1:max_size*WK_fft%Nfft_c) = 0.0d0
!$omp end parallel workshare
!
      max_size = max(fwd%Nbytes, bwd%Nbytes)
      call hipCheck(hipMalloc(WK_fft%data_ptr, max_size))
!
      end subroutine calypso_rocFFT_alloc
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine calypso_fwd_rocFFT_init(fwd)
!
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      type(calypso_rocFFT_params), intent(inout), target :: fwd
!
!
      call rocfftCheck                                                  &
     &   (rocfft_plan_description_create(fwd%rocFFT_description))
      call rocfftCheck(rocfft_plan_description_set_data_layout          &
     &                                      (fwd%rocFFT_description,    &
     &                                       rocfft_array_type_real,    &
     &                                      rocfft_array_type_unset,    &
     &                                               fwd%in_offsets,    &
     &                                              fwd%out_offsets,    &
     &                                          fwd%in_strides_size,    &
     &                                     c_loc(fwd%in_strides(1)),    &
     &                                              fwd%in_distance,    &
     &                                         fwd%out_strides_size,    &
     &                                    c_loc(fwd%out_strides(1)),    &
     &                                            fwd%out_distance))
      call rocfftCheck(rocfft_plan_create(fwd%rocFFT_plan,              &
     &                                    rocfft_placement_inplace,     &
     &                          rocfft_transform_type_real_forward,     &
     &                                     rocfft_precision_double,     &
     &                                     ione_c, c_loc(fwd%Nfft),     &
     &                           fwd%Ncomp, fwd%rocFFT_description))
!
      call rocfftCheck                                                  &
     &   (rocfft_plan_get_work_buffer_size(fwd%rocFFT_plan,             &
     &                                     fwd%rocFFT_wk_buf_size))
      call rocfftCheck                                                  &
     &   (rocfft_execution_info_create(fwd%rocFFT_wk_info))
      if(fwd%rocFFT_wk_buf_size > 0) then
        call hipCheck(hipMalloc(fwd%rocFFT_wk_buffer,                   &
     &                          fwd%rocFFT_wk_buf_size))
        call rocfftCheck(rocfft_execution_info_set_work_buffer          &
     &                                        (fwd%rocFFT_wk_info,      &
     &                                         fwd%rocFFT_wk_buffer,    &
     &                                         fwd%rocFFT_wk_buf_size))
      end if
!
      end subroutine calypso_fwd_rocFFT_init
!
! ------------------------------------------------------------------
!
      subroutine calypso_bwd_rocFFT_init(bwd)
!
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      type(calypso_rocFFT_params), intent(inout), target :: bwd
!
!
      call rocfftCheck                                                  &
     &   (rocfft_plan_description_create(bwd%rocFFT_description))
      call rocfftCheck(rocfft_plan_description_set_data_layout          &
     &                                      (bwd%rocFFT_description,    &
     &                                      rocfft_array_type_unset,    &
     &                                       rocfft_array_type_real,    &
     &                                               bwd%in_offsets,    &
     &                                              bwd%out_offsets,    &
     &                                          bwd%in_strides_size,    &
     &                                     c_loc(bwd%in_strides(1)),    &
     &                                              bwd%in_distance,    &
     &                                         bwd%out_strides_size,    &
     &                                    c_loc(bwd%out_strides(1)),    &
     &                                            bwd%out_distance))
!
      call rocfftCheck(rocfft_plan_create(bwd%rocFFT_plan,              &
                                          rocfft_placement_inplace,     &
                                  rocfft_transform_type_real_inverse,   &
                                          rocfft_precision_double,      &
                                          ione_c, c_loc(bwd%Nfft),      &
                                  bwd%Ncomp, bwd%rocFFT_description))
!
      call rocfftCheck                                                  &
     &   (rocfft_plan_get_work_buffer_size(bwd%rocFFT_plan,             &
     &                                     bwd%rocFFT_wk_buf_size))
      call rocfftCheck                                                  &
     &   (rocfft_execution_info_create(bwd%rocFFT_wk_info))
      if(bwd%rocFFT_wk_buf_size > 0) then
        call hipCheck(hipMalloc(bwd%rocFFT_wk_buffer,                   &
     &                          bwd%rocFFT_wk_buf_size))
        call rocfftCheck(rocfft_execution_info_set_work_buffer          &
     &                                       (bwd%rocFFT_wk_info,       &
     &                                        bwd%rocFFT_wk_buffer,     &
     &                                        bwd%rocFFT_wk_buf_size))
      end if
!
      end subroutine calypso_bwd_rocFFT_init
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine calypso_rocFFT_fin(fwd, bwd, WK_fft)
!
      type(calypso_rocFFT_params), intent(inout), target :: fwd
      type(calypso_rocFFT_params), intent(inout), target :: bwd
      type(calypso_rocFFT_work), intent(inout), target :: WK_fft
!
      if(fwd%Ncomp .gt. 0) call calypso_each_rocFFT_fin(fwd)
      if(bwd%Ncomp .gt. 0) call calypso_each_rocFFT_fin(bwd)
!
      call hipCheck(hipFree(WK_fft%data_ptr))
      deallocate(WK_fft%C_rocFFT, WK_fft%X_rocFFT)
!
      end subroutine calypso_rocFFT_fin
!
! ------------------------------------------------------------------
!
      subroutine calypso_each_rocFFT_fin(trns)
!
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      type(calypso_rocFFT_params), intent(inout), target :: trns
!
      call rocfftCheck                                                  &
     &   (rocfft_execution_info_destroy(trns%rocFFT_wk_info))
      if(trns%rocFFT_wk_buf_size > 0) then
        call hipCheck(hipFree(trns%rocFFT_wk_buffer))
      end if
      call rocfftCheck                                                  &
     &   (rocfft_plan_description_destroy(trns%rocFFT_description))
      call rocfftCheck(rocfft_plan_destroy(trns%rocFFT_plan))
!
      end subroutine calypso_each_rocFFT_fin
!
! ------------------------------------------------------------------
#endif
! ------------------------------------------------------------------
!
      end module t_multi_rocFFT_wrapper
