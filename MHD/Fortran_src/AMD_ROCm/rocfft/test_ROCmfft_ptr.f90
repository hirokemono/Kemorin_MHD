!
      program test_ROCmfft_ptr
!
      use iso_c_binding
      use omp_lib
!
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use m_FFT_size
      use t_fft_test_data
      use normalize_for_OMP_FFTW
!
      implicit none
!
      character(len=kchara) :: file_name = 'prt_ROCmfft_test.dat'
      real(kind = kreal) :: start, finish, elapsed(4)
!
      type(fft_test_data) :: ft1
!
      real(kind = kreal), parameter :: aNfft = one / ngrid
      integer(kind = kint), parameter :: Nfft_c = ngrid/2 + 1
      integer(kind = kint), parameter :: Nfft_r = 2*Nfft_c
      complex(kind = kreal), allocatable, target :: x_cplx(:,:)
      real(kind = kreal), allocatable, target :: x_real(:,:)
      complex(kind = kreal), allocatable, target :: y_cplx(:,:)
      real(kind = kreal), allocatable, target :: y_real(:,:)
      integer(c_size_t) :: Nbytes = Nfft_r * n_field * kreal
      integer(c_size_t), parameter :: ione_c =  ione
      type(c_ptr) :: dx = c_null_ptr
      integer(c_size_t), allocatable, target :: l_real(:)
!
      type(c_ptr) :: plan_fwd = c_null_ptr
      type(c_ptr) :: dx_fwd = c_null_ptr
      type(c_ptr) :: descriptor_fwd = c_null_ptr
      integer(c_size_t) :: fwd_wk_buf_size = 0
      type(c_ptr) :: fwd_wk_info =   c_null_ptr
      type(c_ptr) :: fwd_wk_buffer = c_null_ptr
!
      type(c_ptr) :: plan_bwd = c_null_ptr
      type(c_ptr) :: dx_bwd = c_null_ptr
      type(c_ptr) :: descriptor_bwd = c_null_ptr
      integer(c_size_t) :: bwd_wk_buf_size = 0
      type(c_ptr) :: bwd_wk_info =   c_null_ptr
      type(c_ptr) :: bwd_wk_buffer = c_null_ptr
!
      type kemo_ROCmfft_params
        integer(c_size_t) :: ROCfft_wk_buf_size = 0
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
      end type kemo_ROCmfft_params
!
      type(kemo_ROCmfft_params), target :: fwd
      type(kemo_ROCmfft_params), target :: bwd
!
      integer(kind = kint) :: i, nd, icou
!
      np_smp = omp_get_max_threads()
      write(*,*) 'Number of threads:  ', np_smp
      call init_fft_test_data(n_field, ngrid, ft1)
      call swap_fft_test_input_to_pin(ft1)
!
      allocate(x_real(Nfft_r,n_field))
      allocate(y_real(Nfft_r,n_field))
!$omp parallel workshare
      x_real(1:Nfft_r,1:n_field) = 0.0d0
      y_real(1:Nfft_r,1:n_field) = 0.0d0
!$omp end parallel workshare
!
      allocate(x_cplx(Nfft_c,n_field))
      allocate(y_cplx(Nfft_c,n_field))
!$omp parallel workshare
      x_cplx(1:Nfft_c,1:n_field) = 0.0d0
      y_cplx(1:Nfft_c,1:n_field) = 0.0d0
!$omp end parallel workshare
!
      allocate(l_real(2))
      l_real(1) = ngrid
      l_real(2) = ft1%nfld
!
!   Initialize Forward transform
      start = OMP_GET_WTIME()
      fwd%in_strides_size =   1
      fwd%in_strides(1) =     1
      fwd%in_distance =       Nfft_r
      fwd%out_strides_size =  0
      fwd%out_distance =      0
!
      call hipCheck(hipMalloc(dx_fwd, Nbytes))
      call rocfftCheck(rocfft_plan_description_create(descriptor_fwd))
      call rocfftCheck(rocfft_plan_description_set_data_layout          &
     &                                      (descriptor_fwd,            &
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
      call rocfftCheck(rocfft_plan_create(plan_fwd,                     &
                                          rocfft_placement_inplace,     &
                                  rocfft_transform_type_real_forward,   &
                                          rocfft_precision_double,      &
                                          ione_c, c_loc(l_real(1)),     &
                                  l_real(2), descriptor_fwd))
!
      call rocfftCheck(rocfft_plan_get_work_buffer_size(plan_fwd,       &
     &                                              fwd_wk_buf_size))
      write(*,*) 'fwd_wk_buf_size', fwd_wk_buf_size
      call rocfftCheck(rocfft_execution_info_create(fwd_wk_info))
      if(fwd_wk_buf_size > 0) then
        call hipCheck(hipMalloc(fwd_wk_buffer, fwd_wk_buf_size))
        call rocfftCheck(rocfft_execution_info_set_work_buffer          &
     &                                                 (fwd_wk_info,    &
     &                                                  fwd_wk_buffer,  &
     &                                              fwd_wk_buf_size))
      end if
!
!   Initialize Backword transform
      bwd%in_strides_size =  0
      bwd%in_distance =      0
      bwd%out_strides_size = 1
      bwd%out_strides(1) =   1
      bwd%out_distance =     Nfft_r
!
      call hipCheck(hipMalloc(dx_bwd, Nbytes))
      call rocfftCheck(rocfft_plan_description_create(descriptor_bwd))
      call rocfftCheck(rocfft_plan_description_set_data_layout          &
     &                                      (descriptor_bwd,            &
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
      call rocfftCheck(rocfft_plan_create(plan_bwd,                     &
                                          rocfft_placement_inplace,     &
                                  rocfft_transform_type_real_inverse,   &
                                          rocfft_precision_double,      &
                                          ione_c, c_loc(l_real(1)),     &
                                  l_real(2), descriptor_bwd))
!
      call rocfftCheck(rocfft_plan_get_work_buffer_size(plan_bwd,       &
     &                                       bwd%ROCfft_wk_buf_size))
      write(*,*) 'bwd%ROCfft_wk_buf_size', bwd%ROCfft_wk_buf_size
      call rocfftCheck(rocfft_execution_info_create(bwd_wk_info))
      if(bwd%ROCfft_wk_buf_size > 0) then
        call hipCheck(hipMalloc(bwd_wk_buffer,                          &
     &                          bwd%ROCfft_wk_buf_size))
        call rocfftCheck(rocfft_execution_info_set_work_buffer          &
     &                                                 (bwd_wk_info,    &
     &                                                  bwd_wk_buffer,  &
     &                                        bwd%ROCfft_wk_buf_size))
      end if
      elapsed(3) = OMP_GET_WTIME() - start
!
      elapsed(1:2) = zero
      elapsed(4) = zero
      do icou = 1, n_loop
        if(mod(icou, 20) .eq. 0) write(*,*) 'loop count: ', icou
!
        start = OMP_GET_WTIME()
!$omp parallel workshare
        ft1%s_k(1:ft1%ngrd,1:ft1%nfld) = ft1%org(1:ft1%ngrd,1:ft1%nfld)
!$omp end parallel workshare
!
!   Forward transform
!$omp parallel do private(nd,i)
        do nd = 1, ft1%nfld
          do i = 1, ft1%ngrd
            x_real(i,nd) = ft1%s_k(i,nd)
          end do
          do i = ft1%ngrd+1, Nfft_r
            x_real(i,nd) = zero
          end do
        end do
!$omp end parallel do
        elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call hipCheck(hipMemcpy(dx_fwd, c_loc(x_real(1,1)), Nbytes,     &
     &                          hipMemcpyHostToDevice))
        call rocfftCheck(rocfft_execute(plan_fwd, dx_fwd,               &
     &                                  c_null_ptr, fwd_wk_info))
        call hipCheck(hipDeviceSynchronize())
        call hipCheck(hipMemcpy(c_loc(x_cplx(1,1)), dx_fwd, Nbytes,     &
     &                          hipMemcpyDeviceToHost))
        elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call norm_prt_from_fwd_OMP_FFTW                                 &
     &     (ft1%nfld, aNfft, NFFT_c, x_cplx, ft1%ngrd, ft1%s_k(1,1))
        elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
!
        start = OMP_GET_WTIME()
!$omp parallel do private(nd,i)
        do nd = 1, ft1%nfld
          do i = 1, ft1%ngrd
            ft1%f_x(i,nd) = ft1%s_k(i,nd)
          end do
        end do
!$omp end parallel do
        elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
!
!   Backword transform
        start = OMP_GET_WTIME()
        call norm_prt_to_bwd_OMP_FFTW(ft1%nfld, ft1%ngrd, ft1%f_x(1,1), &
     &                                NFFT_c, y_cplx(1,1))
        elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call hipCheck(hipMemcpy(dx_bwd, c_loc(y_cplx(1,1)), Nbytes,     &
     &                          hipMemcpyHostToDevice))
        call rocfftCheck(rocfft_execute(plan_bwd, dx_bwd,               &
     &                                  c_null_ptr, bwd_wk_info))
        call hipCheck(hipDeviceSynchronize())
        call hipCheck(hipMemcpy(c_loc(y_real(1,1)), dx_bwd, Nbytes,     &
     &                          hipMemcpyDeviceToHost))
        elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
!$omp parallel do private(nd,i)
        do nd = 1, ft1%nfld
          do i = 1, ft1%ngrd
            ft1%f_x(i,nd) = y_real(i,nd)
          end do
        end do
!$omp end parallel do
        elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
      end do
!
!   Finalize
      start = OMP_GET_WTIME()
      call rocfftCheck(rocfft_execution_info_destroy(fwd_wk_info))
      call rocfftCheck(rocfft_execution_info_destroy(bwd_wk_info))
      if(fwd_wk_buf_size > 0) then
        call hipCheck(hipFree(fwd_wk_buffer))
      end if
      if(bwd%ROCfft_wk_buf_size > 0) then
        call hipCheck(hipFree(bwd_wk_buffer))
      end if
      call rocfftCheck(rocfft_plan_description_destroy(descriptor_bwd))
      call rocfftCheck(rocfft_plan_description_destroy(descriptor_fwd))
      call rocfftCheck(rocfft_plan_destroy(plan_bwd))
      call rocfftCheck(rocfft_plan_destroy(plan_fwd))
      call hipCheck(hipFree(dx_bwd))
      call hipCheck(hipFree(dx_fwd))
      deallocate(x_cplx, x_real)
      elapsed(3) = elapsed(3) + OMP_GET_WTIME() - start
!
  10  continue
      if(n_loop .eq. 1) then
        call swap_fft_test_data_to_pout(ft1)
        call write_fft_test_data(file_name, ft1)
      end if
      call dealloc_fft_test_data(ft1)
!
      write(*, '("Time for ROCmfft:    ",1pE16.6e3)') elapsed(1)
      write(*, '("Time for Initialize: ",1pE16.6e3)') elapsed(3)
      write(*, '("Time for Data copy:  ",1pE16.6e3)') elapsed(2)
!
      stop 'finish'
      end program test_ROCmfft_ptr
