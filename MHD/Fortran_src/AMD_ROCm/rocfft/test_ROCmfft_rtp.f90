!
      program test_FFTPACK5
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
!
      implicit none
!
      character(len=kchara) :: file_name = 'rtp_ROCmfft_test.dat'
      real(kind = kreal) :: start, finish, elapsed(3)
!
      type(fft_test_data) :: ft1
!
      integer(kind = kint), parameter :: Nfft_c = ngrid/2 + 1
      integer(kind = kint), parameter :: Nfft_r = 2*Nfft_c
      real(kind = kreal), allocatable, target :: x_real(:,:)
      real(kind = kreal), allocatable, target :: y_real(:,:)
      complex(kind = kreal), allocatable, target :: x_cplx(:,:)
      complex(kind = kreal), allocatable, target :: y_cplx(:,:)
!
      integer(c_size_t) :: Nbytes = Nfft_r * n_field * kreal
      integer(c_size_t), parameter :: ione_c =  ione
      type(c_ptr) :: dx = c_null_ptr
      integer(c_size_t), allocatable, target :: l_real(:)
!
      type(c_ptr) :: plan_fwd = c_null_ptr
      type(c_ptr) :: descriptor_fwd = c_null_ptr
      type(c_ptr) :: in_offsets_fwd =  c_null_ptr
      type(c_ptr) :: out_offsets_fwd = c_null_ptr
      integer(c_size_t), target :: in_strides_size_fwd =  ione
      integer(c_size_t), target :: in_strides_fwd(1) =  (/n_field/)
      integer(c_size_t), target :: in_distance_fwd = ione
      integer(c_size_t), target :: out_strides_size_fwd = ione
      integer(c_size_t), target :: out_strides_fwd(1) = (/n_field/)
      integer(c_size_t), target :: out_distance_fwd = ione
!
      integer(c_size_t) :: fwd_wk_buf_size = 0
      type(c_ptr) :: fwd_wk_info =   c_null_ptr
      type(c_ptr) :: fwd_wk_buffer = c_null_ptr
!
      type(c_ptr) :: plan_bwd = c_null_ptr
      type(c_ptr) :: descriptor_bwd = c_null_ptr
      type(c_ptr) :: in_offsets_bwd =  c_null_ptr
      type(c_ptr) :: out_offsets_bwd = c_null_ptr
      integer(c_size_t), target :: in_strides_size_bwd =  ione
      integer(c_size_t), target :: in_strides_bwd(1) =  (/n_field/)
      integer(c_size_t), target :: in_distance_bwd = ione
      integer(c_size_t), target :: out_strides_size_bwd = ione
      integer(c_size_t), target :: out_strides_bwd(1) = (/n_field/)
      integer(c_size_t), target :: out_distance_bwd = ione
!
      integer(c_size_t) :: bwd_wk_buf_size = 0
      type(c_ptr) :: bwd_wk_info =   c_null_ptr
      type(c_ptr) :: bwd_wk_buffer = c_null_ptr
!
      integer(kind = kint) :: i, nd
      integer(kind = kint) :: icou
!
!
      np_smp = omp_get_max_threads()
      write(*,*) 'Number of threads:  ', np_smp
      call init_fft_test_data(n_field, ngrid, ft1)
!
      allocate(x_real(n_field,Nfft_r))
      allocate(y_real(n_field,Nfft_r))
!$omp parallel workshare
      x_real(1:n_field,1:Nfft_r) = 0.0d0
      y_real(1:n_field,1:Nfft_r) = 0.0d0
!$omp end parallel workshare
!
      allocate(x_cplx(n_field,Nfft_c))
      allocate(y_cplx(n_field,Nfft_c))
!$omp parallel workshare
      x_cplx(1:n_field,1:Nfft_c) = 0.0d0
      y_cplx(1:n_field,1:Nfft_c) = 0.0d0
!$omp end parallel workshare
!
!
      allocate(l_real(3))
      l_real(1) = ngrid
      l_real(2) = ft1%nfld
!
!   Initialize Forward transform
      start = OMP_GET_WTIME()
      call hipCheck(hipMalloc(dx,Nbytes))
!
      call rocfftCheck(rocfft_plan_description_create(descriptor_fwd))
      call rocfftCheck(rocfft_plan_description_set_data_layout          &
     &                                      (descriptor_fwd,            &
     &                                       rocfft_array_type_real,    &
     &                                      rocfft_array_type_unset,    &
     &                                               in_offsets_fwd,    &
     &                                              out_offsets_fwd,    &
     &                                          in_strides_size_fwd,    &
     &                                        c_loc(in_strides_fwd),    &
     &                                              in_distance_fwd,    &
     &                                         out_strides_size_fwd,    &
     &                                       c_loc(out_strides_fwd),    &
     &                                            out_distance_fwd))
      call rocfftCheck(rocfft_plan_create(plan_fwd,                     &
     &                                    rocfft_placement_inplace,     &
     &                          rocfft_transform_type_real_forward,     &
     &                                    rocfft_precision_double,      &
     &                                    ione_c, c_loc(l_real(1)),     &
     &                              l_real(2), descriptor_fwd))
!
      call rocfftCheck(rocfft_plan_get_work_buffer_size(plan_fwd,       &
     &                                             fwd_wk_buf_size))
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
      call rocfftCheck(rocfft_plan_description_create(descriptor_bwd))
      call rocfftCheck(rocfft_plan_description_set_data_layout          &
     &                                      (descriptor_bwd,            &
     &                                      rocfft_array_type_unset,    &
     &                                       rocfft_array_type_real,    &
     &                                               in_offsets_bwd,    &
     &                                              out_offsets_bwd,    &
     &                                          in_strides_size_bwd,    &
     &                                        c_loc(in_strides_bwd),    &
     &                                              in_distance_bwd,    &
     &                                         out_strides_size_bwd,    &
     &                                       c_loc(out_strides_bwd),    &
!     &  c_null_ptr, &
     &                                            out_distance_bwd))
!
      call rocfftCheck(rocfft_plan_create(plan_bwd,                     &
     &                                    rocfft_placement_inplace,     &
     &                            rocfft_transform_type_real_inverse,   &
     &                                    rocfft_precision_double,      &
     &                                    ione_c, c_loc(l_real(1)),     &
     &                        l_real(2), descriptor_bwd))
!
      call rocfftCheck(rocfft_plan_get_work_buffer_size(plan_bwd,       &
     &                                              bwd_wk_buf_size))
      write(*,*) 'bwd_wk_buf_size', bwd_wk_buf_size
      call rocfftCheck(rocfft_execution_info_create(bwd_wk_info))
      if(bwd_wk_buf_size > 0) then
        call hipCheck(hipMalloc(bwd_wk_buffer, bwd_wk_buf_size))
        call rocfftCheck(rocfft_execution_info_set_work_buffer          &
     &                                                 (bwd_wk_info,    &
     &                                                  bwd_wk_buffer,  &
     &                                              bwd_wk_buf_size))
      end if
      elapsed(3) = OMP_GET_WTIME() - start
!
      elapsed(1:2) = zero
      do icou = 1, n_loop
        if(mod(icou, 20) .eq. 0) write(*,*) 'loop count: ', icou
!
        start = OMP_GET_WTIME()
!$omp parallel workshare
        ft1%s_k(1:ft1%nfld,1:ft1%ngrd) = ft1%org(1:ft1%nfld,1:ft1%ngrd)
!$omp end parallel workshare
!
!   Forward transform
!$omp parallel do
        do i = 1, ft1%ngrd
          x_real(1:n_field,i) = ft1%s_k(1:n_field,i)
        end do
!$omp end parallel do
!$omp parallel do
        do i = ft1%ngrd+1, Nfft_r
          x_real(1:n_field,i) = 0.0d0
        end do
!$omp end parallel do
        elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call hipCheck(hipMemcpy(dx, c_loc(x_real(1,1)), Nbytes,         &
     &                          hipMemcpyHostToDevice))
        call rocfftCheck(rocfft_execute(plan_fwd, dx, c_null_ptr,       &
     &                                  fwd_wk_info))
        call hipCheck(hipDeviceSynchronize())
        call hipCheck(hipMemcpy(c_loc(x_cplx(1,1)), dx, Nbytes,         &
     &                          hipMemcpyDeviceToHost))
        elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
!$omp parallel workshare
        x_cplx(1:ft1%nfld,1:Nfft_c)                                     &
     &       = x_cplx(1:ft1%nfld,1:Nfft_c) / dble(ft1%ngrd)
!$omp end parallel workshare

!$omp parallel workshare
        ft1%s_k(1:ft1%nfld,1) = real(x_cplx(1:ft1%nfld,1))
        ft1%s_k(1:ft1%nfld,2) = real(x_cplx(1:ft1%nfld,Nfft_c))
!$omp end parallel workshare
!$omp parallel do
        do i = 2, Nfft_c-1
          ft1%s_k(1:ft1%nfld,2*i-1) =  two * real(x_cplx(1:ft1%nfld,i))
          ft1%s_k(1:ft1%nfld,2*i  ) = -two * imag(x_cplx(1:ft1%nfld,i))
        end do
!$omp end parallel do
!$omp parallel workshare
        ft1%f_x(1:ft1%nfld,1:ft1%ngrd) = ft1%s_k(1:ft1%nfld,1:ft1%ngrd)
!$omp end parallel workshare
        elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
!   Backword transform
!
        start = OMP_GET_WTIME()
!$omp parallel workshare
        y_cplx(1:ft1%nfld,1) =      (ft1%f_x(1:ft1%nfld,1), zero)
        y_cplx(1:ft1%nfld,Nfft_c) = (ft1%f_x(1:ft1%nfld,2), zero)
!$omp end parallel workshare
!$omp parallel do
        do i = 2, Nfft_c-1
          y_cplx(1:ft1%nfld,i) = (half * ft1%f_x(1:ft1%nfld,2*i-1),     &
     &                           -half * ft1%f_x(1:ft1%nfld,2*i  ))
        end do
!$omp end parallel do
        elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call hipCheck(hipMemcpy(dx, c_loc(y_cplx(1,1)), Nbytes,         &
     &                          hipMemcpyHostToDevice))
        call rocfftCheck(rocfft_execute(plan_bwd, dx, c_null_ptr,       &
     &                                  bwd_wk_info))
        call hipCheck(hipDeviceSynchronize())
        call hipCheck(hipMemcpy(c_loc(y_real(1,1)), dx, Nbytes,         &
     &                          hipMemcpyDeviceToHost))
        elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
!$omp parallel do private(i)
        do i = 1, ft1%ngrd
          ft1%f_x(1:ft1%nfld,i) = y_real(1:ft1%nfld,i)
        end do
!$omp end parallel do
        elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
      end do
!
!   Finalize
      start = OMP_GET_WTIME()
      call rocfftCheck(rocfft_execution_info_destroy(fwd_wk_info))
      call rocfftCheck(rocfft_execution_info_destroy(bwd_wk_info))
      if(fwd_wk_buf_size > 0) call hipCheck(hipFree(fwd_wk_buffer))
      if(bwd_wk_buf_size > 0) call hipCheck(hipFree(bwd_wk_buffer))
      call rocfftCheck(rocfft_plan_description_destroy(descriptor_bwd))
      call rocfftCheck(rocfft_plan_description_destroy(descriptor_fwd))
      call rocfftCheck(rocfft_plan_destroy(plan_bwd))
      call rocfftCheck(rocfft_plan_destroy(plan_fwd))
      call hipCheck(hipFree(dx))
      elapsed(3) = elapsed(3) + OMP_GET_WTIME() - start
!
  10  continue
      if(n_loop .eq. 1) call write_fft_test_data(file_name, ft1)
      call dealloc_fft_test_data(ft1)
!
      write(*, '("Time for ROCmfft:    ",1pE16.6e3)') elapsed(1)
      write(*, '("Time for Initialize: ",1pE16.6e3)') elapsed(3)
      write(*, '("Time for Data copy:  ",1pE16.6e3)') elapsed(2)
!
      stop 'finish'
      end program test_FFTPACK5
