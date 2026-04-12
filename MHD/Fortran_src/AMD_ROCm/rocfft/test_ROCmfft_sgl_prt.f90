!
      program test_ROCmfft_single_rtp
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
      character(len = kchara) :: ROCfft_test = 'sgl_prt_ROCmfft_test.dat'
!
      real(kind = kreal) :: start, finish, elapsed(3)
!
      type(fft_test_data) :: ft1
!
      integer(kind = kint), parameter :: Nfft_c = ngrid/2 + 1
      integer(kind = kint), parameter :: Nfft =   2*Nfft_c
      real(kind = kreal), allocatable, target :: x_real(:)
      real(kind = kreal), allocatable, target :: y_real(:)
      complex(kind = kreal), allocatable, target :: x_cplx(:)
      complex(kind = kreal), allocatable, target :: y_cplx(:)
      integer(c_size_t) :: Nbytes = Nfft*kreal
      integer(c_size_t), parameter :: ione_c = ione
!
      type(c_ptr) :: dx = c_null_ptr
      type(c_ptr) :: plan_fwd = c_null_ptr
      type(c_ptr) :: plan_bwd = c_null_ptr
      integer(c_size_t), allocatable, target :: l_real(:)
!
      integer(kind = kint) :: i, nd
      integer(kind = kint) :: icou
!
      np_smp = omp_get_max_threads()
      write(*,*) 'Number of threads:  ', np_smp
      call init_fft_test_data(n_field, ngrid, ft1)
      call swap_fft_test_input_to_pin(ft1)
!
      allocate(x_real(Nfft))
      allocate(y_real(Nfft))
      x_real = 0.0d0
      y_real = 0.0d0
!
      allocate(x_cplx(Nfft_c))
      allocate(y_cplx(Nfft_c))
      x_cplx = (0.0d0, 0.0d0)
      y_cplx = (0.0d0, 0.0d0)
!
      allocate(l_real(1))
      l_real(1) = ngrid
!
      start = OMP_GET_WTIME()
      call hipCheck(hipMalloc(dx,Nbytes))
      call rocfftCheck(rocfft_plan_create(plan_fwd,                     &
                                          rocfft_placement_inplace,     &
                                  rocfft_transform_type_real_forward,   &
                                          rocfft_precision_double,      &
                                          ione_c, c_loc(l_real(1)),     &
                                          ione_c, c_null_ptr))
      call rocfftCheck(rocfft_plan_create(plan_bwd,                     &
                                          rocfft_placement_inplace,     &
                                  rocfft_transform_type_real_inverse,   &
                                          rocfft_precision_double,      &
                                          ione_c, c_loc(l_real(1)),     &
                                          ione_c, c_null_ptr))
      elapsed(3) = OMP_GET_WTIME() - start
!
      elapsed(1:2) = 0.0d0
      do icou = 1, n_loop
        if(mod(icou, 20) .eq. 0) write(*,*) 'loop count: ', icou
!
        start = OMP_GET_WTIME()
!$omp parallel workshare
        ft1%s_k(1:ft1%ngrd,1:ft1%nfld) = ft1%org(1:ft1%ngrd,1:ft1%nfld)
!$omp end parallel workshare
        elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
!   Forward transform
        do nd = 1, ft1%nfld
          start = OMP_GET_WTIME()
!$omp parallel workshare
          x_real(1:Nfft) = 0.0d0
!$omp end parallel workshare
!$omp parallel workshare
          x_real(1:ft1%ngrd) = ft1%s_k(1:ft1%ngrd,nd)
!$omp end parallel workshare
          elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start

          start = OMP_GET_WTIME()
          call hipCheck(hipMemcpy(dx,c_loc(x_real(1)),Nbytes,           &
     &                            hipMemcpyHostToDevice))
          call rocfftCheck(rocfft_execute(plan_fwd, dx,                 &
     &                                  c_null_ptr, c_null_ptr))
          call hipCheck(hipDeviceSynchronize())
          call hipCheck(hipMemcpy(c_loc(x_cplx(1)),dx,Nbytes,           &
     &                            hipMemcpyDeviceToHost))
          elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
          start = OMP_GET_WTIME()
!$omp parallel workshare
          x_cplx(1:Nfft_c) = x_cplx(1:Nfft_c) / dble(ft1%ngrd)
!$omp end parallel workshare
!
          ft1%s_k(1,nd) = real(x_cplx(1)   )
          ft1%s_k(2,nd) = real(x_cplx(Nfft_c))
!$omp parallel do
          do i = 2, Nfft_c-1
            ft1%s_k(2*i-1,nd) =  two * real(x_cplx(i))
            ft1%s_k(2*i,  nd) = -two * imag(x_cplx(i))
          end do
!$omp end parallel do
          elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
        end do
!
        start = OMP_GET_WTIME()
!$omp parallel do
        do nd = 1, ft1%nfld
          ft1%f_x(1:ft1%ngrd,nd) = ft1%s_k(1:ft1%ngrd,nd)
        end do
!$omp end parallel do
        elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
!   Backword transform
        do nd = 1, ft1%nfld
          start = OMP_GET_WTIME()
          y_cplx(1) = cmplx(ft1%f_x(1,nd), 0.0d0, kind(0d0))
!$omp parallel do
          do i = 2, Nfft_c-1
            y_cplx(i) = half * cmplx(ft1%f_x(2*i-1,nd),                 &
     &                              -ft1%f_x(2*i,  nd), kind(0d0))
          end do
!$omp end parallel do
          y_cplx(Nfft_c) = cmplx(ft1%f_x(2,nd), 0.0d0, kind(0d0))
          elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
          start = OMP_GET_WTIME()
          call hipCheck(hipMemcpy(dx,c_loc(y_cplx(1)),Nbytes,           &
     &                            hipMemcpyHostToDevice))
          call rocfftCheck(rocfft_execute(plan_bwd, dx,                 &
     &                                    c_null_ptr, c_null_ptr))
          call hipCheck(hipDeviceSynchronize())
          call hipCheck(hipMemcpy(c_loc(y_real(1)),dx,Nbytes,           &
     &                            hipMemcpyDeviceToHost))
          elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
          start = OMP_GET_WTIME()
!$omp parallel workshare
          ft1%f_x(1:ft1%ngrd,nd) = y_real(1:ft1%ngrd)
!$omp end parallel workshare
          elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
        end do
      end do
!
      start = OMP_GET_WTIME()
      call rocfftCheck(rocfft_plan_destroy(plan_fwd))
      call rocfftCheck(rocfft_plan_destroy(plan_bwd))
      call hipCheck(hipFree(dx))
      elapsed(3) = elapsed(3) + OMP_GET_WTIME() - start
!
   10 continue
      if(n_loop .eq. 1) then
        call swap_fft_test_data_to_pout(ft1)
        call write_fft_test_data(ROCfft_test, ft1)
      end if
      call dealloc_fft_test_data(ft1)
!
      write(*, '("Time for ROCmfft:    ",1pE16.6e3)') elapsed(1)
      write(*, '("Time for Initialize: ",1pE16.6e3)') elapsed(3)
      write(*, '("Time for Data copy:  ",1pE16.6e3)') elapsed(2)
!
      stop 'finish'
      end program test_ROCmfft_single_rtp
