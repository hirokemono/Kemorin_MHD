!
      program test_ROCmfft_single_prt
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
      use t_single_ROCmFFT_wrapper
!
      implicit none
!
      character(len = kchara) :: ROCfft_test = 'sgl_prt_ROCmfft_test.dat'
!
      real(kind = kreal) :: start, finish, elapsed(3)
!
      type(fft_test_data) :: ft1
!
      type(calypso_ROCmfft_params), target :: fwd
      type(calypso_ROCmfft_params), target :: bwd
      type(calypso_ROCmfft_work), target :: WK_fwd
      type(calypso_ROCmfft_work), target :: WK_bwd
!
      integer(c_size_t), parameter :: ione_c = ione
!
      integer(kind = kint) :: i, nd
      integer(kind = kint) :: icou
!
      np_smp = omp_get_max_threads()
      write(*,*) 'Number of threads:  ', np_smp
      call init_fft_test_data(n_field, ngrid, ft1)
      call swap_fft_test_input_to_pin(ft1)
!
      call calypso_sgl_ROCmFFT_set_size(ngrid, fwd, WK_fwd)
      call calypso_sgl_fwd_ROCmFFT_alloc(fwd, WK_fwd)
!
      call calypso_sgl_ROCmFFT_set_size(ngrid, bwd, WK_bwd)
      call calypso_sgl_fwd_ROCmFFT_alloc(bwd, WK_bwd)
!
      start = OMP_GET_WTIME()
      call rocfftCheck(rocfft_plan_create(fwd%ROCfft_plan,              &
                                          rocfft_placement_inplace,     &
                                  rocfft_transform_type_real_forward,   &
                                          rocfft_precision_double,      &
                                          ione_c, c_loc(fwd%Nfft),      &
                                          ione_c, c_null_ptr))
!
      call rocfftCheck(rocfft_plan_create(bwd%ROCfft_plan,              &
                                          rocfft_placement_inplace,     &
                                  rocfft_transform_type_real_inverse,   &
                                          rocfft_precision_double,      &
                                          ione_c, c_loc(bwd%Nfft),      &
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
          WK_fwd%X_ROCmFFT(1:WK_fwd%Nfft_r) = 0.0d0
!$omp end parallel workshare
!$omp parallel workshare
          WK_fwd%X_ROCmFFT(1:ft1%ngrd) = ft1%s_k(1:ft1%ngrd,nd)
!$omp end parallel workshare
          elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start

          start = OMP_GET_WTIME()
          call hipCheck                                                 &
     &       (hipMemcpy(WK_fwd%data_ptr, c_loc(WK_fwd%X_ROCmFFT(1)),    &
     &                  WK_fwd%Nbytes, hipMemcpyHostToDevice))
          call rocfftCheck                                              &
     &       (rocfft_execute(fwd%ROCfft_plan, WK_fwd%data_ptr,          &
     &                       c_null_ptr, c_null_ptr))
          call hipCheck(hipDeviceSynchronize())
          call hipCheck(hipMemcpy                                       &
     &       (c_loc(WK_fwd%C_ROCmFFT(1)), WK_fwd%data_ptr,              &
     &              WK_fwd%Nbytes, hipMemcpyDeviceToHost))
          elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
          start = OMP_GET_WTIME()
!$omp parallel workshare
          WK_fwd%C_ROCmFFT(1:WK_fwd%Nfft_c)                             &
     &          = WK_fwd%C_ROCmFFT(1:WK_fwd%Nfft_c) / dble(ft1%ngrd)
!$omp end parallel workshare
!
          ft1%s_k(1,nd) = real(WK_fwd%C_ROCmFFT(1)            )
          ft1%s_k(2,nd) = real(WK_fwd%C_ROCmFFT(WK_fwd%Nfft_c))
!$omp parallel do
          do i = 2, WK_fwd%Nfft_c-1
            ft1%s_k(2*i-1,nd) =  two * real(WK_fwd%C_ROCmFFT(i))
            ft1%s_k(2*i,  nd) = -two * imag(WK_fwd%C_ROCmFFT(i))
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
          WK_bwd%C_ROCmFFT(1) = cmplx(ft1%f_x(1,nd), 0.0d0, kind(0d0))
!$omp parallel do
          do i = 2, WK_bwd%Nfft_c - 1
            WK_bwd%C_ROCmFFT(i) = half * cmplx( ft1%f_x(2*i-1,nd),      &
     &                                         -ft1%f_x(2*i,  nd),      &
     &                                          kind(0d0))
          end do
!$omp end parallel do
          WK_bwd%C_ROCmFFT(WK_bwd%Nfft_c) = cmplx(ft1%f_x(2,nd),        &
     &                                            0.0d0, kind(0d0))
          elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
          start = OMP_GET_WTIME()
          call hipCheck                                                 &
     &       (hipMemcpy(WK_bwd%data_ptr, c_loc(WK_bwd%C_ROCmFFT(1)),    &
     &                  WK_bwd%Nbytes, hipMemcpyHostToDevice))
          call rocfftCheck                                              &
     &       (rocfft_execute(bwd%ROCfft_plan, WK_bwd%data_ptr,          &
     &                       c_null_ptr, c_null_ptr))
          call hipCheck(hipDeviceSynchronize())
          call hipCheck                                                 &
     &       (hipMemcpy(c_loc(WK_bwd%X_ROCmFFT(1)), WK_bwd%data_ptr,    &
     &                  WK_bwd%Nbytes, hipMemcpyDeviceToHost))
          elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
          start = OMP_GET_WTIME()
!$omp parallel workshare
          ft1%f_x(1:ft1%ngrd,nd) = WK_bwd%X_ROCmFFT(1:ft1%ngrd)
!$omp end parallel workshare
          elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
        end do
      end do
!
      start = OMP_GET_WTIME()
      call rocfftCheck(rocfft_plan_destroy(fwd%ROCfft_plan))
      call rocfftCheck(rocfft_plan_destroy(bwd%ROCfft_plan))
      call hipCheck(hipFree(WK_bwd%data_ptr))
      call hipCheck(hipFree(WK_fwd%data_ptr))
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
      end program test_ROCmfft_single_prt
