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
      write(*,'(a)') '-----  Test single prt ROCmFFT  -----'
      call init_fft_test_data(n_field, ngrid, ft1)
      call swap_fft_test_input_to_pin(ft1)
!
      start = OMP_GET_WTIME()
      call calypso_sgl_ROCmFFT_init(ngrid, fwd, WK_fwd, bwd, WK_bwd)
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
          call calypso_sgl_fwd_ROCmFFT(fwd,                             &
     &                                 WK_fwd%Nfft_r, WK_fwd%X_ROCmFFT, &
     &                                 WK_fwd%Nfft_c, WK_fwd%C_ROCmFFT, &
     &                                 WK_fwd%Nbytes, WK_fwd%data_ptr)
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
          call calypso_sgl_bwd_ROCmFFT(bwd,                             &
     &                                 WK_bwd%Nfft_c, WK_bwd%C_ROCmFFT, &
     &                                 WK_bwd%Nfft_r, WK_bwd%X_ROCmFFT, &
     &                                 WK_bwd%Nbytes, WK_bwd%data_ptr)
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
      call calypso_single_ROCmFFT_fin(fwd, WK_fwd, bwd, WK_bwd)
      elapsed(3) = elapsed(3) + OMP_GET_WTIME() - start
!
   10 continue
      if(n_loop .eq. 1) then
        call swap_fft_test_data_to_pout(ft1)
        call write_fft_test_data(ROCfft_test, ft1)
      end if
      call dealloc_fft_test_data(ft1)
!
      write(*,'(a,i4)') 'Number of threads:  ', np_smp
      write(*, '(a,3i6)')                                               &
     &        "Num (point, field, loop): ", ngrid, n_field, n_loop
      write(*, '("Time for ROCmfft:    ",1pE16.6e3)') elapsed(1)
      write(*, '("Time for Initialize: ",1pE16.6e3)') elapsed(3)
      write(*, '("Time for Data copy:  ",1pE16.6e3)') elapsed(2)
!
      stop 'finish'
      end program test_ROCmfft_single_prt
