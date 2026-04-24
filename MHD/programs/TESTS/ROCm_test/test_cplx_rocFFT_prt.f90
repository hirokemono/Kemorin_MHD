!
      program test_cplx_rocFFT_prt
!
      use iso_c_binding
      use omp_lib
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use m_FFT_size
      use t_fft_test_data
      use t_multi_rocFFT_wrapper
      use multi_pin_complex_rocFFT
!
      implicit none
!
      character(len=kchara) :: file_name = 'prt_rocFFT_test.dat'
      real(kind = kreal) :: start, finish, elapsed(4)
!
      type(fft_test_data) :: ft1
!
      type(calypso_rocFFT_params), target :: fwd
      type(calypso_rocFFT_params), target :: bwd
      type(calypso_rocFFT_work), target :: WK_fft
!
      integer(kind = kint) :: i, nd, icou
!
      write(*,'(a)') '-----  Test prt rocFFT  -----'
      call init_fft_test_data(n_field, ngrid, ft1)
      call swap_fft_test_input_to_pin(ft1)
!
!   Initialize Fourier transform
      start = OMP_GET_WTIME()
      call calypso_pin_rocFFT_init(n_field, n_field, ngrid,             &
     &                             fwd, bwd, WK_fft)
      elapsed(1) = OMP_GET_WTIME() - start
!
      elapsed(2:4) = zero
      do icou = 1, n_loop
        if(mod(icou, 20) .eq. 0) write(*,*) 'loop count: ', icou
!
        start = OMP_GET_WTIME()
!$omp parallel workshare
        ft1%s_k(1:ft1%ngrd,1:ft1%nfld) = ft1%org(1:ft1%ngrd,1:ft1%nfld)
!$omp end parallel workshare
        elapsed(3) = elapsed(3) + OMP_GET_WTIME() - start
!
!   Forward transform
        call multi_pin_fwd_rocFFT_r2c(fwd, WK_fft, ft1%s_k(1,1),        &
     &                                elapsed(2), elapsed(3))
!
        start = OMP_GET_WTIME()
!$omp parallel workshare
        ft1%f_x(1:ft1%ngrd,1:ft1%nfld) = ft1%s_k(1:ft1%ngrd,1:ft1%nfld)
!$omp end parallel workshare
        elapsed(3) = elapsed(3) + OMP_GET_WTIME() - start
!
!   Backword transform
        call multi_pin_bwd_rocFFT_c2r(bwd, WK_fft, ft1%f_x,             &
     &                                elapsed(2), elapsed(3))
      end do
!
!   Finalize
      start = OMP_GET_WTIME()
      call calypso_rocFFT_fin(fwd, bwd, WK_fft)
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
  10  continue
      if(n_loop .eq. 1) then
        call swap_fft_test_data_to_pout(ft1)
        call write_fft_test_data(file_name, ft1)
      end if
      call dealloc_fft_test_data(ft1)
!
      write(*,'(a,i4)') 'Number of threads:  ', np_smp
      write(*, '(a,3i6)')                                               &
     &        "Num (point, field, loop): ", ngrid, n_field, n_loop
      write(*, '("Time for Initialize: ",1pE16.6e3)') elapsed(1)
      write(*, '("Time for rocFFT:     ",1pE16.6e3)') elapsed(2)
      write(*, '("Time for Data copy:  ",1pE16.6e3)') elapsed(3)
      write(*, '("Total FFT:           ",1pE16.6e3)')                   &
     &                           elapsed(2) + elapsed(3)
!
      stop 'finish'
      end program test_cplx_rocFFT_prt
