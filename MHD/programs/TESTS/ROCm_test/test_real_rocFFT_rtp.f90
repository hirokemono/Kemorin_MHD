!
      program test_real_rocFFT_rtp
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
      use t_multi_ROCmFFT_wrapper
      use multi_pout_complex_ROCmFFT
      use multi_pout_real_ROCmFFT
!
      implicit none
!
      character(len=kchara) :: file_name = 'rtp_real_rocFFT_test.dat'
      real(kind = kreal) :: start, finish, elapsed(3)
!
      type(fft_test_data) :: ft1
!
      type(calypso_rocFFT_params), target :: fwd
      type(calypso_rocFFT_params), target :: bwd
      type(calypso_rocFFT_work), target :: WK_fft
!
      integer(kind = kint) :: i, nd
      integer(kind = kint) :: icou
!
!
      write(*,'(a)') '-----  Test rtp REAL only rocFFT  -----'
      call init_fft_test_data(n_field, ngrid, ft1)
!
!   Initialize Fourier transform
      start = OMP_GET_WTIME()
      call calypso_pout_ROCmFFT_init(n_field, n_field, ngrid,           &
     &                               fwd, bwd, WK_fft)
      elapsed(1) = OMP_GET_WTIME() - start
!
      elapsed(2:3) = zero
      do icou = 1, n_loop
        if(mod(icou, 20) .eq. 0) write(*,*) 'loop count: ', icou
!
        start = OMP_GET_WTIME()
!$omp parallel workshare
        ft1%s_k(1:ft1%nfld,1:ft1%ngrd) = ft1%org(1:ft1%nfld,1:ft1%ngrd)
!$omp end parallel workshare
        elapsed(3) = elapsed(3) + OMP_GET_WTIME() - start
!
!   Forward transform
        call multi_pout_fwd_ROCmFFT2(fwd, WK_fft, ft1%s_k(1,1),         &
     &                              elapsed(2), elapsed(3))
!
        start = OMP_GET_WTIME()
!$omp parallel workshare
        ft1%f_x(1:ft1%nfld,1:ft1%ngrd) = ft1%s_k(1:ft1%nfld,1:ft1%ngrd)
!$omp end parallel workshare
        elapsed(3) = elapsed(3) + OMP_GET_WTIME() - start
!
!   Backword transform
        call multi_pout_bwd_ROCmFFT2(bwd, WK_fft, ft1%f_x(1,1),         &
     &                              elapsed(2), elapsed(3))
      end do
!
!   Finalize
      start = OMP_GET_WTIME()
      call calypso_ROCmFFT_finalize(fwd, bwd, WK_fft)
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
  10  continue
      if(n_loop .eq. 1) call write_fft_test_data(file_name, ft1)
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
      end program test_real_rocFFT_rtp
