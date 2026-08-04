!
      program test_sgl_real_rocFFT_rtp
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
      use t_single_rocFFT_wrapper
      use single_pout_rocFFT_offload
!
      implicit none
!
      character(len = kchara) :: file_name                              &
     &              = 'sgl_rtp_real_rocFFT_test.dat'
!
      real(kind = kreal) :: start, finish, elapsed(5)
!
      type(fft_test_data) :: ft1
      type(single_rocFFT_work), target :: WK_fft
!
      integer(kind = kint) :: i, nd
      integer(kind = kint) :: icou
!
      write(*,'(a)') '-----  Test single rtp REAL only rocFFT  -----'
      call init_fft_test_data(n_field, ngrid, ft1)
!
      start = OMP_GET_WTIME()
      call calypso_sgl_rocFFT_init(ngrid, WK_fft)
      elapsed(1) = OMP_GET_WTIME() - start
!
      elapsed(2:3) = 0.0d0
      do icou = 1, n_loop+1
        if(mod(icou, 20) .eq. 0) write(*,*) 'loop count: ', icou
!
        start = OMP_GET_WTIME()
!$omp parallel workshare
        ft1%s_k(1:ft1%nfld,1:ft1%ngrd) = ft1%org(1:ft1%nfld,1:ft1%ngrd)
!$omp end parallel workshare
        elapsed(3) = elapsed(3) + OMP_GET_WTIME() - start
!
!   Forward transform
        call single_pout_fwd_rocFFT_r2r(WK_fft, ft1%nfld, ft1%s_k,      &
     &                                  elapsed(2), elapsed(3))
!
        start = OMP_GET_WTIME()
!$omp parallel workshare
        ft1%f_x(1:ft1%nfld,1:ft1%ngrd) = ft1%s_k(1:ft1%nfld,1:ft1%ngrd)
!$omp end parallel workshare
        elapsed(3) = elapsed(3) + OMP_GET_WTIME() - start
!
!   Backword transform
        call single_pout_bwd_rocFFT_r2r(WK_fft, ft1%nfld, ft1%f_x,      &
     &                                  elapsed(2), elapsed(3))
        if(icou .eq. 1) elapsed(4) = elapsed(2)
      end do
      elapsed(4) = elapsed(2) - elapsed(4) 
!
      start = OMP_GET_WTIME()
      call calypso_sgl_rocFFT_fin(WK_fft)
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
   10 continue
      if(n_loop .eq. 1) call write_fft_test_data(file_name, ft1)
      call dealloc_fft_test_data(ft1)
!
      write(*,'(a,i4)') 'Number of threads:  ', np_smp
      write(*, '(a,3i6)')                                               &
     &        "Num (point, field, loop): ", ngrid, n_field, n_loop
      write(*, '("Time for Initialize:       ",1pE16.6e3)') elapsed(1)
      write(*, '("Time for rocFFT:           ",1pE16.6e3)') elapsed(2)
      write(*, '("Time for rocFFT w/o first: ",1pE16.6e3)') elapsed(4)
      write(*, '("Time for Data copy:        ",1pE16.6e3)') elapsed(3)
      write(*, '("Total FFT:                 ",1pE16.6e3)')             &
     &                           elapsed(2) + elapsed(3)
      write(*,'(a)') '-----------------------------'
      write(*,'(a)') ' '
!
      stop 'finish'
      end program test_sgl_real_rocFFT_rtp
