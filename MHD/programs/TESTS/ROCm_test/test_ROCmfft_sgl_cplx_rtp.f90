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
      use t_single_ROCmFFT_wrapper
      use single_pout_ROCmFFT_offload
!
      implicit none
!
      character(len = kchara) :: file_name = 'sgl_rtp_ROCmfft_test.dat'
!
      real(kind = kreal) :: start, finish, elapsed(3)
!
      type(fft_test_data) :: ft1
!
      type(single_ROCmfft_params), target :: fwd
      type(single_ROCmfft_params), target :: bwd
      type(single_ROCmfft_work), target :: WK_fft
!
      integer(c_size_t), parameter :: ione_c = ione
!
      integer(kind = kint) :: i, nd
      integer(kind = kint) :: icou
!
      write(*,'(a)') '-----  Test single rtp ROCmFFT  -----'
      call init_fft_test_data(n_field, ngrid, ft1)
!
      start = OMP_GET_WTIME()
      call calypso_sgl_ROCmFFT_init(ngrid, WK_fft)
      elapsed(1) = OMP_GET_WTIME() - start
!
      elapsed(2:3) = 0.0d0
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
        call single_pout_fwd_ROCmFFT_r2c(WK_fft, ft1%nfld, ft1%s_k,     &
     &                                   elapsed(2), elapsed(3))
!
        start = OMP_GET_WTIME()
!$omp parallel workshare
        ft1%f_x(1:ft1%nfld,1:ft1%ngrd) = ft1%s_k(1:ft1%nfld,1:ft1%ngrd)
!$omp end parallel workshare
        elapsed(3) = elapsed(3) + OMP_GET_WTIME() - start
!
!   Backword transform
        call single_pout_bwd_ROCmFFT_c2r(WK_fft, ft1%nfld, ft1%f_x,     &
     &                                   elapsed(2), elapsed(3))
      end do
!
      start = OMP_GET_WTIME()
      call calypso_single_ROCmFFT_fin(WK_fft)
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
   10 continue
      if(n_loop .eq. 1) call write_fft_test_data(file_name, ft1)
      call dealloc_fft_test_data(ft1)
!
      write(*,'(a,i4)') 'Number of threads:  ', np_smp
      write(*, '(a,3i6)')                                               &
     &        "Num (point, field, loop): ", ngrid, n_field, n_loop
      write(*, '("Time for Initialize: ",1pE16.6e3)') elapsed(3)
      write(*, '("Time for ROCmfft:    ",1pE16.6e3)') elapsed(1)
      write(*, '("Time for Data copy:  ",1pE16.6e3)') elapsed(2)
      write(*, '("Total FFT:           ",1pE16.6e3)')                   &
     &                           elapsed(2) + elapsed(3)
!
      stop 'finish'
      end program test_ROCmfft_single_rtp
