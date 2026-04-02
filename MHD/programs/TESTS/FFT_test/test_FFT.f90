!
      program test_FFT
!
      use omp_lib
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_FFT_size
!
      use m_FFT_selector
      use t_FFT_selector
      use t_fft_test_data
!
      implicit none
!
      type(working_FFTs) :: WK_FFTS
      type(fft_test_data) :: ft0
      integer(kind = kint) :: iflag_FFT_t
!
      integer(kind = kint) :: iloop = 0
!
      write(*,'(a)') '-----  Test FFT  -----'
      iflag_debug = 1
      call init_fft_test_data(n_field, ngrid, ft0)
!
      write(*,*) 'select FFT library'
      write(*,*) '1: FFTPACK5'
      write(*,*) '2: FFTW3 (if avaiable)'
      write(*,*) '3: SINGLE FFTW3 (if avaiable)'
      write(*,*) '4: ISPACK-0.93'
      write(*,*) '5: ISPACK-3.01'
      read(*,*) iflag_FFT_t
!
!
      ft0%start = OMP_GET_WTIME()
      call initialize_FFT_select                                        &
     &   (0, iflag_FFT_t, np_smp, ft0%nstack, ft0%ngrd, WK_FFTS)
      ft0%elapsed(1) = ft0%elapsed(1) + OMP_GET_WTIME() - ft0%start
!
      do iloop = 1, n_loop
        if(mod(iloop, 20) .eq. 0) write(*,*) 'loop count: ', iloop
!
        ft0%start = OMP_GET_WTIME()
!$omp parallel workshare
        ft0%s_k(1:ft0%nfld,1:ft0%ngrd) = ft0%org(1:ft0%nfld,1:ft0%ngrd)
!$omp end parallel workshare
        ft0%elapsed(3) = ft0%elapsed(3) + OMP_GET_WTIME() - ft0%start
!
        ft0%start = OMP_GET_WTIME()
        call forward_FFT_select(iflag_FFT_t, np_smp,                    &
     &      ft0%nstack, ft0%nfld, ft0%ngrd, ft0%s_k, WK_FFTS)
        ft0%elapsed(2) = ft0%elapsed(2) + OMP_GET_WTIME() - ft0%start
!
        ft0%start = OMP_GET_WTIME()
!$omp parallel workshare
        ft0%f_x(1:ft0%nfld,1:ft0%ngrd) = ft0%s_k(1:ft0%nfld,1:ft0%ngrd)
!$omp end parallel workshare
        ft0%elapsed(3) = ft0%elapsed(3) + OMP_GET_WTIME() - ft0%start
!
        ft0%start = OMP_GET_WTIME()
        call backward_FFT_select(iflag_FFT_t, np_smp,                   &
     &      ft0%nstack, ft0%nfld, ft0%ngrd, ft0%f_x, WK_FFTS)
        ft0%elapsed(2) = ft0%elapsed(2) + OMP_GET_WTIME() - ft0%start
      end do
!
      if(n_loop .eq. 1) call write_fft_test_data('fft_test.dat', ft0)
      call dealloc_fft_test_data(ft0)
!
      write(*,'(a,i4)') 'Number of threads:  ', np_smp
      write(*, '(a,3i6)')                                               &
     &        "Num (point, field, loop): ", ngrid, n_field, n_loop
      write(*, '("Initialize:      ",1pE16.6e3)') ft0%elapsed(1)
      write(*, '("Wrapped FFTPACK: ",1pE16.6e3)') ft0%elapsed(2)
      write(*, '("Data copy:       ",1pE16.6e3)') ft0%elapsed(3)
!
      stop
      end program test_FFT

