!
      program test_FFT
!
      use omp_lib
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_FFT_size
      use m_FFT_labels
!
      use t_fft_test_data
      use FFT_test_loop
!
      implicit none
!
      type(working_FFTs) :: WK_FFTS
      type(fft_test_data) :: ft0
      integer(kind = kint) :: iflag_FFT_t
!
      call init_FFT_mode_flags()
!
      write(*,'(a)') '-----  Test FFT  -----'
      call init_fft_test_data(n_field, ngrid, ft0)
!
      write(*,*) ' Select FFT library'
      write(*,*) 'Negative values is set to use wave number ', &
     &           'as the inner most loop'
      write(*,*) ' 1: FFTPACK5'
      write(*,*) ' 2: SINGLE FFTPACK5'
      write(*,*) '11: FFTW3        (if avaiable)'
      write(*,*) '12: SINGLE FFTW3 (if avaiable)'
      write(*,*) '21: ISPACK-0.93'
      write(*,*) '24: SINGLE ISPACK-0.93'
      write(*,*) '31: ISPACK-3.01'
      write(*,*) '34: SINGLE ISPACK-3.01'
      read(*,*) iflag_FFT_t
!
      write(*,*) 'iflag_FFT_t', iflag_FFT_t,                            &
     &          iflag_ISPACK3, iflag_single_fft
      if(iflag_FFT_t .lt. 0) then
        call FFT_test_with_phi_in_data(iflag_FFT_t, n_loop,             &
     &                                 ft0, WK_FFTS)
      else
        call FFT_test_with_phi_out_data(iflag_FFT_t, n_loop,            &
     &                                  ft0, WK_FFTS)
      end if
!
!
      if(n_loop .eq. 1) call write_fft_test_data('fft_test.dat', ft0)
      call dealloc_fft_test_data(ft0)
!
      write(*,'(a,i4)') 'Number of threads:  ', np_smp
      write(*, '(a,3i6)')                                               &
     &        "Num (point, field, loop): ", ngrid, n_field, n_loop
      write(*, '("Initialize:      ",1pE16.6e3)') ft0%elapsed(1)
      write(*, '("Wrapped FFT:     ",1pE16.6e3)') ft0%elapsed(2)
      write(*, '("Data copy:       ",1pE16.6e3)') ft0%elapsed(3)
      write(*,'(a)') '-----------------------------'
      write(*,'(a)') ' '
!
      stop
      end program test_FFT

