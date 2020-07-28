!
      program test_FFT
!
      use m_precision
      use m_constants
      use m_machine_parameter
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
!
      iflag_debug = 1
      np_smp = 2
      call init_fft_test_data(512, ft0)
!
      write(*,*) 'select FFT library'
      write(*,*) '1: FFTPACK5'
      write(*,*) '2: FFTW3 (if avaiable)'
      write(*,*) '3: SINGLE FFTW3 (if avaiable)'
      write(*,*) '4: ISPACK'
      read(*,*) iflag_FFT_t
!
!
!$omp parallel workshare
      ft0%z(1:ft0%nfld,1:ft0%ngrd) = ft0%x(1:ft0%nfld,1:ft0%ngrd)
!$omp end parallel workshare
!
      call initialize_FFT_select                                        &
     &   (0, iflag_FFT_t, np_smp, ft0%nstack, ft0%ngrd, WK_FFTS)
!
      call forward_FFT_select(iflag_FFT_t, np_smp,                      &
     &    ft0%nstack, ft0%nfld, ft0%ngrd, ft0%x, WK_FFTS)
!
!$omp parallel workshare
      ft0%y(1:ft0%nfld,1:ft0%ngrd) = ft0%x(1:ft0%nfld,1:ft0%ngrd)
!$omp end parallel workshare
!
      call backward_FFT_select(iflag_FFT_t, np_smp,                     &
     &    ft0%nstack, ft0%nfld, ft0%ngrd, ft0%x, WK_FFTS)
!
      call write_fft_test_data('fft_test.dat', ft0)
      call dealloc_fft_test_data(ft0)
!
      stop
      end program test_FFT

