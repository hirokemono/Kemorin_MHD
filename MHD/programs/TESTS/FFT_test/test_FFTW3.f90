!
      program test_FFTW3
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_FFTW_wrapper
      use t_multi_FFTW_wrapper
      use t_fft_test_data
!
      implicit none
!
      character(len = kchara) :: mul_fftw_test = 'mul_fftw_test.dat'
      character(len = kchara) :: sgl_fftw_test = 'sgl_fftw_test.dat'
      character(len = kchara) :: file_name
!
      type(working_FFTW) ::     WK_FFTW_t
      type(working_mul_FFTW) :: WK_MUL_FFTW_t
      type(fft_test_data) :: ft3
!
      integer(kind = kint) :: iflag_FFT
!
!
      iflag_debug = 1
      np_smp = 2
      call init_fft_test_data(512, ft3)
!
      write(*,*) 'select FFT library'
      write(*,*) '2: FFTW3'
      write(*,*) '3: SINGLE FFTW3'
      read(*,*) iflag_FFT
!
!
!$omp parallel workshare
      ft3%z(1:ft3%nfld,1:ft3%ngrd) = ft3%x(1:ft3%nfld,1:ft3%ngrd)
!$omp end parallel workshare
!
      if(iflag_FFT .eq. 2) then
        write(*,*) 'Use FFTW'
        file_name = mul_fftw_test
        call init_FFTW_mul_type                                         &
     &     (np_smp, ft3%nstack, ft3%ngrd, WK_MUL_FFTW_t)
        call FFTW_mul_forward_type(np_smp, ft3%nstack,                  &
     &      ft3%nfld, ft3%ngrd, ft3%x, WK_MUL_FFTW_t)
      else if(iflag_FFT .eq. 3) then
        write(*,*) 'Use single transform in FFTW'
        file_name = sgl_fftw_test
        call init_FFTW_type                                             &
     &     (ft3%nstack(np_smp), ft3%ngrd, WK_FFTW_t)
        call FFTW_forward_type(np_smp, ft3%nstack,                      &
     &      ft3%nfld, ft3%ngrd, ft3%x, WK_FFTW_t)
      end if
!
!$omp parallel workshare
      ft3%y(1:ft3%nfld,1:ft3%ngrd) = ft3%x(1:ft3%nfld,1:ft3%ngrd)
!$omp end parallel workshare
!
      if(iflag_FFT .eq. 2) then
        call FFTW_mul_backward_type(np_smp, ft3%nstack,                 &
     &      ft3%nfld, ft3%ngrd, ft3%x, WK_MUL_FFTW_t)
      else if(iflag_FFT .eq. 3) then
        call FFTW_backward_type(np_smp, ft3%nstack,                     &
     &      ft3%nfld, ft3%ngrd, ft3%x, WK_FFTW_t)
      end if
!
      call write_fft_test_data(file_name, ft3)
      call dealloc_fft_test_data(ft3)
!
      stop
      end program test_FFTW3

