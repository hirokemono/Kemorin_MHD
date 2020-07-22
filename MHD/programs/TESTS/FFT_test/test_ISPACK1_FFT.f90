!
      program test_ISPACK1_FFT
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_ispack_FFT_wrapper
      use t_fft_test_data
!
      implicit none
!
      type(working_ISPACK) ::  WK_FFTPACK_t
      type(fft_test_data) :: ft0
!
!
      np_smp = 2
      call init_fft_test_data(512, ft0)
!
!
!$omp parallel workshare
      ft0%z(1:ft0%nfld,1:ft0%ngrd) = ft0%x(1:ft0%nfld,1:ft0%ngrd)
!$omp end parallel workshare
!
      call init_wk_ispack_t(np_smp, ft0%nstack, ft0%ngrd, WK_FFTPACK_t)
!
      call FTTRUF_kemo_t(np_smp, ft0%nstack, ft0%nfld, ft0%ngrd,        &
     &                   ft0%x, WK_FFTPACK_t)
!
!$omp parallel workshare
      ft0%y(1:ft0%nfld,1:ft0%ngrd) = ft0%x(1:ft0%nfld,1:ft0%ngrd)
!$omp end parallel workshare
!
      call FTTRUB_kemo_t(np_smp, ft0%nstack, ft0%nfld, ft0%ngrd,        &
     &                   ft0%x, WK_FFTPACK_t)
!
      call write_fft_test_data('ISPACK1_test.dat', ft0)
      call dealloc_fft_test_data(ft0)
!
      stop
      end program test_ISPACK1_FFT

