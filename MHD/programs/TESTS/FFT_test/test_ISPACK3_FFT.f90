!
      program test_ISPACK3_FFT
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_ispack3_FFT_wrapper
      use t_fft_test_data
!
      implicit none
!
      type(working_ISPACK3) ::  WK_ISPACK3_t
      type(fft_test_data) :: ft0
      integer(kind = kint_gl) ::  Nfft8, nfld8
      integer(kind = kint), parameter ::  ngrid = 128
!
!
      np_smp = 2
      call init_fft_test_data(ngrid, ft0)
      Nfft8 = ft0%ngrd
      nfld8 = ft0%nfld
!
!$omp parallel workshare
      ft0%z(1:nfld8,1:Nfft8) = ft0%x(1:nfld8,1:Nfft8)
!$omp end parallel workshare
!
      call init_wk_ispack3_t(np_smp, ft0%nstack, Nfft8, WK_ISPACK3_t)
!
      call FXRTFA_kemo_t(np_smp, ft0%nstack, nfld8, Nfft8,              &
     &                   ft0%x, WK_ISPACK3_t)
!
!$omp parallel workshare
      ft0%y(1:nfld8,1:Nfft8) = ft0%x(1:nfld8,1:Nfft8)
!$omp end parallel workshare
!
      call FXRTBA_kemo_t(np_smp, ft0%nstack, nfld8, Nfft8,              &
     &                   ft0%x, WK_ISPACK3_t)
!
      call write_fft_test_data('ISPACK3_test.dat', ft0)
      call dealloc_fft_test_data(ft0)
!
      stop
      end program test_ISPACK3_FFT

