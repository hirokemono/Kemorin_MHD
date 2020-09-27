!
      program test_OMP_FFTW3
!
      use m_precision
      use m_constants
!
      use t_OMP_FFTW_wrapper
      use t_fft_test_data
!
      implicit none
!
      character(len = kchara) :: mul_fftw_test = 'omp_fftw_test.dat'
!
      type(working_OMP_FFTW) :: WK_OMP_FFTW_t
      type(fft_test_data) :: ft3
!
      integer(kind = kint), parameter ::  ngrid = 8192
!
      integer :: i
!
      iflag_debug = 1
      call init_fft_test_data(ngrid, ft3)
!
        write(*,*) 'Test FFTW'
        call init_OMP_FFTW_type                                         &
     &     (ft3%nfld, ft3%ngrd, WK_OMP_FFTW_t)
!
      do i = 1, 10000
!
!$omp parallel workshare
      ft3%z(1:ft3%nfld,1:ft3%ngrd) = ft3%x(1:ft3%nfld,1:ft3%ngrd)
!$omp end parallel workshare
!
        call OMP_forward_FFTW_type                                      &
     &     (ft3%nfld, ft3%ngrd, ft3%x, WK_OMP_FFTW_t)
!
!$omp parallel workshare
      ft3%y(1:ft3%nfld,1:ft3%ngrd) = ft3%x(1:ft3%nfld,1:ft3%ngrd)
!$omp end parallel workshare
!
        call OMP_backward_FFTW_type                                     &
     &     (ft3%nfld, ft3%ngrd, ft3%x, WK_OMP_FFTW_t)
      end do
!
      call write_fft_test_data(mul_fftw_test, ft3)
      call dealloc_fft_test_data(ft3)
!
      stop
      end program test_OMP_FFTW3

