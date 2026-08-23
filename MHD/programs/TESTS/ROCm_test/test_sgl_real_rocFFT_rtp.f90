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
      use t_fft_test_data
      use t_parameters_FFT_tests
      use t_ctl_data_4_FFT_tests
      use t_single_rocFFT_wrapper
      use single_pout_rocFFT_offload
!
      implicit none
!
      character(len = kchara), parameter                                &
     &             :: test_name = 'rtp_single_real_rocFFT'
      character(len = kchara), parameter                                &
     &             :: def_fname = 'sgl_rtp_real_rocFFT_test.dat'
!
      character(len = kchara) :: ctl_file_name
      type(FFT_tests_ctl), save :: fft_c1
      type(FFT_test_parameters), save :: fft_test_p1
!
      type(fft_test_data) :: ft1
      type(single_rocFFT_work), target :: WK_fft
!
      real(kind = kreal) :: start, finish, elapsed(5)
      integer(kind = kint) :: i, nd
      integer(kind = kint) :: icou
!
      write(*,'(a)') '-----  Test single rtp REAL only rocFFT  -----'
!
      call default_FFT_test_parameters(test_name, def_fname,            &
     &                                 fft_test_p1)
      if(command_argument_count() .ge. 1) then
        call get_command_argument(1, ctl_file_name)
        call read_control_file_FFT_tests(ctl_file_name, fft_c1)
        call set_FFT_test_parameters(fft_c1, fft_test_p1)
      else
        write(*,*) 'No control file name in command: Use default'
      end if
!
      iflag_debug = 1
      call init_fft_test_data                                           &
     &   (fft_test_p1%Ncomp_test, fft_test_p1%Nfft_test, ft1)
!
      start = OMP_GET_WTIME()
      call calypso_sgl_rocFFT_init(fft_test_p1%Nfft_test, WK_fft)
      elapsed(1) = OMP_GET_WTIME() - start
!
      elapsed(2:3) = 0.0d0
      do icou = 1, fft_test_p1%Nloop_test + 1
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
      if(fft_test_p1%Nloop_test .eq. 1)  then
        call write_fft_test_data(fft_test_p1%file_name, ft1)
      end if
      call dealloc_fft_test_data(ft1)
!
      call write_rocFFT_test_elapsed(fft_test_p1, elapsed(1))
!
      stop 'finish'
      end program test_sgl_real_rocFFT_rtp
