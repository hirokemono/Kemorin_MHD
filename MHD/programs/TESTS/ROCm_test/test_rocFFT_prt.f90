!
      program test_OMP_rocFFT_prt
!
      use iso_c_binding
      use omp_lib
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_FFT_labels
!
      use t_fft_test_data
      use t_parameters_FFT_tests
      use t_ctl_data_4_FFT_tests
      use t_multi_rocFFT_wrapper
      use multi_pin_complex_rocFFT
      use select_pin_rocFFT
!
      implicit none
!
      character(len = kchara) :: test_name = 'prt_rocFFT'
      character(len = kchara), parameter                                &
     &             :: def_fname = 'prt_rocFFT_test.dat'
!
      character(len = kchara) :: ctl_file_name
      type(FFT_tests_ctl), save :: fft_c1
      type(FFT_test_parameters), save :: fft_test_p1
!
      type(fft_test_data) :: ft1
!
      type(calypso_rocFFT_params), target :: fwd
      type(calypso_rocFFT_params), target :: bwd
      type(calypso_rocFFT_work), target :: WK_fft
!
      real(kind = kreal) :: start, finish, elapsed(5)
      integer(kind = kint) :: i, nd, icou
!
      write(*,'(a)') '-----  Test prt rocFFT  -----'
!
      call init_FFT_mode_flags()
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
      if(    ((fft_test_p1%iflag_FFT/10) .ne. (iflag_OMP_rocFFT/10))    &
     & .and. ((fft_test_p1%iflag_FFT/10) .ne. (iflag_real_rocFFT/10))   &
     & .and. ((fft_test_p1%iflag_FFT/10) .ne. (iflag_rocFFT/10))) then
        fft_test_p1%iflag_FFT = iflag_OMP_rocFFT
      end if
      fft_test_p1%test_name = find_FFT_label(fft_test_p1%iflag_FFT)
      write(*,*) 'fft_test_p1%iflag_FFT', fft_test_p1%iflag_FFT,        &
     &          '  ', trim(fft_test_p1%test_name)
!
      iflag_debug = 1
      call init_fft_test_data                                           &
     &   (fft_test_p1%Ncomp_test, fft_test_p1%Nfft_test, ft1)
      call swap_fft_test_input_to_pin(ft1)
!
!   Initialize Fourier transform
      start = OMP_GET_WTIME()
      call calypso_pin_rocFFT_init                                      &
     &   (fft_test_p1%Ncomp_test, fft_test_p1%Ncomp_test,               &
     &    fft_test_p1%Nfft_test, fwd, bwd, WK_fft)
      elapsed(1) = OMP_GET_WTIME() - start
!
      elapsed(2:4) = zero
      do icou = 1, fft_test_p1%Nloop_test + 1
        if(mod(icou, 20) .eq. 0) write(*,*) 'loop count: ', icou
!
        start = OMP_GET_WTIME()
!$omp parallel workshare
        ft1%s_k(1:ft1%ngrd,1:ft1%nfld) = ft1%org(1:ft1%ngrd,1:ft1%nfld)
!$omp end parallel workshare
        elapsed(3) = elapsed(3) + OMP_GET_WTIME() - start
!
!   Forward transform
        call sel_multi_pin_fwd_rocFFT(fft_test_p1%iflag_FFT, ft1%nfld,  &
     &      fwd, WK_fft, ft1%s_k(1,1), elapsed(2), elapsed(3))
!
        start = OMP_GET_WTIME()
!$omp parallel workshare
        ft1%f_x(1:ft1%ngrd,1:ft1%nfld) = ft1%s_k(1:ft1%ngrd,1:ft1%nfld)
!$omp end parallel workshare
        elapsed(3) = elapsed(3) + OMP_GET_WTIME() - start
!
!   Backword transform
        call sel_multi_pin_bwd_rocFFT(fft_test_p1%iflag_FFT, ft1%nfld,  &
     &      bwd, WK_fft, ft1%f_x(1,1), elapsed(2), elapsed(3))
        if(icou .eq. 1) elapsed(4) = elapsed(2)
      end do
      elapsed(4) = elapsed(2) - elapsed(4)
!
!   Finalize
      start = OMP_GET_WTIME()
      call calypso_rocFFT_fin(fwd, bwd, WK_fft)
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
  10  continue
      if(fft_test_p1%Nloop_test .eq. 1)  then
        call swap_fft_test_data_to_pout(ft1)
        call write_fft_test_data(fft_test_p1%file_name, ft1)
      end if
      call dealloc_fft_test_data(ft1)
!
      call write_rocFFT_test_elapsed(fft_test_p1, elapsed(1))
!
      stop 'finish'
      end program test_OMP_rocFFT_prt
