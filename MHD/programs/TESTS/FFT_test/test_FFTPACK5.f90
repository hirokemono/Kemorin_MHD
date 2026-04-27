!
      program test_FFTPACK5
!
      use omp_lib
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_FFT_size
      use m_FFT_selector
!
      use t_fft_test_data
      use t_FFTPACK5_wrapper
!
      use calypso_multi_fftpack
!
      implicit none
!
      character(len = kchara), parameter                                &
     &                      :: file_name = 'fftpack_test.dat'
!
      type(working_FFTPACK) :: WK_FFTPACK_T
      type(fft_test_data) :: ft1
      integer(kind = kint) :: iloop = 0
!
      character(len = kchara) :: title
!
      call init_FFT_mode_flags()
!
      write(title,'(a)') trim('at_once_FFTPACK_flags')
      call write_multi_flags(6, title, at_once_FFTPACK_flags)
      write(title,'(a)') trim('domain_FFTPACK_flags')
      call write_multi_flags(6, title, domain_FFTPACK_flags)
      write(title,'(a)') trim('comp_FFTPACK_flags')
      call write_multi_flags(6, title, comp_FFTPACK_flags)
      write(title,'(a)') trim('single_FFTPACK_flags')
      call write_multi_flags(6, title, single_FFTPACK_flags)
!
      write(*,*) ''
      write(title,'(a)') trim('at_once_FFTW_flags')
      call write_multi_flags(6, title, at_once_FFTW_flags)
      write(title,'(a)') trim('domain_FFTW_flags')
      call write_multi_flags(6, title, domain_FFTW_flags)
      write(title,'(a)') trim('comp_FFTW_flags')
      call write_multi_flags(6, title, comp_FFTW_flags)
      write(title,'(a)') trim('single_FFTW_flags')
      call write_multi_flags(6, title, single_FFTW_flags)
!
      write(*,*) ''
      write(title,'(a)') trim('at_once_FFTW_flags')
      call write_multi_flags(6, title, at_once_OMP_FFTW_flags)
      write(title,'(a)') trim('domain_FFTW_flags')
      call write_multi_flags(6, title, domain_OMP_FFTW_flags)
      write(title,'(a)') trim('comp_FFTW_flags')
      call write_multi_flags(6, title, comp_OMP_FFTW_flags)
      write(title,'(a)') trim('single_FFTW_flags')
      call write_multi_flags(6, title, single_OMP_FFTW_flags)
!
      write(*,*) ''
      write(title,'(a)') trim('at_once_ISPACK0_flags')
      call write_multi_flags(6, title, at_once_ISPACK0_flags)
      write(title,'(a)') trim('domain_ISPACK0_flags')
      call write_multi_flags(6, title, domain_ISPACK0_flags)
      write(title,'(a)') trim('comp_ISPACK0_flags')
      call write_multi_flags(6, title, comp_ISPACK0_flags)
      write(title,'(a)') trim('single_ISPACK0_flags')
      call write_multi_flags(6, title, single_ISPACK0_flags)
!
      write(*,*) ''
      write(title,'(a)') trim('at_once_ISPACK3_flags')
      call write_multi_flags(6, title, at_once_ISPACK3_flags)
      write(title,'(a)') trim('domain_ISPACK3_flags')
      call write_multi_flags(6, title, domain_ISPACK3_flags)
      write(title,'(a)') trim('comp_ISPACK3_flags')
      call write_multi_flags(6, title, comp_ISPACK3_flags)
      write(title,'(a)') trim('single_ISPACK3_flags')
      call write_multi_flags(6, title, single_ISPACK3_flags)
!
      write(*,*) ''
      write(title,'(a)') trim('rocFFT_r2c_flags')
      call write_multi_flags(6, title, rocFFT_r2c_flags)
!
      write(*,*) ''
      write(title,'(a)') trim('at_once_rocFFT_r2c_flags')
      call write_multi_flags(6, title, at_once_rocFFT_r2c_flags)
      write(title,'(a)') trim('domain_rocFFT_r2c_flags')
      call write_multi_flags(6, title, domain_rocFFT_r2c_flags)
      write(title,'(a)') trim('comp_rocFFT_r2c_flags')
      call write_multi_flags(6, title, comp_rocFFT_r2c_flags)
      write(title,'(a)') trim('single_rocFFT_r2c_flags')
      call write_multi_flags(6, title, single_rocFFT_r2c_flags)
!
!
      write(*,*) ''
      write(title,'(a)') trim('rocFFT_r2r_flags')
      call write_multi_flags(6, title, rocFFT_r2r_flags)
!
      write(*,*) ''
      write(title,'(a)') trim('at_once_rocFFT_r2r_flags')
      call write_multi_flags(6, title, at_once_rocFFT_r2r_flags)
      write(title,'(a)') trim('domain_rocFFT_r2r_flags')
      call write_multi_flags(6, title, domain_rocFFT_r2r_flags)
      write(title,'(a)') trim('comp_rocFFT_r2r_flags')
      call write_multi_flags(6, title, comp_rocFFT_r2r_flags)
      write(title,'(a)') trim('single_rocFFT_r2r_flags')
      call write_multi_flags(6, title, single_rocFFT_r2r_flags)
!
!
      write(*,*) ''
      write(title,'(a)') trim('rocFFT_OMP_flags')
      call write_multi_flags(6, title, rocFFT_OMP_flags)
!
      write(*,*) ''
      write(title,'(a)') trim('at_once_OMP_rocFFT_flags')
      call write_multi_flags(6, title, at_once_OMP_rocFFT_flags)
      write(title,'(a)') trim('domain_OMP_rocFFT_flags')
      call write_multi_flags(6, title, domain_OMP_rocFFT_flags)
      write(title,'(a)') trim('comp_OMP_rocFFT_flags')
      call write_multi_flags(6, title, comp_OMP_rocFFT_flags)
      write(title,'(a)') trim('single_OMP_rocFFT_flags')
      call write_multi_flags(6, title, single_OMP_rocFFT_flags)
!
!
!
      write(*,'(a)') '-----  Test FFTPACK  -----'
      iflag_debug = 1
      call init_fft_test_data(n_field, ngrid, ft1)
!
      ft1%start = OMP_GET_WTIME()
      call init_WK_FFTPACK_t                                            &
     &   (np_smp, ft1%nstack, ft1%ngrd, WK_FFTPACK_T)
      ft1%elapsed(1) = ft1%elapsed(1) + OMP_GET_WTIME() - ft1%start
!
      do iloop = 1, n_loop
        if(mod(iloop, 20) .eq. 0) write(*,*) 'loop count: ', iloop
!
        ft1%start = OMP_GET_WTIME()
!$omp parallel workshare
        ft1%s_k(1:ft1%nfld,1:ft1%ngrd) = ft1%org(1:ft1%nfld,1:ft1%ngrd)
!$omp end parallel workshare
        ft1%elapsed(3) = ft1%elapsed(3) + OMP_GET_WTIME() - ft1%start
!
        call CALYPSO_RFFTMF_t(np_smp, ft1%nstack, ft1%nfld, ft1%ngrd,   &
     &      ft1%s_k, WK_FFTPACK_T, ft1%elapsed(2), ft1%elapsed(3))
!
        ft1%start = OMP_GET_WTIME()
!$omp parallel workshare
        ft1%f_x(1:ft1%nfld,1:ft1%ngrd) = ft1%s_k(1:ft1%nfld,1:ft1%ngrd)
!$omp end parallel workshare
        ft1%elapsed(3) = ft1%elapsed(3) + OMP_GET_WTIME() - ft1%start
!
        call CALYPSO_RFFTMB_t(np_smp, ft1%nstack, ft1%nfld, ft1%ngrd,   &
     &      ft1%f_x, WK_FFTPACK_T, ft1%elapsed(2), ft1%elapsed(3))
      end do
!
      if(n_loop .eq. 1) call write_fft_test_data(file_name, ft1)
      call dealloc_fft_test_data(ft1)
!
      write(*,'(a,i4)') 'Number of threads:  ', np_smp
      write(*, '(a,3i6)')                                               &
     &        "Num (point, field, loop): ", ngrid, n_field, n_loop
      write(*, '("Initialize:      ",1pE16.6e3)') ft1%elapsed(1)
      write(*, '("Wrapped FFTPACK: ",1pE16.6e3)') ft1%elapsed(2)
      write(*, '("Data copy:       ",1pE16.6e3)') ft1%elapsed(3)
!
      stop 'finish'
      end program test_FFTPACK5

