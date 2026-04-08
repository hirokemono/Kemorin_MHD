!
      program test_ISPACK3_FFT
!
      use omp_lib
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_FFT_size
!
      use t_ispack3_FFT_wrapper
      use t_fft_test_data
      use calypso_multi_ispack3
!
      implicit none
!
      character(len = kchara), parameter                                &
     &                       :: file_name = 'ISPACK3_test.dat'
!
      type(working_ISPACK3) ::  WK_ISPACK3_t
      type(fft_test_data) :: ft0
      integer(kind = kint_gl) ::  Nfft8, nfld8
      integer(kind = kint) :: iloop = 0
!
!
      write(*,'(a)') '-----  Test ISPACK3  -----'
      iflag_debug = 1
      call init_fft_test_data(n_field, ngrid, ft0)
      Nfft8 = ft0%ngrd
      nfld8 = ft0%nfld
!
      ft0%start = OMP_GET_WTIME()
      call init_wk_ispack3_t(np_smp, ft0%nstack, Nfft8, WK_ISPACK3_t)
      ft0%elapsed(1) = ft0%elapsed(1) + OMP_GET_WTIME() - ft0%start
!
      do iloop = 1, n_loop
        if(mod(iloop, 20) .eq. 0) write(*,*) 'loop count: ', iloop
!
        ft0%start = OMP_GET_WTIME()
!$omp parallel workshare
        ft0%s_k(1:nfld8,1:Nfft8) = ft0%org(1:nfld8,1:Nfft8)
!$omp end parallel workshare
        ft0%elapsed(3) = ft0%elapsed(3) + OMP_GET_WTIME() - ft0%start
!
        call FXRTFA_kemo_t(np_smp, ft0%nstack, nfld8, Nfft8, ft0%s_k,   &
     &                    WK_ISPACK3_t, ft0%elapsed(2), ft0%elapsed(3))
!
        ft0%start = OMP_GET_WTIME()
!$omp parallel workshare
        ft0%f_x(1:nfld8,1:Nfft8) = ft0%s_k(1:nfld8,1:Nfft8)
!$omp end parallel workshare
        ft0%elapsed(3) = ft0%elapsed(3) + OMP_GET_WTIME() - ft0%start
!
        call FXRTBA_kemo_t(np_smp, ft0%nstack, nfld8, Nfft8, ft0%f_x,   &
     &      WK_ISPACK3_t, ft0%elapsed(2), ft0%elapsed(3))
      end do
!
      if(n_loop .eq. 1) call write_fft_test_data(file_name, ft0)
      call dealloc_fft_test_data(ft0)
!
      write(*,'(a,i4)') 'Number of threads:  ', np_smp
      write(*, '(a,3i6)')                                               &
     &        "Num (point, field, loop): ", ngrid, n_field, n_loop
      write(*, '("Initialize:      ",1pE16.6e3)') ft0%elapsed(1)
      write(*, '("Wrapped FFTPACK: ",1pE16.6e3)') ft0%elapsed(2)
      write(*, '("Data copy:       ",1pE16.6e3)') ft0%elapsed(3)
!
      stop 'finish'
      end program test_ISPACK3_FFT

