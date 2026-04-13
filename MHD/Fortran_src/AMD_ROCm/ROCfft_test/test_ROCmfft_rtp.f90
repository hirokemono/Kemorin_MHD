!
      program test_ROCmfft_rtp
!
      use iso_c_binding
      use omp_lib
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use m_FFT_size
      use t_fft_test_data
      use t_ROCmFFT_wrapper
      use normalize_for_OMP_FFTW
!
      implicit none
!
      character(len=kchara) :: file_name = 'rtp_ROCmfft_test.dat'
      real(kind = kreal) :: start, finish, elapsed(3)
!
      type(fft_test_data) :: ft1
!
      type(calypso_ROCmfft_params), target :: fwd
      type(calypso_ROCmfft_params), target :: bwd
      type(calypso_ROCmfft_work), target :: WK_fwd
      type(calypso_ROCmfft_work), target :: WK_bwd
!
      integer(kind = kint) :: i, nd
      integer(kind = kint) :: icou
!
!
      write(*,'(a)') '-----  Test rtp ROCmFFT  -----'
      call init_fft_test_data(n_field, ngrid, ft1)
!
!   Initialize Forward transform
      start = OMP_GET_WTIME()
      call calypso_ROCmFFT_set_size(n_field, ngrid, fwd, WK_fwd)
      call calypso_pout_fwd_ROCmFFT_init(fwd, WK_fwd)
      call calypso_fwd_ROCmFFT_init(fwd)
!
!   Initialize Backword transform
      call calypso_ROCmFFT_set_size(n_field, ngrid, bwd, WK_bwd)
      call calypso_pout_bwd_ROCmFFT_init(bwd, WK_bwd)
      call calypso_bwd_ROCmFFT_init(bwd)
      elapsed(3) = OMP_GET_WTIME() - start
!
      elapsed(1:2) = zero
      do icou = 1, n_loop
        if(mod(icou, 20) .eq. 0) write(*,*) 'loop count: ', icou
!
        start = OMP_GET_WTIME()
!$omp target teams distribute parallel do collapse(2)
        do i = 1, ft1%ngrd
          do nd = 1, ft1%nfld
            ft1%s_k(nd,i) = ft1%org(nd,i)
          end do
        end do
!$omp end target teams distribute parallel do
!
!   Forward transform
!$omp target teams distribute parallel do collapse(2)
        do i = 1, ft1%ngrd
          do nd = 1, ft1%nfld
            WK_fwd%X_ROCmFFT(nd,i) = ft1%s_k(nd,i)
          end do
        end do
!$omp end target teams distribute parallel do
        if(ft1%ngrd .lt. WK_fwd%Nfft_r) then
!$omp target teams distribute parallel do collapse(2)
          do i = ft1%ngrd+1, WK_fwd%Nfft_r
            do nd = 1, ft1%nfld
              WK_fwd%X_ROCmFFT(nd,i) = 0.0d0
            end do
          end do
!$omp end target teams distribute parallel do
        end if
        elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call calypso_forward_ROCmFFT(fwd,                               &
     &                            WK_fwd%Nfft_r, WK_fwd%X_ROCmFFT(1,1), &
     &                            WK_fwd%Nfft_c, WK_fwd%C_ROCmFFT(1,1), &
     &                            WK_fwd%Nbytes, WK_fwd%data_ptr)
        elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call norm_rtp_from_fwd_OMP_FFTW(ft1%nfld, WK_fwd%aNfft,         &
     &      WK_fwd%NFFT_c, WK_fwd%C_ROCmFFT(1,1), ft1%ngrd, ft1%s_k(1,1))
        elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
!$omp target teams distribute parallel do collapse(2)
        do i = 1, ft1%ngrd
          do nd = 1, ft1%nfld
            ft1%f_x(nd,i) = ft1%s_k(nd,i)
          end do
        end do
!$omp end target teams distribute parallel do
        elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
!   Backword transform
!
        start = OMP_GET_WTIME()
        call norm_rtp_to_bwd_OMP_FFTW(ft1%nfld, ft1%ngrd, ft1%f_x(1,1), &
     &      WK_bwd%NFFT_c, WK_bwd%C_ROCmFFT(1,1))
        elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call calypso_backward_ROCmFFT(bwd,                              &
     &                            WK_bwd%Nfft_c, WK_bwd%C_ROCmFFT(1,1), &
     &                            WK_bwd%Nfft_r, WK_bwd%X_ROCmFFT(1,1), &
     &                            WK_bwd%Nbytes, WK_bwd%data_ptr)
        elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
!$omp target teams distribute parallel do collapse(2)
        do i = 1, ft1%ngrd
          do nd = 1, ft1%nfld
            ft1%f_x(nd,i) = WK_bwd%X_ROCmFFT(nd,i)
          end do
        end do
!$omp end target teams distribute parallel do
        elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
      end do
!
!   Finalize
      start = OMP_GET_WTIME()
      call calypso_pin_ROCmFFT_fin(fwd, WK_fwd, bwd, WK_bwd)
      elapsed(3) = elapsed(3) + OMP_GET_WTIME() - start
!
  10  continue
      if(n_loop .eq. 1) call write_fft_test_data(file_name, ft1)
      call dealloc_fft_test_data(ft1)
!
      write(*,'(a,i4)') 'Number of threads:  ', np_smp
      write(*, '(a,3i6)')                                               &
     &        "Num (point, field, loop): ", ngrid, n_field, n_loop
      write(*, '("Time for ROCmfft:    ",1pE16.6e3)') elapsed(1)
      write(*, '("Time for Initialize: ",1pE16.6e3)') elapsed(3)
      write(*, '("Time for Data copy:  ",1pE16.6e3)') elapsed(2)
!
      stop 'finish'
      end program test_ROCmfft_rtp
