!
      program test_half_OMP_rocFFT_rtp
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
      use t_multi_rocFFT_wrapper
      use multi_pout_complex_rocFFT
      use multi_pout_real_rocFFT
!
      implicit none
!
      character(len=kchara) :: file_name = 'rtp_half_OMP_rocFFT_test.dat'
      real(kind = kreal) :: start, finish, elapsed(3)
!
      type(fft_test_data) :: ft1
!
      type(calypso_rocFFT_params), target :: fwd
      type(calypso_rocFFT_params), target :: bwd
      type(calypso_rocFFT_work), target :: WK_fft
      type(working_mul_FFTW), target :: WK_MUL_FFTW_t
!
      integer(kind = kint), parameter :: n_half = n_field / 2
      integer(kind = kint), allocatable :: istack_half(:)
!
      integer(kind = kint) :: i, nd
      integer(kind = kint) :: icou
!
!
      write(*,'(a)')                                                    &
     &    '-----  Test rtp OpenMP rocFFT and FFTW shareing -----'
      call init_fft_test_data(n_field, ngrid, ft1)
!
!   Initialize Fourier transform
!
      allocate(istack_half(0:np_smp))
      istack_half(0:np_smp) = ft1%nstack(0:np_smp) / 2
!
      start = OMP_GET_WTIME()
      call calypso_pout_rocFFT_init(n_half, n_half, ngrid,              &
     &                              fwd, bwd, WK_fft)
      call init_FFTW_mul_type                                           &
     &   (np_smp, istack_half, ft1%ngrd, WK_MUL_FFTW_t)
      elapsed(1) = OMP_GET_WTIME() - start
!
      elapsed(2:3) = zero
      do icou = 1, n_loop
        if(mod(icou, 20) .eq. 0) write(*,*) 'loop count: ', icou
!
        start = OMP_GET_WTIME()
!$omp parallel workshare
        ft1%s_k(1:ft1%nfld,1:ft1%ngrd) = ft1%org(1:ft1%nfld,1:ft1%ngrd)
!$omp end parallel workshare
        elapsed(3) = elapsed(3) + OMP_GET_WTIME() - start
!
!   Forward transform
        call multi_pout_fwd_OMP_rocFFT(n_field, fwd, WK_fft,            &
     &      ft1%s_k(1,1), elapsed(2), elapsed(3))
!
        start = OMP_GET_WTIME()
!$omp parallel workshare
        ft1%f_x(1:ft1%nfld,1:ft1%ngrd) = ft1%s_k(1:ft1%nfld,1:ft1%ngrd)
!$omp end parallel workshare
        elapsed(3) = elapsed(3) + OMP_GET_WTIME() - start
!
!   Backword transform
        call multi_pout_bwd_OMP_rocFFT(n_field, bwd, WK_fft,            &
     &      ft1%f_x(1,1), elapsed(2), elapsed(3))
      end do
!
!   Finalize
      start = OMP_GET_WTIME()
      call calypso_rocFFT_fin(fwd, bwd, WK_fft)
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
  10  continue
      if(n_loop .eq. 1) call write_fft_test_data(file_name, ft1)
      call dealloc_fft_test_data(ft1)
!
      write(*,'(a,i4)') 'Number of threads:  ', np_smp
      write(*, '(a,3i6)')                                               &
     &        "Num (point, field, loop): ", ngrid, n_field, n_loop
      write(*, '("Time for Initialize:     ",1pE16.6e3)') elapsed(1)
      write(*, '("Time for OpenMP rocFFT:  ",1pE16.6e3)') elapsed(2)
      write(*, '("Time for Data copy:      ",1pE16.6e3)') elapsed(3)
      write(*, '("Total FFT:               ",1pE16.6e3)')               &
     &                           elapsed(2) + elapsed(3)
      write(*,'(a)') '-----------------------------'
      write(*,'(a)') ' '
!
      stop 'finish'
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine calypso_fwd_OpenMP_rocFFT(fwd_plan, fwd_wk_info,       &
     &          Nhalf, Ncomp, Nfft_r, X_rocFFT)
!
      type(c_ptr), intent(in), target :: fwd_plan
      type(c_ptr), intent(in), target :: fwd_wk_info
      integer(c_size_t), intent(in) :: Nhalf, Ncomp, Nfft_r
!
      real(kind = kreal), intent(inout), target                         &
     &                   :: X_rocFFT(Nfft_r*Nhalf)
      real(kind = kreal), intent(inout), target                         &
     &                   :: X_FFT(Nfft_r*(Ncomp-Nhalf))
!
!$OMP target enter data map(to:X_rocFFT)
!$OMP target data use_device_addr(X_rocFFT)
!      call rocblasCheck(rocblas_set_pointer_mode(rocblas_handle, 0))
      call rocfftCheck(rocfft_execute(fwd_plan,                         &
     &    c_loc(X_rocFFT(1)), c_null_ptr, fwd_wk_info))
!
!$OMP parallel do private(i,X_WK)
      do i = 1, Ncomp-Nhalf
        X_WK(1:Nfft_r) = X_FFT()
        call dfftw_execute_dft(plan_fftw, X_WK(1), X_WK(1))
      end do
!$OMP end parallel do
!
      call hipCheck(hipDeviceSynchronize())
!$OMP target update from(X_rocFFT)
!$OMP end target data
!$OMP target exit data map(delete:X_rocFFT)
!
      end subroutine calypso_fwd_OpenMP_rocFFT
!
! ------------------------------------------------------------------
      end program test_half_OMP_rocFFT_rtp
