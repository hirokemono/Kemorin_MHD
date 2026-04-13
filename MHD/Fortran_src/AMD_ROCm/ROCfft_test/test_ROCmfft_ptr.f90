!
      program test_ROCmfft_ptr
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
      use m_FFT_size
      use t_fft_test_data
      use t_ROCmFFT_wrapper
      use normalize_for_OMP_FFTW
!
      implicit none
!
      character(len=kchara) :: file_name = 'prt_ROCmfft_test.dat'
      real(kind = kreal) :: start, finish, elapsed(4)
!
      type(fft_test_data) :: ft1
!
      real(kind = kreal), parameter :: aNfft = one / ngrid
      integer(kind = kint), parameter :: Nfft_c = ngrid/2 + 1
      integer(kind = kint), parameter :: Nfft_r = 2*Nfft_c
      complex(kind = kreal), allocatable, target :: x_cplx(:,:)
      real(kind = kreal), allocatable, target :: x_real(:,:)
      complex(kind = kreal), allocatable, target :: y_cplx(:,:)
      real(kind = kreal), allocatable, target :: y_real(:,:)
      integer(c_size_t) :: Nbytes = Nfft_r * n_field * kreal
      integer(c_size_t), parameter :: ione_c =  ione
      integer(c_size_t), allocatable, target :: l_real(:)
!
      type(calypso_ROCmfft_params), target :: fwd
      type(calypso_ROCmfft_params), target :: bwd
!
      integer(kind = kint) :: i, nd, icou
!
      np_smp = omp_get_max_threads()
      write(*,*) 'Number of threads:  ', np_smp
      call init_fft_test_data(n_field, ngrid, ft1)
      call swap_fft_test_input_to_pin(ft1)
!
      allocate(x_real(Nfft_r,n_field))
      allocate(y_real(Nfft_r,n_field))
!$omp parallel workshare
      x_real(1:Nfft_r,1:n_field) = 0.0d0
      y_real(1:Nfft_r,1:n_field) = 0.0d0
!$omp end parallel workshare
!
      allocate(x_cplx(Nfft_c,n_field))
      allocate(y_cplx(Nfft_c,n_field))
!$omp parallel workshare
      x_cplx(1:Nfft_c,1:n_field) = 0.0d0
      y_cplx(1:Nfft_c,1:n_field) = 0.0d0
!$omp end parallel workshare
!
      allocate(l_real(2))
      l_real(1) = ngrid
      l_real(2) = ft1%nfld
!
!   Initialize Fourier transform
      start = OMP_GET_WTIME()
      call calypso_pin_ROCmFFT_init(Nfft_r, Nbytes, l_real, fwd, bwd)
      elapsed(3) = OMP_GET_WTIME() - start
!
      elapsed(1:2) = zero
      elapsed(4) = zero
      do icou = 1, n_loop
        if(mod(icou, 20) .eq. 0) write(*,*) 'loop count: ', icou
!
        start = OMP_GET_WTIME()
!$omp target teams distribute parallel do collapse(2)
        do nd = 1, ft1%nfld
          do i = 1, ft1%ngrd
            ft1%s_k(i,nd) = ft1%org(i,nd)
          end do
        end do
!$omp end target teams distribute parallel do
!
!   Forward transform
!$omp parallel do private(nd,i)
        do nd = 1, ft1%nfld
          do i = 1, ft1%ngrd
            x_real(i,nd) = ft1%s_k(i,nd)
          end do
          do i = ft1%ngrd+1, Nfft_r
            x_real(i,nd) = zero
          end do
        end do
!$omp end parallel do
        elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call calypso_pin_fwd_ROCmFFT(fwd, ft1%nfld, Nfft_r, x_real,     &
     &      Nfft_c, x_cplx, Nbytes, fwd%data_ptr)
        elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call norm_prt_from_fwd_OMP_FFTW                                 &
     &     (ft1%nfld, aNfft, NFFT_c, x_cplx, ft1%ngrd, ft1%s_k(1,1))
        elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
!
        start = OMP_GET_WTIME()
!$omp target teams distribute parallel do collapse(2)
        do nd = 1, ft1%nfld
          do i = 1, ft1%ngrd
            ft1%f_x(i,nd) = ft1%s_k(i,nd)
          end do
        end do
!$omp end target teams distribute parallel do
        elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
!
!   Backword transform
        start = OMP_GET_WTIME()
        call norm_prt_to_bwd_OMP_FFTW(ft1%nfld, ft1%ngrd, ft1%f_x(1,1), &
     &                                NFFT_c, y_cplx(1,1))
        elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call calypso_pin_bwd_ROCmFFT(bwd, n_field, Nfft_c, y_cplx,      &
     &      Nfft_r, y_real, Nbytes, bwd%data_ptr)
        elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
!$omp parallel do private(nd,i)
        do nd = 1, ft1%nfld
          do i = 1, ft1%ngrd
            ft1%f_x(i,nd) = y_real(i,nd)
          end do
        end do
!$omp end parallel do
        elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
      end do
!
!   Finalize
      start = OMP_GET_WTIME()
      call calypso_pin_ROCmFFT_fin(fwd, bwd)
      deallocate(x_cplx, x_real)
      elapsed(3) = elapsed(3) + OMP_GET_WTIME() - start
!
  10  continue
      if(n_loop .eq. 1) then
        call swap_fft_test_data_to_pout(ft1)
        call write_fft_test_data(file_name, ft1)
      end if
      call dealloc_fft_test_data(ft1)
!
      write(*, '("Time for ROCmfft:    ",1pE16.6e3)') elapsed(1)
      write(*, '("Time for Initialize: ",1pE16.6e3)') elapsed(3)
      write(*, '("Time for Data copy:  ",1pE16.6e3)') elapsed(2)
!
      stop 'finish'
      end program test_ROCmfft_ptr
