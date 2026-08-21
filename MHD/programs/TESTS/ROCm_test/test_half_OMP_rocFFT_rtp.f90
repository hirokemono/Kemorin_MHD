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
      use t_FFTPACK5_wrapper
      use pout_OMP_rocFFT_FFTPACK
!
      implicit none
!
!
      real(kind = kreal), parameter :: ratio_rocFFT = 0.5
!
      character(len=kchara), parameter                                  &
     &             :: file_name = 'rtp_half_OMP_rocFFT_test.dat'
      real(kind = kreal) :: start, elapsed(9)
!
      type(fft_test_data) :: ft1
!
      type(calypso_rocFFT_params), target :: fwd
      type(calypso_rocFFT_params), target :: bwd
      type(calypso_rocFFT_work), target :: WK_rocFFT
      type(working_FFTPACK) :: WK_FFTPACK_T
!
      integer(kind = kint) :: ncomp_GPU
      integer(kind = kint) :: ncomp_CPU
      integer(kind = kint) :: max_4_smp
      integer(kind = kint), allocatable :: istack_FFTPACK(:)
      integer(kind = kint) :: i, nd, icou
!
!
      ncomp_GPU = ratio_rocFFT * n_field
      ncomp_CPU =   n_field - ncomp_GPU
      write(*,*) 'ncomp_GPU, ncomp_CPU', ncomp_GPU, ncomp_CPU
!
      write(*,'(a)') '-----  Test rtp OpenMP rocFFT and FFTPACK -----'
      call init_fft_test_data(n_field, ngrid, ft1)
!
!   Initialize Fourier transform
      start = OMP_GET_WTIME()
      allocate(istack_FFTPACK(0:np_smp))
      istack_FFTPACK(0:np_smp) = 0
      call init_pout_OMP_rocFFT_FFTPACK(n_field, Ncomp_GPU, Ncomp_CPU,  &
     &                                 ngrid, np_smp, istack_FFTPACK,   &
     &                                 fwd, bwd, WK_rocFFT, WK_FFTPACK)
      write(*,*) 'istack_FFTPACK', istack_FFTPACK
      elapsed(1) = OMP_GET_WTIME() - start
!
      elapsed(2:6) = zero
      do icou = 1, n_loop+1
        if(mod(icou, 20) .eq. 0) write(*,*) 'loop count: ', icou
!
        start = OMP_GET_WTIME()
!$omp parallel workshare
        ft1%s_k(1:ft1%nfld,1:ft1%ngrd) = ft1%org(1:ft1%nfld,1:ft1%ngrd)
!$omp end parallel workshare
        elapsed(3) = elapsed(3) + OMP_GET_WTIME() - start
!
!   Forward transform
        call pout_fwd_OMP_rocFFT_FFTPACK(n_field, fwd, WK_rocFFT,       &
     &      WK_FFTPACK_T, ft1%s_k(1,1), elapsed(2:5))
!
        start = OMP_GET_WTIME()
!$omp parallel workshare
        ft1%f_x(1:ft1%nfld,1:ft1%ngrd) = ft1%s_k(1:ft1%nfld,1:ft1%ngrd)
!$omp end parallel workshare
        elapsed(3) = elapsed(3) + OMP_GET_WTIME() - start
!
!   Backword transform
        call pout_bwd_OMP_rocFFT_FFTPACK(n_field, bwd, WK_rocFFT,       &
     &      WK_FFTPACK_T, ft1%f_x(1,1), elapsed(2:5))
        if(icou .eq. 1) elapsed(6:9) = elapsed(2:5)
      end do
      elapsed(6) = elapsed(2) - elapsed(6)
      elapsed(8) = elapsed(4) - elapsed(8)
      elapsed(9) = elapsed(5) - elapsed(9)
!
!   Finalize
      start = OMP_GET_WTIME()
      call calypso_rocFFT_fin(fwd, bwd, WK_rocFFT)
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
  10  continue
      if(n_loop .eq. 1) call write_fft_test_data(file_name, ft1)
      call dealloc_fft_test_data(ft1)
!
      write(*,'(a,i4)') 'Number of threads:  ', np_smp
      write(*, '(a,3i6)')                                               &
     &      "Num (point, field, loop):   ", ngrid, n_field, n_loop
      write(*, '(a,3i6)')                                               &
     &      "Num (field_GPU, field_CPU):   ", ncomp_GPU, ncomp_CPU
      write(*, '("Time for Initialize:       ",1pE16.6e3)') elapsed(1)
      write(*, '("Time for FFT:              ",1pE16.6e3)') elapsed(2)
      write(*, '("Time for rocFFT w/o first: ",1pE16.6e3)') elapsed(6)
      write(*, '("Time for FFT on CPU:       ",1pE16.6e3)') elapsed(8)
      write(*, '("Time for FFT on GPU:       ",1pE16.6e3)') elapsed(9)
      write(*, '("Time for Data copy:        ",1pE16.6e3)') elapsed(3)
      write(*, '("Total FFT:                 ",1pE16.6e3)')             &
     &                           elapsed(2) + elapsed(3)
      write(*,'(a)') '-----------------------------'
      write(*,'(a)') ' '
!
      stop 'finish'
!
      end program test_half_OMP_rocFFT_rtp
!
! mpif90 --offload-arch=gfx942 -mcmodel=medium -mcmodel=medium -O3 -g -fopenmp -fopenmp-target-fast  -I. -I/home/hrmatsui/src_kemo/work -I/opt/rocm-7.2.0/include/hipfort/amdgcn -DPNG_OUTPUT -DZLIB_IO -DFFTW3 -D_AMD_ROCM_ -o ./test_half_OMP_rocFFT_rtp test_half_OMP_rocFFT_rtp.f90 /home/hrmatsui/src_kemo/work/m_FFT_size.o /home/hrmatsui/src_kemo/work/t_fft_test_data.o -L/home/hrmatsui/src_kemo/work -lkemo_core -lkemo_c -lfftpack.5d -L/home/hrmatsui/local/amd/lib -lpng -L/home/hrmatsui/local/amd/lib -lz -L/home/hrmatsui/local/amd/lib -lfftw3 -L/opt/rocm-7.2.0/lib -lrocfft -lrocblas -lhipfort-amdgcn -lamdhip64
