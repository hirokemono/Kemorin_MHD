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
      use t_FFTPACK5_wrapper
      use calypso_multi_fftpack
      use cal_minmax_and_stacks
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
!
      write(*,'(a)') '-----  Test rtp OpenMP rocFFT and FFTPACK -----'
      call init_fft_test_data(n_field, ngrid, ft1)
!
      allocate(istack_FFTPACK(0:np_smp))
      istack_FFTPACK(0:np_smp) = 0
      call count_number_4_smp(np_smp, (ncomp_GPU+1), n_field,           &
     &                        istack_FFTPACK, max_4_smp)
!
      write(*,*) 'ncomp_GPU, ncomp_CPU', ncomp_GPU, ncomp_CPU
      write(*,*) 'istack_FFTPACK', istack_FFTPACK
!
!   Initialize Fourier transform
      start = OMP_GET_WTIME()
      call calypso_pout_rocFFT_init(ncomp_GPU, ncomp_GPU, ngrid,        &
     &                              fwd, bwd, WK_rocFFT)
      call init_WK_FFTPACK_t(np_smp, istack_FFTPACK,                    &
     &                       ngrid, WK_FFTPACK_T)
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
        call multi_pout_fwd_OMP_rocFFT2(n_field, istack_FFTPACK,        &
     &      fwd, WK_rocFFT, WK_FFTPACK_T, ft1%s_k(1,1),                 &
     &      elapsed(2:5))
!
        start = OMP_GET_WTIME()
!$omp parallel workshare
        ft1%f_x(1:ft1%nfld,1:ft1%ngrd) = ft1%s_k(1:ft1%nfld,1:ft1%ngrd)
!$omp end parallel workshare
        elapsed(3) = elapsed(3) + OMP_GET_WTIME() - start
!
!   Backword transform
        call multi_pout_bwd_OMP_rocFFT2(n_field, istack_FFTPACK,        &
     &      bwd, WK_rocFFT, WK_FFTPACK_T, ft1%f_x(1,1), elapsed(2:5))
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
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine multi_pout_fwd_OMP_rocFFT2(Ncomp, istack_FFTPACK,      &
     &          fwd, WK_fft, WK_FFTPACK, X, elapsed)
!
      use copy_field_for_FFT
      use normalize_for_rocFFT
      use calypso_multi_rocFFT
!
      use multi_pout_FFTPACK_smp
      use normalize_for_FFTPACK
!
      integer(kind = kint), intent(in) :: Ncomp
      type(calypso_rocFFT_params), intent(in), target :: fwd
!
      integer(kind = kint), intent(in) :: istack_FFTPACK(0:np_smp)
!
      type(calypso_rocFFT_work), intent(inout) :: WK_fft
      type(working_FFTPACK), intent(inout) :: WK_FFTPACK
      real(kind = kreal), intent(inout) :: X(Ncomp,fwd%Nfft)
      real(kind = kreal), intent(inout) :: elapsed(4)
!
      real(kind = kreal) :: start
      real(kind = kreal) :: st_c, st_g
!
!
      start = OMP_GET_WTIME()
      call copy_pout_field_to_FFT(ione, Ncomp, int(fwd%Nfft), X(1,1),   &
     &    int(fwd%Ncomp), int(fwd%Nfft), WK_fft%X_rocFFT(1))
      call copy_rtp_fld_to_RFFTMF                                       &
     &   (np_smp, istack_FFTPACK, WK_FFTPACK%Mmax_smp, int(fwd%Nfft),   &
     &    Ncomp, X(1,1), WK_FFTPACK%X_FFTPACK5)
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
!!   1. Create a CPU thread team
!$omp parallel
!!   2. Isolate a single thread to spawn the GPU work asynchronously
!$omp single
      st_g = OMP_GET_WTIME()
      call calypso_fwd_OpenMP_rocFFT                                    &
     &   (fwd%rocFFT_plan, fwd%rocFFT_wk_info, fwd%Ncomp,               &
     &    WK_fft%aNfft, WK_fft%Nfft_r, WK_fft%X_rocFFT(1))
      elapsed(4) = elapsed(4) + OMP_GET_WTIME() - st_g
!$omp end single nowait
!
!!   3. The rest of the CPU threads immediately and execute
      st_c = OMP_GET_WTIME()
      call multi_pout_RFFTMF_smp                                        &
     &   (np_smp, istack_FFTPACK, WK_FFTPACK%Mmax_smp, int(fwd%Nfft),   &
     &    WK_FFTPACK%X_FFTPACK5, WK_FFTPACK%lsave_FFTPACK,              &
     &    WK_FFTPACK%WSAVE_FFTPACK, WK_FFTPACK%WORK_FFTPACK)
      elapsed(3) = elapsed(3) + OMP_GET_WTIME() - st_c
!$omp end parallel
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call norm_rtp_from_fwd_rocFFT                                     &
     &   (int(fwd%Ncomp), int(WK_fft%NFFT_r), WK_fft%X_rocFFT(1),       &
     &    Ncomp, int(fwd%Nfft), X(1,1))
      call copy_rtp_spectr_from_RFFTMF                                  &
     &   (np_smp, istack_FFTPACK, WK_FFTPACK%Mmax_smp, int(fwd%Nfft),   &
     &    WK_FFTPACK%X_FFTPACK5, Ncomp, X(1,1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
      write(*,*) 'CPU FFT clock',   elapsed(3)
      write(*,*) 'GPU FFT clock',   elapsed(4)
      write(*,*) 'Total FFT clock', elapsed(1)
!
      end subroutine multi_pout_fwd_OMP_rocFFT2
!
! ------------------------------------------------------------------
!
      subroutine multi_pout_bwd_OMP_rocFFT2(Ncomp, istack_FFTPACK,      &
     &          bwd, WK_rocFFT, WK_FFTPACK, X, elapsed)
!
      use copy_field_for_FFT
      use normalize_for_rocFFT
      use calypso_multi_rocFFT
!
      use multi_pout_FFTPACK_smp
      use normalize_for_FFTPACK
!
      integer(kind = kint), intent(in) :: Ncomp
      integer(kind = kint), intent(in) :: istack_FFTPACK(0:np_smp)
      type(calypso_rocFFT_params), intent(in), target :: bwd
!
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_FFTPACK), intent(inout) :: WK_FFTPACK
      real(kind = kreal), intent(inout) :: X(Ncomp,bwd%Nfft)
      real(kind = kreal), intent(inout) :: elapsed(4)
!
      real(kind = kreal) :: start
      real(kind = kreal) :: st_c, st_g
      integer(kind = kint) :: i, ist
!
!
      start = OMP_GET_WTIME()
      call norm_rtp_to_bwd_rocFFT(ione, Ncomp, int(bwd%Nfft), X(1,1),   &
     &    int(bwd%Ncomp), int(WK_rocFFT%Nfft_r), WK_rocFFT%X_rocFFT(1))
      call copy_rtp_spectr_to_RFFTMB                                    &
     &   (np_smp, istack_FFTPACK, WK_FFTPACK%Mmax_smp, int(bwd%Nfft),   &
     &    Ncomp, X(1,1), WK_FFTPACK%X_FFTPACK5)
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
!!   1. Create a CPU thread team
      start = OMP_GET_WTIME()
!$omp parallel
!!   2. Isolate a single thread to spawn the GPU work asynchronously
!$omp single
      st_g = OMP_GET_WTIME()
      call calypso_bwd_OpenMP_rocFFT                                    &
         (bwd%rocFFT_plan, bwd%rocFFT_wk_info,                          &
     &    bwd%Ncomp, WK_rocFFT%Nfft_r, WK_rocFFT%X_rocFFT(1))
      elapsed(4) = elapsed(4) + OMP_GET_WTIME() - st_g
!$omp end single nowait
!
!!   3. The rest of the CPU threads immediately and execute
      st_c = OMP_GET_WTIME()
      call multi_pout_RFFTMB_smp                                        &
     &   (np_smp, istack_FFTPACK, WK_FFTPACK%Mmax_smp, int(bwd%Nfft),   &
     &    WK_FFTPACK%X_FFTPACK5, WK_FFTPACK%lsave_FFTPACK,              &
     &    WK_FFTPACK%WSAVE_FFTPACK, WK_FFTPACK%WORK_FFTPACK)
      elapsed(3) = elapsed(3) + OMP_GET_WTIME() - st_c
!$omp end parallel
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call copy_pout_field_from_FFT                                     &
     &   (int(bwd%Ncomp), int(bwd%Nfft), WK_rocFFT%X_rocFFT(1),         &
     &    Ncomp, bwd%Nfft, ione, X(1,1))
      call copy_rtp_fld_from_RFFTMB                                     &
     &   (np_smp, istack_FFTPACK, WK_FFTPACK%Mmax_smp, int(bwd%Nfft),   &
     &    WK_FFTPACK%X_FFTPACK5, Ncomp, X(1,1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
      write(*,*) 'CPU FFT clock',   elapsed(3)
      write(*,*) 'GPU FFT clock',   elapsed(4)
      write(*,*) 'Total FFT clock', elapsed(1)
!
      end subroutine multi_pout_bwd_OMP_rocFFT2
!
! ------------------------------------------------------------------
!
      end program test_half_OMP_rocFFT_rtp
!
! mpif90 --offload-arch=gfx942 -mcmodel=medium -mcmodel=medium -O3 -g -fopenmp -fopenmp-target-fast  -I. -I/home/hrmatsui/src_kemo/work -I/opt/rocm-7.2.0/include/hipfort/amdgcn -DPNG_OUTPUT -DZLIB_IO -DFFTW3 -D_AMD_ROCM_ -o ./test_half_OMP_rocFFT_rtp test_half_OMP_rocFFT_rtp.f90 /home/hrmatsui/src_kemo/work/m_FFT_size.o /home/hrmatsui/src_kemo/work/t_fft_test_data.o -L/home/hrmatsui/src_kemo/work -lkemo_core -lkemo_c -lfftpack.5d -L/home/hrmatsui/local/amd/lib -lpng -L/home/hrmatsui/local/amd/lib -lz -L/home/hrmatsui/local/amd/lib -lfftw3 -L/opt/rocm-7.2.0/lib -lrocfft -lrocblas -lhipfort-amdgcn -lamdhip64
