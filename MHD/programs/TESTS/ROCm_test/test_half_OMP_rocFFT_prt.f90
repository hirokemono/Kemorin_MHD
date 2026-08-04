!
      program test_half_OMP_rocFFT_prt
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
      use multi_pin_complex_rocFFT
      use multi_pin_real_rocFFT
!
      use t_single_FFTW_wrapper
      use calypso_single_FFTW3
!
      implicit none
!
!
      real(kind = kreal), parameter :: ratio_rocFFT = 0.5
!
      character(len=kchara) :: file_name = 'prt_half_OMP_rocFFT_test.dat'
      real(kind = kreal) :: start, finish, elapsed(4)
!
      type(fft_test_data) :: ft1
!
      type(calypso_rocFFT_params), target :: fwd
      type(calypso_rocFFT_params), target :: bwd
      type(calypso_rocFFT_work), target :: WK_fft
      type(working_FFTW), target :: WK_FFTW
!
      integer(kind = kint) :: ncomp_rocFFT
      integer(kind = kint) :: ncomp_FFTW
      integer(kind = kint), allocatable :: istack_half(:)
      integer(kind = kint) :: i, nd, icou
!
      ncomp_rocFFT = ratio_rocFFT * n_field
      ncomp_FFTW =   n_field - ncomp_rocFFT
!
      write(*,'(a)') '-----  Test prt shared OpenMP rocFFT  -----'
      call init_fft_test_data(n_field, ngrid, ft1)
      call swap_fft_test_input_to_pin(ft1)
!
!      allocate(istack_half(0:np_smp))
!      istack_half(0:np_smp) = ft1%nstack(0:np_smp) / 2
!
!   Initialize Fourier transform
      start = OMP_GET_WTIME()
!      write(*,*) 'np_smp', np_smp
!      write(*,*) 'ft1%nstack',  ft1%nstack
!      write(*,*) 'istack_half', istack_half
!
      call calypso_pin_rocFFT_init(ncomp_rocFFT, ncomp_rocFFT, ngrid,   &
     &                             fwd, bwd, WK_fft)
      call init_FFTW_type2(ncomp_FFTW, ngrid, WK_FFTW)
      elapsed(1) = OMP_GET_WTIME() - start
!
      elapsed(2:4) = zero
      do icou = 1, n_loop + 1
        if(mod(icou, 20) .eq. 0) write(*,*) 'loop count: ', icou
!
        start = OMP_GET_WTIME()
!$omp parallel workshare
        ft1%s_k(1:ft1%ngrd,1:ft1%nfld) = ft1%org(1:ft1%ngrd,1:ft1%nfld)
!$omp end parallel workshare
        elapsed(3) = elapsed(3) + OMP_GET_WTIME() - start
!
!   Forward transform
        call single_pin_fwd_FFTW3_smp2(fwd, WK_fft, WK_FFTW,            &
     &      Ncomp_FFTW, ft1%ngrd, ft1%s_k(1,1), ft1%s_k(1,fwd%Ncomp+1), &
     &      elapsed(2), elapsed(3))
!
        start = OMP_GET_WTIME()
!$omp parallel workshare
        ft1%f_x(1:ft1%ngrd,1:ft1%nfld) = ft1%s_k(1:ft1%ngrd,1:ft1%nfld)
!$omp end parallel workshare
        elapsed(3) = elapsed(3) + OMP_GET_WTIME() - start
!
!   Backword transform
        call single_pin_bwd_FFTW3_smp2(bwd, WK_fft, WK_FFTW,            &
     &      Ncomp_FFTW, ft1%ngrd, ft1%f_x(1,1), ft1%f_x(1,bwd%Ncomp+1), &
     &      elapsed(2), elapsed(3))
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
      if(n_loop .eq. 1) then
        call swap_fft_test_data_to_pout(ft1)
        call write_fft_test_data(file_name, ft1)
      end if
      call dealloc_fft_test_data(ft1)
      call finalize_FFTW_type(ncomp_FFTW, WK_FFTW)
!
      write(*,'(a,i4)') 'Number of threads:  ', np_smp
      write(*, '(a,3i6)')                                               &
     &        "Num (point, field, loop):   ", ngrid, n_field, n_loop
      write(*, '("Time for Initialize:       ",1pE16.6e3)') elapsed(1)
      write(*, '("Time for OpenMP rocFFT:    ",1pE16.6e3)') elapsed(2)
      write(*, '("Time for rocFFT w/o first: ",1pE16.6e3)') elapsed(4)
      write(*, '("Time for Data copy:        ",1pE16.6e3)') elapsed(3)
      write(*, '("Total FFT:                 ",1pE16.6e3)')             &
     &                           elapsed(2) + elapsed(3)
!
      stop 'finish'
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine single_pin_fwd_FFTW3_smp2                              &
     &         (fwd, WK_fft, WK_FFTW, Ncomp_FFTW, Nfft,                 &
     &          X1, X2, elapsed_fft, elapsed_cpy)
!
      use normalize_for_rocFFT
      use calypso_multi_rocFFT
!
      use normalize_for_FFTW
!
      integer(kind = kint), intent(in) :: Ncomp_FFTW, Nfft
!
      type(calypso_rocFFT_params), intent(in), target :: fwd
!
      type(calypso_rocFFT_work), intent(inout) :: WK_fft
      type(working_FFTW), intent(inout) :: WK_FFTW
!
      real(kind = kreal), intent(inout) :: X1(Nfft,fwd%Ncomp)
      real(kind = kreal), intent(inout) :: X2(Nfft,Ncomp_FFTW)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
      integer(kind = kint) :: j, ip
!
!
        start = OMP_GET_WTIME()
!$omp parallel do private(nd,i,j)
        do nd = 1, fwd%Ncomp
          do i = 1, fwd%Nfft
            j = i + (nd-1) * WK_fft%Nfft_r
            WK_fft%X_rocFFT(j) = X1(i,nd)
          end do
          do i = fwd%Nfft+1, WK_fft%Nfft_r
            j = i + (nd-1) * WK_fft%Nfft_r
            WK_fft%X_rocFFT(j) = zero
          end do
        end do
!$omp end parallel do
        elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call calypso_fwd_OpenMP_rocFFT2                                 &
     &     (fwd, WK_fft%aNfft, WK_fft%Nfft_r, WK_fft%X_rocFFT(1),       &
     &      WK_FFTW, Nfft, Ncomp_FFTW, X2)
        elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call norm_prt_from_fwd_rocFFT                                   &
     &     (int(fwd%Ncomp), int(WK_fft%NFFT_r), WK_fft%X_rocFFT(1),     &
     &      int(fwd%Nfft), X1(1,1))
        call copy_from_prt_fwd_OMP_FFTW(Ncomp_FFTW, WK_FFTW%Nfft_c,     &
     &      WK_FFTW%C_FFTW(1,1), Nfft, X2(1,1))
        elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine single_pin_fwd_FFTW3_smp2
!
! ------------------------------------------------------------------
!
      subroutine single_pin_bwd_FFTW3_smp2                              &
     &         (bwd, WK_fft, WK_FFTW, Ncomp_FFTW, Nfft,                 &
     &          X1, X2, elapsed_fft, elapsed_cpy)
!
      use normalize_for_rocFFT
      use calypso_multi_rocFFT
!
      use normalize_for_FFTW
!
      integer(kind = kint), intent(in) :: Nfft, Ncomp_FFTW
      type(calypso_rocFFT_params), intent(in), target :: bwd
!
      type(calypso_rocFFT_work), intent(inout) :: WK_fft
      type(working_FFTW), intent(inout) :: WK_FFTW
!
      real(kind = kreal), intent(inout) :: X1(Nfft,bwd%Ncomp)
      real(kind = kreal), intent(inout) :: X2(Nfft,Ncomp_FFTW)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
      integer(kind = kint) :: j, ip
!
!
!
        start = OMP_GET_WTIME()
        call norm_prt_to_bwd_rocFFT                                     &
     &     (int(bwd%Ncomp), int(bwd%Nfft), X1(1,1),                     &
     &      int(WK_fft%Nfft_r), WK_fft%X_rocFFT(1))
        call norm_copy_to_prt_bwd_OMP_FFTW                              &
     &     (ncomp_FFTW, Ncomp_FFTW, X2(1,1),                            &
     &      WK_FFTW%Nfft_c, WK_FFTW%C_FFTW(1,1))
        elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call calypso_bwd_OpenMP_rocFFT2                                 &
           (bwd, WK_fft%Nfft_r, WK_fft%X_rocFFT(1),                     &
     &      WK_FFTW, Nfft, Ncomp_FFTW, X2)
        elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
!$omp parallel do private(nd,i,j)
        do nd = 1, bwd%Ncomp
          do i = 1, bwd%Nfft
            j = i + (nd-1) * WK_fft%Nfft_r
            X1(i,nd) = WK_fft%X_rocFFT(j)
          end do
        end do
!$omp end parallel do
        elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine single_pin_bwd_FFTW3_smp2
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine calypso_fwd_OpenMP_rocFFT2(fwd, aNfft,                 &
     &          Nfft_r, X_rocFFT, WK_FFTW, Nfft, Ncomp_FFTW, X2)
!
      use iso_c_binding
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      type(calypso_rocFFT_params), target :: fwd
      integer(c_size_t), intent(in) :: Nfft_r
      real(kind = kreal) :: aNfft
!
      type(working_FFTW), intent(inout) :: WK_FFTW
      real(kind = kreal), intent(inout), target                         &
     &                   :: X_rocFFT(Nfft_r*fwd%Ncomp) 
!
      integer(kind = kint), intent(in) :: Nfft, Ncomp_FFTW
      real(kind = kreal), intent(inout) :: X2(Nfft,Ncomp_FFTW)
!
      integer(kind = kint) :: j
      real(kind = kreal) :: start
      real(kind = kreal) :: st_c, st_g
      real(kind = kreal) :: ed_c, ed_g
!
!
!!
!!   1. Create a CPU thread team
      ed_c = 0.0d0
      ed_g = 0.0d0
      start = OMP_GET_WTIME()
!      write(*,*) 'OMP parallel start', OMP_GET_WTIME()
!$omp parallel
!!
!!   2. Isolate a single thread to spawn the GPU work asynchronously
!
!$omp single
      st_g = OMP_GET_WTIME()
!$OMP target teams distribute
      do i = 1, Nfft_r*fwd%Ncomp
        X_rocFFT(i) = aNfft * X_rocFFT(i)
      end do
!$OMP end target teams distribute
!      write(*,*) 'RocFFT normalize end', OMP_GET_WTIME() - start
!
!$OMP target enter data map(to:X_rocFFT)
!$OMP target data use_device_addr(X_rocFFT)
!      write(*,*) 'rocfft_execute start', OMP_GET_WTIME() - start
      call rocfftCheck(rocfft_execute(fwd%rocFFT_plan,                &
     &    c_loc(X_rocFFT(1)), c_null_ptr, fwd%rocFFT_wk_info))
!      write(*,*) 'rocfft_execute end', OMP_GET_WTIME() - start
!
!      write(*,*) 'hipDeviceSynchronize start', OMP_GET_WTIME() - start
      call hipCheck(hipDeviceSynchronize())
!      write(*,*) 'hipDeviceSynchronize end', OMP_GET_WTIME() - start
!
!$OMP target update from(X_rocFFT)
!$OMP end target data
!$OMP target exit data map(delete:X_rocFFT)
      ed_g = ed_g + OMP_GET_WTIME() - st_g
!$omp end single nowait
!
!!      3. The rest of the CPU threads immediately pick up and execute the remaining chunk
!      write(*,*) 'FFT loop start', OMP_GET_WTIME() - start
!$omp do private(j,st_c) reduction(+:ed_c)
        do j = 1, Ncomp_FFTW
!          if(mod(j,5) .eq. 0) write(*,*)                               &
!     &        'dfftw_execute_dft_r2c', j, OMP_GET_WTIME() - start
          st_c = OMP_GET_WTIME()
          call dfftw_execute_dft_r2c(WK_FFTW%plan_forward(j),          &
     &                               X2(1,j), WK_FFTW%C_FFTW(1,j))
          WK_FFTW%C_FFTW(1:WK_FFTW%Nfft_c,j)                           &
     &       = aNfft * WK_FFTW%C_FFTW(1:WK_FFTW%Nfft_c,j)
          ed_c = ed_c + OMP_GET_WTIME() - st_c
        end do
!$omp end do
!$omp end parallel
!
      ed_c = ed_c / dble(omp_get_max_threads())
!      write(*,*) 'CPU wall clock', ed_c
!      write(*,*) 'GPU wall clock', ed_g
!      write(*,*) 'Total wall clock', OMP_GET_WTIME() - start
!
      end subroutine calypso_fwd_OpenMP_rocFFT2
!
! ------------------------------------------------------------------
!
      subroutine calypso_bwd_OpenMP_rocFFT2                             &
     &         (fwd, Nfft_r, X_rocFFT, WK_FFTW, Nfft, Ncomp_FFTW, X2)
!
      use iso_c_binding
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      type(calypso_rocFFT_params), target :: fwd
      integer(c_size_t), intent(in) :: Nfft_r
!
      real(kind = kreal), intent(inout), target                         &
     &                   :: X_rocFFT(Nfft_r*bwd%Ncomp)
!
      integer(kind = kint), intent(in) :: Nfft, Ncomp_FFTW
      real(kind = kreal), intent(inout) :: X2(Nfft,Ncomp_FFTW)
      type(working_FFTW), intent(inout) :: WK_FFTW
!
      integer(kind = kint) :: j
      real(kind = kreal) :: start
      real(kind = kreal) :: st_c, st_g
      real(kind = kreal) :: ed_c, ed_g
!
!!   1. Create a CPU thread team
      ed_c = 0.0d0
      ed_g = 0.0d0
      start = OMP_GET_WTIME()
!      write(*,*) 'OMP parallel start', OMP_GET_WTIME()
!$omp parallel
!
!$omp single
      st_g = OMP_GET_WTIME()
!$OMP target enter data map(to:X_rocFFT)
!$OMP target data use_device_addr(X_rocFFT)
!      call rocblasCheck(rocblas_set_pointer_mode(rocblas_handle, 0))
      call rocfftCheck(rocfft_execute(bwd%rocFFT_plan,                  &
     &    c_loc(X_rocFFT(1)), c_null_ptr, bwd%rocFFT_wk_info))
!
      call hipCheck(hipDeviceSynchronize())
!$OMP target update from(X_rocFFT)
!$OMP end target data
!$OMP target exit data map(delete:X_rocFFT)
      ed_g = ed_g + OMP_GET_WTIME() - st_g
!$omp end single nowait
!
!$omp do private(j,st_c) reduction(+:ed_c)
      do j = 1, Ncomp_FFTW
        st_c = OMP_GET_WTIME()
        call dfftw_execute_dft_c2r(WK_FFTW%plan_backward(j),            &
     &                             WK_FFTW%C_FFTW(1,j), X2(1,j))
        ed_c = ed_c + OMP_GET_WTIME() - st_c
      end do
!$omp end do
!
!$omp end parallel
!
      ed_c = ed_c / dble(omp_get_max_threads())
!      write(*,*) 'CPU wall clock', ed_c
!      write(*,*) 'GPU wall clock', ed_g
!      write(*,*) 'Total wall clock', OMP_GET_WTIME() - start
!
      end subroutine calypso_bwd_OpenMP_rocFFT2
!
! ------------------------------------------------------------------
!
      subroutine init_FFTW_type2(Ncomp, Nfft, WK)
!
      integer(kind = kint), intent(in) ::  Ncomp, Nfft
!
      type(working_FFTW), intent(inout) :: WK
!
!
      call alloc_work_4_FFTW_t2(Ncomp, Nfft, WK)
      call init_single_FFTW_2(Ncomp, Nfft, WK%Nfft_c, WK%plan_forward,  &
     &                          WK%plan_backward, WK%X_FFTW, WK%C_FFTW)
!
      end subroutine init_FFTW_type2
!
! ------------------------------------------------------------------
!
      subroutine alloc_work_4_FFTW_t2(Ncomp, Nfft, WK)
!
      integer(kind = kint), intent(in) :: Ncomp, Nfft
      type(working_FFTW), intent(inout) :: WK
!
!
      allocate(WK%plan_forward(Ncomp))
      allocate(WK%plan_backward(Ncomp))
!
      WK%iflag_fft_len = Nfft*Ncomp
      WK%Nfft_c =        (Nfft+1)/2 + 1
      WK%aNfft = one / dble(Nfft)
      allocate( WK%X_FFTW(Nfft,Ncomp) )
      allocate( WK%C_FFTW(WK%Nfft_c,Ncomp) )
      WK%X_FFTW = 0.0d0
      WK%C_FFTW = 0.0d0
!
      end subroutine alloc_work_4_FFTW_t2
!
! ------------------------------------------------------------------
!
      subroutine init_single_FFTW_2(Ncomp, Nfft, NFFT_c,                &
     &          plan_forward, plan_backward, X_FFTW, C_FFTW)
!
      integer(kind = kint), intent(in) ::  Nfft, Nfft_c
      integer(kind = kint), intent(in) ::  Ncomp
!
      integer(kind = fftw_plan), intent(inout) :: plan_forward(Ncomp)
      integer(kind = fftw_plan), intent(inout) :: plan_backward(Ncomp)
      real(kind = kreal), intent(inout) :: X_FFTW(Nfft,Ncomp)
      complex(kind = fftw_complex), intent(inout)                       &
     &                                  :: C_FFTW(Nfft_c,Ncomp)
!
      integer(kind = kint) :: j
      integer :: Nfft4
!
!
      Nfft4 = int(Nfft)
      do j = 1, Ncomp
        call dfftw_plan_dft_r2c_1d(plan_forward(j), Nfft4,              &
     &      X_FFTW(1,j), C_FFTW(1,j), FFTW_KEMO_EST)
        call dfftw_plan_dft_c2r_1d(plan_backward(j), Nfft4,             &
     &      C_FFTW(1,j), X_FFTW(1,j), FFTW_KEMO_EST)
      end do
!
      end subroutine init_single_FFTW_2
!
! ------------------------------------------------------------------
!
      end program test_half_OMP_rocFFT_prt
!
! mpif90 --offload-arch=gfx942 -mcmodel=medium -mcmodel=medium -O3 -g -fopenmp -fopenmp-target-fast  -I. -I/home/hrmatsui/src_kemo/work -I/opt/rocm-7.2.0/include/hipfort/amdgcn -DPNG_OUTPUT -DZLIB_IO -DFFTW3 -D_AMD_ROCM_ -o ./test_half_OMP_rocFFT_prt test_half_OMP_rocFFT_prt.f90 /home/hrmatsui/src_kemo/work/m_FFT_size.o /home/hrmatsui/src_kemo/work/t_fft_test_data.o -L/home/hrmatsui/src_kemo/work -lkemo_core -lkemo_c -L/home/hrmatsui/local/amd/lib -lpng -L/home/hrmatsui/local/amd/lib -lz -L/home/hrmatsui/local/amd/lib -lfftw3 -L/opt/rocm-7.2.0/lib -lrocfft -lrocblas -lhipfort-amdgcn -lamdhip64
