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
     &             :: file_name = 'prt_half_OMP_rocFFT_test.dat'
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
      ncomp_CPU = n_field - ncomp_GPU
!
      write(*,'(a)') '-----  Test prt shared OpenMP rocFFT  -----'
!
      call init_fft_test_data(n_field, ngrid, ft1)
      call swap_fft_test_input_to_pin(ft1)
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
      call calypso_pin_rocFFT_init(ncomp_GPU, ncomp_GPU, ngrid,         &
     &                             fwd, bwd, WK_rocFFT)
      call init_WK_FFTPACK_t(np_smp, istack_FFTPACK,                    &
     &                       ngrid, WK_FFTPACK_T)
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
        call pin_fwd_OMP_rocFFT_FFTPACK(n_field, fwd, WK_rocFFT,        &
     &      WK_FFTPACK_T, ft1%s_k(1,1), elapsed(2:5))
!
        start = OMP_GET_WTIME()
!$omp parallel workshare
        ft1%f_x(1:ft1%ngrd,1:ft1%nfld) = ft1%s_k(1:ft1%ngrd,1:ft1%nfld)
!$omp end parallel workshare
        elapsed(3) = elapsed(3) + OMP_GET_WTIME() - start
!
!   Backword transform
        call pin_bwd_OMP_rocFFT_FFTPACK(n_field, bwd, WK_rocFFT,        &
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
      if(n_loop .eq. 1) then
        call swap_fft_test_data_to_pout(ft1)
        call write_fft_test_data(file_name, ft1)
      end if
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
      subroutine pin_fwd_OMP_rocFFT_FFTPACK(Ncomp, fwd, WK_rocFFT,      &
     &                                      WK_FFTPACK, X, elapsed)
!
      use iso_c_binding
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      use copy_field_for_FFT
!
      integer(kind = kint), intent(in) :: Ncomp
      type(calypso_rocFFT_params), target :: fwd
!
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_FFTPACK), intent(inout) :: WK_FFTPACK
!
      real(kind = kreal), intent(inout) :: X(fwd%Nfft,Ncomp)
      real(kind = kreal), intent(inout) :: elapsed(4)
!
      real(kind = kreal) :: start, st_c, st_g
!
!
      start = OMP_GET_WTIME()
      call sel_copy_pin_field_to_FFT(int(fwd%Ncomp), int(fwd%Nfft),     &
     &    X(1,1), int(WK_rocFFT%Nfft_r), WK_rocFFT%X_rocFFT(1))
      call swap_prt_fld_to_RFFTMF(WK_FFTPACK%Nplan_FFTPACK,             &
     &   WK_FFTPACK%istack_FFTPACK, WK_FFTPACK%Mmax_smp, int(fwd%Nfft), &
     &   Ncomp, X(1,1), WK_FFTPACK%X_FFTPACK5)
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
!!   1. Create a CPU thread team
      start = OMP_GET_WTIME()
!      write(*,*) 'OMP parallel start', OMP_GET_WTIME()
!$omp parallel
!!
!!   2. Isolate a single thread to spawn the GPU work asynchronously
!
!$omp single
      st_g = OMP_GET_WTIME()
      call calypso_fwd_OpenMP_rocFFT                                    &
     &   (fwd%rocFFT_plan, fwd%rocFFT_wk_info, fwd%Ncomp,               &
     &    WK_rocFFT%aNfft, WK_rocFFT%Nfft_r, WK_rocFFT%X_rocFFT(1))
      elapsed(4) = elapsed(4) + OMP_GET_WTIME() - st_g
!$omp end single nowait
!
!!   3. The rest of the CPU threads immediately and execute
!      write(*,*) 'FFT loop start', OMP_GET_WTIME() - start
      st_c = OMP_GET_WTIME()
      call multi_pin_RFFTMF_smp(WK_FFTPACK%Nplan_FFTPACK,               &
     &   WK_FFTPACK%istack_FFTPACK, WK_FFTPACK%Mmax_smp, int(fwd%Nfft), &
     &   WK_FFTPACK%X_FFTPACK5, WK_FFTPACK%lsave_FFTPACK,               &
     &   WK_FFTPACK%WSAVE_FFTPACK, WK_FFTPACK%WORK_FFTPACK)
      elapsed(3) = elapsed(3) + OMP_GET_WTIME() - st_c
!$omp end parallel
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call norm_prt_from_fwd_rocFFT                                     &
     &   (int(fwd%Ncomp), int(WK_rocFFT%NFFT_r), WK_rocFFT%X_rocFFT,    &
     &    int(fwd%Nfft), X(1,1))
      call swap_prt_spectr_from_RFFTMF(WK_FFTPACK%Nplan_FFTPACK,        &
     &   WK_FFTPACK%istack_FFTPACK, WK_FFTPACK%Mmax_smp, int(fwd%Nfft), &
     &   WK_FFTPACK%X_FFTPACK5, Ncomp, X(1,1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
      write(*,*) 'CPU FFT clock',   elapsed(3)
      write(*,*) 'GPU FFT clock',   elapsed(4)
      write(*,*) 'Total FFT clock', elapsed(1)
!
      end subroutine pin_fwd_OMP_rocFFT_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine pin_fwd_OMP_rocFFT_ISPACK1(Ncomp, fwd, WK_rocFFT,      &
     &                                      WK_ISPACK1, X, elapsed)
!
      use iso_c_binding
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      use multi_pin_ISPACK1_smp
      use copy_field_for_FFT
      use swap_prt_data_for_ISPACK
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: Ncomp
      type(calypso_rocFFT_params), target :: fwd
!
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_ISPACK), intent(inout) :: WK_ISPACK1
!
      real(kind = kreal), intent(inout) :: X(fwd%Nfft,Ncomp)
      real(kind = kreal), intent(inout) :: elapsed(4)
!
      real(kind = kreal) :: start, st_c, st_g
!
!
      start = OMP_GET_WTIME()
      call sel_copy_pin_field_to_FFT(int(fwd%Ncomp), int(fwd%Nfft),     &
     &    X(1,1), int(WK_rocFFT%Nfft_r), WK_rocFFT%X_rocFFT(1))
      call swap_prt_fld_to_FXRTFA(WK_ISPACK1%Nplan_ISPACK,              &
     &    WK_ISPACK1%istack_ISPACK, cast_long(WK_ISPACK1%Mmax_smp),     &
     &    fwd%Nfft, cast_long(Ncomp), X(1,1), WK_ISPACK1%X_ispack(1,1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
!!   1. Create a CPU thread team
      start = OMP_GET_WTIME()
!      write(*,*) 'OMP parallel start', OMP_GET_WTIME()
!$omp parallel
!!
!!   2. Isolate a single thread to spawn the GPU work asynchronously
!
!$omp single
      st_g = OMP_GET_WTIME()
      call calypso_fwd_OpenMP_rocFFT                                    &
     &   (fwd%rocFFT_plan, fwd%rocFFT_wk_info, fwd%Ncomp,               &
     &    WK_rocFFT%aNfft, WK_rocFFT%Nfft_r, WK_rocFFT%X_rocFFT(1))
      elapsed(4) = elapsed(4) + OMP_GET_WTIME() - st_g
!$omp end single nowait
!
!!   3. The rest of the CPU threads immediately and execute
!      write(*,*) 'FFT loop start', OMP_GET_WTIME() - start
      st_c = OMP_GET_WTIME()
      call multi_pin_FTTRUF_smp(WK_ISPACK1%Nplan_ISPACK,                &
     &    WK_ISPACK1%istack_ISPACK, WK_ISPACK1%Mmax_smp, int(fwd%Nfft), &
     &    WK_ISPACK1%X_ispack, WK_ISPACK1%IT_ispack,                    &
     &    WK_ISPACK1%T_ispack, WK_ISPACK1%WORK_ispack)
      elapsed(3) = elapsed(3) + OMP_GET_WTIME() - st_c
!$omp end parallel
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call norm_prt_from_fwd_rocFFT                                     &
     &   (int(fwd%Ncomp), int(WK_rocFFT%NFFT_r), WK_rocFFT%X_rocFFT,    &
     &    int(fwd%Nfft), X(1,1))
      call swap_prt_spectr_from_FXRTFA(WK_ISPACK1%Nplan_ISPACK,         &
     &    WK_ISPACK1%istack_ISPACK,cast_long(WK_ISPACK1%Mmax_smp),      &
     &    fwd%Nfft, WK_ISPACK1%X_ispack(1,1), cast_long(Ncomp), X(1,1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
      write(*,*) 'CPU FFT clock',   elapsed(3)
      write(*,*) 'GPU FFT clock',   elapsed(4)
      write(*,*) 'Total FFT clock', elapsed(1)
!
      end subroutine pin_fwd_OMP_rocFFT_ISPACK1
!
! ------------------------------------------------------------------
!
      subroutine pin_fwd_OMP_rocFFT_ISPACK3(Ncomp, fwd, WK_rocFFT,      &
     &                                      WK_ISPACK3, X, elapsed)
!
      use iso_c_binding
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      use copy_field_for_FFT
      use swap_prt_data_for_ISPACK
      use multi_pin_ISPACK3_smp
!
      integer(kind = kint), intent(in) :: Ncomp
      type(calypso_rocFFT_params), target :: fwd
!
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_ISPACK3), intent(inout) :: WK_ISPACK3
!
      real(kind = kreal), intent(inout) :: X(fwd%Nfft,Ncomp)
      real(kind = kreal), intent(inout) :: elapsed(4)
!
      real(kind = kreal) :: start, st_c, st_g
!
!
      start = OMP_GET_WTIME()
      call sel_copy_pin_field_to_FFT(int(fwd%Ncomp), int(fwd%Nfft),     &
     &    X(1,1), int(WK_rocFFT%Nfft_r), WK_rocFFT%X_rocFFT(1))
      call swap_prt_fld_to_FXRTFA(WK_ISPACK3%Nplan_ISPACK3,             &
     &    WK_ISPACK3%istack_ISPACK3, WK_ISPACK3%Mmax_smp, fwd%Nfft,     &
     &    Ncomp, X(1,1), WK_ISPACK3%X_ispack(1,1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
!!   1. Create a CPU thread team
      start = OMP_GET_WTIME()
!      write(*,*) 'OMP parallel start', OMP_GET_WTIME()
!$omp parallel
!!
!!   2. Isolate a single thread to spawn the GPU work asynchronously
!
!$omp single
      st_g = OMP_GET_WTIME()
      call calypso_fwd_OpenMP_rocFFT                                    &
     &   (fwd%rocFFT_plan, fwd%rocFFT_wk_info, fwd%Ncomp,               &
     &    WK_rocFFT%aNfft, WK_rocFFT%Nfft_r, WK_rocFFT%X_rocFFT(1))
      elapsed(4) = elapsed(4) + OMP_GET_WTIME() - st_g
!$omp end single nowait
!
!!   3. The rest of the CPU threads immediately and execute
!      write(*,*) 'FFT loop start', OMP_GET_WTIME() - start
      st_c = OMP_GET_WTIME()
      call multi_pin_FXRTFA_smp(WK_ISPACK3%Nplan_ISPACK3,               &
     &    WK_ISPACK3%istack_ISPACK3, WK_ISPACK3%Mmax_smp, fwd%Nfft,     &
     &    WK_ISPACK3%X_ispack, WK_ISPACK3%IT_ispack,                    &
     &    WK_ISPACK3%T_ispack)
      elapsed(3) = elapsed(3) + OMP_GET_WTIME() - st_c
!$omp end parallel
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call norm_prt_from_fwd_rocFFT                                     &
     &   (int(fwd%Ncomp), int(WK_rocFFT%NFFT_r), WK_rocFFT%X_rocFFT,    &
     &    int(fwd%Nfft), X(1,1))
      call swap_prt_spectr_from_FXRTFA(WK_ISPACK3%Nplan_ISPACK3,        &
     &    WK_ISPACK3%istack_ISPACK3, WK_ISPACK3%Mmax_smp,               &
     &    fwd%Nfft, WK_ISPACK3%X_ispack(1,1), Ncomp, X(1,1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
      write(*,*) 'CPU FFT clock',   elapsed(3)
      write(*,*) 'GPU FFT clock',   elapsed(4)
      write(*,*) 'Total FFT clock', elapsed(1)
!
      end subroutine pin_fwd_OMP_rocFFT_ISPACK3
!
! ------------------------------------------------------------------
!
      subroutine pin_fwd_OMP_rocFFT_FFTW(Ncomp, Ncomp_CPU,              &
     &          istack_FFTW, fwd, WK_rocFFT, WK_FFTW, X, elapsed)
!
      use iso_c_binding
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      use copy_field_for_FFT
      use swap_prt_data_for_ISPACK
      use multi_pin_ISPACK3_smp
!
      integer(kind = kint), intent(in) :: Ncomp, Ncomp_CPU
      integer(kind = kint), intent(in) :: istack_FFTW(0:np_smp)
      type(calypso_rocFFT_params), target :: fwd
!
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_mul_FFTW), intent(inout) :: WK_FFTW
!
      real(kind = kreal), intent(inout) :: X(fwd%Nfft,Ncomp)
      real(kind = kreal), intent(inout) :: elapsed(4)
!
      real(kind = kreal) :: start, st_c, st_g
!
!
      start = OMP_GET_WTIME()
      call sel_copy_pin_field_to_FFT(int(fwd%Ncomp), int(fwd%Nfft),     &
     &    X(1,1), int(WK_rocFFT%Nfft_r), WK_rocFFT%X_rocFFT(1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
!!   1. Create a CPU thread team
      start = OMP_GET_WTIME()
!      write(*,*) 'OMP parallel start', OMP_GET_WTIME()
!$omp parallel
!!
!!   2. Isolate a single thread to spawn the GPU work asynchronously
!
!$omp single
      st_g = OMP_GET_WTIME()
      call calypso_fwd_OpenMP_rocFFT                                    &
     &   (fwd%rocFFT_plan, fwd%rocFFT_wk_info, fwd%Ncomp,               &
     &    WK_rocFFT%aNfft, WK_rocFFT%Nfft_r, WK_rocFFT%X_rocFFT(1))
      elapsed(4) = elapsed(4) + OMP_GET_WTIME() - st_g
!$omp end single nowait
!
!!   3. The rest of the CPU threads immediately and execute
!      write(*,*) 'FFT loop start', OMP_GET_WTIME() - start
      st_c = OMP_GET_WTIME()
      call multi_pin_fwd_FFTW3_smp(WK_FFTW%plan_fowd_mul,               &
     &    WK_FFTW%Nplan_FFTW, WK_FFTW%istack_FFTW, Ncomp, fwd%Nfft,     &
     &    WK_FFTW%Nfft_c, X(1,fwd%Ncomp+1), WK_FFTW%C_FFTW_mul(1,1))
      elapsed(3) = elapsed(3) + OMP_GET_WTIME() - st_c
!$omp end parallel
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call norm_prt_from_fwd_rocFFT                                     &
     &   (int(fwd%Ncomp), int(WK_rocFFT%NFFT_r), WK_rocFFT%X_rocFFT,    &
     &    int(fwd%Nfft), X(1,1))
!
      call normalize_fwd_OMP_FFTW(WK%aNfft, Ncomp_CPU, WK_FFTW%Nfft_c,  &
     &                            WK_FFTW%C_FFTW_mul(1,1))
      call copy_from_prt_fwd_OMP_FFTW                                   &
     &   (Ncomp_CPU, WK_FFTW%Nfft_c, C_FFTW(1,1),                       &
     &    Ncomp_CPU, Nfft, X(1,fwd%Ncomp+1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
      write(*,*) 'CPU FFT clock',   elapsed(3)
      write(*,*) 'GPU FFT clock',   elapsed(4)
      write(*,*) 'Total FFT clock', elapsed(1)
!
      end subroutine pin_fwd_OMP_rocFFT_FFTW
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine pin_bwd_OMP_rocFFT_FFTPACK(Ncomp, bwd, WK_rocFFT,      &
     &                                      WK_FFTPACK, X, elapsed)
!
      use iso_c_binding
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      use copy_field_for_FFT
!
      integer(kind = kint), intent(in) :: Ncomp
      type(calypso_rocFFT_params), target :: bwd
!
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_FFTPACK), intent(inout) :: WK_FFTPACK
      real(kind = kreal), intent(inout) :: X(bwd%Nfft,Ncomp)
      real(kind = kreal), intent(inout) :: elapsed(4)
!
      real(kind = kreal) :: start
      real(kind = kreal) :: st_c, st_g
!
      start = OMP_GET_WTIME()
      call norm_prt_to_bwd_rocFFT                                       &
     &   (int(bwd%Ncomp), int(bwd%Nfft), X(1,1),                        &
     &    int(WK_rocFFT%Nfft_r), WK_rocFFT%X_rocFFT(1))
      call swap_prt_spectr_to_RFFTMB(WK_FFTPACK%Nplan_FFTPACK,          &
     &   WK_FFTPACK%istack_FFTPACK, WK_FFTPACK%Mmax_smp, int(bwd%Nfft), &
     &   Ncomp, X(1,1), WK_FFTPACK%X_FFTPACK5)
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
!      write(*,*) 'OMP parallel start', OMP_GET_WTIME()
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
      call multi_pin_RFFTMB_smp(WK_FFTPACK%Nplan_FFTPACK,               &
     &   WK_FFTPACK%istack_FFTPACK, WK_FFTPACK%Mmax_smp, int(bwd%Nfft), &
     &   WK_FFTPACK%X_FFTPACK5, WK_FFTPACK%lsave_FFTPACK,               &
     &   WK_FFTPACK%WSAVE_FFTPACK, WK_FFTPACK%WORK_FFTPACK)
      elapsed(3) = elapsed(3) + OMP_GET_WTIME() - st_c
!$omp end parallel
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call sel_copy_pin_field_from_FFT                                  &
     &   (int(bwd%Ncomp), int(WK_rocFFT%Nfft_r), WK_rocFFT%X_rocFFT(1), &
     &    int(bwd%Nfft), X(1,1))
      call swap_prt_fld_from_RFFTMB(WK_FFTPACK%Nplan_FFTPACK,           &
     &   WK_FFTPACK%istack_FFTPACK, WK_FFTPACK%Mmax_smp, int(bwd%Nfft), &
     &   WK_FFTPACK%X_FFTPACK5, Ncomp, X(1,1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
      write(*,*) 'CPU FFT clock',   elapsed(3)
      write(*,*) 'GPU FFT clock',   elapsed(4)
      write(*,*) 'Total FFT clock', elapsed(1)
!
      end subroutine pin_bwd_OMP_rocFFT_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine pin_bwd_OMP_rocFFT_ISPACK1(Ncomp, bwd, WK_rocFFT,      &
     &                                      WK_FFTPACK, X, elapsed)
!
      use iso_c_binding
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      use copy_field_for_FFT
      use swap_prt_data_for_ISPACK
      use transfer_to_long_integers
      use multi_pin_ISPACK1_smp
!
      integer(kind = kint), intent(in) :: Ncomp
      type(calypso_rocFFT_params), target :: bwd
!
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_ISPACK), intent(inout) :: WK_ISPACK1
      real(kind = kreal), intent(inout) :: X(bwd%Nfft,Ncomp)
      real(kind = kreal), intent(inout) :: elapsed(4)
!
      real(kind = kreal) :: start
      real(kind = kreal) :: st_c, st_g
!
      start = OMP_GET_WTIME()
      call norm_prt_to_bwd_rocFFT                                       &
     &   (int(bwd%Ncomp), int(bwd%Nfft), X(1,1),                        &
     &    int(WK_rocFFT%Nfft_r), WK_rocFFT%X_rocFFT(1))
      call swap_prt_spectr_to_FXRTBA(WK_ISPACK1%Nplan_ISPACK,           &
     &    WK_ISPACK1%istack_ISPACK, cast_long(WK_ISPACK1%Mmax_smp),     &
     &    bwd%Nfft, cast_long(Ncomp), X(1,1), WK_ISPACK1%X_ispack(1,1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
!      write(*,*) 'OMP parallel start', OMP_GET_WTIME()
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
     &          )
      call multi_pin_FTTRUB_smp(WK_ISPACK1%Nplan_ISPACK,                &
     &    WK_ISPACK1%istack_ISPACK, WK_ISPACK1%Mmax_smp, int(bwd%Nfft), &
     &    WK_ISPACK1%X_ispack, WK_ISPACK1%IT_ispack,                    &
     &    WK_ISPACK1%T_ispack, WK_ISPACK1%WORK_ispack)
      elapsed(3) = elapsed(3) + OMP_GET_WTIME() - st_c
!$omp end parallel
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call sel_copy_pin_field_from_FFT                                  &
     &   (int(bwd%Ncomp), int(WK_rocFFT%Nfft_r), WK_rocFFT%X_rocFFT(1), &
     &    int(bwd%Nfft), X(1,1))
      call swap_prt_fld_from_FXRTBA(WK_ISPACK1%Nplan_ISPACK,            &
     &    WK_ISPACK1%istack_ISPACK, cast_long(WK_ISPACK1%Mmax_smp),     &
     &    bwd%Nfft, WK_ISPACK1%X_ispack(1,1), cast_long(Ncomp), X(1,1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
      write(*,*) 'CPU FFT clock',   elapsed(3)
      write(*,*) 'GPU FFT clock',   elapsed(4)
      write(*,*) 'Total FFT clock', elapsed(1)
!
      end subroutine pin_bwd_OMP_rocFFT_ISPACK1
!
! ------------------------------------------------------------------
!
      subroutine pin_bwd_OMP_rocFFT_ISPACK3(Ncomp, Ncomp_CPU,           &
     &          bwd, WK_rocFFT, WK_FFTPACK, X, elapsed)
!
      use iso_c_binding
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      use swap_prt_data_for_ISPACK
      use copy_field_for_FFT
      use multi_pin_ISPACK3_smp
!
      integer(kind = kint), intent(in) :: Ncomp, Ncomp_CPU
      type(calypso_rocFFT_params), target :: bwd
!
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_ISPACK3), intent(inout) :: WK_ISPACK3
!
      real(kind = kreal), intent(inout) :: X(bwd%Nfft,Ncomp)
      real(kind = kreal), intent(inout) :: elapsed(4)
!
      real(kind = kreal) :: start
      real(kind = kreal) :: st_c, st_g
!
      start = OMP_GET_WTIME()
      call norm_prt_to_bwd_rocFFT                                       &
     &   (int(bwd%Ncomp), int(bwd%Nfft), X(1,1),                        &
     &    int(WK_rocFFT%Nfft_r), WK_rocFFT%X_rocFFT(1))
      call swap_prt_spectr_to_FXRTBA(WK_ISPACK3%Nplan_ISPACK3,          &
     &    WK_ISPACK3%istack_ISPACK3, cast_long(WK_ISPACK3%Mmax_smp),    &
     &    bwd%Nfft, cast_long(Ncomp), X(1,1), WK_ISPACK3%X_ispack(1,1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
!      write(*,*) 'OMP parallel start', OMP_GET_WTIME()
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
      call multi_pin_FXRTBA_smp(WK_ISPACK3%Nplan_ISPACK3,               &
     &    WK_ISPACK3%istack_ISPACK3, WK_ISPACK3%Mmax_smp, bwd%Nfft,     &
     &    WK_ISPACK3%X_ispack, WK_ISPACK3%IT_ispack,                    &
     &    WK_ISPACK3%T_ispack)
      elapsed(3) = elapsed(3) + OMP_GET_WTIME() - st_c
!$omp end parallel
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call sel_copy_pin_field_from_FFT                                  &
     &   (int(bwd%Ncomp), int(WK_rocFFT%Nfft_r), WK_rocFFT%X_rocFFT(1), &
     &    int(bwd%Nfft), X(1,1))
      call swap_prt_fld_from_FXRTBA(WK_ISPACK3%Nplan_ISPACK3,           &
     &    WK_ISPACK3%istack_ISPACK3, cast_long(WK_ISPACK3%Mmax_smp),    &
     &    bwd%Nfft, WK_ISPACK3%X_ispack(1,1),                           &
     &    cast_long(Ncomp_CPU), X(1,bwd%Ncomp+1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
      write(*,*) 'CPU FFT clock',   elapsed(3)
      write(*,*) 'GPU FFT clock',   elapsed(4)
      write(*,*) 'Total FFT clock', elapsed(1)
!
      end subroutine pin_bwd_OMP_rocFFT_ISPACK3
!
! ------------------------------------------------------------------
!
      subroutine pin_bwd_OMP_rocFFT_FFTW3(Ncomp, Ncomp_CPU,             &
     &          bwd, WK_rocFFT, WK_FFTW, X, elapsed)
!
      use iso_c_binding
      use hipfort
      use hipfort_check
      use hipfort_rocfft
!
      use swap_prt_data_for_ISPACK
      use copy_field_for_FFT
      use multi_pin_ISPACK3_smp
!
      integer(kind = kint), intent(in) :: Ncomp, Ncomp_CPU
      type(calypso_rocFFT_params), target :: bwd
!
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_mul_FFTW), intent(inout) :: WK_FFTW
      real(kind = kreal), intent(inout) :: X(bwd%Nfft,Ncomp)
      real(kind = kreal), intent(inout) :: elapsed(4)
!
      real(kind = kreal) :: start
      real(kind = kreal) :: st_c, st_g
!
      start = OMP_GET_WTIME()
      call norm_prt_to_bwd_rocFFT                                       &
     &   (int(bwd%Ncomp), int(bwd%Nfft), X(1,1),                        &
     &    int(WK_rocFFT%Nfft_r), WK_rocFFT%X_rocFFT(1))
      call norm_copy_to_prt_bwd_OMP_FFTW                                &
     &   (Ncomp_CPU, Nfft, X(1,bwd%Ncomp+1),                            &
     &    Ncomp_CPU, NFFT_c, WK_FFTW%C_FFTW_mul(1,1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
!      write(*,*) 'OMP parallel start', OMP_GET_WTIME()
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
      call multi_pin_bwd_FFTW3_smp(WK_FFTW%plan_back_mul,               &
     &    WK_FFTW%Nplan_FFTW, WK_FFTW%istack_FFTW,                      &
     &    Ncomp_CPU, WK_FFTW%Nfft_c, WK_FFTW%C_FFTW_mul(1,1),           &
     &    bwd%Nfft, X(1,bwd%Ncomp+1))
      elapsed(3) = elapsed(3) + OMP_GET_WTIME() - st_c
!$omp end parallel
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call sel_copy_pin_field_from_FFT                                  &
     &   (int(bwd%Ncomp), int(WK_rocFFT%Nfft_r), WK_rocFFT%X_rocFFT(1), &
     &    int(bwd%Nfft), X(1,1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
      write(*,*) 'CPU FFT clock',   elapsed(3)
      write(*,*) 'GPU FFT clock',   elapsed(4)
      write(*,*) 'Total FFT clock', elapsed(1)
!
      end subroutine pin_bwd_OMP_rocFFT_FFTW3
!
! ------------------------------------------------------------------
!
      end program test_half_OMP_rocFFT_prt
!
! mpif90 --offload-arch=gfx942 -mcmodel=medium -mcmodel=medium -O3 -g -fopenmp -fopenmp-target-fast  -I. -I/home/hrmatsui/src_kemo/work -I/opt/rocm-7.2.0/include/hipfort/amdgcn -DPNG_OUTPUT -DZLIB_IO -DFFTW3 -D_AMD_ROCM_ -o ./test_half_OMP_rocFFT_prt test_half_OMP_rocFFT_prt.f90 /home/hrmatsui/src_kemo/work/m_FFT_size.o /home/hrmatsui/src_kemo/work/t_fft_test_data.o -L/home/hrmatsui/src_kemo/work -lkemo_core -lkemo_c -L/home/hrmatsui/local/amd/lib -lpng -L/home/hrmatsui/local/amd/lib -lz -L/home/hrmatsui/local/amd/lib -lfftw3 -L/opt/rocm-7.2.0/lib -lrocfft -lrocblas -lhipfort-amdgcn -lamdhip64
