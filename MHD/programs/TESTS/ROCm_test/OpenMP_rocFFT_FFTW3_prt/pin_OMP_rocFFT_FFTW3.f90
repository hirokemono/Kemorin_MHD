!>@file   pin_OMP_rocFFT_FFTW3.f90
!!@brief  module pin_OMP_rocFFT_FFTW3
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2026
!!
!!
!>@brief  FFT by OpenMP rocFFT and FFTW with innermost data series
!!
!!@verbatim
!!      subroutine init_pin_OMP_rocFFT_FFTW3                            &
!!     &         (Ncomp, Ncomp_GPU, Ncomp_CPU, Nfft, Nsmp,              &
!!     &          fwd_rocFFT, bwd_rocFFT, WK_rocFFT, WK_FFTW)
!!      subroutine finalize_OMP_rocFFT_FFTW3                            &
!!     &         (Nsmp, fwd_rocFFT, bwd_rocFFT, WK_rocFFT, WK_FFTW)
!!        integer(kind = kint), intent(in) :: Nsmp
!!        integer(kind = kint), intent(in) :: Ncomp, Ncomp_GPU, Ncomp_CPU
!!        integer(kind = kint), intent(in) :: Nfft
!!        type(calypso_rocFFT_params), intent(inout) :: fwd_rocFFT
!!        type(calypso_rocFFT_params), intent(inout) :: bwd_rocFFT
!!        type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!!        type(working_mul_FFTW), intent(inout) :: WK_FFTW
!!
!!      subroutine pin_fwd_OMP_rocFFT_FFTW3(Ncomp, Ncomp_CPU,           &
!!     &          fwd_rocFFT, WK_rocFFT, WK_FFTW, X, elapsed)
!!        integer(kind = kint), intent(in) :: Ncomp, Ncomp_CPU
!!        type(calypso_rocFFT_params), target :: fwd_rocFFT
!!        type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!!        type(working_mul_FFTW), intent(inout) :: WK_FFTW
!!        real(kind = kreal), intent(inout) :: X(fwd_rocFFT%Nfft,Ncomp)
!!        real(kind = kreal), intent(inout) :: elapsed(4)
!!      subroutine pin_bwd_OMP_rocFFT_FFTW3(Ncomp, Ncomp_CPU,           &
!!     &          bwd_rocFFT, WK_rocFFT, WK_FFTW, X, elapsed)
!!        integer(kind = kint), intent(in) :: Ncomp, Ncomp_CPU
!!        type(calypso_rocFFT_params), target :: bwd_rocFFT
!!        type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!!        type(working_mul_FFTW), intent(inout) :: WK_FFTW
!!        real(kind = kreal), intent(inout) :: X(bwd_rocFFT%Nfft,Ncomp)
!!        real(kind = kreal), intent(inout) :: elapsed(4)
!!@endverbatim
      module pin_OMP_rocFFT_FFTW3
!
      use t_multi_rocFFT_wrapper
      use t_multi_FFTW_wrapper
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_pin_OMP_rocFFT_FFTW3                              &
     &         (Ncomp, Ncomp_GPU, Ncomp_CPU, Nfft, Nsmp,                &
     &          fwd_rocFFT, bwd_rocFFT, WK_rocFFT, WK_FFTW)
!
      use calypso_rocFFT_init
      use calypso_multi_FFTW3
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: Nsmp
      integer(kind = kint), intent(in) :: Ncomp, Ncomp_GPU, Ncomp_CPU
      integer(kind = kint), intent(in) :: Nfft
!
      type(calypso_rocFFT_params), intent(inout) :: fwd_rocFFT
      type(calypso_rocFFT_params), intent(inout) :: bwd_rocFFT
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_mul_FFTW), intent(inout) :: WK_FFTW
!
      integer(kind = kint), allocatable :: istack_smp(:)
      integer(kind = kint) :: max_4_smp = 0
!
!
      allocate(istack_smp(0:Nsmp))
      istack_smp(0:Nsmp) = 0
!
      call count_number_4_smp(Nsmp, (Ncomp_GPU+1), Ncomp,               &
     &                        istack_smp, max_4_smp)
!
      call calypso_pin_rocFFT_init(Ncomp_GPU, Ncomp_GPU, Nfft,          &
     &                             fwd_rocFFT, bwd_rocFFT, WK_rocFFT)
      call init_FFTW_mul_type(Nsmp, istack_smp, Nfft, WK_FFTW)
      deallocate(istack_smp)
!
      end subroutine init_pin_OMP_rocFFT_FFTW3
!
! ------------------------------------------------------------------
!
      subroutine finalize_OMP_rocFFT_FFTW3                              &
     &         (Nsmp, fwd_rocFFT, bwd_rocFFT, WK_rocFFT, WK_FFTW)
!
      use calypso_rocFFT_init
      use calypso_multi_FFTW3
!
      integer(kind = kint), intent(in) :: Nsmp
      type(calypso_rocFFT_params), intent(inout) :: fwd_rocFFT
      type(calypso_rocFFT_params), intent(inout) :: bwd_rocFFT
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_mul_FFTW), intent(inout) :: WK_FFTW
!
!
      call calypso_rocFFT_fin(fwd_rocFFT, bwd_rocFFT, WK_rocFFT)
      call finalize_FFTW_mul_type(Nsmp, WK_FFTW)
!
      end subroutine finalize_OMP_rocFFT_FFTW3
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine pin_fwd_OMP_rocFFT_FFTW3(Ncomp, Ncomp_CPU,             &
     &          fwd_rocFFT, WK_rocFFT, WK_FFTW, X, elapsed)
!
      use calypso_multi_rocFFT
      use calypso_multi_FFTW3
      use normalize_for_rocFFT
      use normalize_for_FFTW
      use copy_field_for_FFT
!
      integer(kind = kint), intent(in) :: Ncomp, Ncomp_CPU
      type(calypso_rocFFT_params), target :: fwd_rocFFT
!
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_mul_FFTW), intent(inout) :: WK_FFTW
!
      real(kind = kreal), intent(inout) :: X(fwd_rocFFT%Nfft,Ncomp)
      real(kind = kreal), intent(inout) :: elapsed(4)
!
      real(kind = kreal) :: start, st_c, st_g
!
!
      start = OMP_GET_WTIME()
      call sel_copy_pin_field_to_FFT                                    &
     &   (int(fwd_rocFFT%Ncomp), int(fwd_rocFFT%Nfft),                  &
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
      call calypso_fwd_OpenMP_rocFFT(fwd_rocFFT%rocFFT_plan,            &
     &    fwd_rocFFT%rocFFT_wk_info, fwd_rocFFT%Ncomp, WK_rocFFT%aNfft, &
     &    WK_rocFFT%Nfft_r, WK_rocFFT%X_rocFFT(1))
      elapsed(4) = elapsed(4) + OMP_GET_WTIME() - st_g
!$omp end single nowait
!
!!   3. The rest of the CPU threads immediately and execute
!      write(*,*) 'FFT loop start', OMP_GET_WTIME() - start
      st_c = OMP_GET_WTIME()
      call multi_fwd_FFTW3_smp(WK_FFTW%plan_mul_fwd,                    &
     &    WK_FFTW%Nplan_FFTW, WK_FFTW%istack_FFTW,                      &
     &    Ncomp, int(fwd_rocFFT%Nfft), X(1,fwd_rocFFT%Ncomp+1),         &
     &    WK_FFTW%Nfft_c, WK_FFTW%C_FFTW_mul(1,1))
      elapsed(3) = elapsed(3) + OMP_GET_WTIME() - st_c
!$omp end parallel
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call norm_prt_from_fwd_rocFFT(int(fwd_rocFFT%Ncomp),              &
     &    int(WK_rocFFT%NFFT_r), WK_rocFFT%X_rocFFT,                    &
     &    int(fwd_rocFFT%Nfft), X(1,1))
!
      call normalize_fwd_OMP_FFTW(WK_FFTW%aNfft,                        &
     &    Ncomp_CPU, WK_FFTW%Nfft_c, WK_FFTW%C_FFTW_mul(1,1))
      call copy_from_prt_fwd_OMP_FFTW                                   &
     &   (Ncomp_CPU, WK_FFTW%Nfft_c, WK_FFTW%C_FFTW_mul(1,1),           &
     &    Ncomp_CPU, int(fwd_rocFFT%Nfft), X(1,fwd_rocFFT%Ncomp+1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
      end subroutine pin_fwd_OMP_rocFFT_FFTW3
!
! ------------------------------------------------------------------
!
      subroutine pin_bwd_OMP_rocFFT_FFTW3(Ncomp, Ncomp_CPU,             &
     &          bwd_rocFFT, WK_rocFFT, WK_FFTW, X, elapsed)
!
      use calypso_multi_rocFFT
      use calypso_multi_FFTW3
      use normalize_for_rocFFT
      use normalize_for_FFTW
      use copy_field_for_FFT
!
      integer(kind = kint), intent(in) :: Ncomp, Ncomp_CPU
      type(calypso_rocFFT_params), target :: bwd_rocFFT
!
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_mul_FFTW), intent(inout) :: WK_FFTW
      real(kind = kreal), intent(inout) :: X(bwd_rocFFT%Nfft,Ncomp)
      real(kind = kreal), intent(inout) :: elapsed(4)
!
      real(kind = kreal) :: start
      real(kind = kreal) :: st_c, st_g
!
      start = OMP_GET_WTIME()
      call norm_prt_to_bwd_rocFFT                                       &
     &   (int(bwd_rocFFT%Ncomp), int(bwd_rocFFT%Nfft), X(1,1),          &
     &    int(WK_rocFFT%Nfft_r), WK_rocFFT%X_rocFFT(1))
      call norm_copy_to_prt_bwd_OMP_FFTW                                &
     &   (Ncomp_CPU, int(bwd_rocFFT%Nfft), X(1,bwd_rocFFT%Ncomp+1),     &
     &    Ncomp_CPU, WK_FFTW%Nfft_c, WK_FFTW%C_FFTW_mul(1,1))
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
         (bwd_rocFFT%rocFFT_plan, bwd_rocFFT%rocFFT_wk_info,            &
     &    bwd_rocFFT%Ncomp, WK_rocFFT%Nfft_r, WK_rocFFT%X_rocFFT(1))
      elapsed(4) = elapsed(4) + OMP_GET_WTIME() - st_g
!$omp end single nowait
!
!!   3. The rest of the CPU threads immediately and execute
      st_c = OMP_GET_WTIME()
      call multi_bwd_FFTW3_smp(WK_FFTW%plan_mul_bwd,                    &
     &    WK_FFTW%Nplan_FFTW, WK_FFTW%istack_FFTW,                      &
     &    Ncomp_CPU, WK_FFTW%Nfft_c, WK_FFTW%C_FFTW_mul(1,1),           &
     &    int(bwd_rocFFT%Nfft), X(1,bwd_rocFFT%Ncomp+1))
      elapsed(3) = elapsed(3) + OMP_GET_WTIME() - st_c
!$omp end parallel
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call sel_copy_pin_field_from_FFT(int(bwd_rocFFT%Ncomp),           &
     &    int(WK_rocFFT%Nfft_r), WK_rocFFT%X_rocFFT(1),                 &
     &    int(bwd_rocFFT%Nfft), X(1,1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
      end subroutine pin_bwd_OMP_rocFFT_FFTW3
!
! ------------------------------------------------------------------
!
      end module pin_OMP_rocFFT_FFTW3
