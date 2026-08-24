!>@file   pin_OMP_rocFFT_ISPACK1.f90
!!@brief  module pin_OMP_rocFFT_ISPACK1
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2026
!!
!!
!>@brief  FFT by OpenMP rocFFT and ISPACK v0.9 with innermost data series
!!
!!@verbatim
!!      subroutine init_pin_OMP_rocFFT_ISPACK1                          &
!!     &         (Ncomp, Ncomp_GPU, N8omp_CPU, Nfft, Nsmp,              &
!!     &          fwd_rocFFT, bwd_rocFFT, WK_rocFFT, WK_ISPACK1)
!!      subroutine finalize_OMP_rocFFT_ISPACK1(fwd_rocFFT, bwd_rocFFT,  &
!!     &                                       WK_rocFFT, WK_ISPACK1)
!!        integer(kind = kint), intent(in) :: Nsmp
!!        integer(kind = kint), intent(in) :: Ncomp, Ncomp_GPU, Ncomp_CPU
!!        integer(kind = kint), intent(in) :: Nfft
!!        type(calypso_rocFFT_params), intent(inout) :: fwd_rocFFT
!!        type(calypso_rocFFT_params), intent(inout) :: bwd_rocFFT
!!        type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!!        type(working_ISPACK3), intent(inout) :: WK_ISPACK1
!!
!!      subroutine pin_fwd_OMP_rocFFT_ISPACK1                           &
!!     &         (Ncomp, fwd_rocFFT, WK_rocFFT, WK_ISPACK1, X, elapsed)
!!        integer(kind = kint), intent(in) :: Ncomp
!!        type(calypso_rocFFT_params), intent(in), target :: fwd_rocFFT
!!        type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!!        type(working_ISPACK), intent(inout) :: WK_ISPACK1
!!        real(kind = kreal), intent(inout) :: X(fwd_rocFFT%Nfft,Ncomp)
!!        real(kind = kreal), intent(inout) :: elapsed(4)
!!      subroutine pin_bwd_OMP_rocFFT_ISPACK1(Ncomp, Ncomp_CPU,         &
!!     &          bwd_rocFFT, WK_rocFFT, WK_ISPACK1, X, elapsed)
!!        integer(kind = kint), intent(in) :: Ncomp, Ncomp_CPU
!!        type(calypso_rocFFT_params), intent(in), target :: bwd_rocFFT
!!        type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!!        type(working_ISPACK), intent(inout) :: WK_ISPACK1
!!        real(kind = kreal), intent(inout) :: X(bwd_rocFFT%Nfft,Ncomp)
!!        real(kind = kreal), intent(inout) :: elapsed(4)
!!@endverbatim
      module pin_OMP_rocFFT_ISPACK1
!
      use t_multi_rocFFT_wrapper
      use t_ispack_FFT_wrapper
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_pin_OMP_rocFFT_ISPACK1                            &
     &         (Ncomp, Ncomp_GPU, Ncomp_CPU, Nfft, Nsmp,                &
     &          fwd_rocFFT, bwd_rocFFT, WK_rocFFT, WK_ISPACK1)
!
      use multi_pin_complex_rocFFT
      use calypso_multi_ispack
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: Nsmp
      integer(kind = kint), intent(in) :: Ncomp, Ncomp_GPU, Ncomp_CPU
      integer(kind = kint), intent(in) :: Nfft
!
      type(calypso_rocFFT_params), intent(inout) :: fwd_rocFFT
      type(calypso_rocFFT_params), intent(inout) :: bwd_rocFFT
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_ISPACK), intent(inout) :: WK_ISPACK1
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
      call init_wk_ispack_t(Nsmp, istack_smp, Nfft, WK_ISPACK1)
      deallocate(istack_smp)
!
      end subroutine init_pin_OMP_rocFFT_ISPACK1
!
! ------------------------------------------------------------------
!
      subroutine finalize_OMP_rocFFT_ISPACK1(fwd_rocFFT, bwd_rocFFT,    &
     &                                       WK_rocFFT, WK_ISPACK1)
!
      type(calypso_rocFFT_params), intent(inout) :: fwd_rocFFT
      type(calypso_rocFFT_params), intent(inout) :: bwd_rocFFT
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_ISPACK), intent(inout) :: WK_ISPACK1
!
!
      call calypso_rocFFT_fin(fwd_rocFFT, bwd_rocFFT, WK_rocFFT)
      call finalize_wk_ispack_t(WK_ISPACK1)
!
      end subroutine finalize_OMP_rocFFT_ISPACK1
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine pin_fwd_OMP_rocFFT_ISPACK1                             &
     &         (Ncomp, fwd_rocFFT, WK_rocFFT, WK_ISPACK1, X, elapsed)
!
      use calypso_multi_rocFFT
      use multi_pin_ISPACK1_smp
      use swap_prt_data_for_ISPACK
      use normalize_for_rocFFT
      use copy_field_for_FFT
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: Ncomp
      type(calypso_rocFFT_params), intent(in), target :: fwd_rocFFT
!
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_ISPACK), intent(inout) :: WK_ISPACK1
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
      call swap_prt_fld_to_FXRTFA(WK_ISPACK1%Nplan_ISPACK,              &
     &    WK_ISPACK1%istack_ISPACK, cast_long(WK_ISPACK1%Mmax_smp),     &
     &    fwd_rocFFT%Nfft, cast_long(Ncomp), X(1,1),                    &
     &    WK_ISPACK1%X_ispack(1,1))
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
     &    fwd_rocFFT%rocFFT_wk_info, fwd_rocFFT%Ncomp,                  &
     &    WK_rocFFT%aNfft, WK_rocFFT%Nfft_r, WK_rocFFT%X_rocFFT(1))
      elapsed(4) = elapsed(4) + OMP_GET_WTIME() - st_g
!$omp end single nowait
!
!!   3. The rest of the CPU threads immediately and execute
!      write(*,*) 'FFT loop start', OMP_GET_WTIME() - start
      st_c = OMP_GET_WTIME()
      call multi_pin_FTTRUF_smp(WK_ISPACK1%Nplan_ISPACK,                &
     &    WK_ISPACK1%istack_ISPACK, WK_ISPACK1%Mmax_smp,                &
     &    int(fwd_rocFFT%Nfft), WK_ISPACK1%X_ispack,                    &
     &    WK_ISPACK1%IT_ispack, WK_ISPACK1%T_ispack,                    &
     &    WK_ISPACK1%WORK_ispack)
      elapsed(3) = elapsed(3) + OMP_GET_WTIME() - st_c
!$omp end parallel
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call norm_prt_from_fwd_rocFFT(int(fwd_rocFFT%Ncomp),              &
     &    int(WK_rocFFT%NFFT_r), WK_rocFFT%X_rocFFT,                    &
     &    int(fwd_rocFFT%Nfft), X(1,1))
      call swap_prt_spectr_from_FXRTFA(WK_ISPACK1%Nplan_ISPACK,         &
     &    WK_ISPACK1%istack_ISPACK,cast_long(WK_ISPACK1%Mmax_smp),      &
     &    fwd_rocFFT%Nfft, WK_ISPACK1%X_ispack(1,1),                    &
     &    cast_long(Ncomp), X(1,1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
!      write(*,*) 'CPU FFT clock',   elapsed(3)
!      write(*,*) 'GPU FFT clock',   elapsed(4)
!      write(*,*) 'Total FFT clock', elapsed(1)
!
      end subroutine pin_fwd_OMP_rocFFT_ISPACK1
!
! ------------------------------------------------------------------
!
      subroutine pin_bwd_OMP_rocFFT_ISPACK1(Ncomp, Ncomp_CPU,           &
     &          bwd_rocFFT, WK_rocFFT, WK_ISPACK1, X, elapsed)
!
      use calypso_multi_rocFFT
      use multi_pin_ISPACK1_smp
      use swap_prt_data_for_ISPACK
      use normalize_for_rocFFT
      use copy_field_for_FFT
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: Ncomp, Ncomp_CPU
      type(calypso_rocFFT_params), intent(in), target :: bwd_rocFFT
!
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_ISPACK), intent(inout) :: WK_ISPACK1
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
      call swap_prt_spectr_to_FXRTBA(WK_ISPACK1%Nplan_ISPACK,           &
     &    WK_ISPACK1%istack_ISPACK, cast_long(WK_ISPACK1%Mmax_smp),     &
     &    bwd_rocFFT%Nfft, cast_long(Ncomp_CPU),                        &
     &    X(1,bwd_rocFFT%Ncomp+1), WK_ISPACK1%X_ispack(1,1))
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
      call multi_pin_FTTRUB_smp(WK_ISPACK1%Nplan_ISPACK,                &
     &    WK_ISPACK1%istack_ISPACK, WK_ISPACK1%Mmax_smp,                &
     &    int(bwd_rocFFT%Nfft), WK_ISPACK1%X_ispack,                    &
     &    WK_ISPACK1%IT_ispack, WK_ISPACK1%T_ispack,                    &
     &    WK_ISPACK1%WORK_ispack)
      elapsed(3) = elapsed(3) + OMP_GET_WTIME() - st_c
!$omp end parallel
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call sel_copy_pin_field_from_FFT(int(bwd_rocFFT%Ncomp),           &
     &    int(WK_rocFFT%Nfft_r), WK_rocFFT%X_rocFFT(1),                 &
     &    int(bwd_rocFFT%Nfft), X(1,1))
      call swap_prt_fld_from_FXRTBA(WK_ISPACK1%Nplan_ISPACK,            &
     &    WK_ISPACK1%istack_ISPACK, cast_long(WK_ISPACK1%Mmax_smp),     &
     &    bwd_rocFFT%Nfft, WK_ISPACK1%X_ispack(1,1),                    &
     &    cast_long(Ncomp_CPU), X(1,bwd_rocFFT%Ncomp+1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
!      write(*,*) 'CPU FFT clock',   elapsed(3)
!      write(*,*) 'GPU FFT clock',   elapsed(4)
!      write(*,*) 'Total FFT clock', elapsed(1)
!
      end subroutine pin_bwd_OMP_rocFFT_ISPACK1
!
! ------------------------------------------------------------------
!
      end module pin_OMP_rocFFT_ISPACK1
