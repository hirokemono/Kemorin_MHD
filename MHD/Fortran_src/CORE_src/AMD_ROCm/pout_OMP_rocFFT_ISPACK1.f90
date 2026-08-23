!>@file   pout_OMP_rocFFT_ISPACK1.f90
!!@brief  module pout_OMP_rocFFT_ISPACK1
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2026
!!
!!
!>@brief  FFT by OpenMP rocFFT and ISPACK v0.9 with outmost data series
!!
!!@verbatim
!!      subroutine init_pout_OMP_rocFFT_ISPACK1                         &
!!     &         (Ncomp, Ncomp_GPU, N8omp_CPU, Nfft, Nsmp, Nstacksmp,   &
!!     &          fwd_rocFFT, bwd_rocFFT, WK_rocFFT, WK_ISPACK1)
!!      subroutine finalize_OMP_rocFFT_ISPACK1(fwd_rocFFT, bwd_rocFFT,  &
!!     &                                       WK_rocFFT, WK_ISPACK1)
!!        integer(kind = kint), intent(in) :: Nsmp
!!        integer(kind = kint), intent(in) :: Ncomp, Ncomp_GPU, Ncomp_CPU
!!        integer(kind = kint), intent(in) :: Nfft
!!        integer(kind = kint), intent(inout) :: Nstacksmp(0:Nsmp)
!!        type(calypso_rocFFT_params), intent(inout) :: fwd_rocFFT
!!        type(calypso_rocFFT_params), intent(inout) :: bwd_rocFFT
!!        type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!!        type(working_ISPACK3), intent(inout) :: WK_ISPACK1
!!
!!      subroutine pout_fwd_OMP_rocFFT_ISPACK1                          &
!!     &         (Ncomp, fwd_rocFFT, WK_rocFFT, WK_ISPACK1, X, elapsed)
!!        integer(kind = kint), intent(in) :: Ncomp
!!        type(calypso_rocFFT_params), intent(in), target :: fwd_rocFFT
!!        type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!!        type(working_ISPACK), intent(inout) :: WK_ISPACK1
!!        real(kind = kreal), intent(inout) :: X(Ncomp,fwd_rocFFT%Nfft)
!!        real(kind = kreal), intent(inout) :: elapsed(4)
!!      subroutine pout_bwd_OMP_rocFFT_ISPACK1                          &
!!     &         (Ncomp, bwd_rocFFT, WK_rocFFT, WK_ISPACK1, X, elapsed)
!!        integer(kind = kint), intent(in) :: Ncomp
!!        type(calypso_rocFFT_params), intent(in), target :: bwd_rocFFT
!!        type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!!        type(working_ISPACK), intent(inout) :: WK_ISPACK1
!!        real(kind = kreal), intent(inout) :: X(Ncomp,bwd_rocFFT%Nfft)
!!        real(kind = kreal), intent(inout) :: elapsed(4)
!!@endverbatim
      module pout_OMP_rocFFT_ISPACK1
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
      subroutine init_pout_OMP_rocFFT_ISPACK1                           &
     &         (Ncomp, Ncomp_GPU, Ncomp_CPU, Nfft, Nsmp, Nstacksmp,     &
     &          fwd_rocFFT, bwd_rocFFT, WK_rocFFT, WK_ISPACK1)
!
      use multi_pout_complex_rocFFT
      use calypso_multi_ispack
      use cal_minmax_and_stacks
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: Nsmp
      integer(kind = kint), intent(in) :: Ncomp, Ncomp_GPU, Ncomp_CPU
      integer(kind = kint), intent(in) :: Nfft
!
      integer(kind = kint), intent(inout) :: Nstacksmp(0:Nsmp)
      type(calypso_rocFFT_params), intent(inout) :: fwd_rocFFT
      type(calypso_rocFFT_params), intent(inout) :: bwd_rocFFT
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_ISPACK), intent(inout) :: WK_ISPACK1
!
      integer(kind = kint) :: max_4_smp = 0
!
!
      call count_number_4_smp(Nsmp, (Ncomp_GPU+1), Ncomp,               &
     &                        Nstacksmp, max_4_smp)
!
      call calypso_pout_rocFFT_init(Ncomp_GPU, Ncomp_GPU, Nfft,         &
     &                              fwd_rocFFT, bwd_rocFFT, WK_rocFFT)
      call init_wk_ispack_t(Nsmp, Nstacksmp, Nfft, WK_ISPACK1)
!
      end subroutine init_pout_OMP_rocFFT_ISPACK1
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
      subroutine pout_fwd_OMP_rocFFT_ISPACK1                            &
     &         (Ncomp, fwd_rocFFT, WK_rocFFT, WK_ISPACK1, X, elapsed)
!
      use calypso_multi_rocFFT
      use multi_pout_ISPACK1_smp
      use normalize_for_rocFFT
      use normalize_for_ISPACK
      use copy_field_for_FFT
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: Ncomp
      type(calypso_rocFFT_params), intent(in), target :: fwd_rocFFT
!
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_ISPACK), intent(inout) :: WK_ISPACK1
      real(kind = kreal), intent(inout) :: X(Ncomp,fwd_rocFFT%Nfft)
      real(kind = kreal), intent(inout) :: elapsed(4)
!
      real(kind = kreal) :: start
      real(kind = kreal) :: st_c, st_g
!
!
      start = OMP_GET_WTIME()
      call copy_pout_field_to_FFT                                       &
     &   (ione, Ncomp, int(fwd_rocFFT%Nfft), X(1,1),                    &
     &    int(fwd_rocFFT%Ncomp), int(WK_rocFFT%Nfft_r),                 &
     &    WK_rocFFT%X_rocFFT(1))
      call copy_rtp_fld_to_FXRTFA(WK_ISPACK1%Nplan_ISPACK,              &
     &    WK_ISPACK1%istack_ISPACK, cast_long(WK_ISPACK1%Mmax_smp),     &
     &    fwd_rocFFT%Nfft, cast_long(Ncomp), X(1,1),                    &
     &    WK_ISPACK1%X_ispack(1,1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
!!   1. Create a CPU thread team
!$omp parallel
!!   2. Isolate a single thread to spawn the GPU work asynchronously
!$omp single
      st_g = OMP_GET_WTIME()
      call calypso_fwd_OpenMP_rocFFT(fwd_rocFFT%rocFFT_plan,            &
     &    fwd_rocFFT%rocFFT_wk_info, fwd_rocFFT%Ncomp,                  &
     &    WK_rocFFT%aNfft, WK_rocFFT%Nfft_r, WK_rocFFT%X_rocFFT(1))
      elapsed(4) = elapsed(4) + OMP_GET_WTIME() - st_g
!$omp end single nowait
!
!!   3. The rest of the CPU threads immediately and execute
      st_c = OMP_GET_WTIME()
      call multi_pout_FTTRUF_smp(WK_ISPACK1%Nplan_ISPACK,               &
     &    WK_ISPACK1%istack_ISPACK, WK_ISPACK1%Mmax_smp,                &
     &    int(fwd_rocFFT%Nfft), WK_ISPACK1%X_ispack,                    &
     &    WK_ISPACK1%IT_ispack, WK_ISPACK1%T_ispack,                    &
     &    WK_ISPACK1%WORK_ispack)
      elapsed(3) = elapsed(3) + OMP_GET_WTIME() - st_c
!$omp end parallel
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call norm_rtp_from_fwd_rocFFT(int(fwd_rocFFT%Ncomp),              &
     &    int(WK_rocFFT%NFFT_r), WK_rocFFT%X_rocFFT(1),                 &
     &    Ncomp, int(fwd_rocFFT%Nfft), X(1,1))
      call norm_rtp_spectr_from_FXRTFA(WK_ISPACK1%Nplan_ISPACK,         &
     &    WK_ISPACK1%istack_ISPACK, cast_long(WK_ISPACK1%Mmax_smp),     &
     &    fwd_rocFFT%Nfft, WK_ISPACK1%X_ispack(1,1),                    &
     &    cast_long(Ncomp), X(1,1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
      write(*,*) 'CPU FFT clock',   elapsed(3)
      write(*,*) 'GPU FFT clock',   elapsed(4)
      write(*,*) 'Total FFT clock', elapsed(1)
!
      end subroutine pout_fwd_OMP_rocFFT_ISPACK1
!
! ------------------------------------------------------------------
!
      subroutine pout_bwd_OMP_rocFFT_ISPACK1                            &
     &         (Ncomp, bwd_rocFFT, WK_rocFFT, WK_ISPACK1, X, elapsed)
!
      use calypso_multi_rocFFT
      use multi_pout_ISPACK1_smp
      use normalize_for_rocFFT
      use normalize_for_ISPACK
      use copy_field_for_FFT
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: Ncomp
      type(calypso_rocFFT_params), intent(in), target :: bwd_rocFFT
!
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_ISPACK), intent(inout) :: WK_ISPACK1
      real(kind = kreal), intent(inout) :: X(Ncomp,bwd_rocFFT%Nfft)
      real(kind = kreal), intent(inout) :: elapsed(4)
!
      real(kind = kreal) :: start
      real(kind = kreal) :: st_c, st_g
!
!
      start = OMP_GET_WTIME()
      call norm_rtp_to_bwd_rocFFT                                       &
     &   (ione, Ncomp, int(bwd_rocFFT%Nfft), X(1,1),                    &
     &    int(bwd_rocFFT%Ncomp), int(WK_rocFFT%Nfft_r),                 &
     &    WK_rocFFT%X_rocFFT(1))
      call norm_rtp_spectr_to_FXRTBA(WK_ISPACK1%Nplan_ISPACK,           &
     &    WK_ISPACK1%istack_ISPACK, cast_long(WK_ISPACK1%Mmax_smp),     &
     &    bwd_rocFFT%Nfft, cast_long(Ncomp), X(1,1),                    &
     &    WK_ISPACK1%X_ispack(1,1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
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
      call multi_pout_FTTRUB_smp(WK_ISPACK1%Nplan_ISPACK,               &
     &    WK_ISPACK1%istack_ISPACK, WK_ISPACK1%Mmax_smp,                &
     &    int(bwd_rocFFT%Nfft), WK_ISPACK1%X_ispack,                    &
     &    WK_ISPACK1%IT_ispack, WK_ISPACK1%T_ispack,                    &
     &    WK_ISPACK1%WORK_ispack)
      elapsed(3) = elapsed(3) + OMP_GET_WTIME() - st_c
!$omp end parallel
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call copy_pout_field_from_FFT(int(bwd_rocFFT%Ncomp),              &
     &    int(WK_rocFFT%Nfft_r), WK_rocFFT%X_rocFFT(1),                 &
     &    Ncomp, int(bwd_rocFFT%Nfft), ione, X(1,1))
      call copy_rtp_fld_from_FXRTBA(WK_ISPACK1%Nplan_ISPACK,            &
     &    WK_ISPACK1%istack_ISPACK, cast_long(WK_ISPACK1%Mmax_smp),     &
     &    bwd_rocFFT%Nfft, WK_ISPACK1%X_ispack(1,1),                    &
     &    cast_long(Ncomp), X(1,1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
      write(*,*) 'CPU FFT clock',   elapsed(3)
      write(*,*) 'GPU FFT clock',   elapsed(4)
      write(*,*) 'Total FFT clock', elapsed(1)
!
      end subroutine pout_bwd_OMP_rocFFT_ISPACK1
!
! ------------------------------------------------------------------
!
      end module pout_OMP_rocFFT_ISPACK1
!
