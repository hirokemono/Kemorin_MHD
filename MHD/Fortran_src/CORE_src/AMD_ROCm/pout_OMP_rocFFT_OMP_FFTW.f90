!>@file   pout_OMP_rocFFT_OMP_FFTW.f90
!!@brief  module pout_OMP_rocFFT_OMP_FFTW
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2026
!!
!!
!>@brief  FFT by OpenMP rocFFT and OpenMP FFTW with outmost data series
!!
!!@verbatim
!!      subroutine init_pout_OMP_rocFFT_OMP_FFTW3                       &
!!     &         (ncomp_GPU, Ncomp_CPU, Nfft, fwd_rocFFT, bwd_rocFFT,   &
!!     &          WK_rocFFT, WK_OMP_FFTW)
!!      subroutine fin_pout_OMP_rocFFT_OMP_FFTW3                        &
!!     &         (fwd_rocFFT, bwd_rocFFT, WK_rocFFT, WK_OMP_FFTW)
!!        integer(kind = kint), intent(in) :: ncomp_GPU, Ncomp_CPU
!!        integer(kind = kint), intent(in) :: Nfft
!!        type(calypso_rocFFT_params), intent(inout) :: fwd_rocFFT
!!        type(calypso_rocFFT_params), intent(inout) :: bwd_rocFFT
!!        type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!!        type(working_OMP_FFTW), intent(inout) :: WK_OMP_FFTW
!!
!!      subroutine pout_fwd_OMP_rocFFT_OMP_FFTW3(Ncomp, Ncomp_CPU,      &
!!     &          fwd_rocFFT, WK_rocFFT, WK_OMP_FFTW, X, elapsed)
!!        integer(kind = kint), intent(in) :: Ncomp, Ncomp_CPU
!!        type(calypso_rocFFT_params), intent(in), target :: fwd_rocFFT
!!        type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!!        type(working_OMP_FFTW), intent(inout) :: WK_OMP_FFTW
!!        real(kind = kreal), intent(inout) :: X(Ncomp,fwd_rocFFT%Nfft)
!!        real(kind = kreal), intent(inout) :: elapsed(4)
!!      subroutine pout_bwd_OMP_rocFFT_OMP_FFTW3(Ncomp, Ncomp_CPU,      &
!!     &          bwd_rocFFT, WK_rocFFT, WK_OMP_FFTW, X, elapsed)
!!        integer(kind = kint), intent(in) :: Ncomp, Ncomp_CPU
!!        type(calypso_rocFFT_params), intent(in), target :: bwd_rocFFT
!!        type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!!        type(working_OMP_FFTW), intent(inout) :: WK_OMP_FFTW
!!        real(kind = kreal), intent(inout) :: X(Ncomp,bwd_rocFFT%Nfft)
!!        real(kind = kreal), intent(inout) :: elapsed(4)
!!@endverbatim
      module pout_OMP_rocFFT_OMP_FFTW
!
      use t_multi_rocFFT_wrapper
      use t_OMP_FFTW_wrapper
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_pout_OMP_rocFFT_OMP_FFTW3                         &
     &         (ncomp_GPU, Ncomp_CPU, Nfft, fwd_rocFFT, bwd_rocFFT,     &
     &          WK_rocFFT, WK_OMP_FFTW)
!
      use multi_pout_complex_rocFFT
!
      integer(kind = kint), intent(in) :: ncomp_GPU, Ncomp_CPU
      integer(kind = kint), intent(in) :: Nfft
!
      type(calypso_rocFFT_params), intent(inout) :: fwd_rocFFT
      type(calypso_rocFFT_params), intent(inout) :: bwd_rocFFT
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_OMP_FFTW), intent(inout) :: WK_OMP_FFTW
!
!
      call calypso_pout_rocFFT_init(ncomp_GPU, ncomp_GPU, Nfft,         &
     &                              fwd_rocFFT, bwd_rocFFT, WK_rocFFT)
      call init_OMP_FFTW_type(Ncomp_CPU, Nfft, WK_OMP_FFTW)
!
      end subroutine init_pout_OMP_rocFFT_OMP_FFTW3
!
! ------------------------------------------------------------------
!
      subroutine fin_pout_OMP_rocFFT_OMP_FFTW3                          &
     &         (fwd_rocFFT, bwd_rocFFT, WK_rocFFT, WK_OMP_FFTW)
!
      type(calypso_rocFFT_params), intent(inout) :: fwd_rocFFT
      type(calypso_rocFFT_params), intent(inout) :: bwd_rocFFT
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_OMP_FFTW), intent(inout) :: WK_OMP_FFTW
!
!
      call calypso_rocFFT_fin(fwd_rocFFT, bwd_rocFFT, WK_rocFFT)
      call finalize_OMP_FFTW_type(WK_OMP_FFTW)
!
      end subroutine fin_pout_OMP_rocFFT_OMP_FFTW3
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine pout_fwd_OMP_rocFFT_OMP_FFTW3(Ncomp, Ncomp_CPU,        &
     &          fwd_rocFFT, WK_rocFFT, WK_OMP_FFTW, X, elapsed)
!
      use calypso_multi_rocFFT
      use copy_field_for_FFT
      use normalize_for_rocFFT
      use normalize_for_FFTW
      use normalize_for_OMP_FFTW
!
      integer(kind = kint), intent(in) :: Ncomp, Ncomp_CPU
      type(calypso_rocFFT_params), intent(in), target :: fwd_rocFFT
!
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_OMP_FFTW), intent(inout) :: WK_OMP_FFTW
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
      call copy_pout_field_to_FFT                                       &
     &   (int(fwd_rocFFT%Ncomp+1), Ncomp, int(fwd_rocFFT%Nfft), X(1,1), &
     &    Ncomp_CPU, int(fwd_rocFFT%Nfft), WK_OMP_FFTW%X_FFTW_mul(1,1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
!!   1. Create a CPU thread team
!$omp parallel
!!   2. Isolate a single thread to spawn the GPU work asynchronously
!$omp single
      st_g = OMP_GET_WTIME()
      call calypso_fwd_OpenMP_rocFFT                                    &
     &   (fwd_rocFFT%rocFFT_plan, fwd_rocFFT%rocFFT_wk_info,            &
     &    fwd_rocFFT%Ncomp, WK_rocFFT%aNfft, WK_rocFFT%Nfft_r,          &
     &    WK_rocFFT%X_rocFFT(1))
      elapsed(4) = elapsed(4) + OMP_GET_WTIME() - st_g
!$omp end single nowait
!
!!   3. The rest of the CPU threads immediately and execute
      st_c = OMP_GET_WTIME()
      call dfftw_execute_dft_r2c(WK_OMP_FFTW%omp_plan_fwd,              &
     &    WK_OMP_FFTW%X_FFTW_mul(1,1), WK_OMP_FFTW%C_FFTW_mul(1,1))
      elapsed(3) = elapsed(3) + OMP_GET_WTIME() - st_c
!$omp end parallel
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call norm_rtp_from_fwd_rocFFT(int(fwd_rocFFT%Ncomp),              &
     &    int(WK_rocFFT%NFFT_r), WK_rocFFT%X_rocFFT(1),                 &
     &    Ncomp, int(fwd_rocFFT%Nfft), X(1,1))
!
      call normalize_fwd_OMP_FFTW(WK_OMP_FFTW%aNfft, Ncomp_CPU,         &
     &    WK_OMP_FFTW%Nfft_c, WK_OMP_FFTW%C_FFTW_mul(1,1))
      call norm_rtp_from_fwd_OMP_FFTW                                   &
     &   (Ncomp_CPU, WK_OMP_FFTW%Nfft_c, WK_OMP_FFTW%C_FFTW_mul(1,1),   &
     &    Ncomp, int(fwd_rocFFT%Nfft), int(fwd_rocFFT%Ncomp+1), X(1,1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
      write(*,*) 'CPU FFT clock',   elapsed(3)
      write(*,*) 'GPU FFT clock',   elapsed(4)
      write(*,*) 'Total FFT clock', elapsed(1)
!
      end subroutine pout_fwd_OMP_rocFFT_OMP_FFTW3
!
! ------------------------------------------------------------------
!
      subroutine pout_bwd_OMP_rocFFT_OMP_FFTW3(Ncomp, Ncomp_CPU,        &
     &          bwd_rocFFT, WK_rocFFT, WK_OMP_FFTW, X, elapsed)
!
      use copy_field_for_FFT
      use normalize_for_rocFFT
      use calypso_multi_rocFFT
      use normalize_for_OMP_FFTW
!
      integer(kind = kint), intent(in) :: Ncomp, Ncomp_CPU
      type(calypso_rocFFT_params), intent(in), target :: bwd_rocFFT
!
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_OMP_FFTW), intent(inout) :: WK_OMP_FFTW
      real(kind = kreal), intent(inout) :: X(Ncomp,bwd_rocFFT%Nfft)
      real(kind = kreal), intent(inout) :: elapsed(4)
!
      real(kind = kreal) :: start
      real(kind = kreal) :: st_c, st_g
      integer(kind = kint) :: i, ist
!
!
      start = OMP_GET_WTIME()
      call norm_rtp_to_bwd_rocFFT                                       &
     &   (ione, Ncomp, int(bwd_rocFFT%Nfft), X(1,1),                    &
     &    int(bwd_rocFFT%Ncomp), int(WK_rocFFT%Nfft_r),                 &
     &    WK_rocFFT%X_rocFFT(1))
      call norm_rtp_to_bwd_OMP_FFTW                                     &
     &   (int(bwd_rocFFT%Ncomp+1), Ncomp, int(bwd_rocFFT%Nfft), X(1,1), &
     &    Ncomp_CPU, WK_OMP_FFTW%NFFT_c, WK_OMP_FFTW%C_FFTW_mul(1,1))
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
      call dfftw_execute_dft_c2r(WK_OMP_FFTW%omp_plan_bwd,              &
     &    WK_OMP_FFTW%C_FFTW_mul(1,1), WK_OMP_FFTW%X_FFTW_mul(1,1))
      elapsed(3) = elapsed(3) + OMP_GET_WTIME() - st_c
!$omp end parallel
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call copy_pout_field_from_FFT(int(bwd_rocFFT%Ncomp),              &
     &    int(WK_rocFFT%Nfft_r), WK_rocFFT%X_rocFFT(1),                 &
     &    Ncomp, int(bwd_rocFFT%Nfft), ione, X(1,1))
      call copy_pout_field_from_FFT                                     &
     &   (Ncomp_CPU, int(bwd_rocFFT%Nfft), WK_OMP_FFTW%X_FFTW_mul(1,1), &
     &    Ncomp, int(bwd_rocFFT%Nfft), int(bwd_rocFFT%Ncomp+1), X(1,1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
      write(*,*) 'CPU FFT clock',   elapsed(3)
      write(*,*) 'GPU FFT clock',   elapsed(4)
      write(*,*) 'Total FFT clock', elapsed(1)
!
      end subroutine pout_bwd_OMP_rocFFT_OMP_FFTW3
!
! ------------------------------------------------------------------
!
      end module pout_OMP_rocFFT_OMP_FFTW
!
