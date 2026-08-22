!>@file   pout_cplx_rocFFT_FFTPACK.f90
!!@brief  module pout_cplx_rocFFT_FFTPACK
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2026
!!
!!
!>@brief  FFT by real rocFFT and FFTPACK5 with outmost data series
!!
!!@verbatim
!!      subroutine init_pout_cplx_rocFFT_FFTPACK                        &
!!     &         (Ncomp, Ncomp_GPU, N8omp_CPU, Nfft, Nsmp, Nstacksmp,   &
!!     &          fwd_rocFFT, bwd_rocFFT, WK_rocFFT, WK_FFTPACK)
!!      subroutine finalize_cplx_rocFFT_FFTPACK(fwd_rocFFT, bwd_rocFFT, &
!!     &                                        WK_rocFFT, WK_FFTPACK)
!!        integer(kind = kint), intent(in) :: Nsmp
!!        integer(kind = kint), intent(in) :: Ncomp, Ncomp_GPU, Ncomp_CPU
!!        integer(kind = kint), intent(in) :: Nfft
!!        integer(kind = kint), intent(inout) :: Nstacksmp(0:Nsmp)
!!        type(calypso_rocFFT_params), intent(inout) :: fwd_rocFFT
!!        type(calypso_rocFFT_params), intent(inout) :: bwd_rocFFT
!!        type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!!        type(working_FFTPACK), intent(inout) :: WK_FFTPACK
!!
!!      subroutine pout_fwd_cplx_rocFFT_FFTPACK                         &
!!     &         (Ncomp, fwd_rocFFT, WK_rocFFT, WK_FFTPACK, X, elapsed)
!!        integer(kind = kint), intent(in) :: Ncomp
!!        type(calypso_rocFFT_params), intent(in), target :: fwd_rocFFT
!!        type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!!        type(working_FFTPACK), intent(inout) :: WK_FFTPACK
!!        real(kind = kreal), intent(inout) :: X(Ncomp,fwd_rocFFT%Nfft)
!!        real(kind = kreal), intent(inout) :: elapsed(4)
!!      subroutine pout_bwd_cplx_rocFFT_FFTPACK                         &
!!     &         (Ncomp, bwd_rocFFT, WK_rocFFT, WK_FFTPACK, X, elapsed)
!!        integer(kind = kint), intent(in) :: Ncomp
!!        type(calypso_rocFFT_params), intent(in), target :: bwd_rocFFT
!!        type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!!        type(working_FFTPACK), intent(inout) :: WK_FFTPACK
!!        real(kind = kreal), intent(inout) :: X(Ncomp,bwd_rocFFT%Nfft)
!!        real(kind = kreal), intent(inout) :: elapsed(4)
!!@endverbatim
      module pout_real_rocFFT_FFTPACK
!
      use t_multi_rocFFT_wrapper
      use t_FFTPACK5_wrapper
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_pout_cplx_rocFFT_FFTPACK                          &
     &         (Ncomp, Ncomp_GPU, Ncomp_CPU, Nfft, Nsmp, Nstacksmp,     &
     &          fwd_rocFFT, bwd_rocFFT, WK_rocFFT, WK_FFTPACK)
!
      use multi_pout_complex_rocFFT
      use calypso_multi_fftpack
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: Nsmp
      integer(kind = kint), intent(in) :: Ncomp, Ncomp_GPU, Ncomp_CPU
      integer(kind = kint), intent(in) :: Nfft
!
      integer(kind = kint), intent(inout) :: Nstacksmp(0:Nsmp)
      type(calypso_rocFFT_params), intent(inout) :: fwd_rocFFT
      type(calypso_rocFFT_params), intent(inout) :: bwd_rocFFT
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_FFTPACK), intent(inout) :: WK_FFTPACK
!
      integer(kind = kint) :: max_4_smp = 0
!
!
      call count_number_4_smp(Nsmp, (Ncomp_GPU+1), Ncomp,               &
     &                        Nstacksmp, max_4_smp)
!
      call calypso_pout_rocFFT_init(Ncomp_GPU, Ncomp_GPU, Nfft,         &
     &                              fwd_rocFFT, bwd_rocFFT, WK_rocFFT)
      call init_WK_FFTPACK_t(Nsmp, Nstacksmp, Nfft, WK_FFTPACK)
!
      end subroutine init_pout_cplx_rocFFT_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine finalize_cplx_rocFFT_FFTPACK(fwd_rocFFT, bwd_rocFFT,   &
     &                                        WK_rocFFT, WK_FFTPACK)
!
      type(calypso_rocFFT_params), intent(inout) :: fwd_rocFFT
      type(calypso_rocFFT_params), intent(inout) :: bwd_rocFFT
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_FFTPACK), intent(inout) :: WK_FFTPACK
!
!
      call calypso_rocFFT_fin(fwd_rocFFT, bwd_rocFFT, WK_rocFFT)
      call finalize_WK_FFTPACK_t(WK_FFTPACK)
!
      end subroutine finalize_cplx_rocFFT_FFTPACK
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine pout_fwd_cplx_rocFFT_FFTPACK                           &
     &         (Ncomp, fwd_rocFFT, WK_rocFFT, WK_FFTPACK, X, elapsed)
!
      use calypso_multi_rocFFT
      use multi_pout_FFTPACK_smp
      use normalize_for_rocFFT
      use normalize_for_OMP_FFTW
      use normalize_for_FFTPACK
      use copy_field_for_FFT
!
      integer(kind = kint), intent(in) :: Ncomp
      type(calypso_rocFFT_params), intent(in), target :: fwd_rocFFT
!
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_FFTPACK), intent(inout) :: WK_FFTPACK
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
      call copy_rtp_fld_to_RFFTMF(WK_FFTPACK%Nplan_FFTPACK,             &
     &    WK_FFTPACK%istack_FFTPACK, WK_FFTPACK%Mmax_smp,               &
     &    int(fwd_rocFFT%Nfft), Ncomp, X(1,1), WK_FFTPACK%X_FFTPACK5)
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
!!   1. Create a CPU thread team
!$omp parallel
!!   2. Isolate a single thread to spawn the GPU work asynchronously
!$omp single
      st_g = OMP_GET_WTIME()
      call calypso_forward_rocFFT_r2c(fwd_rocFFT%rocFFT_plan,           &
     &    fwd_rocFFT%rocFFT_wk_info, fwd_rocFFT%Ncomp, WK_rocFFT%aNfft, &
     &    WK_rocFFT%Nfft_r, WK_rocFFT%X_rocFFT(1),                      &
     &    WK_rocFFT%Nfft_c, WK_rocFFT%C_rocFFT(1),                      &
     &    fwd_rocFFT%Nbytes, WK_rocFFT%data_ptr)
      elapsed(4) = elapsed(4) + OMP_GET_WTIME() - st_g
!$omp end single nowait
!
!!   3. The rest of the CPU threads immediately and execute
      st_c = OMP_GET_WTIME()
      call multi_pout_RFFTMF_smp(WK_FFTPACK%Nplan_FFTPACK,              &
     &    WK_FFTPACK%istack_FFTPACK, WK_FFTPACK%Mmax_smp,               &
     &    int(fwd_rocFFT%Nfft), WK_FFTPACK%X_FFTPACK5,                  &
     &    WK_FFTPACK%lsave_FFTPACK, WK_FFTPACK%WSAVE_FFTPACK,           &
     &    WK_FFTPACK%WORK_FFTPACK)
      elapsed(3) = elapsed(3) + OMP_GET_WTIME() - st_c
!$omp end parallel
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call norm_rtp_from_fwd_OMP_FFTW(int(fwd_rocFFT%Ncomp),            &
     &    int(WK_rocFFT%NFFT_c), WK_rocFFT%C_rocFFT(1),                 &
     &    Ncomp, int(fwd_rocFFT%Nfft), ione, X(1,1))
      call copy_rtp_spectr_from_RFFTMF(WK_FFTPACK%Nplan_FFTPACK,        &
     &    WK_FFTPACK%istack_FFTPACK, WK_FFTPACK%Mmax_smp,               &
     &    int(fwd_rocFFT%Nfft), WK_FFTPACK%X_FFTPACK5, Ncomp, X(1,1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
      write(*,*) 'CPU FFT clock',   elapsed(3)
      write(*,*) 'GPU FFT clock',   elapsed(4)
      write(*,*) 'Total FFT clock', elapsed(1)
!
      end subroutine pout_fwd_cplx_rocFFT_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine pout_bwd_cplx_rocFFT_FFTPACK                           &
     &         (Ncomp, bwd_rocFFT, WK_rocFFT, WK_FFTPACK, X, elapsed)
!
      use calypso_multi_rocFFT
      use multi_pout_FFTPACK_smp
      use normalize_for_rocFFT
      use normalize_for_OMP_FFTW
      use normalize_for_FFTPACK
      use copy_field_for_FFT
!
      integer(kind = kint), intent(in) :: Ncomp
      type(calypso_rocFFT_params), intent(in), target :: bwd_rocFFT
!
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_FFTPACK), intent(inout) :: WK_FFTPACK
      real(kind = kreal), intent(inout) :: X(Ncomp,bwd_rocFFT%Nfft)
      real(kind = kreal), intent(inout) :: elapsed(4)
!
      real(kind = kreal) :: start
      real(kind = kreal) :: st_c, st_g
!
!
      start = OMP_GET_WTIME()
      call norm_rtp_to_bwd_OMP_FFTW                                     &
     &   (ione, Ncomp, int(bwd_rocFFT%Nfft), X(1,1),                    &
     &    int(bwd_rocFFT%Ncomp), int(WK_rocFFT%Nfft_r),                 &
     &    WK_rocFFT%C_rocFFT(1))
      call copy_rtp_spectr_to_RFFTMB(WK_FFTPACK%Nplan_FFTPACK,          &
     &   WK_FFTPACK%istack_FFTPACK, WK_FFTPACK%Mmax_smp,                &
     &   int(bwd_rocFFT%Nfft), Ncomp, X(1,1), WK_FFTPACK%X_FFTPACK5)
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
!!   1. Create a CPU thread team
      start = OMP_GET_WTIME()
!$omp parallel
!!   2. Isolate a single thread to spawn the GPU work asynchronously
!$omp single
      st_g = OMP_GET_WTIME()
      call calypso_backward_rocFFT_c2r(bwd_rocFFT%rocFFT_plan,          &
     &    bwd_rocFFT%rocFFT_wk_info, bwd_rocFFT%Ncomp,                  &
     &    WK_rocFFT%Nfft_c, WK_rocFFT%C_rocFFT(1),                      &
     &    WK_rocFFT%Nfft_r, WK_rocFFT%X_rocFFT(1),                      &
     &    bwd_rocFFT%Nbytes, WK_rocFFT%data_ptr)
      elapsed(4) = elapsed(4) + OMP_GET_WTIME() - st_g
!$omp end single nowait
!
!!   3. The rest of the CPU threads immediately and execute
      st_c = OMP_GET_WTIME()
      call multi_pout_RFFTMB_smp(WK_FFTPACK%Nplan_FFTPACK,              &
     &    WK_FFTPACK%istack_FFTPACK, WK_FFTPACK%Mmax_smp,               &
     &    int(bwd_rocFFT%Nfft), WK_FFTPACK%X_FFTPACK5,                  &
     &    WK_FFTPACK%lsave_FFTPACK, WK_FFTPACK%WSAVE_FFTPACK,           &
     &    WK_FFTPACK%WORK_FFTPACK)
      elapsed(3) = elapsed(3) + OMP_GET_WTIME() - st_c
!$omp end parallel
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call copy_pout_field_from_FFT(int(bwd_rocFFT%Ncomp),              &
     &    int(WK_rocFFT%Nfft_r), WK_rocFFT%X_rocFFT(1),                 &
     &    Ncomp, int(bwd_rocFFT%Nfft), ione, X(1,1))
      call copy_rtp_fld_from_RFFTMB(WK_FFTPACK%Nplan_FFTPACK,           &
     &    WK_FFTPACK%istack_FFTPACK, WK_FFTPACK%Mmax_smp,               &
     &    int(bwd_rocFFT%Nfft), WK_FFTPACK%X_FFTPACK5, Ncomp, X(1,1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
      write(*,*) 'CPU FFT clock',   elapsed(3)
      write(*,*) 'GPU FFT clock',   elapsed(4)
      write(*,*) 'Total FFT clock', elapsed(1)
!
      end subroutine pout_bwd_cplx_rocFFT_FFTPACK
!
! ------------------------------------------------------------------
!
      end module pout_cplx_rocFFT_FFTPACK
!
