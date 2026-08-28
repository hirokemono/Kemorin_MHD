!>@file   sel_pout_rocFFT_CPU_FFT.f90
!!@brief  module sel_pout_rocFFT_CPU_FFT
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2026
!!
!!
!>@brief  FFT by rocFFT and FFTPACK5 with outmost data series
!!
!!@verbatim
!!      subroutine init_pout_rocFFT_FFTs(iflag_CPU_FFT,                 &
!!     &          Ncomp, Ncomp_GPU, Ncomp_CPU, Nfft, Nsmp,              &
!!     &          fwd_rocFFT, bwd_rocFFT, WK_rocFFT, WK_FFTs)
!!        integer(kind = kint), intent(in) :: iflag_CPU_FFT
!!        integer(kind = kint), intent(in) :: Nsmp
!!        integer(kind = kint), intent(in) :: Ncomp, Ncomp_GPU, Ncomp_CPU
!!        integer(kind = kint), intent(in) :: Nfft
!!        type(calypso_rocFFT_params), intent(inout) :: fwd_rocFFT
!!        type(calypso_rocFFT_params), intent(inout) :: bwd_rocFFT
!!        type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!!        type(working_FFTs), intent(inout) :: WK_FFTs
!!      subroutine finalize_rocFFT_FFTs(iflag_CPU_FFT, Nsmp,            &
!!     &          fwd_rocFFT, bwd_rocFFT, WK_rocFFT, WK_FFTs)
!!        integer(kind = kint), intent(in) :: iflag_CPU_FFT
!!        integer(kind = kint), intent(in) :: Nsmp
!!        type(calypso_rocFFT_params), intent(inout) :: fwd_rocFFT
!!        type(calypso_rocFFT_params), intent(inout) :: bwd_rocFFT
!!        type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!!        type(working_FFTs), intent(inout) :: WK_FFTs
!!
!!      subroutine sel_pout_fwd_rocFFT_FFTs                             &
!!     &         (iflag_GPU_FFT, iflag_CPU_FFT, Ncomp, Ncomp_CPU,       &
!!     &          fwd_rocFFT, WK_rocFFT, WK_FFTs, X, elapsed)
!!        integer(kind = kint), intent(in) :: iflag_GPU_FFT
!!        integer(kind = kint), intent(in) :: iflag_CPU_FFT
!!        integer(kind = kint), intent(in) :: Ncomp, Ncomp_CPU
!!        type(calypso_rocFFT_params), intent(in), target :: fwd_rocFFT
!!        type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!!        type(working_FFTs), intent(inout) :: WK_FFTs
!!        real(kind = kreal), intent(inout) :: X(Ncomp,fwd_rocFFT%Nfft)
!!        real(kind = kreal), intent(inout) :: elapsed(4)
!!      subroutine sel_pout_bwd_rocFFT_FFTs                             &
!!     &         (iflag_GPU_FFT, iflag_CPU_FFT, Ncomp,  Ncomp_CPU,      &
!!     &          bwd_rocFFT, WK_rocFFT, WK_FFTs, X, elapsed)
!!        integer(kind = kint), intent(in) :: iflag_GPU_FFT
!!        integer(kind = kint), intent(in) :: iflag_CPU_FFT
!!        integer(kind = kint), intent(in) :: Ncomp, Ncomp_CPU
!!        type(calypso_rocFFT_params), intent(in), target :: bwd_rocFFT
!!        type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!!        type(working_FFTs), intent(inout) :: WK_FFTs
!!        real(kind = kreal), intent(inout) :: X(Ncomp,bwd_rocFFT%Nfft)
!!        real(kind = kreal), intent(inout) :: elapsed(4)
!!@endverbatim
      module sel_pout_rocFFT_CPU_FFT
!
      use t_multi_rocFFT_wrapper
      use t_FFT_selector
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_pout_rocFFT_FFTs(iflag_CPU_FFT,                   &
     &          Ncomp, Ncomp_GPU, Ncomp_CPU, Nfft, Nsmp,                &
     &          fwd_rocFFT, bwd_rocFFT, WK_rocFFT, WK_FFTs)
!
      use multi_pout_complex_rocFFT
      use select_multi_FFT_init
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: iflag_CPU_FFT
      integer(kind = kint), intent(in) :: Nsmp
      integer(kind = kint), intent(in) :: Ncomp, Ncomp_GPU, Ncomp_CPU
      integer(kind = kint), intent(in) :: Nfft
!
      type(calypso_rocFFT_params), intent(inout) :: fwd_rocFFT
      type(calypso_rocFFT_params), intent(inout) :: bwd_rocFFT
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_FFTs), intent(inout) :: WK_FFTs
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
      call calypso_pout_rocFFT_init(Ncomp_GPU, Ncomp_GPU, Nfft,         &
     &                              fwd_rocFFT, bwd_rocFFT, WK_rocFFT)
      call sel_multi_FFT_init(iflag_CPU_FFT, Nsmp, istack_smp,          &
     &                        Ncomp_CPU, Nfft, WK_FFTs)
      deallocate(istack_smp)
!
      end subroutine init_pout_rocFFT_FFTs
!
! ------------------------------------------------------------------
!
      subroutine finalize_rocFFT_FFTs(iflag_CPU_FFT, Nsmp,              &
     &          fwd_rocFFT, bwd_rocFFT, WK_rocFFT, WK_FFTs)
!
      use select_multi_FFT_init
!
      integer(kind = kint), intent(in) :: iflag_CPU_FFT
      integer(kind = kint), intent(in) :: Nsmp
!
      type(calypso_rocFFT_params), intent(inout) :: fwd_rocFFT
      type(calypso_rocFFT_params), intent(inout) :: bwd_rocFFT
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_FFTs), intent(inout) :: WK_FFTs
!
!
      call calypso_rocFFT_fin(fwd_rocFFT, bwd_rocFFT, WK_rocFFT)
      call sel_multi_FFT_fin(iflag_CPU_FFT, Nsmp, WK_FFTs)
!
      end subroutine finalize_rocFFT_FFTs
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sel_pout_fwd_rocFFT_FFTs                               &
     &         (iflag_GPU_FFT, iflag_CPU_FFT, Ncomp, Ncomp_CPU,         &
     &          fwd_rocFFT, WK_rocFFT, WK_FFTs, X, elapsed)
!
      use select_pout_rocFFT
      use multi_pout_FFT_select
      use sel_copy_field_pout_FFT
      use copy_field_for_FFT
!
      integer(kind = kint), intent(in) :: iflag_GPU_FFT
      integer(kind = kint), intent(in) :: iflag_CPU_FFT
      integer(kind = kint), intent(in) :: Ncomp, Ncomp_CPU
      type(calypso_rocFFT_params), intent(in), target :: fwd_rocFFT
!
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_FFTs), intent(inout) :: WK_FFTs
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
      call sel_norm_pout_field_to_FFT(iflag_CPU_FFT, Ncomp_CPU,         &
     &    Ncomp, int(fwd_rocFFT%Nfft), int(fwd_rocFFT%Ncomp+1), X(1,1), &
     &    WK_FFTs)
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
!!   1. Create a CPU thread team
!$omp parallel
!!   2. Isolate a single thread to spawn the GPU work asynchronously
!$omp single
      st_g = OMP_GET_WTIME()
      call select_forward_rocFFT(iflag_GPU_FFT,                         &
     &                           fwd_rocFFT, WK_rocFFT)
      elapsed(4) = elapsed(4) + OMP_GET_WTIME() - st_g
!$omp end single nowait
!
!!   3. The rest of the CPU threads immediately and execute
      st_c = OMP_GET_WTIME()
      call select_fwd_pout_FFT_smp(iflag_CPU_FFT, Ncomp_CPU,            &
     &                             int(fwd_rocFFT%Nfft), WK_FFTs)
      elapsed(3) = elapsed(3) + OMP_GET_WTIME() - st_c
!$omp end parallel
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call sel_norm_pout_from_fwd_rocFFT                                &
     &   (iflag_GPU_FFT, fwd_rocFFT, WK_rocFFT, ione, Ncomp, X(1,1))
      call sel_norm_pout_spectr_from_FFT                                &
     &   (iflag_CPU_FFT, Ncomp_CPU, WK_FFTs, Ncomp,                     &
     &    int(fwd_rocFFT%Nfft), int(fwd_rocFFT%Ncomp+1), X(1,1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
      end subroutine sel_pout_fwd_rocFFT_FFTs
!
! ------------------------------------------------------------------
!
      subroutine sel_pout_bwd_rocFFT_FFTs                               &
     &         (iflag_GPU_FFT, iflag_CPU_FFT, Ncomp,  Ncomp_CPU,        &
     &          bwd_rocFFT, WK_rocFFT, WK_FFTs, X, elapsed)
!
      use select_pout_rocFFT
      use multi_pout_FFT_select
      use sel_copy_field_pout_FFT
      use copy_field_for_FFT
!
      integer(kind = kint), intent(in) :: iflag_GPU_FFT
      integer(kind = kint), intent(in) :: iflag_CPU_FFT
      integer(kind = kint), intent(in) :: Ncomp, Ncomp_CPU
      type(calypso_rocFFT_params), intent(in), target :: bwd_rocFFT
!
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_FFTs), intent(inout) :: WK_FFTs
      real(kind = kreal), intent(inout) :: X(Ncomp,bwd_rocFFT%Nfft)
      real(kind = kreal), intent(inout) :: elapsed(4)
!
      real(kind = kreal) :: start
      real(kind = kreal) :: st_c, st_g
!
!
      start = OMP_GET_WTIME()
      call sel_norm_rtp_to_bwd_rocFFT(iflag_GPU_FFT, ione, Ncomp,       &
     &                                X(1,1), bwd_rocFFT, WK_rocFFT)
      call sel_copy_pout_spectr_to_FFT(iflag_CPU_FFT, Ncomp_CPU,        &
     &    Ncomp, int(bwd_rocFFT%Nfft), int(bwd_rocFFT%Ncomp+1),         &
     &    X(1,1), WK_FFTs)
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
!!   1. Create a CPU thread team
      start = OMP_GET_WTIME()
!$omp parallel
!!   2. Isolate a single thread to spawn the GPU work asynchronously
!$omp single
      st_g = OMP_GET_WTIME()
      call select_backward_rocFFT(iflag_GPU_FFT,                        &
     &                            bwd_rocFFT, WK_rocFFT)
      elapsed(4) = elapsed(4) + OMP_GET_WTIME() - st_g
!$omp end single nowait
!
!!   3. The rest of the CPU threads immediately and execute
      st_c = OMP_GET_WTIME()
      call select_bwd_pout_FFT_smp(iflag_CPU_FFT, Ncomp_CPU,            &
     &                             int(bwd_rocFFT%Nfft), WK_FFTs)
      elapsed(3) = elapsed(3) + OMP_GET_WTIME() - st_c
!$omp end parallel
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call copy_pout_field_from_FFT(int(bwd_rocFFT%Ncomp),              &
     &    int(WK_rocFFT%Nfft_r), WK_rocFFT%X_rocFFT(1),                 &
     &    Ncomp, int(bwd_rocFFT%Nfft), ione, X(1,1))
      call sel_copy_pout_field_from_FFT                                 &
     &   (iflag_CPU_FFT, Ncomp_CPU, WK_FFTs,                            &
     &    Ncomp, int(bwd_rocFFT%Nfft), int(bwd_rocFFT%Ncomp+1), X)
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
      end subroutine sel_pout_bwd_rocFFT_FFTs
!
! ------------------------------------------------------------------
!
      end module sel_pout_rocFFT_CPU_FFT
