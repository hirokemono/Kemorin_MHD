!>@file   sel_pin_rocFFT_FFTPACK.f90
!!@brief  module sel_pin_rocFFT_FFTPACK
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2026
!!
!!
!>@brief  FFT by rocFFT and FFTPACK5 with innermost data series
!!
!!@verbatim
!!      subroutine init_pin_rocFFT_FFTPACK                              &
!!     &         (Ncomp, Ncomp_GPU, Ncomp_CPU, Nfft, Nsmp,              &
!!     &          fwd_rocFFT, bwd_rocFFT, WK_rocFFT, WK_FFTPACK)
!!      subroutine finalize_rocFFT_FFTPACK(fwd_rocFFT, bwd_rocFFT,      &
!!     &                                   WK_rocFFT, WK_FFTPACK)
!!        integer(kind = kint), intent(in) :: Nsmp
!!        integer(kind = kint), intent(in) :: Ncomp, Ncomp_GPU, Ncomp_CPU
!!        integer(kind = kint), intent(in) :: Nfft
!!        type(calypso_rocFFT_params), intent(inout) :: fwd_rocFFT
!!        type(calypso_rocFFT_params), intent(inout) :: bwd_rocFFT
!!        type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!!        type(working_FFTPACK), intent(inout) :: WK_FFTPACK
!!
!!      subroutine sel_pin_fwd_rocFFT_FFTPACK(iflag_GPU_FFT, Ncomp,     &
!!     &          fwd_rocFFT, WK_rocFFT, WK_FFTPACK, X, elapsed)
!!        integer(kind = kint), intent(in) :: iflag_GPU_FFT
!!        integer(kind = kint), intent(in) :: Ncomp
!!        type(calypso_rocFFT_params), intent(in), target :: fwd_rocFFT
!!        type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!!        type(working_FFTPACK), intent(inout) :: WK_FFTPACK
!!        real(kind = kreal), intent(inout) :: X(fwd_rocFFT%Nfft,Ncomp)
!!        real(kind = kreal), intent(inout) :: elapsed(4)
!!      subroutine sel_pin_bwd_rocFFT_FFTPACK(iflag_GPU_FFT, Ncomp,     &
!!     &          bwd_rocFFT, WK_rocFFT, WK_FFTPACK, X, elapsed)
!!        integer(kind = kint), intent(in) :: iflag_GPU_FFT
!!        integer(kind = kint), intent(in) :: Ncomp
!!        type(calypso_rocFFT_params), intent(in), target :: bwd_rocFFT
!!        type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!!        type(working_FFTPACK), intent(inout) :: WK_FFTPACK
!!        real(kind = kreal), intent(inout) :: X(bwd_rocFFT%Nfft,Ncomp)
!!        real(kind = kreal), intent(inout) :: elapsed(4)
!!@endverbatim
      module sel_pin_rocFFT_FFTPACK
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
      subroutine init_pin_rocFFT_FFTPACK                                &
     &         (Ncomp, Ncomp_GPU, Ncomp_CPU, Nfft, Nsmp,                &
     &          fwd_rocFFT, bwd_rocFFT, WK_rocFFT, WK_FFTPACK)
!
      use multi_pin_complex_rocFFT
      use calypso_multi_fftpack
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: Nsmp
      integer(kind = kint), intent(in) :: Ncomp, Ncomp_GPU, Ncomp_CPU
      integer(kind = kint), intent(in) :: Nfft
!
      type(calypso_rocFFT_params), intent(inout) :: fwd_rocFFT
      type(calypso_rocFFT_params), intent(inout) :: bwd_rocFFT
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_FFTPACK), intent(inout) :: WK_FFTPACK
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
      call init_WK_FFTPACK_t(Nsmp, istack_smp, Nfft, WK_FFTPACK)
      deallocate(istack_smp)
!
      end subroutine init_pin_rocFFT_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine finalize_rocFFT_FFTPACK(fwd_rocFFT, bwd_rocFFT,        &
     &                                   WK_rocFFT, WK_FFTPACK)
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
      end subroutine finalize_rocFFT_FFTPACK
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sel_pin_fwd_rocFFT_FFTPACK(iflag_GPU_FFT, Ncomp,       &
     &          fwd_rocFFT, WK_rocFFT, WK_FFTPACK, X, elapsed)
!
      use select_pin_rocFFT
      use select_pout_rocFFT
      use multi_pin_FFTPACK_smp
      use swap_prt_data_for_FFTPACK
!
      integer(kind = kint), intent(in) :: iflag_GPU_FFT
      integer(kind = kint), intent(in) :: Ncomp
      type(calypso_rocFFT_params), intent(in), target :: fwd_rocFFT
!
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_FFTPACK), intent(inout) :: WK_FFTPACK
!
      real(kind = kreal), intent(inout) :: X(fwd_rocFFT%Nfft,Ncomp)
      real(kind = kreal), intent(inout) :: elapsed(4)
!
      real(kind = kreal) :: start, st_c, st_g
!
!
      start = OMP_GET_WTIME()
      call sel_copy_pin_field_to_FFT                                    &
     &   (int(fwd_rocFFT%Ncomp), int(fwd_rocFFT%Nfft), X(1,1),          &
     &    int(WK_rocFFT%Nfft_r), WK_rocFFT%X_rocFFT(1))
      call swap_prt_fld_to_RFFTMF(WK_FFTPACK%Nplan_FFTPACK,             &
     &    WK_FFTPACK%istack_FFTPACK, WK_FFTPACK%Mmax_smp,               &
     &    int(fwd_rocFFT%Nfft), Ncomp, X(1,1), WK_FFTPACK%X_FFTPACK5)
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
      call select_forward_rocFFT(iflag_GPU_FFT, fwd_rocFFT, WK_rocFFT)
      elapsed(4) = elapsed(4) + OMP_GET_WTIME() - st_g
!$omp end single nowait
!
!!   3. The rest of the CPU threads immediately and execute
!      write(*,*) 'FFT loop start', OMP_GET_WTIME() - start
      st_c = OMP_GET_WTIME()
      call multi_pin_RFFTMF_smp(WK_FFTPACK%Nplan_FFTPACK,               &
     &    WK_FFTPACK%istack_FFTPACK, WK_FFTPACK%Mmax_smp,               &
     &    int(fwd_rocFFT%Nfft), WK_FFTPACK%X_FFTPACK5,                  &
     &    WK_FFTPACK%lsave_FFTPACK, WK_FFTPACK%WSAVE_FFTPACK,           &
     &    WK_FFTPACK%WORK_FFTPACK)
      elapsed(3) = elapsed(3) + OMP_GET_WTIME() - st_c
!$omp end parallel
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call sel_norm_pin_from_fwd_rocFFT                                 &
     &   (iflag_GPU_FFT, fwd_rocFFT, WK_rocFFT, ione, Ncomp, X(1,1))
      call swap_prt_spectr_from_RFFTMF(WK_FFTPACK%Nplan_FFTPACK,        &
     &   WK_FFTPACK%istack_FFTPACK, WK_FFTPACK%Mmax_smp,                &
     &   int(fwd_rocFFT%Nfft), WK_FFTPACK%X_FFTPACK5, Ncomp, X(1,1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
!      write(*,*) 'CPU FFT clock',   elapsed(3)
!      write(*,*) 'GPU FFT clock',   elapsed(4)
!      write(*,*) 'Total FFT clock', elapsed(1)
!
      end subroutine sel_pin_fwd_rocFFT_FFTPACK
!
! ------------------------------------------------------------------
!
      subroutine sel_pin_bwd_rocFFT_FFTPACK(iflag_GPU_FFT, Ncomp,       &
     &          bwd_rocFFT, WK_rocFFT, WK_FFTPACK, X, elapsed)
!
      use select_pin_rocFFT
      use select_pout_rocFFT
      use multi_pin_FFTPACK_smp
      use swap_prt_data_for_FFTPACK
!
      integer(kind = kint), intent(in) :: iflag_GPU_FFT
      integer(kind = kint), intent(in) :: Ncomp
      type(calypso_rocFFT_params), intent(in), target :: bwd_rocFFT
!
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      type(working_FFTPACK), intent(inout) :: WK_FFTPACK
      real(kind = kreal), intent(inout) :: X(bwd_rocFFT%Nfft,Ncomp)
      real(kind = kreal), intent(inout) :: elapsed(4)
!
      real(kind = kreal) :: start
      real(kind = kreal) :: st_c, st_g
!
      start = OMP_GET_WTIME()
      call sel_norm_prt_to_bwd_rocFFT(iflag_GPU_FFT, ione, Ncomp,       &
     &                                X(1,1), bwd_rocFFT, WK_rocFFT)
      call swap_prt_spectr_to_RFFTMB(WK_FFTPACK%Nplan_FFTPACK,          &
     &   WK_FFTPACK%istack_FFTPACK, WK_FFTPACK%Mmax_smp,                &
     &   int(bwd_rocFFT%Nfft), Ncomp, X(1,1), WK_FFTPACK%X_FFTPACK5)
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
!      write(*,*) 'OMP parallel start', OMP_GET_WTIME()
!!   1. Create a CPU thread team
      start = OMP_GET_WTIME()
!$omp parallel
!!   2. Isolate a single thread to spawn the GPU work asynchronously
!$omp single
      st_g = OMP_GET_WTIME()
      call select_backward_rocFFT(iflag_GPU_FFT, bwd_rocFFT, WK_rocFFT)
      elapsed(4) = elapsed(4) + OMP_GET_WTIME() - st_g
!$omp end single nowait
!
!!   3. The rest of the CPU threads immediately and execute
      st_c = OMP_GET_WTIME()
      call multi_pin_RFFTMB_smp(WK_FFTPACK%Nplan_FFTPACK,               &
     &    WK_FFTPACK%istack_FFTPACK, WK_FFTPACK%Mmax_smp,               &
     &    int(bwd_rocFFT%Nfft), WK_FFTPACK%X_FFTPACK5,                  &
     &    WK_FFTPACK%lsave_FFTPACK, WK_FFTPACK%WSAVE_FFTPACK,           &
     &    WK_FFTPACK%WORK_FFTPACK)
      elapsed(3) = elapsed(3) + OMP_GET_WTIME() - st_c
!$omp end parallel
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call sel_copy_pin_field_from_FFT(int(bwd_rocFFT%Ncomp),           &
     &    int(WK_rocFFT%Nfft_r), WK_rocFFT%X_rocFFT(1),                 &
     &    int(bwd_rocFFT%Nfft), X(1,1))
      call swap_prt_fld_from_RFFTMB(WK_FFTPACK%Nplan_FFTPACK,           &
     &    WK_FFTPACK%istack_FFTPACK, WK_FFTPACK%Mmax_smp,               &
     &    int(bwd_rocFFT%Nfft), WK_FFTPACK%X_FFTPACK5, Ncomp, X(1,1))
      elapsed(2) = elapsed(2) + OMP_GET_WTIME() - start
!
!      write(*,*) 'CPU FFT clock',   elapsed(3)
!      write(*,*) 'GPU FFT clock',   elapsed(4)
!      write(*,*) 'Total FFT clock', elapsed(1)
!
      end subroutine sel_pin_bwd_rocFFT_FFTPACK
!
! ------------------------------------------------------------------
!
      end module sel_pin_rocFFT_FFTPACK
!
