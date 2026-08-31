!>@file   multi_FFT_select.F90
!!@brief  module multi_FFT_select
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2026
!
!>@brief FFT selector
!!
!!@verbatim
!! ------------------------------------------------------------------
!! wrapper subroutine for forward Fourier transform
!! ------------------------------------------------------------------
!!
!!      subroutine select_pin_fwd_FFTs(iflag_FFT, Ncomp, Nfft, X,       &
!!     &          WK_FFTs, elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) :: iflag_FFT
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft
!!        type(working_FFTs), intent(inout) :: WK_FFTs
!!        real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!!      subroutine select_pout_fwd_FFTs(iflag_FFT, Ncomp, Nfft, X,      &
!!     &          WK_FFTs, elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) :: iflag_FFT
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft
!!        type(working_FFTs), intent(inout) :: WK_FFTs
!!        real(kind = kreal), intent(inout) :: X(Ncomp, Nfft)
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!!
!! ------------------------------------------------------------------
!!   a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!          *  \cos (\frac{2\pi j k}{Nfft})
!!   b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!          *  \sin (\frac{2\pi j k}{Nfft})
!!
!!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!    K = Nfft/2....
!!   a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!          * \cos (\frac{2\pi j k}{Nfft})
!!
!! ------------------------------------------------------------------
!!
!! ------------------------------------------------------------------
!! wrapper subroutine for backward Fourier transform
!! ------------------------------------------------------------------
!!
!!      subroutine select_pin_bwd_FFTs(iflag_FFT, Ncomp, Nfft, X,       &
!!     &          WK_FFTs, elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) :: iflag_FFT
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft
!!        type(working_FFTs), intent(inout) :: WK_FFTs
!!        real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!!      subroutine select_pout_bwd_FFTs(iflag_FFT, Ncomp, Nfft, X,      &
!!     &          WK_FFTs, elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) :: iflag_FFT
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft
!!        type(working_FFTs), intent(inout) :: WK_FFTs
!!        real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!!
!! ------------------------------------------------------------------
!!   x_{k} = a_{0} + (-1)^{j} a_{Nfft/2} + sum_{k=1}^{Nfft/2-1}
!!          (a_{k} \cos(2\pijk/Nfft) + b_{k} \sin(2\pijk/Nfft))
!!
!! ------------------------------------------------------------------
!!
!!       i = 1:     a_{0}
!!       i = 2:     a_{Nfft/2}
!!       i = 3:     a_{1}
!!       i = 4:     b_{1}
!!       ...
!!       i = 2*k+1: a_{k}
!!       i = 2*k+2: b_{k}
!!       ...
!!       i = Nfft-1:   a_{Nfft/2-1}
!!       i = Nfft:     b_{Nfft/2-1}
!!
!! ------------------------------------------------------------------
!!@endverbatim
!!
!!@n @param Nsmp  Number of SMP processors
!!@n @param Nstacksmp(0:Nsmp)   End number for each SMP process
!!@n @param M           Number of components for Fourier transforms
!!@n @param Nfft        Data length for eadh FFT
!!@n @param X(Nfft,M)  Data for Fourier transform
!!
!!@n @param Mmax_smp    Maximum number of component for each SMP process
!!@n @param X_FFTPACK5(Mmax_smp*Nfft,Nsmp)
!!                 Data for multiple Fourier transform
!!@n @param lSAVE                     Size of work constant for FFTPACK
!!@n @param WSAVE(lSAVE)              Work constatnts for FFTPACK
!!@n @param WORK(Mmax_smp*Nfft,Nsmp)  Work area for FFTPACK
!
      module multi_FFT_select
!
      use omp_lib
!
      use m_precision
      use m_constants
!
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
      subroutine select_pin_fwd_FFTs(iflag_FFT, Ncomp, Nfft, X,         &
     &          WK_FFTs, elapsed_fft, elapsed_cpy)
!
      use multi_pin_FFT_select
      use sel_swap_field_pin_FFT
!
      integer(kind = kint), intent(in) :: iflag_FFT
      integer(kind = kint), intent(in) :: Ncomp, Nfft
!
      type(working_FFTs), intent(inout) :: WK_FFTs
      real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
!
!
      start = OMP_GET_WTIME()
      call sel_swap_pin_field_to_FFT(iflag_FFT, Ncomp, Nfft,            &
     &                               X(1,1), WK_FFTs)
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
!$omp parallel
      call select_fwd_pin_FFT_smp(iflag_FFT, Ncomp, Nfft, X, WK_FFTs)
!$omp end parallel
      elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call sel_swap_prt_spectr_from_FFT(iflag_FFT, WK_FFTs,             &
     &                                  Ncomp, Nfft, X(1,1))
!
      end subroutine select_pin_fwd_FFTs
!
! ------------------------------------------------------------------
!
      subroutine select_pin_bwd_FFTs(iflag_FFT, Ncomp, Nfft, X,         &
     &          WK_FFTs, elapsed_fft, elapsed_cpy)
!
      use multi_pin_FFT_select
      use sel_swap_field_pin_FFT
!
      integer(kind = kint), intent(in) :: iflag_FFT
      integer(kind = kint), intent(in) :: Ncomp, Nfft
!
      type(working_FFTs), intent(inout) :: WK_FFTs
      real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
!
!
      start = OMP_GET_WTIME()
      call sel_swap_pin_spectr_to_FFT(iflag_FFT, Ncomp,                 &
     &                                Nfft, X(1,1), WK_FFTs)
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
!$omp parallel
      call select_bwd_pin_FFT_smp(iflag_FFT, WK_FFTs,                   &
     &                            Ncomp, Nfft, X(1,1))
!$omp end parallel
      elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call sel_swap_pin_field_from_FFT(iflag_FFT, WK_FFTs, Ncomp,       &
     &                                 Nfft, X(1,1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine select_pin_bwd_FFTs
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine select_pout_fwd_FFTs(iflag_FFT, Ncomp, Nfft, X,        &
     &          WK_FFTs, elapsed_fft, elapsed_cpy)
!
      use multi_pout_FFT_select
      use sel_copy_field_pout_FFT
!
      integer(kind = kint), intent(in) :: iflag_FFT
      integer(kind = kint), intent(in) :: Ncomp, Nfft
!
      type(working_FFTs), intent(inout) :: WK_FFTs
      real(kind = kreal), intent(inout) :: X(Ncomp, Nfft)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
!
!
      start = OMP_GET_WTIME()
      call sel_norm_pout_field_to_FFT(iflag_FFT, Ncomp, Ncomp, Nfft,    &
     &                                ione, X(1,1), WK_FFTs)
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
!$omp parallel
      call select_fwd_pout_FFT_smp(iflag_FFT, Ncomp, Nfft, WK_FFTs)
!$omp end parallel
      elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call sel_norm_pout_spectr_from_FFT(iflag_FFT, Ncomp, WK_FFTs,     &
     &                                   Ncomp, Nfft, ione, X(1,1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine select_pout_fwd_FFTs
!
! ------------------------------------------------------------------
!
      subroutine select_pout_bwd_FFTs(iflag_FFT, Ncomp, Nfft, X,        &
     &          WK_FFTs, elapsed_fft, elapsed_cpy)
!
      use multi_pout_FFT_select
      use sel_copy_field_pout_FFT
!
      integer(kind = kint), intent(in) :: iflag_FFT
      integer(kind = kint), intent(in) :: Ncomp, Nfft
!
      type(working_FFTs), intent(inout) :: WK_FFTs
      real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
!
!
      start = OMP_GET_WTIME()
      call sel_copy_pout_spectr_to_FFT(iflag_FFT, Ncomp, Ncomp, Nfft,   &
     &                                 ione, X(1,1), WK_FFTs)
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
!$omp parallel
      call select_bwd_pout_FFT_smp(iflag_FFT, Ncomp, Nfft, WK_FFTs)
!$omp end parallel
      elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call sel_copy_pout_field_from_FFT(iflag_FFT, Ncomp, WK_FFTs,      &
     &                                  Ncomp, Nfft, ione, X(1,1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine select_pout_bwd_FFTs
!
! ------------------------------------------------------------------
!
      end module multi_FFT_select
