!>@file   multi_pout_ROCmFFT_offload.f90
!!@brief  module multi_pout_ROCmFFT_offload
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! wrapper subroutine for initialization of ROCmFFT
!!      subroutine calypso_pout_ROCmFFT_init(Ncomp_fwd, Ncomp_bwd, Nfft,&
!!     &                                     fwd, bwd, WK_fft)
!!        integer(kind = kint), intent(in) :: Ncomp_fwd, Ncomp_bwd
!!        integer(kind = kint), intent(in) :: Nfft
!!        type(calypso_ROCmfft_params), intent(inout), target :: fwd
!!        type(calypso_ROCmfft_params), intent(inout), target :: bwd
!!        type(calypso_ROCmfft_work), intent(inout), target :: WK_fft
!!
!! wrapper subroutine for forward Fourier transform by ROCmFFT
!!      subroutine multi_pout_fwd_ROCmFFT(fwd, WK_fwd, X,               &
!!     &                                  elapsed_fft, elapsed_cpy)
!!        type(calypso_ROCmfft_params), intent(in), target :: fwd
!!        type(calypso_ROCmfft_work), intent(inout) :: WK_fwd
!!        real(kind = kreal), intent(inout) :: X(fwd%Ncomp,fwd%Nfft)
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!! ------------------------------------------------------------------
!!
!!   a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!   b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \sin (\frac{2\pi j k}{Nfft})
!!
!!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!    K = Nfft/2....
!!   a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for backward Fourier transform by ROCmFFT
!!      subroutine multi_pout_bwd_ROCmFFT(bwd, WK_bwd, X,               &
!!     &                                  elapsed_fft, elapsed_cpy)
!!        type(calypso_ROCmfft_params), intent(in), target :: bwd
!!        type(calypso_ROCmfft_work), intent(inout) :: WK_bwd
!!        real(kind = kreal), intent(inout) :: X(bwd%Ncomp,bwd%Nfft)
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!! ------------------------------------------------------------------
!!
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
!
      module multi_pout_ROCmFFT_offload
!
      use omp_lib
!
      use m_precision
      use m_constants
      use t_ROCmFFT_wrapper
!
      implicit none
!
      private :: calypso_pout_fwd_ROCmFFT_init
      private :: calypso_pout_bwd_ROCmFFT_init
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine calypso_pout_ROCmFFT_init(Ncomp_fwd, Ncomp_bwd, Nfft,  &
     &                                     fwd, bwd, WK_fft)
!
      integer(kind = kint), intent(in) :: Ncomp_fwd, Ncomp_bwd
      integer(kind = kint), intent(in) :: Nfft
      type(calypso_ROCmfft_params), intent(inout), target :: fwd
      type(calypso_ROCmfft_params), intent(inout), target :: bwd
      type(calypso_ROCmfft_work), intent(inout), target :: WK_fft
!
!
      call calypso_ROCmFFT_set_size(Ncomp, Nfft, fwd, bwd, WK_fft)
      call calypso_ROCmFFT_alloc(fwd, bwd, WK_fft)
!
!   Initialize Forward transform
      call calypso_pout_fwd_ROCmFFT_init(fwd)
      call calypso_fwd_ROCmFFT_init(fwd)
!
!   Initialize Backword transform
      call calypso_pout_bwd_ROCmFFT_init(bwd)
      call calypso_bwd_ROCmFFT_init(bwd)
!
      write(*,*) 'takotakotako'
!
      end subroutine calypso_pout_ROCmFFT_init
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine calypso_pout_fwd_ROCmFFT_init(fwd)
!
      type(calypso_ROCmfft_params), intent(inout), target :: fwd
!
!   Initialize Forward transform
      fwd%in_strides_size =  1
      fwd%in_strides(1) =    fwd%Ncomp
      fwd%in_distance =      1
      fwd%out_strides_size = 1
      fwd%out_strides(1) =   fwd%Ncomp
      fwd%out_distance =     1
!
      end subroutine calypso_pout_fwd_ROCmFFT_init
!
! ------------------------------------------------------------------
!
      subroutine calypso_pout_bwd_ROCmFFT_init(bwd)
!
      type(calypso_ROCmfft_params), intent(inout), target :: bwd
!
!   Initialize Forward transform
      bwd%in_strides_size =  1
      bwd%in_strides(1) =    bwd%Ncomp
      bwd%in_distance =      1
      bwd%out_strides_size = 1
      bwd%out_strides(1) =   bwd%Ncomp
      bwd%out_distance =     1
!
      end subroutine calypso_pout_bwd_ROCmFFT_init
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine multi_pout_fwd_ROCmFFT(fwd, WK_fwd, X,                 &
     &                                  elapsed_fft, elapsed_cpy)
!
      use normalize_for_OMP_FFTW
!
      type(calypso_ROCmfft_params), intent(in), target :: fwd
!
      type(calypso_ROCmfft_work), intent(inout) :: WK_fwd
      real(kind = kreal), intent(inout) :: X(fwd%Ncomp,fwd%Nfft)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
      integer(kind = kint) :: i, ist
!
!
        start = OMP_GET_WTIME()
!$omp parallel do private(i,ist)
        do i = 1, fwd%Nfft
          ist = (i-1) * fwd%Ncomp
          WK_fwd%X_ROCmFFT(ist+1:ist+fwd%Ncomp) = X(1:fwd%Ncomp,i)
        end do
!$omp end parallel do
        if(fwd%Nfft .lt. WK_fwd%Nfft_r) then
!$omp parallel do private(i,ist)
          do i = fwd%Nfft+1, WK_fwd%Nfft_r
            ist = (i-1) * fwd%Ncomp
            WK_fwd%X_ROCmFFT(ist+1:ist+fwd%Ncomp) = 0.0d0
          end do
!$omp end parallel do
        end if
        elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call calypso_forward_ROCmFFT(fwd,                               &
     &                            WK_fwd%Nfft_r, WK_fwd%X_ROCmFFT(1),   &
     &                            WK_fwd%Nfft_c, WK_fwd%C_ROCmFFT(1),   &
     &                            WK_fwd%Nbytes, WK_fwd%data_ptr)
        elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call norm_rtp_from_fwd_OMP_FFTW(int(fwd%Ncomp), WK_fwd%aNfft,   &
     &                           WK_fwd%NFFT_c, WK_fwd%C_ROCmFFT(1),    &
     &                           int(fwd%Nfft), X(1,1))
        elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine multi_pout_fwd_ROCmFFT
!
! ------------------------------------------------------------------
!
      subroutine multi_pout_bwd_ROCmFFT(bwd, WK_bwd, X,                 &
     &                                  elapsed_fft, elapsed_cpy)
!
      use normalize_for_OMP_FFTW
!
      type(calypso_ROCmfft_params), intent(in), target :: bwd
!
      type(calypso_ROCmfft_work), intent(inout) :: WK_bwd
      real(kind = kreal), intent(inout) :: X(bwd%Ncomp,bwd%Nfft)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
      integer(kind = kint) :: i
!
!
      start = OMP_GET_WTIME()
      call norm_rtp_to_bwd_OMP_FFTW(int(bwd%Ncomp), int(bwd%Nfft),      &
     &    X(1,1), WK_bwd%NFFT_c, WK_bwd%C_ROCmFFT(1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call calypso_backward_ROCmFFT(bwd,                                &
     &                            WK_bwd%Nfft_c, WK_bwd%C_ROCmFFT(1),   &
     &                            WK_bwd%Nfft_r, WK_bwd%X_ROCmFFT(1),   &
     &                            WK_bwd%Nbytes, WK_bwd%data_ptr)
      elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
!$omp parallel  workshare
      X(1:bwd%Ncomp*bwd%Nfft) = WK_bwd%X_ROCmFFT(1:bwd%Ncomp*bwd%Nfft)
!$omp end workshare
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine multi_pout_bwd_ROCmFFT
!
! ------------------------------------------------------------------
!
      end module multi_pout_ROCmFFT_offload
