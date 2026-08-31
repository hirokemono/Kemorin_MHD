!>@file   calypso_rocFFT_init.f90
!!@brief  module calypso_rocFFT_init
!!
!!@author H. Matsui
!!@date Programmed in April, 2026
!
!>@brief  Fourier transform using rocFFT
!!
!!@verbatim
!! wrapper subroutine for initialization of rocFFT
!!      subroutine calypso_pin_rocFFT_init(Ncomp_fwd, Ncomp_bwd, Nfft,  &
!!     &                                   fwd, bwd, WK_fft)
!!        integer(kind = kint), intent(in) :: Ncomp_fwd, Ncomp_bwd
!!        integer(kind = kint), intent(in) :: Nfft
!!        type(calypso_rocFFT_params), intent(inout), target :: fwd
!!        type(calypso_rocFFT_params), intent(inout), target :: bwd
!!        type(calypso_rocFFT_work), intent(inout), target :: WK_fft
!!
!!      subroutine calypso_pout_rocFFT_init(Ncomp_fwd, Ncomp_bwd, Nfft, &
!!     &                                     fwd, bwd, WK_fft)
!!        integer(kind = kint), intent(in) :: Ncomp_fwd, Ncomp_bwd
!!        integer(kind = kint), intent(in) :: Nfft
!!        type(calypso_rocFFT_params), intent(inout), target :: fwd
!!        type(calypso_rocFFT_params), intent(inout), target :: bwd
!!        type(calypso_rocFFT_work), intent(inout), target :: WK_fft
!!
!! ------------------------------------------------------------------
!!
!!   a_{k} = \frac{2}{Nfft}
!!          \sum_{j=0}^{Nfft-1} [x_{j} \cos (\frac{2\pi j k}{Nfft})]
!!   b_{k} = \frac{2}{Nfft}
!!          \sum_{j=0}^{Nfft-1} [x_{j} \sin (\frac{2\pi j k}{Nfft})]
!!
!!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!    K = Nfft/2....
!!   a_{k} = \frac{1}{Nfft}
!!          \sum_{j=0}^{Nfft-1} [x_{j} \cos (\frac{2\pi j k}{Nfft})]
!!
!! ------------------------------------------------------------------
!!
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
      module calypso_rocFFT_init
!
      use omp_lib
!
      use m_precision
      use m_constants
      use t_multi_rocFFT_wrapper
!
      implicit none
!
      private :: calypso_pin_fwd_rocFFT_init
      private :: calypso_pin_bwd_rocFFT_init
!
      private :: calypso_pout_fwd_rocFFT_init
      private :: calypso_pout_bwd_rocFFT_init
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine calypso_pin_rocFFT_init(Ncomp_fwd, Ncomp_bwd, Nfft,    &
     &                                   fwd, bwd, WK_fft)
!
      integer(kind = kint), intent(in) :: Ncomp_fwd, Ncomp_bwd
      integer(kind = kint), intent(in) :: Nfft
      type(calypso_rocFFT_params), intent(inout), target :: fwd
      type(calypso_rocFFT_params), intent(inout), target :: bwd
      type(calypso_rocFFT_work), intent(inout), target :: WK_fft
!
!
      call calypso_rocFFT_set_size(Ncomp_fwd, Ncomp_bwd, Nfft,          &
     &                             fwd, bwd, WK_fft)
      call calypso_rocFFT_alloc(fwd, bwd, WK_fft)
!
!   Initialize Forward transform
      call calypso_pin_fwd_rocFFT_init(WK_fft%Nfft_r, fwd)
      if(Ncomp_fwd .gt. 0) call calypso_fwd_rocFFT_init(fwd)
!
!   Initialize Backword transform
      call calypso_pin_bwd_rocFFT_init(WK_fft%Nfft_r, bwd)
      if(Ncomp_bwd .gt. 0) call calypso_bwd_rocFFT_init(bwd)
!
      end subroutine calypso_pin_rocFFT_init
!
! ------------------------------------------------------------------
!
      subroutine calypso_pout_rocFFT_init(Ncomp_fwd, Ncomp_bwd, Nfft,   &
     &                                    fwd, bwd, WK_fft)
!
      integer(kind = kint), intent(in) :: Ncomp_fwd, Ncomp_bwd
      integer(kind = kint), intent(in) :: Nfft
      type(calypso_rocFFT_params), intent(inout), target :: fwd
      type(calypso_rocFFT_params), intent(inout), target :: bwd
      type(calypso_rocFFT_work), intent(inout), target :: WK_fft
!
!
      call calypso_rocFFT_set_size(Ncomp_fwd, Ncomp_bwd, Nfft,          &
     &                             fwd, bwd, WK_fft)
      call calypso_rocFFT_alloc(fwd, bwd, WK_fft)
!
!   Initialize Forward transform
      call calypso_pout_fwd_rocFFT_init(fwd)
      call calypso_fwd_rocFFT_init(fwd)
!
!   Initialize Backword transform
      call calypso_pout_bwd_rocFFT_init(bwd)
      call calypso_bwd_rocFFT_init(bwd)
!
      end subroutine calypso_pout_rocFFT_init
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine calypso_pin_fwd_rocFFT_init(Nfft_r, fwd)
!
      integer(c_size_t), intent(in) :: Nfft_r
      type(calypso_rocFFT_params), intent(inout), target :: fwd
!
!   Initialize Forward transform
      fwd%in_strides_size =   1
      fwd%in_strides(1) =     1
      fwd%in_distance =       Nfft_r
      fwd%out_strides_size =  0
      fwd%out_strides(1) =    1
      fwd%out_distance =      0
!
      end subroutine calypso_pin_fwd_rocFFT_init
!
! ------------------------------------------------------------------
!
      subroutine calypso_pout_fwd_rocFFT_init(fwd)
!
      type(calypso_rocFFT_params), intent(inout), target :: fwd
!
!   Initialize Forward transform
      fwd%in_strides_size =  1
      fwd%in_strides(1) =    fwd%Ncomp
      fwd%in_distance =      1
      fwd%out_strides_size = 1
      fwd%out_strides(1) =   fwd%Ncomp
      fwd%out_distance =     1
!
      end subroutine calypso_pout_fwd_rocFFT_init
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine calypso_pin_bwd_rocFFT_init(Nfft_r, bwd)
!
      integer(c_size_t), intent(in) :: Nfft_r
      type(calypso_rocFFT_params), intent(inout), target :: bwd
!
!   Initialize Backword transform
      bwd%in_strides_size =  1
      bwd%in_strides(1) =    1
      bwd%in_distance =      0
      bwd%out_strides_size = 1
      bwd%out_strides(1) =   1
      bwd%out_distance =     Nfft_r
!
      end subroutine calypso_pin_bwd_rocFFT_init
!
! ------------------------------------------------------------------
!
      subroutine calypso_pout_bwd_rocFFT_init(bwd)
!
      type(calypso_rocFFT_params), intent(inout), target :: bwd
!
!   Initialize Forward transform
      bwd%in_strides_size =  1
      bwd%in_strides(1) =    bwd%Ncomp
      bwd%in_distance =      1
      bwd%out_strides_size = 1
      bwd%out_strides(1) =   bwd%Ncomp
      bwd%out_distance =     1
!
      end subroutine calypso_pout_bwd_rocFFT_init
!
! ------------------------------------------------------------------
!
      end module calypso_rocFFT_init
