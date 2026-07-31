!>@file   multi_pout_complex_rocFFT.f90
!!@brief  module multi_pout_complex_rocFFT
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! wrapper subroutine for initialization of rocFFT
!!      subroutine calypso_pout_rocFFT_init(Ncomp_fwd, Ncomp_bwd, Nfft, &
!!     &                                     fwd, bwd, WK_fft)
!!        integer(kind = kint), intent(in) :: Ncomp_fwd, Ncomp_bwd
!!        integer(kind = kint), intent(in) :: Nfft
!!        type(calypso_rocFFT_params), intent(inout), target :: fwd
!!        type(calypso_rocFFT_params), intent(inout), target :: bwd
!!        type(calypso_rocFFT_work), intent(inout), target :: WK_fft
!!
!! wrapper subroutine for forward Fourier transform by rocFFT
!!      subroutine multi_pout_fwd_rocFFT_r2c(fwd, WK_fft, X,            &
!!     &                                     elapsed_fft, elapsed_cpy)
!!        type(calypso_rocFFT_params), intent(in), target :: fwd
!!        type(calypso_rocFFT_work), intent(inout) :: WK_fft
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
!! wrapper subroutine for backward Fourier transform by rocFFT
!!      subroutine multi_pout_bwd_rocFFT_c2r(bwd, WK_fft, X,            &
!!     &                                     elapsed_fft, elapsed_cpy)
!!        type(calypso_rocFFT_params), intent(in), target :: bwd
!!        type(calypso_rocFFT_work), intent(inout) :: WK_fft
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
      module multi_pout_complex_rocFFT
!
      use omp_lib
!
      use m_precision
      use m_constants
      use t_multi_rocFFT_wrapper
!
      implicit none
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
! ------------------------------------------------------------------
!
      subroutine multi_pout_fwd_rocFFT_r2c(fwd, WK_fft, X,              &
     &                                     elapsed_fft, elapsed_cpy)
!
      use normalize_for_OMP_FFTW
      use calypso_multi_rocFFT
!
      type(calypso_rocFFT_params), intent(in), target :: fwd
!
      type(calypso_rocFFT_work), intent(inout) :: WK_fft
      real(kind = kreal), intent(inout) :: X(fwd%Ncomp,fwd%Nfft)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
!
!
      if(fwd%Ncomp .le. 0) return
        start = OMP_GET_WTIME()
        call copy_pout_fld_to_rocFFT_real(fwd%Ncomp, fwd%Nfft, X(1,1),  &
     &      WK_fft%Nfft_r, WK_fft%X_rocFFT(1))
        elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call calypso_forward_rocFFT_r2c                                 &
     &     (fwd%rocFFT_plan, fwd%rocFFT_wk_info, fwd%Ncomp,             &
     &      WK_fft%Nfft_r, WK_fft%X_rocFFT(1),                          &
     &      WK_fft%Nfft_c, WK_fft%C_rocFFT(1),                          &
     &      fwd%Nbytes, WK_fft%data_ptr)
!$omp parallel workshare
        WK_fft%C_rocFFT(1:fwd%Ncomp*WK_fft%Nfft_c)                      &
     &      = WK_fft%aNfft * WK_fft%C_rocFFT(1:fwd%Ncomp*WK_fft%Nfft_c)
!$omp end parallel workshare
        elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
        start = OMP_GET_WTIME()
        call norm_rtp_from_fwd_OMP_FFTW                                 &
     &     (int(fwd%Ncomp), int(WK_fft%NFFT_c), WK_fft%C_rocFFT(1),     &
     &      int(fwd%Nfft), X(1,1))
        elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine multi_pout_fwd_rocFFT_r2c
!
! ------------------------------------------------------------------
!
      subroutine multi_pout_bwd_rocFFT_c2r(bwd, WK_fft, X,              &
     &                                     elapsed_fft, elapsed_cpy)
!
      use normalize_for_OMP_FFTW
      use calypso_multi_rocFFT
!
      type(calypso_rocFFT_params), intent(in), target :: bwd
!
      type(calypso_rocFFT_work), intent(inout) :: WK_fft
      real(kind = kreal), intent(inout) :: X(bwd%Ncomp,bwd%Nfft)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
      integer(c_size_t) :: i, ist
!
!
      if(bwd%Ncomp .le. 0) return
      start = OMP_GET_WTIME()
      call norm_rtp_to_bwd_OMP_FFTW(int(bwd%Ncomp), int(bwd%Nfft),      &
     &    X(1,1), int(WK_fft%NFFT_c), WK_fft%C_rocFFT(1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call calypso_backward_rocFFT_c2r                                  &
     &   (bwd%rocFFT_plan, bwd%rocFFT_wk_info, bwd%Ncomp,               &
     &    WK_fft%Nfft_c, WK_fft%C_rocFFT(1),                            &
     &    WK_fft%Nfft_r, WK_fft%X_rocFFT(1),                            &
     &    bwd%Nbytes, WK_fft%data_ptr)
      elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
!$omp parallel do private(i,ist)
      do i = 1, bwd%Nfft
        ist = (i-1) * bwd%Ncomp
        X(1:bwd%Ncomp,i) = WK_fft%X_rocFFT(ist+1:ist+bwd%Ncomp)
      end do
!$omp end parallel do
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine multi_pout_bwd_rocFFT_c2r
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_pout_fld_to_rocFFT_real(Ncomp, Nfft, X,           &
     &                                        Nfft_r, X_rocFFT)
!
      integer(c_size_t), intent(in) :: Ncomp
      integer(c_size_t), intent(in) :: Nfft, Nfft_r
      real(kind = kreal), intent(in) :: X(Ncomp,Nfft)
!
      real(kind = kreal), intent(inout) :: X_rocFFT(Ncomp,Nfft_r)
!
      integer(c_size_t) :: i
!
!
!$omp parallel do private(i)
        do i = 1, Nfft
          X_rocFFT(1:Ncomp,i) = X(1:Ncomp,i)
        end do
!$omp end parallel do
        if(Nfft .lt. Nfft_r) then
!$omp parallel do private(i)
          do i = Nfft+1, Nfft_r
            X_rocFFT(1:Ncomp,i) = 0.0d0
          end do
!$omp end parallel do
        end if
!
      end subroutine copy_pout_fld_to_rocFFT_real
!
! ------------------------------------------------------------------
!
      end module multi_pout_complex_rocFFT
