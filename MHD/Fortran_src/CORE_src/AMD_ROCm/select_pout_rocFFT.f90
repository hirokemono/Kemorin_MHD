!>@file   select_pout_rocFFT.f90
!!@brief  module select_pout_rocFFT
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief  Selector for rocFFT for outmost series data array
!!
!!@verbatim
!!      subroutine sel_multi_pout_fwd_rocFFT(iflag_FFT, Ncomp,          &
!!     &          fwd_rocFFT, WK_rocFFT, X, elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) :: iflag_FFT
!!        integer(kind = kint), intent(in) :: Ncomp
!!        type(calypso_rocFFT_params), intent(in), target :: fwd_rocFFT
!!        type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!!        real(kind = kreal), intent(inout) :: X(Ncomp,fwd_rocFFT%Nfft)
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!!      subroutine sel_multi_pout_bwd_rocFFT(iflag_FFT, Ncomp,          &
!!     &          bwd_rocFFT, WK_rocFFT, X, elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) :: iflag_FFT
!!        integer(kind = kint), intent(in) :: Ncomp
!!        type(calypso_rocFFT_params), intent(in), target :: bwd_rocFFT
!!        type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!!        real(kind = kreal), intent(inout) :: X(Ncomp,bwd_rocFFT%Nfft)
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!!
!!      subroutine sel_pout_forward_rocFFT(iflag_FFT, fwd_rocFFT,       &
!!     &                                   WK_rocFFT)
!!        integer(kind = kint), intent(in) :: iflag_FFT
!!        type(calypso_rocFFT_params), intent(in), target :: fwd_rocFFT
!!        type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!!      subroutine sel_pout_backward_rocFFT(iflag_fft, bwd_rocFFT,      &
!!     &                                    WK_rocFFT)
!!        integer(kind = kint), intent(in) :: iflag_fft
!!        type(calypso_rocFFT_params), intent(in), target :: bwd_rocFFT
!!        type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!!
!!      subroutine sel_norm_pout_from_fwd_rocFFT                        &
!!     &         (iflag_FFT, fwd_rocFFT, WK_rocFFT, ist_comp, Ncomp, X)
!!        integer(kind = kint), intent(in) :: iflag_FFT
!!        integer(kind = kint), intent(in) :: ist_comp, Ncomp
!!        type(calypso_rocFFT_params), intent(in), target :: fwd_rocFFT
!!        type(calypso_rocFFT_work), intent(in) :: WK_rocFFT
!!        real(kind = kreal), intent(inout) :: X(Ncomp,fwd_rocFFT%Nfft)
!!
!!      subroutine sel_norm_rtp_to_bwd_rocFFT                           &
!!     &         (iflag_FFT, ist_comp, Ncomp, X, bwd_rocFFT, WK_rocFFT)
!!        integer(kind = kint), intent(in) :: iflag_FFT
!!        integer(kind = kint), intent(in) :: ist_comp, Ncomp
!!        type(calypso_rocFFT_params), intent(in), target :: bwd_rocFFT
!!        real(kind = kreal), intent(in) :: X(Ncomp,bwd_rocFFT%Nfft)
!!        type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
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
!!   x_{k} = a_{0} + (-1)^{j} a_{Nfft/2} + sum_{k=1}^{Nfft/2-1}
!!          (a_{k} \cos(2\pijk/Nfft) + b_{k} \sin(2\pijk/Nfft))
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
!! ------------------------------------------------------------------
!!@endverbatim
!
      module select_pout_rocFFT
!
      use m_precision
      use m_constants
      use m_FFT_selector
!
      use t_multi_rocFFT_wrapper
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine sel_multi_pout_fwd_rocFFT(iflag_FFT, Ncomp,            &
     &          fwd_rocFFT, WK_rocFFT, X, elapsed_fft, elapsed_cpy)
!
      use copy_field_for_FFT
!
      integer(kind = kint), intent(in) :: iflag_FFT
      integer(kind = kint), intent(in) :: Ncomp
      type(calypso_rocFFT_params), intent(in), target :: fwd_rocFFT
!
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      real(kind = kreal), intent(inout) :: X(Ncomp,fwd_rocFFT%Nfft)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
!
!
      start = OMP_GET_WTIME()
      call copy_pout_field_to_FFT                                       &
     &   (ione, Ncomp, int(fwd_rocFFT%Nfft), X(1,1),                    &
     &    int(fwd_rocFFT%Ncomp), int(WK_rocFFT%Nfft_r),                 &
     &    WK_rocFFT%X_rocFFT(1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call sel_pout_forward_rocFFT(iflag_FFT, fwd_rocFFT, WK_rocFFT)
      elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call sel_norm_pout_from_fwd_rocFFT                                &
     &   (iflag_FFT, fwd_rocFFT, WK_rocFFT, ione, Ncomp, X(1,1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine sel_multi_pout_fwd_rocFFT
!
! ------------------------------------------------------------------
!
      subroutine sel_multi_pout_bwd_rocFFT(iflag_FFT, Ncomp,            &
     &          bwd_rocFFT, WK_rocFFT, X, elapsed_fft, elapsed_cpy)
!
      use copy_field_for_FFT
!
      integer(kind = kint), intent(in) :: iflag_FFT
      integer(kind = kint), intent(in) :: Ncomp
      type(calypso_rocFFT_params), intent(in), target :: bwd_rocFFT
!
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
      real(kind = kreal), intent(inout) :: X(Ncomp,bwd_rocFFT%Nfft)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
!
!
      start = OMP_GET_WTIME()
      call sel_norm_rtp_to_bwd_rocFFT(iflag_FFT, ione, Ncomp,           &
     &                                X(1,1), bwd_rocFFT, WK_rocFFT)
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call sel_pout_backward_rocFFT(iflag_FFT, bwd_rocFFT, WK_rocFFT)
      elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call copy_pout_field_from_FFT(int(bwd_rocFFT%Ncomp),              &
     &    int(WK_rocFFT%Nfft_r), WK_rocFFT%X_rocFFT(1),                 &
     &    Ncomp, int(bwd_rocFFT%Nfft), ione, X(1,1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine sel_multi_pout_bwd_rocFFT
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sel_pout_forward_rocFFT(iflag_FFT, fwd_rocFFT,         &
     &                                   WK_rocFFT)
!
      use calypso_multi_rocFFT
!
      integer(kind = kint), intent(in) :: iflag_FFT
      type(calypso_rocFFT_params), intent(in), target :: fwd_rocFFT
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!
!
      if((iflag_FFT/10) .eq. (iflag_rocFFT/10)) then
!        write(*,*) 'calypso_forward_rocFFT_r2c'
        call calypso_forward_rocFFT_r2c                                 &
     &     (fwd_rocFFT%rocFFT_plan, fwd_rocFFT%rocFFT_wk_info,          &
     &      fwd_rocFFT%Ncomp, WK_rocFFT%aNfft,                          &
     &      WK_rocFFT%Nfft_r, WK_rocFFT%X_rocFFT(1),                    &
     &      WK_rocFFT%Nfft_c, WK_rocFFT%C_rocFFT(1),                    &
     &      fwd_rocFFT%Nbytes, WK_rocFFT%data_ptr)
      else if((iflag_FFT/10) .eq. (iflag_real_rocFFT/10)) then
!        write(*,*) 'calypso_forward_rocFFT_r2r'
        call calypso_forward_rocFFT_r2r(fwd_rocFFT%rocFFT_plan,         &
     &      fwd_rocFFT%rocFFT_wk_info, fwd_rocFFT%Ncomp,                &
     &      WK_rocFFT%aNfft, WK_rocFFT%Nfft_r, WK_rocFFT%X_rocFFT(1),   &
     &      fwd_rocFFT%Nbytes, WK_rocFFT%data_ptr)
!      else if((iflag_FFT/10) .eq. (iflag_OMP_rocFFT/10)) then
      else
!        write(*,*) 'calypso_fwd_OpenMP_rocFFT'
        call calypso_fwd_OpenMP_rocFFT(fwd_rocFFT%rocFFT_plan,          &
     &      fwd_rocFFT%rocFFT_wk_info, fwd_rocFFT%Ncomp,                &
     &      WK_rocFFT%aNfft, WK_rocFFT%Nfft_r, WK_rocFFT%X_rocFFT(1))
      end if
!
      end subroutine sel_pout_forward_rocFFT
!
! ------------------------------------------------------------------
!
      subroutine sel_pout_backward_rocFFT(iflag_fft, bwd_rocFFT,        &
     &                                    WK_rocFFT)
!
      use calypso_multi_rocFFT
!
      integer(kind = kint), intent(in) :: iflag_fft
      type(calypso_rocFFT_params), intent(in), target :: bwd_rocFFT
!
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!
!
      if((iflag_fft/10) .eq. (iflag_rocFFT/10)) then
!        write(*,*) 'calypso_backward_rocFFT_c2r'
        call calypso_backward_rocFFT_c2r(bwd_rocFFT%rocFFT_plan,        &
     &      bwd_rocFFT%rocFFT_wk_info, bwd_rocFFT%Ncomp,                &
     &      WK_rocFFT%Nfft_c, WK_rocFFT%C_rocFFT(1),                    &
     &      WK_rocFFT%Nfft_r, WK_rocFFT%X_rocFFT(1),                    &
     &      bwd_rocFFT%Nbytes, WK_rocFFT%data_ptr)
      else if((iflag_fft/10) .eq. (iflag_real_rocFFT/10)) then
!        write(*,*) 'calypso_backward_rocFFT_r2r'
        call calypso_backward_rocFFT_r2r                                &
     &     (bwd_rocFFT%rocFFT_plan, bwd_rocFFT%rocFFT_wk_info,          &
     &      bwd_rocFFT%Ncomp, WK_rocFFT%Nfft_r, WK_rocFFT%X_rocFFT(1),  &
     &      bwd_rocFFT%Nbytes, WK_rocFFT%data_ptr)
!      else if((iflag_fft/10) .eq. (iflag_OMP_rocFFT/10)) then
      else
!        write(*,*) 'calypso_bwd_OpenMP_rocFFT'
        call calypso_bwd_OpenMP_rocFFT                                  &
           (bwd_rocFFT%rocFFT_plan, bwd_rocFFT%rocFFT_wk_info,          &
     &      bwd_rocFFT%Ncomp, WK_rocFFT%Nfft_r, WK_rocFFT%X_rocFFT(1))
      end if
!
      end subroutine sel_pout_backward_rocFFT
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sel_norm_pout_from_fwd_rocFFT                          &
     &         (iflag_FFT, fwd_rocFFT, WK_rocFFT, ist_comp, Ncomp, X)
!
      use normalize_for_rocFFT
      use normalize_for_OMP_FFTW
!
      integer(kind = kint), intent(in) :: iflag_FFT
      integer(kind = kint), intent(in) :: ist_comp, Ncomp
      type(calypso_rocFFT_params), intent(in), target :: fwd_rocFFT
      type(calypso_rocFFT_work), intent(in) :: WK_rocFFT
!
      real(kind = kreal), intent(inout) :: X(Ncomp,fwd_rocFFT%Nfft)
!
!
      if((iflag_FFT/10) .eq. (iflag_rocFFT/10)) then
!        write(*,*) 'norm_rtp_from_fwd_OMP_FFTW'
        call norm_rtp_from_fwd_OMP_FFTW(int(fwd_rocFFT%Ncomp),          &
     &      int(WK_rocFFT%NFFT_c), WK_rocFFT%C_rocFFT(1),               &
     &      ist_comp, Ncomp, int(fwd_rocFFT%Nfft), X(1,1))
!      else if((iflag_FFT/10) .eq. (iflag_real_rocFFT/10)) then
!      else if((iflag_FFT/10) .eq. (iflag_OMP_rocFFT/10)) then
      else
!        write(*,*) 'norm_rtp_from_fwd_rocFFT'
        call norm_rtp_from_fwd_rocFFT(int(fwd_rocFFT%Ncomp),            &
     &      int(WK_rocFFT%NFFT_r), WK_rocFFT%X_rocFFT(1),               &
     &      ist_comp, Ncomp, int(fwd_rocFFT%Nfft), X(1,1))
      end if
!
      end subroutine sel_norm_pout_from_fwd_rocFFT
!
! ------------------------------------------------------------------
!
      subroutine sel_norm_rtp_to_bwd_rocFFT                             &
     &         (iflag_FFT, ist_comp, Ncomp, X, bwd_rocFFT, WK_rocFFT)
!
      use normalize_for_rocFFT
      use normalize_for_OMP_FFTW
!
      integer(kind = kint), intent(in) :: iflag_FFT
      integer(kind = kint), intent(in) :: ist_comp, Ncomp
      type(calypso_rocFFT_params), intent(in), target :: bwd_rocFFT
      real(kind = kreal), intent(in) :: X(Ncomp,bwd_rocFFT%Nfft)
!
      type(calypso_rocFFT_work), intent(inout) :: WK_rocFFT
!
!
      if((iflag_FFT/10) .eq. (iflag_rocFFT/10)) then
!        write(*,*) 'norm_rtp_to_bwd_OMP_FFTW'
        call norm_rtp_to_bwd_OMP_FFTW                                   &
     &     (ist_comp, Ncomp, int(bwd_rocFFT%Nfft), X(1,1),              &
     &      int(bwd_rocFFT%Ncomp), int(WK_rocFFT%Nfft_c),               &
     &      WK_rocFFT%C_rocFFT(1))
!      else if((iflag_FFT/10) .eq. (iflag_real_rocFFT/10)) then
!      else if((iflag_FFT/10) .eq. (iflag_OMP_rocFFT/10)) then
      else
!        write(*,*) 'norm_rtp_to_bwd_rocFFT'
        call norm_rtp_to_bwd_rocFFT                                     &
     &     (ist_comp, Ncomp, int(bwd_rocFFT%Nfft), X(1,1),              &
     &      int(bwd_rocFFT%Ncomp), int(WK_rocFFT%Nfft_r),               &
     &      WK_rocFFT%X_rocFFT(1))
      end if
!
      end subroutine sel_norm_rtp_to_bwd_rocFFT
!
! ------------------------------------------------------------------
!
      end module select_pout_rocFFT
