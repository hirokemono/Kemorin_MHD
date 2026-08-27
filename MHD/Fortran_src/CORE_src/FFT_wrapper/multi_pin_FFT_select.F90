!>@file   multi_pin_FFT_select.F90
!!@brief  module multi_pin_FFT_select
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2026
!>@brief  Selector for FFT
!!
!!@verbatim
!! ------------------------------------------------------------------
!!   wrapper subroutine for forward FFT
!! ------------------------------------------------------------------
!!      subroutine select_fwd_pin_FFT_smp(iflag_FFT, Ncomp, Nfft, X,    &
!!     &                                  WKS)
!!        integer(kind = kint), intent(in) :: iflag_FFT
!!        integer(kind = kint), intent(in) :: Nfft, Ncomp
!!        real(kind = kreal), intent(in) :: X(Nfft,Ncomp)
!!        type(working_FFTs), intent(inout) :: WKS
!!
!! ------------------------------------------------------------------
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
!!   wrapper subroutine for backward FFT
!! ------------------------------------------------------------------
!!      subroutine select_bwd_pin_FFT_smp(iflag_FFT, WKS,               &
!!     &                                  Ncomp, Nfft, X)
!!        integer(kind = kint), intent(in) :: iflag_FFT
!!        integer(kind = kint), intent(in) :: Nfft, Ncomp
!!        type(working_FFTs), intent(inout) :: WKS
!!        real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
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
!
      module multi_pin_FFT_select
!
      use omp_lib
!
      use m_precision
      use m_machine_parameter
      use m_FFT_selector
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
      subroutine select_fwd_pin_FFT_smp(iflag_FFT, Ncomp, Nfft, X,      &
     &                                  WKS)
!
      use transfer_to_long_integers
      use calypso_multi_ispack
      use calypso_multi_ispack3
      use calypso_multi_fftpack
#ifdef FFTW3
      use calypso_multi_FFTW3
#endif
!
      integer(kind = kint), intent(in) :: iflag_FFT
      integer(kind = kint), intent(in) :: Nfft, Ncomp
      real(kind = kreal), intent(in) :: X(Nfft,Ncomp)
!
      type(working_FFTs), intent(inout) :: WKS
!
!
      if     ((iflag_FFT/10) .eq. (iflag_ISPACK0/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'multi_FTTRUF_smp'
        call multi_FTTRUF_smp(WKS%WK_ISPACK1%Nplan_ISPACK,              &
     &      WKS%WK_ISPACK1%istack_ISPACK, WKS%WK_ISPACK1%Mmax_smp,      &
     &      Nfft, WKS%WK_ISPACK1%X_ispack, WKS%WK_ISPACK1%IT_ispack,    &
     &      WKS%WK_ISPACK1%T_ispack, WKS%WK_ISPACK1%WORK_ispack)
      else if((iflag_FFT/10) .eq. (iflag_ISPACK3/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'multi_FXRTFA_smp'
        call multi_FXRTFA_smp(WKS%WK_ISPACK3%Nplan_ISPACK3,             &
     &      WKS%WK_ISPACK3%istack_ISPACK3, WKS%WK_ISPACK3%Mmax_smp,     &
     &      cast_long(Nfft), WKS%WK_ISPACK3%X_ispack,                   &
     &      WKS%WK_ISPACK3%IT_ispack, WKS%WK_ISPACK3%T_ispack)
#ifdef FFTW3
      else if((iflag_FFT/10) .eq. (iflag_FFTW/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'multi_fwd_FFTW3_smp'
        call multi_fwd_FFTW3_smp(WKS%WK_MUL_FFTW%plan_mul_fwd,          &
     &      WKS%WK_MUL_FFTW%Nplan_FFTW, WKS%WK_MUL_FFTW%istack_FFTW,    &
     &      Ncomp, Nfft, X(1,1), WKS%WK_MUL_FFTW%Nfft_c,                &
     &      WKS%WK_MUL_FFTW%C_FFTW_mul(1,1))
#endif
#ifdef OMP_FFTW3
      else if((iflag_FFT/10) .eq. (iflag_OMP_FFTW/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'multi_fwd_FFTW3_smp'
        call multi_fwd_FFTW3_smp(WKS%WK_MUL_FFTW%plan_mul_fwd,          &
     &      WKS%WK_MUL_FFTW%Nplan_FFTW, WKS%WK_MUL_FFTW%istack_FFTW,    &
     &      Ncomp, Nfft, X(1,1), WKS%WK_MUL_FFTW%Nfft_c,                &
     &      WKS%WK_MUL_FFTW%C_FFTW_mul(1,1))
#endif
      else if((iflag_FFT/10) .eq. (iflag_FFTPACK/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'multi_RFFTMF_smp'
        call multi_RFFTMF_smp(WKS%WK_FFTPACK%Nplan_FFTPACK,             &
     &      WKS%WK_FFTPACK%istack_FFTPACK, WKS%WK_FFTPACK%Mmax_smp,     &
     &      Nfft, WKS%WK_FFTPACK%X_FFTPACK5(1,1),                       &
     &      WKS%WK_FFTPACK%lsave_FFTPACK, WKS%WK_FFTPACK%WSAVE_FFTPACK, &
     &      WKS%WK_FFTPACK%WORK_FFTPACK)
      end if
!
      end subroutine select_fwd_pin_FFT_smp
!
! ------------------------------------------------------------------
!
      subroutine select_bwd_pin_FFT_smp(iflag_FFT, WKS,                 &
     &                                  Ncomp, Nfft, X)
!
      use transfer_to_long_integers
      use calypso_multi_ispack
      use calypso_multi_ispack3
      use calypso_multi_fftpack
#ifdef FFTW3
      use calypso_multi_FFTW3
#endif
!
      integer(kind = kint), intent(in) :: iflag_FFT
      integer(kind = kint), intent(in) :: Nfft, Ncomp
!
      type(working_FFTs), intent(inout) :: WKS
      real(kind = kreal), intent(inout) :: X(Nfft,Ncomp)
!
!
      if     ((iflag_FFT/10) .eq. (iflag_ISPACK0/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'multi_FTTRUB_smp'
        call multi_FTTRUB_smp(WKS%WK_ISPACK1%Nplan_ISPACK,              &
     &      WKS%WK_ISPACK1%istack_ISPACK, WKS%WK_ISPACK1%Mmax_smp,      &
     &      Nfft, WKS%WK_ISPACK1%X_ispack, WKS%WK_ISPACK1%IT_ispack,    &
     &      WKS%WK_ISPACK1%T_ispack, WKS%WK_ISPACK1%WORK_ispack)
      else if((iflag_FFT/10) .eq. (iflag_ISPACK3/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'multi_FXRTBA_smp'
        call multi_FXRTBA_smp(WKS%WK_ISPACK3%Nplan_ISPACK3,             &
     &      WKS%WK_ISPACK3%istack_ISPACK3, WKS%WK_ISPACK3%Mmax_smp,     &
     &      cast_long(Nfft), WKS%WK_ISPACK3%X_ispack,                   &
     &      WKS%WK_ISPACK3%IT_ispack, WKS%WK_ISPACK3%T_ispack)
#ifdef FFTW3
      else if((iflag_FFT/10) .eq. (iflag_FFTW/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'multi_bwd_FFTW3_smp'
        call multi_bwd_FFTW3_smp(WKS%WK_MUL_FFTW%plan_mul_bwd,          &
     &      WKS%WK_MUL_FFTW%Nplan_FFTW, WKS%WK_MUL_FFTW%istack_FFTW,    &
     &      Ncomp, WKS%WK_MUL_FFTW%Nfft_c,                              &
     &      WKS%WK_MUL_FFTW%C_FFTW_mul(1,1), Nfft, X(1,1))
#endif
#ifdef OMP_FFTW3
      else if((iflag_FFT/10) .eq. (iflag_OMP_FFTW/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'multi_bwd_FFTW3_smp'
        call multi_bwd_FFTW3_smp(WKS%WK_MUL_FFTW%plan_mul_bwd,          &
     &      WKS%WK_MUL_FFTW%Nplan_FFTW, WKS%WK_MUL_FFTW%istack_FFTW,    &
     &      Ncomp, WKS%WK_MUL_FFTW%Nfft_c,                              &
     &       WKS%WK_MUL_FFTW%C_FFTW_mul(1,1), Nfft, X(1,1))
#endif
      else if((iflag_FFT/10) .eq. (iflag_FFTPACK/10)) then
        if(iflag_debug .gt. 0) write(*,*) 'multi_RFFTMB_smp'
        call multi_RFFTMB_smp(WKS%WK_FFTPACK%Nplan_FFTPACK,             &
     &      WKS%WK_FFTPACK%istack_FFTPACK, WKS%WK_FFTPACK%Mmax_smp,     &
     &      Nfft, WKS%WK_FFTPACK%X_FFTPACK5,                            &
     &      WKS%WK_FFTPACK%lsave_FFTPACK, WKS%WK_FFTPACK%WSAVE_FFTPACK, &
     &      WKS%WK_FFTPACK%WORK_FFTPACK)
      end if
!
      end subroutine select_bwd_pin_FFT_smp
!
! ------------------------------------------------------------------
!
      end module multi_pin_FFT_select
