!>@file   t_FFT_selector.f90
!!@brief  module t_FFT_selector
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!      module t_FFT_selector
!
!
!>@brief  Selector of Fourier transform using structure
!!
!!@verbatim
!!      subroutine initialize_FFT_select(id_rank, iflag_FFT,            &
!!     &          Nsmp, Nstacksmp, Nfft, WKS)
!!      subroutine finalize_FFT_sel_t(iflag_FFT, Nsmp, Nstacksmp, WKS)
!!      subroutine verify_FFT_select                                    &
!!     &         (iflag_FFT, Nsmp, Nstacksmp, Nfft, WKS)
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT for ISPACK
!! ------------------------------------------------------------------
!!
!!      subroutine forward_FFT_select                                   &
!!     &         (iflag_FFT, Nsmp, Nstacksmp, M, Nfft, X, WKS)
!! ------------------------------------------------------------------
!!
!!   wrapper subroutine for FFT in ISPACK
!!
!!   a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!   b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!    K = Nfft/2....
!!   a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!! ------------------------------------------------------------------
!!
!!      subroutine backward_FFT_select                                  &
!!     &         (iflag_FFT, Nsmp, Nstacksmp, M, Nfft, X, WKS)
!! ------------------------------------------------------------------
!!
!!   wrapper subroutine for backward FFT
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
!!
!!@n @param Nsmp  Number of SMP processors
!!@n @param Nstacksmp(0:Nsmp)   End number for each SMP process
!!@n @param M           Number of components for Fourier transforms
!!@n @param Nfft        Data length for eadh FFT
!!@n @param X(M, Nfft)  Data for Fourier transform
!!@n @param WKS         Work structure for ISPACK
!
      module t_FFT_selector
!
      use m_precision
      use m_machine_parameter
      use m_FFT_selector
      use t_FFTPACK5_wrapper
      use t_ispack_FFT_wrapper
      use t_ispack3_FFT_wrapper
!
      use t_FFTW_wrapper
      use t_multi_FFTW_wrapper
!
      implicit none
!
!>      structure for working data for FFT
      type working_FFTs
        type(working_ISPACK) ::   WK_ISPACK1
        type(working_ISPACK3) ::  WK_ISPACK3
        type(working_FFTPACK) ::  WK_FFTPACK
        type(working_FFTW) ::     WK_FFTW
        type(working_mul_FFTW) :: WK_MUL_FFTW
      end type working_FFTs
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine initialize_FFT_select(id_rank, iflag_FFT,              &
     &          Nsmp, Nstacksmp, Nfft, WKS)
!
      use transfer_to_long_integers
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: iflag_FFT
      integer(kind = kint), intent(in) :: Nfft
      integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
!
      type(working_FFTs), intent(inout) :: WKS
!
!
      if(iflag_FFT .eq. iflag_ISPACK1_ONCE) then
        if(id_rank .eq. 0) write(*,*) 'Use ISPACK V0.93'
        call init_wk_ispack_t(Nsmp, Nstacksmp, Nfft, WKS%WK_ISPACK1)
      else if(iflag_FFT .eq. iflag_ISPACK3_ONCE) then
        if(id_rank .eq. 0) write(*,*) 'Use ISPACK V3.0.1'
        call init_wk_ispack3_t(Nsmp, Nstacksmp,                         &
     &                         cast_long(Nfft), WKS%WK_ISPACK3)
#ifdef FFTW3
      else if(iflag_FFT .eq. iflag_FFTW_ONCE) then
        if(id_rank .eq. 0) write(*,*) 'Use FFTW'
        call init_FFTW_mul_type(Nsmp, Nstacksmp, Nfft, WKS%WK_MUL_FFTW)
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        if(id_rank .eq. 0) write(*,*) 'Use single transform in FFTW'
        call init_FFTW_type(Nstacksmp(Nsmp), Nfft, WKS%WK_FFTW)
#endif
      else
        if(id_rank .eq. 0) write(*,*) 'Use FFTPACK'
        call init_WK_FFTPACK_t(Nsmp, Nstacksmp, Nfft, WKS%WK_FFTPACK)
      end if
!
!
      end subroutine initialize_FFT_select
!
! ------------------------------------------------------------------
!
      subroutine finalize_FFT_sel_t(iflag_FFT, Nsmp, Nstacksmp, WKS)
!
      integer(kind = kint), intent(in) :: iflag_FFT
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      type(working_FFTs), intent(inout) :: WKS
!
!
      if(iflag_FFT .eq. iflag_ISPACK1_ONCE) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize ISPACK V0.93'
        call finalize_wk_ispack_t(WKS%WK_ISPACK1)
      else if(iflag_FFT .eq. iflag_ISPACK3_ONCE) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize ISPACK V3.0.1'
        call finalize_wk_ispack3_t(WKS%WK_ISPACK3)
#ifdef FFTW3
      else if(iflag_FFT .eq. iflag_FFTW_ONCE) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize FFTW'
        call finalize_FFTW_mul_type(Nsmp, WKS%WK_MUL_FFTW)
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize single FFTW'
        call finalize_FFTW_type(Nstacksmp(Nsmp), WKS%WK_FFTW)
#endif
      else
        if(iflag_debug .gt. 0) write(*,*) 'Finalize FFTPACK'
        call finalize_WK_FFTPACK_t(WKS%WK_FFTPACK)
      end if
!
      end subroutine finalize_FFT_sel_t
!
! ------------------------------------------------------------------
!
      subroutine verify_FFT_select                                      &
     &         (iflag_FFT, Nsmp, Nstacksmp, Nfft, WKS)
!
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: iflag_FFT
      integer(kind = kint), intent(in) ::  Nfft
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
      type(working_FFTs), intent(inout) :: WKS
!
!
      if(iflag_FFT .eq. iflag_ISPACK1_ONCE) then
        if(iflag_debug .gt. 0) write(*,*) 'Use ISPACK V0.93'
        call verify_wk_ispack_t(Nsmp, Nstacksmp, Nfft, WKS%WK_ISPACK1)
      else if(iflag_FFT .eq. iflag_ISPACK3_ONCE) then
        if(iflag_debug .gt. 0) write(*,*) 'Use ISPACK V0.93'
        call verify_wk_ispack3_t(Nsmp, Nstacksmp,                       &
     &                          cast_long(Nfft), WKS%WK_ISPACK3)
#ifdef FFTW3
      else if(iflag_FFT .eq. iflag_FFTW_ONCE) then
        if(iflag_debug .gt. 0) write(*,*) 'Use FFTW'
        call verify_wk_FFTW_mul_type(Nsmp, Nstacksmp,                   &
     &      Nfft, WKS%WK_MUL_FFTW)
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        if(iflag_debug .gt. 0) write(*,*) 'Use single FFTW transforms'
        call verify_wk_FFTW_type(Nstacksmp(Nsmp), Nfft, WKS%WK_FFTW)
#endif
      else
        if(iflag_debug .gt. 0) write(*,*) 'Use FFTPACK'
        call verify_wk_FFTPACK_t(Nsmp, Nstacksmp, Nfft, WKS%WK_FFTPACK)
      end if
!
      end subroutine verify_FFT_select
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine forward_FFT_select                                     &
     &         (iflag_FFT, Nsmp, Nstacksmp, M, Nfft, X, WKS)
!
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: iflag_FFT
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(M, Nfft)
      type(working_FFTs), intent(inout) :: WKS
!
!
      if(iflag_FFT .eq. iflag_ISPACK1_ONCE) then
        call FTTRUF_kemo_t(Nsmp, Nstacksmp, M, Nfft, X, WKS%WK_ISPACK1)
      else if(iflag_FFT .eq. iflag_ISPACK3_ONCE) then
        call FXRTFA_kemo_t(Nsmp, Nstacksmp, cast_long(M),               &
     &                     cast_long(Nfft), X, WKS%WK_ISPACK3)
#ifdef FFTW3
      else if(iflag_FFT .eq. iflag_FFTW_ONCE) then
        call FFTW_mul_forward_type(Nsmp, Nstacksmp, M, Nfft, X,         &
     &      WKS%WK_MUL_FFTW)
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        call FFTW_forward_type(Nsmp, Nstacksmp, M, Nfft, X,             &
     &      WKS%WK_FFTW)
#endif
      else
        call CALYPSO_RFFTMF_t(Nsmp, Nstacksmp, M, Nfft, X,              &
     &      WKS%WK_FFTPACK)
      end if
!
      end subroutine forward_FFT_select
!
! ------------------------------------------------------------------
!
      subroutine backward_FFT_select                                    &
     &         (iflag_FFT, Nsmp, Nstacksmp, M, Nfft, X, WKS)
!
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: iflag_FFT
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(M,Nfft)
      type(working_FFTs), intent(inout) :: WKS
!
!
      if(iflag_FFT .eq. iflag_ISPACK1_ONCE) then
        call FTTRUB_kemo_t(Nsmp, Nstacksmp, M, Nfft, X, WKS%WK_ISPACK1)
      else if(iflag_FFT .eq. iflag_ISPACK3_ONCE) then
        call FXRTBA_kemo_t(Nsmp, Nstacksmp, cast_long(M),               &
     &                     cast_long(Nfft), X, WKS%WK_ISPACK3)
#ifdef FFTW3
      else if(iflag_FFT .eq. iflag_FFTW_ONCE) then
        call FFTW_mul_backward_type(Nsmp, Nstacksmp, M, Nfft, X,        &
     &      WKS%WK_MUL_FFTW)
      else if(iflag_FFT .eq. iflag_FFTW_SINGLE) then
        call FFTW_backward_type(Nsmp, Nstacksmp, M, Nfft, X,            &
     &      WKS%WK_FFTW)
#endif
      else
        call CALYPSO_RFFTMB_t(Nsmp, Nstacksmp, M, Nfft, X,              &
     &      WKS%WK_FFTPACK)
      end if
!
      end subroutine backward_FFT_select
!
! ------------------------------------------------------------------
!
      end module t_FFT_selector
