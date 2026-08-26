!>@file   phi_inside_FFT_selector.F90
!!@brief  module phi_inside_FFT_selector
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!      module phi_inside_FFT_selector
!
!
!>@brief  Selector of Fourier transform using structure
!!
!!@verbatim
!!      subroutine init_pin_FFT_select(id_rank, iflag_FFT,              &
!!     &          Nsmp, Nstacksmp, Nfft, WKS, elapsed_init)
!!      subroutine fin_pin_FFT_select(iflag_FFT, Nsmp, Nstacksmp, WKS)
!!      subroutine verify_pin_FFT_select                                &
!!     &         (iflag_FFT, Nsmp, Nstacksmp, Nfft, WKS)
!!        integer, intent(in) :: id_rank
!!        integer(kind = kint), intent(in) :: iflag_FFT
!!        integer(kind = kint), intent(in) ::  Nfft
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        type(working_FFTs), intent(inout) :: WKS
!!        real(kind = kreal), intent(inout) :: elapsed_init
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT for ISPACK
!! ------------------------------------------------------------------
!!
!!      subroutine fwd_pin_FFT_select(iflag_FFT, M, Nfft, X, WKS,       &
!!     &          elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) :: iflag_FFT
!!        integer(kind = kint), intent(in) :: M, Nfft
!!        real(kind = kreal), intent(inout) :: X(Nfft,M)
!!        type(working_FFTs), intent(inout) :: WKS
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!! ------------------------------------------------------------------
!!
!!   wrapper subroutine for FFT in ISPACK
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
!!      subroutine back_pin_FFT_select(iflag_FFT, M, Nfft, X, WKS,      &
!!     &                               elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) :: iflag_FFT
!!        integer(kind = kint), intent(in) :: M, Nfft
!!        real(kind = kreal), intent(inout) :: X(Nfft,M)
!!        type(working_FFTs), intent(inout) :: WKS
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
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
      module phi_inside_FFT_selector
!
      use omp_lib
!
      use m_precision
      use m_machine_parameter
      use m_FFT_selector
      use t_FFTPACK5_wrapper
      use t_ispack_FFT_wrapper
      use t_ispack3_FFT_wrapper
!
      use t_single_FFTW_wrapper
      use t_multi_FFTW_wrapper
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
      subroutine init_pin_FFT_select(id_rank, iflag_FFT,                &
     &          Nsmp, Nstacksmp, Nfft, WKS, elapsed_init)
!
      use transfer_to_long_integers
      use calypso_multi_ispack
      use calypso_single_ispack
      use calypso_multi_ispack3
      use calypso_single_ispack3
      use calypso_multi_fftpack
      use calypso_single_fftpack
      use calypso_multi_FFTW3
      use calypso_single_FFTW3
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: iflag_FFT
      integer(kind = kint), intent(in) :: Nfft
      integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
!
      type(working_FFTs), intent(inout) :: WKS
      real(kind = kreal), intent(inout) :: elapsed_init
!
      real(kind = kreal) :: start
!
      start = 0.0d0
      start = OMP_GET_WTIME()
      if(abs(iflag_FFT) .eq. (iflag_ISPACK0 + iflag_once_fft)) then
        if(id_rank .eq. 0) write(*,*) 'Use ISPACK V0.93'
        call init_wk_ispack_t(Nsmp, Nstacksmp, Nfft, WKS%WK_ISPACK1)
      else if(abs(iflag_FFT)                                            &
     &         .eq. (iflag_ISPACK0 + iflag_single_fft)) then
        call calypso_single_FTTRUI(Nsmp, Nstacksmp,                     &
     &                            Nfft, WKS%WK_ISPACK1)
      else if(abs(iflag_FFT)                                            &
     &         .eq. (iflag_ISPACK3 + iflag_once_fft)) then
        if(id_rank .eq. 0) write(*,*) 'Use ISPACK V3.0.1'
        call init_wk_ispack3_t(Nsmp, Nstacksmp,                         &
     &                         cast_long(Nfft), WKS%WK_ISPACK3)
      else if(abs(iflag_FFT)                                            &
     &         .eq. (iflag_ISPACK3 + iflag_single_fft)) then
        call calypso_single_FXRINI(Nsmp, Nstacksmp,                     &
     &                             cast_long(Nfft), WKS%WK_ISPACK3)
#ifdef FFTW3
      else if(abs(iflag_FFT) .eq. (iflag_FFTW + iflag_once_fft)) then
        if(id_rank .eq. 0) write(*,*) 'Use FFTW'
        call init_FFTW_mul_type(Nsmp, Nstacksmp, Nfft, WKS%WK_MUL_FFTW)
      else if(abs(iflag_FFT) .eq. (iflag_FFTW + iflag_single_fft)) then
        if(id_rank .eq. 0) write(*,*) 'Use single transform in FFTW'
        call init_FFTW_type(Nsmp, Nstacksmp, Nfft, WKS%WK_FFTW)
#endif
      else if(abs(iflag_FFT)                                            &
     &         .eq. (iflag_FFTPACK + iflag_single_fft)) then
        if(id_rank .eq. 0) write(*,*) 'Use FFTPACK'
        call calypso_single_RFFTMI(Nsmp, Nstacksmp,                     &
     &                             Nfft, WKS%WK_FFTPACK)
      else
        if(id_rank .eq. 0) write(*,*) 'Use FFTPACK'
        call init_WK_FFTPACK_t(Nsmp, Nstacksmp, Nfft, WKS%WK_FFTPACK)
      end if
      elapsed_init = elapsed_init + OMP_GET_WTIME() - start
!
      end subroutine init_pin_FFT_select
!
! ------------------------------------------------------------------
!
      subroutine fin_pin_FFT_select(iflag_FFT, Nsmp, Nstacksmp, WKS)
!
      use calypso_multi_FFTW3
      use calypso_single_FFTW3
!
      integer(kind = kint), intent(in) :: iflag_FFT
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      type(working_FFTs), intent(inout) :: WKS
!
      real(kind = kreal) :: elapsed_init = 0.0d0
      real(kind = kreal) :: start
!
      start = 0.0d0
      start = OMP_GET_WTIME()
      if(     (abs(iflag_FFT) .eq. (iflag_ISPACK0 + iflag_once_fft))    &
     &   .or. (abs(iflag_FFT) .eq. (iflag_ISPACK0 + iflag_single_fft))  &
     &  ) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize ISPACK V0.93'
        call finalize_wk_ispack_t(WKS%WK_ISPACK1)
      else if((abs(iflag_FFT) .eq. (iflag_ISPACK3 + iflag_once_fft))    &
     &   .or. (abs(iflag_FFT) .eq. (iflag_ISPACK3 + iflag_single_fft))  &
     &       ) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize ISPACK V3.0.1'
        call finalize_wk_ispack3_t(WKS%WK_ISPACK3)
#ifdef FFTW3
      else if(abs(iflag_FFT) .eq. (iflag_FFTW + iflag_once_fft)) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize FFTW'
        call finalize_FFTW_mul_type(Nsmp, WKS%WK_MUL_FFTW)
      else if(abs(iflag_FFT) .eq. (iflag_FFTW + iflag_single_fft)) then
        if(iflag_debug .gt. 0) write(*,*) 'Finalize single FFTW'
        call finalize_FFTW_type(Nsmp, WKS%WK_FFTW)
#endif
      else
        if(iflag_debug .gt. 0) write(*,*) 'Finalize FFTPACK'
        call finalize_WK_FFTPACK_t(WKS%WK_FFTPACK)
      end if
      elapsed_init = elapsed_init + OMP_GET_WTIME() - start
!
      end subroutine fin_pin_FFT_select
!
! ------------------------------------------------------------------
!
      subroutine verify_pin_FFT_select                                  &
     &         (iflag_FFT, Nsmp, Nstacksmp, Nfft, WKS)
!
      use transfer_to_long_integers
      use calypso_multi_FFTW3
      use calypso_single_FFTW3
!
      integer(kind = kint), intent(in) :: iflag_FFT
      integer(kind = kint), intent(in) ::  Nfft
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
      type(working_FFTs), intent(inout) :: WKS
!
!
      if(     (abs(iflag_FFT) .eq. (iflag_ISPACK0 + iflag_once_fft))    &
     &   .or. (abs(iflag_FFT) .eq. (iflag_ISPACK0 + iflag_single_fft))  &
     &  ) then
        if(iflag_debug .gt. 0) write(*,*) 'Use ISPACK V0.93'
        call verify_wk_ispack_t(Nsmp, Nstacksmp, Nfft, WKS%WK_ISPACK1)
      else if((abs(iflag_FFT) .eq. (iflag_ISPACK3 + iflag_once_fft))    &
     &   .or. (abs(iflag_FFT) .eq. (iflag_ISPACK3 + iflag_single_fft))  &
     &       ) then
        if(iflag_debug .gt. 0) write(*,*) 'Use ISPACK V3.0.1'
        call verify_wk_ispack3_t(Nsmp, Nstacksmp,                       &
     &                          cast_long(Nfft), WKS%WK_ISPACK3)
#ifdef FFTW3
      else if(abs(iflag_FFT) .eq. (iflag_FFTW + iflag_once_fft)) then
        if(iflag_debug .gt. 0) write(*,*) 'Use FFTW'
        call verify_wk_FFTW_mul_type(Nsmp, Nstacksmp,                   &
     &      Nfft, WKS%WK_MUL_FFTW)
      else if(abs(iflag_FFT) .eq. (iflag_FFTW + iflag_single_fft)) then
        if(iflag_debug .gt. 0) write(*,*) 'Use single FFTW transforms'
        call verify_wk_FFTW_type(Nsmp, Nstacksmp, Nfft, WKS%WK_FFTW)
#endif
      else
        if(iflag_debug .gt. 0) write(*,*) 'Use FFTPACK'
        call verify_wk_FFTPACK_t(Nsmp, Nstacksmp, Nfft, WKS%WK_FFTPACK)
      end if
!
      end subroutine verify_pin_FFT_select
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine fwd_pin_FFT_select(iflag_FFT, M, Nfft, X, WKS,         &
     &          elapsed_fft, elapsed_cpy)
!
      use transfer_to_long_integers
      use calypso_single_ispack
      use calypso_single_ispack3
      use calypso_single_fftpack
      use calypso_multi_FFTW3
      use calypso_single_FFTW3
      use multi_pin_FFTPACK_smp
      use multi_pin_ISPACK1_smp
      use multi_pin_ISPACK3_smp
!
      integer(kind = kint), intent(in) :: iflag_FFT
      integer(kind = kint), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(Nfft,M)
      type(working_FFTs), intent(inout) :: WKS
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
!
      if(abs(iflag_FFT) .eq. (iflag_ISPACK0 + iflag_once_fft)) then
        call multi_pin_FTTRUF(M, Nfft, X, WKS%WK_ISPACK1,               &
     &                        elapsed_fft, elapsed_cpy)
      else if(abs(iflag_FFT)                                            &
     &         .eq. (iflag_ISPACK0 + iflag_single_fft)) then
        call calypso_single_pin_FTTRUF(M, Nfft, X, WKS%WK_ISPACK1,      &
     &                                 elapsed_fft, elapsed_cpy)
      else if(abs(iflag_FFT)                                            &
     &         .eq. (iflag_ISPACK3 + iflag_once_fft)) then
        call multi_pin_FXRTFA(cast_long(M), cast_long(Nfft), X,         &
     &                        WKS%WK_ISPACK3, elapsed_fft, elapsed_cpy)
      else if(abs(iflag_FFT)                                            &
     &         .eq. (iflag_ISPACK3 + iflag_single_fft)) then
        call calypso_single_pin_FXRTFA(cast_long(M), cast_long(Nfft),   &
     &                                 X, WKS%WK_ISPACK3,               &
     &                                 elapsed_fft, elapsed_cpy)
#ifdef FFTW3
      else if(abs(iflag_FFT) .eq. (iflag_FFTW + iflag_once_fft)) then
        call calypso_multi_pin_fwd_FFTW3(M, Nfft, X, WKS%WK_MUL_FFTW,   &
     &                                   elapsed_fft, elapsed_cpy)
      else if(abs(iflag_FFT) .eq. (iflag_FFTW + iflag_single_fft)) then
        call calypso_single_pin_fwd_FFTW3(M, Nfft, X, WKS%WK_FFTW,      &
     &                                    elapsed_fft, elapsed_cpy)
#endif
      else if(abs(iflag_FFT)                                            &
     &         .eq. (iflag_FFTPACK + iflag_single_fft)) then
        call calypso_single_pin_RFFTMF(M, Nfft, X, WKS%WK_FFTPACK,      &
     &                                 elapsed_fft, elapsed_cpy)
      else
        call calypso_pin_RFFTMF(M, Nfft, X, WKS%WK_FFTPACK,             &
     &                          elapsed_fft, elapsed_cpy)
      end if
!
      end subroutine fwd_pin_FFT_select
!
! ------------------------------------------------------------------
!
      subroutine back_pin_FFT_select(iflag_FFT, M, Nfft, X, WKS,        &
     &                               elapsed_fft, elapsed_cpy)
!
      use transfer_to_long_integers
      use calypso_single_ispack
      use calypso_single_ispack3
      use calypso_single_fftpack
      use calypso_multi_FFTW3
      use calypso_single_FFTW3
      use multi_pin_FFTPACK_smp
      use multi_pin_ISPACK1_smp
      use multi_pin_ISPACK3_smp
!
      integer(kind = kint), intent(in) :: iflag_FFT
      integer(kind = kint), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(Nfft,M)
      type(working_FFTs), intent(inout) :: WKS
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
!
      if(abs(iflag_FFT) .eq. (iflag_ISPACK0 + iflag_once_fft)) then
        call multi_pin_FTTRUB(M, Nfft, X, WKS%WK_ISPACK1,               &
     &                        elapsed_fft, elapsed_cpy)
      else if(abs(iflag_FFT)                                            &
     &          .eq. (iflag_ISPACK0 + iflag_single_fft)) then
        call calypso_single_pin_FTTRUB(M, Nfft, X, WKS%WK_ISPACK1,      &
     &                                 elapsed_fft, elapsed_cpy)
      else if(abs(iflag_FFT)                                            &
     &          .eq. (iflag_ISPACK3 + iflag_once_fft)) then
        call multi_pin_FXRTBA(cast_long(M), cast_long(Nfft), X,         &
     &                        WKS%WK_ISPACK3, elapsed_fft, elapsed_cpy)
      else if(abs(iflag_FFT)                                            &
     &          .eq. (iflag_ISPACK3 + iflag_single_fft)) then
        call calypso_single_pin_FXRTBA(cast_long(M), cast_long(Nfft),   &
     &                                 X, WKS%WK_ISPACK3,               &
     &                                 elapsed_fft, elapsed_cpy)
#ifdef FFTW3
      else if(abs(iflag_FFT) .eq. (iflag_FFTW + iflag_once_fft)) then
        call calypso_multi_pin_bwd_FFTW3(M, Nfft, X, WKS%WK_MUL_FFTW,   &
     &                                   elapsed_fft, elapsed_cpy)
      else if(abs(iflag_FFT) .eq. (iflag_FFTW + iflag_single_fft)) then
        call calypso_single_pin_bwd_FFTW3(M, Nfft, X, WKS%WK_FFTW,      &
     &                                    elapsed_fft, elapsed_cpy)
#endif
      else if(abs(iflag_FFT)                                            &
     &         .eq. (iflag_FFTPACK + iflag_single_fft)) then
        call calypso_single_pin_RFFTMB(M, Nfft, X, WKS%WK_FFTPACK,      &
     &                                 elapsed_fft, elapsed_cpy)
      else
        call calypso_pin_RFFTMB(M, Nfft, X, WKS%WK_FFTPACK,             &
     &                          elapsed_fft, elapsed_cpy)
      end if
!
      end subroutine back_pin_FFT_select
!
! ------------------------------------------------------------------
!
      end module phi_inside_FFT_selector
