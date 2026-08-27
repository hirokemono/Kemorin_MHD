!>@file   multi_FFT_select.F90
!!@brief  module multi_FFT_select
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2026
!>@brief  Selector for FFT
!!
!!@verbatim
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFT
!! ------------------------------------------------------------------
!!
!! ------------------------------------------------------------------
!!
!! ------------------------------------------------------------------
!!   wrapper subroutine for FFT in ISPACK
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
!!      subroutine backward_FFT_select(iflag_FFT, M, Nfft, X, WKS,      &
!!     &                               elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) :: iflag_FFT
!!        integer(kind = kint), intent(in) :: M, Nfft
!!        real(kind = kreal), intent(inout) :: X(M,Nfft)
!!        type(working_FFTs), intent(inout) :: WKS
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!! ------------------------------------------------------------------
!!
!! ------------------------------------------------------------------
!!   wrapper subroutine for backward FFT
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
      module multi_FFT_select
!
      use omp_lib
!
      use m_precision
      use m_machine_parameter
      use m_FFT_selector
!
      use t_FFT_selector
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine select_multi_FFT_init(iflag_FFT, Nsmp, Nstacksmp,      &
     &                                 Ncomp, Nfft, WKS)
!
      integer(kind = kint), intent(in) :: iflag_FFT
!
      integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Nfft, Ncomp
!
      type(working_FFTs), intent(inout) :: WKS
!
!
      if     ((iflag_FFT/10) .eq. (iflag_ISPACK0/10)) then
        write(*,*) 'init_wk_ispack_t'
        call init_wk_ispack_t(Nsmp, Nstacksmp, Nfft, WKS%WK_ISPACK1)
      else if((flag_FFT/10) .eq. (iflag_ISPACK3/10)) then
        write(*,*) 'init_wk_ispack3_t'
        call init_wk_ispack3_t(Nsmp, Nstacksmp,                         &
     &                         cast_long(Nfft), WKS%WK_ISPACK3)
#ifdef FFTW3
      else if((flag_FFT/10) .eq. (iflag_FFTW/10)) then
        write(*,*) 'init_FFTW_mul_type'
        call init_FFTW_mul_type(Nsmp, Nstacksmp, Nfft, WKS%WK_MUL_FFTW)
#endif
#ifdef OMP_FFTW3
      else if((flag_FFT/10) .eq. (iflag_OMP_FFTW/10)) then
        write(*,*) 'init_OMP_FFTW_type'
        call init_OMP_FFTW_type(Ncomp, Nfft, WKS%WK_MUL_FFTW)
#endif
      else if((flag_FFT/10) .eq. (iflag_FFTPACK/10)) then
        write(*,*) 'init_WK_FFTPACK_t'
        call init_WK_FFTPACK_t(Nsmp, Nstacksmp, Nfft, WKS%WK_FFTPACK)
      end if
!
      end subroutine select_multi_FFT_init
!
! ------------------------------------------------------------------
!
      subroutine select_multi_FFT_fin(iflag_FFT, Nsmp, WKS)
!
      use t_OMP_FFTW_wrapper
!
      integer(kind = kint), intent(in) :: iflag_FFT
      integer(kind = kint), intent(in) :: Nsmp
!
      type(working_FFTs), intent(inout) :: WKS
!
!
      if     ((iflag_FFT/10) .eq. (iflag_ISPACK0/10)) then
        write(*,*) 'finalize_wk_ispack_t'
        call finalize_wk_ispack_t(WKS%WK_ISPACK1)
      else if((flag_FFT/10) .eq. (iflag_ISPACK3/10)) then
        write(*,*) 'finalize_wk_ispack3_t'
        call finalize_wk_ispack3_t(WKS%WK_ISPACK3)
#ifdef FFTW3
      else if((flag_FFT/10) .eq. (iflag_FFTW/10)) then
        write(*,*) 'finalize_FFTW_mul_type'
        call finalize_FFTW_mul_type(Nsmp, WKS%WK_MUL_FFTW)
#endif
#ifdef OMP_FFTW3
      else if((flag_FFT/10) .eq. (iflag_OMP_FFTW/10)) then
        write(*,*) 'finalize_OMP_FFTW_type'
        call finalize_OMP_FFTW_type(WKS%WK_MUL_FFTW)
#endif
      else if((flag_FFT/10) .eq. (iflag_FFTPACK/10)) then
        write(*,*) 'finalize_WK_FFTPACK_t'
        call finalize_WK_FFTPACK_t(WKS%WK_FFTPACK)
      end if
!
      end subroutine select_multi_FFT_fin
!
! ------------------------------------------------------------------
!
      end module multi_FFT_select
