!>@file   calypso_multi_ispack3.f90
!!@brief  module calypso_multi_ispack3
!!
!!@author H. Matsui
!!@date Programmed on Apr., 2013
!
!
!>@brief  Fourier transform with work structures for ISPACK
!!
!!@verbatim
!! ------------------------------------------------------------------
!! wrapper subroutine for initierize FFT for ISPACK
!! ------------------------------------------------------------------
!!      subroutine init_wk_ispack3_t(Nsmp, Nstacksmp, Nfft, WK)
!!        integer(kind = kint_gl), intent(in) ::  Nfft
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        type(working_ISPACK3), intent(inout) :: WK
!!
!!      subroutine calypso_multi_pin_FXRTFA(Nsmp, Nstacksmp,            &
!!     &          M, Nfft, X, WK, elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint_gl), intent(in) :: M, Nfft
!!        real(kind = kreal), intent(inout) :: X(Nfft,M)
!!        type(working_ISPACK3), intent(inout) :: WK
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!!      subroutine FXRTFA_kemo_t(Nsmp, Nstacksmp, M, Nfft, X, WK,       &
!!     &                         elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint_gl), intent(in) :: M, Nfft
!!        real(kind = kreal), intent(inout) :: X(M, Nfft)
!!        type(working_ISPACK3), intent(inout) :: WK
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for forward Fourier transform by ISPACK-3
!!
!! a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!! b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!! a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!! K = Nfft/2....
!! a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!! ------------------------------------------------------------------
!!
!!
!!      subroutine calypso_multi_pin_FXRTBA(Nsmp, Nstacksmp,            &
!!     &          M, Nfft, X, WK, elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint_gl), intent(in) :: M, Nfft
!!        real(kind = kreal), intent(inout) :: X(Nfft,M)
!!        type(working_ISPACK3), intent(inout) :: WK
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!!      subroutine FXRTBA_kemo_t(Nsmp, Nstacksmp, M, Nfft, X, WK,       &
!!     &                         elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint_gl), intent(in) :: M, Nfft
!!        real(kind = kreal), intent(inout) :: X(M,Nfft)
!!        type(working_ISPACK3), intent(inout) :: WK
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for backward Fourier transform by ISPACK-3
!!
!! x_{k} = a_{0} + (-1)^{j} a_{Nfft/2} + sum_{k=1}^{Nfft/2-1}
!! (a_{k} \cos(2\pijk/Nfft) + b_{k} \sin(2\pijk/Nfft))
!!
!! ------------------------------------------------------------------
!!@endverbatim
!!
!!@n @param Nsmp  Number of SMP processors
!!@n @param Nstacksmp(0:Nsmp)   End number for each SMP process
!!@n @param M           Number of components for Fourier transforms
!!@n @param Nfft        Data length for eadh FFT
!!@n @param X(M, Nfft)  Data for Fourier transform
!!@n @param WK          Work structure for ISPACK
!
      module calypso_multi_ispack3
!
      use m_precision
      use m_constants
      use t_ispack3_FFT_wrapper
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_wk_ispack3_t(Nsmp, Nstacksmp, Nfft, WK)
!
      use ispack3_FFT_wrapper
!
      integer(kind = kint_gl), intent(in) ::  Nfft
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
      type(working_ISPACK3), intent(inout) :: WK
!
      integer(kind = kint) :: ip
!
!
      WK%Mmax_smp = Nstacksmp(1)
      do ip = 1, Nsmp
        WK%Mmax_smp                                                     &
     &      = max(WK%Mmax_smp, (Nstacksmp(ip) - Nstacksmp(ip-1)) )
      end do
!
      call alloc_const_ispack3_t(Nfft, WK)
      call FXRINI_kemo(Nfft, WK%IT_ispack, WK%T_ispack)
!
      call alloc_work_ispack3_t(Nsmp, Nfft, WK)
!
      end subroutine init_wk_ispack3_t
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine calypso_multi_pin_FXRTFA(Nsmp, Nstacksmp,              &
     &          M, Nfft, X, WK, elapsed_fft, elapsed_cpy)
!
      use multi_pin_ISPACK3_smp
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(Nfft,M)
      type(working_ISPACK3), intent(inout) :: WK
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
!
      call multi_pin_FXRTFA_smp(Nsmp, Nstacksmp, M, Nfft, X,            &
     &    WK%X_ispack, WK%Mmax_smp, WK%IT_ispack, WK%T_ispack,          &
     &    elapsed_fft, elapsed_cpy)
!
      end subroutine calypso_multi_pin_FXRTFA
!
! ------------------------------------------------------------------
!
      subroutine calypso_multi_pin_FXRTBA(Nsmp, Nstacksmp,              &
     &          M, Nfft, X, WK, elapsed_fft, elapsed_cpy)
!
      use multi_pin_ISPACK3_smp
!
      integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(Nfft,M)
      type(working_ISPACK3), intent(inout) :: WK
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
!
      call multi_pin_FXRTBA_smp(Nsmp, Nstacksmp, M, Nfft, X,            &
     &    WK%X_ispack, WK%Mmax_smp, WK%IT_ispack, WK%T_ispack,          &
     &    elapsed_fft, elapsed_cpy)
!
      end subroutine calypso_multi_pin_FXRTBA
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine FXRTFA_kemo_t(Nsmp, Nstacksmp, M, Nfft, X, WK,         &
     &                         elapsed_fft, elapsed_cpy)
!
      use ispack3_FFT_wrapper
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(M, Nfft)
      type(working_ISPACK3), intent(inout) :: WK
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
!
      call FXRTFA_kemo_smp(Nsmp, Nstacksmp, M, Nfft, X,                 &
     &    WK%X_ispack, WK%Mmax_smp, WK%IT_ispack, WK%T_ispack,          &
     &    elapsed_fft, elapsed_cpy)
!
      end subroutine FXRTFA_kemo_t
!
! ------------------------------------------------------------------
!
      subroutine FXRTBA_kemo_t(Nsmp, Nstacksmp, M, Nfft, X, WK,         &
     &                         elapsed_fft, elapsed_cpy)
!
      use ispack3_FFT_wrapper
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(M,Nfft)
      type(working_ISPACK3), intent(inout) :: WK
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
!
      call FXRTBA_kemo_smp(Nsmp, Nstacksmp, M, Nfft, X,                 &
     &    WK%X_ispack, WK%Mmax_smp, WK%IT_ispack, WK%T_ispack,          &
     &    elapsed_fft, elapsed_cpy)
!
      end subroutine FXRTBA_kemo_t
!
! ------------------------------------------------------------------
!
      end module calypso_multi_ispack3
