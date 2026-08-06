!>@file   calypso_multi_ispack.f90
!!@brief  module calypso_multi_ispack
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
!!      subroutine init_wk_ispack_t(Nsmp, Nstacksmp, Nfft, WK)
!!        integer(kind = kint), intent(in) ::  Nfft
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        type(working_ISPACK), intent(inout) :: WK
!!
!!      subroutine calypso_multi_pin_FTTRUF(Nsmp, Nstacksmp, WK,        &
!!     &          M, Nfft, X, elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: M, Nfft
!!        real(kind = kreal), intent(inout) :: X(Nfft,M)
!!        type(working_ISPACK), intent(inout) :: WK
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!!      subroutine FTTRUF_kemo_t(Nsmp, Nstacksmp, M, Nfft, X, WK,       &
!!     &                         elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: M, Nfft
!!        real(kind = kreal), intent(inout) :: X(M, Nfft)
!!        type(working_ISPACK), intent(inout) :: WK
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for forward Fourier transform by ISPACK
!!
!! a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!! b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \sin (\frac{2\pi j k}{Nfft})
!!
!! a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!! K = Nfft/2....
!! a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!! ------------------------------------------------------------------
!!
!!      subroutine calypso_multi_pin_FTTRUB(Nsmp, Nstacksmp,            &
!!     &          M, Nfft, X, WK, elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: M, Nfft
!!        real(kind = kreal), intent(inout) :: X(Nfft,M)
!!        type(working_ISPACK), intent(inout) :: WK
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!!      subroutine FTTRUB_kemo_t(Nsmp, Nstacksmp, M, Nfft, X, WK)
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: M, Nfft
!!        real(kind = kreal), intent(inout) :: X(M,Nfft)
!!        type(working_ISPACK), intent(inout) :: WK
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for backward Fourier transform by ISPACK
!!
!! x_{k} = a_{0} + (-1)^{j} a_{Nfft/2} + sum_{k=1}^{Nfft/2-1}
!! (a_{k} \cos(2\pijk/Nfft) + b_{k} \sin(2\pijk/Nfft))
!!
!! ------------------------------------------------------------------
!!
!! i = 1:     a_{0}
!! i = 2:     a_{Nfft/2}
!! i = 3:     a_{1}
!! i = 4:     b_{1}
!! ...
!! i = 2*k+1: a_{k}
!! i = 2*k+2: b_{k}
!! ...
!! i = Nfft-1:   a_{Nfft/2-1}
!! i = Nfft:     b_{Nfft/2-1}
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
      module calypso_multi_ispack
!
      use m_precision
      use m_constants
      use t_ispack_FFT_wrapper
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_wk_ispack_t(Nsmp, Nstacksmp, Nfft, WK)
!
      use multi_pout_ISPACK1_smp
!
      integer(kind = kint), intent(in) ::  Nfft
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
      type(working_ISPACK), intent(inout) :: WK
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
      call alloc_const_ispack_t(Nfft, WK)
      call FTTRUI_kemo(Nfft, WK%IT_ispack, WK%T_ispack)
!
      call alloc_work_ispack_t(Nsmp, WK%Mmax_smp, Nfft, WK)
!
      end subroutine init_wk_ispack_t
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine calypso_multi_pin_FTTRUF(Nsmp, Nstacksmp,              &
     &          M, Nfft, X, WK, elapsed_fft, elapsed_cpy)
!
      use multi_pin_ISPACK1_smp
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(Nfft,M)
      type(working_ISPACK), intent(inout) :: WK
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
!
      call multi_pin_FTTRUF_smp(Nsmp, Nstacksmp, M, Nfft, X,            &
     &    WK%X_ispack, WK%Mmax_smp, WK%IT_ispack, WK%T_ispack,          &
     &    WK%WORK_ispack, elapsed_fft, elapsed_cpy)
!
      end subroutine calypso_multi_pin_FTTRUF
!
! ------------------------------------------------------------------
!
      subroutine calypso_multi_pin_FTTRUB(Nsmp, Nstacksmp,              &
     &          M, Nfft, X, WK, elapsed_fft, elapsed_cpy)
!
      use multi_pin_ISPACK1_smp
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(Nfft,M)
      type(working_ISPACK), intent(inout) :: WK
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
!
      call multi_pin_FTTRUB_smp(Nsmp, Nstacksmp, M, Nfft, X,            &
     &    WK%X_ispack, WK%Mmax_smp, WK%IT_ispack, WK%T_ispack,          &
     &    WK%WORK_ispack, elapsed_fft, elapsed_cpy)
!
      end subroutine calypso_multi_pin_FTTRUB
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine FTTRUF_kemo_t(Nsmp, Nstacksmp, M, Nfft, X, WK,         &
     &                         elapsed_fft, elapsed_cpy)
!
      use multi_pout_ISPACK1_smp
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(M, Nfft)
      type(working_ISPACK), intent(inout) :: WK
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
!
      call multi_pout_FTTRUF_smp(Nsmp, Nstacksmp, M, Nfft, X,           &
     &    WK%X_ispack, WK%Mmax_smp, WK%IT_ispack, WK%T_ispack,          &
     &    WK%WORK_ispack, elapsed_fft, elapsed_cpy)
!
      end subroutine FTTRUF_kemo_t
!
! ------------------------------------------------------------------
!
      subroutine FTTRUB_kemo_t(Nsmp, Nstacksmp, M, Nfft, X, WK,         &
     &                         elapsed_fft, elapsed_cpy)
!
      use multi_pout_ISPACK1_smp
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(M,Nfft)
      type(working_ISPACK), intent(inout) :: WK
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
!
      call multi_pout_FTTRUB(Nsmp, Nstacksmp, M, Nfft, X,               &
     &    WK%X_ispack, WK%Mmax_smp, WK%IT_ispack, WK%T_ispack,          &
     &    WK%WORK_ispack, elapsed_fft, elapsed_cpy)
!
      end subroutine FTTRUB_kemo_t
!
! ------------------------------------------------------------------
!
      end module calypso_multi_ispack
