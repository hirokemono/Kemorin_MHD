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
!! ------------------------------------------------------------------
!! wrapper subroutine for forward Fourier transform by ISPACK-3
!! ------------------------------------------------------------------
!!
!!      subroutine multi_FXRTFA_smp(Nsmp, Nstacksmp, Mmax_smp, Nfft,    &
!!     &          X_ispack, IT_ispack, T_ispack)
!!        integer(kind = kint), intent(in) ::  Nsmp
!!        integer(kind = kint_gl), intent(in) ::  Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: M, Nfft, Mmax_smp
!!        integer(kind = 4), intent(in) :: IT_ispack(nfft/2)
!!        real(kind = 8), intent(in) :: T_ispack(Nfft+Nfft/2)
!!        real(kind = kreal), intent(inout) :: X(Nfft,M)
!!        real(kind = kreal), intent(inout)                             &
!!     &                 :: X_ispack(Mmax_smp*Nfft,Nsmp)
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
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
!! wrapper subroutine for backward Fourier transform by ISPACK-3
!! ------------------------------------------------------------------
!!
!!      subroutine multi_FXRTBA_smp(Nsmp, Nstacksmp, Mmax_smp, Nfft,    &
!!     &                            X_ispack, IT_ispack, T_ispack)
!!        integer(kind = kint), intent(in) ::  Nsmp
!!        integer(kind = kint_gl), intent(in) ::  Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: M, Nfft, Mmax_smp
!!        integer(kind = 4), intent(in) :: IT_ispack(nfft/2)
!!        real(kind = 8), intent(in) :: T_ispack(Nfft+Nfft/2)
!!        real(kind = kreal), intent(inout) :: X(Nfft,M)
!!        real(kind = kreal), intent(inout)                             &
!!     &                 :: X_ispack(Mmax_smp*Nfft,Nsmp)
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!!
!! ------------------------------------------------------------------
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
      integer(kind = kint_gl), intent(in) ::  Nfft
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
      type(working_ISPACK3), intent(inout) :: WK
!
!
      call alloc_const_ispack3_t(Nsmp, Nfft, WK)
      call count_ISPACK3_smp(Nsmp, Nstacksmp, WK)
!
      call FXRINI_kemo(Nfft, WK%IT_ispack, WK%T_ispack)
!
      call alloc_work_ispack3_t(Nsmp, WK%Mmax_smp, Nfft, WK)
!
      end subroutine init_wk_ispack3_t
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine multi_FXRTFA_smp(Nsmp, Nstacksmp, Mmax_smp, Nfft,      &
     &                            X_ispack, IT_ispack, T_ispack)
!
      integer(kind = kint), intent(in) ::  Nsmp
      integer(kind = kint_gl), intent(in) ::  Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: Nfft, Mmax_smp
      integer(kind = kint_gl), intent(in) :: IT_ispack(Nfft/2)
      real(kind = 8), intent(in) :: T_ispack(Nfft+Nfft/2)
!
      real(kind = kreal), intent(inout) :: X_ispack(Mmax_smp*Nfft,Nsmp)
!
      integer(kind = kint_gl) :: ip, num8
!
!
!$omp do private(num8)
      do ip = 1, Nsmp
        num8 = Nstacksmp(ip) - Nstacksmp(ip-1)
        if(num8 .le. 0) cycle
!
        call FXRTFA(num8, Nfft, X_ispack(1,ip),                         &
     &              IT_ispack(1), T_ispack(1))
      end do
!$omp end do nowait
!
      end subroutine multi_FXRTFA_smp
!
! ------------------------------------------------------------------
!
      subroutine multi_FXRTBA_smp(Nsmp, Nstacksmp, Mmax_smp, Nfft,      &
     &                            X_ispack, IT_ispack, T_ispack)
!
      integer(kind = kint), intent(in) ::  Nsmp
      integer(kind = kint_gl), intent(in) ::  Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: Nfft, Mmax_smp
      integer(kind = kint_gl), intent(in) :: IT_ispack(Nfft/2)
      real(kind = 8), intent(in) :: T_ispack(Nfft+Nfft/2)
!
      real(kind = kreal), intent(inout)                                 &
     &                              :: X_ispack(Mmax_smp*Nfft,Nsmp)
!
      integer(kind = kint_gl) :: ip, num8
!
!
!$omp do private(num8)
      do ip = 1, Nsmp
        num8 = Nstacksmp(ip) - Nstacksmp(ip-1)
        if(num8 .le. 0) cycle
!
        call FXRTBA(num8, Nfft, X_ispack(1,ip),                         &
     &              IT_ispack(1), T_ispack(1))
      end do
!$omp end do nowait
!
      end subroutine multi_FXRTBA_smp
!
! ------------------------------------------------------------------
!
      end module calypso_multi_ispack3
