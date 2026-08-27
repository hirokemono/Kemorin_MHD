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
!! ------------------------------------------------------------------
!! wrapper subroutine for forward Fourier transform by ISPACK
!! ------------------------------------------------------------------
!!
!!      subroutine multi_FTTRUF_smp(Nsmp, Nstacksmp, Mmax_smp,          &
!!     &          Nfft, X_ispack, IT_ispack, T_ispack, WORK_ispack)
!!        integer(kind = kint), intent(in) :: Nsmp
!!        integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
!!        integer(kind = kint_gl), intent(in) :: Mmax_smp
!!        integer(kind = kint), intent(in) :: Nfft
!!        integer(kind = 4), intent(in) :: IT_ispack(5)
!!        real(kind = 8), intent(in) :: T_ispack(itwo*Nfft)
!!        real(kind = kreal), intent(inout) :: X(M, Nfft)
!!        real(kind = kreal), intent(inout)                             &
!!     &                             :: X_ispack(Mmax_smp*Nfft,Nsmp)
!!        real(kind = 8), intent(inout)                                 &
!!     &                             :: WORK_ispack(Mmax_smp*Nfft,Nsmp)
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
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
!! ------------------------------------------------------------------
!!
!! ------------------------------------------------------------------
!! wrapper subroutine for backward Fourier transform by ISPACK
!! ------------------------------------------------------------------
!!
!!      subroutine multi_FTTRUB_smp(Nsmp, Nstacksmp, Mmax_smp,          &
!!     &          Nfft, X_ispack, IT_ispack, T_ispack, WORK_ispack)
!!        integer(kind = kint), intent(in) :: Nsmp
!!        integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
!!        integer(kind = kint_gl), intent(in) :: Mmax_smp
!!        integer(kind = kint), intent(in) :: Nfft
!!        integer(kind = 4), intent(in) :: IT_ispack(5)
!!        real(kind = 8), intent(in) :: T_ispack(itwo*Nfft)
!!        real(kind = kreal), intent(inout) :: X(M,Nfft)
!!        real(kind = kreal), intent(inout)                             &
!!     &     :: X_ispack(Mmax_smp*Nfft,Nsmp)
!!        real(kind = 8), intent(inout)                                 &
!!     &     :: WORK_ispack(Mmax_smp*Nfft,Nsmp)
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!!
!! ------------------------------------------------------------------
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
      integer(kind = kint), intent(in) ::  Nfft
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
      type(working_ISPACK), intent(inout) :: WK
!
!
      call alloc_const_ispack_t(Nsmp, Nfft, WK)
      call count_ispack_smp(Nsmp, Nstacksmp, WK)
!
      call FTTRUI_kemo(Nfft, WK%IT_ispack, WK%T_ispack)
!
      call alloc_work_ispack_t(Nsmp, WK%Mmax_smp, Nfft, WK)
!
      end subroutine init_wk_ispack_t
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine multi_FTTRUF_smp(Nsmp, Nstacksmp, Mmax_smp,            &
     &          Nfft, X_ispack, IT_ispack, T_ispack, WORK_ispack)
!
      use ispack_0931
!
      integer(kind = kint), intent(in) :: Nsmp
      integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: Mmax_smp
      integer(kind = kint), intent(in) :: Nfft
      integer(kind = 4), intent(in) :: IT_ispack(5)
      real(kind = 8), intent(in) :: T_ispack(itwo*Nfft)
!
      real(kind = kreal), intent(inout) :: X_ispack(Mmax_smp*Nfft,Nsmp)
      real(kind = 8), intent(inout) :: WORK_ispack(Mmax_smp*Nfft,Nsmp)
!
      integer(kind = kint_gl) :: ismp, num8
!
!
!$omp do private(num8)
      do ismp = 1, Nsmp
        num8 = Nstacksmp(ismp) - Nstacksmp(ismp-1)
        if(num8 .le. 0) cycle
!
        call FTTRUF(int(num8), Nfft, X_ispack(1,ismp),                  &
     &      WORK_ispack(1,ismp), IT_ispack(1), T_ispack(1))
      end do
!$omp end do nowait
!
      end subroutine multi_FTTRUF_smp
!
! ------------------------------------------------------------------
!
      subroutine multi_FTTRUB_smp(Nsmp, Nstacksmp, Mmax_smp,            &
     &          Nfft, X_ispack, IT_ispack, T_ispack, WORK_ispack)
!
      use ispack_0931
!
      integer(kind = kint), intent(in) :: Nsmp
      integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: Mmax_smp
      integer(kind = kint), intent(in) :: Nfft
      integer(kind = 4), intent(in) :: IT_ispack(5)
      real(kind = 8), intent(in) :: T_ispack(itwo*Nfft)
!
      real(kind = kreal), intent(inout)                                 &
     &                              :: X_ispack(Mmax_smp*Nfft,Nsmp)
      real(kind = 8), intent(inout) :: WORK_ispack(Mmax_smp*Nfft,Nsmp)
!
      integer(kind = kint_gl) :: ismp, num8
!
!
!$omp do private(num8)
      do ismp = 1, Nsmp
        num8 = Nstacksmp(ismp) - Nstacksmp(ismp-1)
        if(num8 .le. 0) cycle
!
        call FTTRUB(int(num8), Nfft, X_ispack(1,ismp),                  &
     &      WORK_ispack(1,ismp), IT_ispack(1), T_ispack(1))
      end do
!$omp end do nowait
!
      end subroutine multi_FTTRUB_smp
!
! ------------------------------------------------------------------
!
      end module calypso_multi_ispack
