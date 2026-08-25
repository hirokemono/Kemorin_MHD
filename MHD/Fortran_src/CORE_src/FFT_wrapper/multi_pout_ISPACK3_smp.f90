!>@file   multi_pout_ISPACK3_smp.f90
!!@brief  module multi_pout_ISPACK3_smp
!!
!!@author H. Matsui
!!@date Programmed in 2008
!
!
!>@brief  Fourier transform using ISPACK
!!
!!@verbatim
!!  ---------------------------------------------------------------------
!!
!!      subroutine FXRINI_kemo(Nfft, IT_ispack, T_ispack)
!!        integer(kind = kint_gl), intent(in) :: Nfft
!!        integer(kind = kint_gl), intent(inout) :: IT_ispack(Nfft/2)
!!        real(kind = 8), intent(inout) :: T_ispack(Nfft+Nfft/2)
!! ------------------------------------------------------------------
!! wrapper subroutine for initierize FFT for ISPACK
!! ------------------------------------------------------------------
!!
!!      subroutine multi_pout_FXRTFA(Nsmp, Nstacksmp, M, Nfft, X,       &
!!     &          X_ispack, Mmax_smp, IT_ispack, T_ispack,              &
!!     &          elapsed_fft, elapsed_cpy)
!!      subroutine multi_pout_FXRTFA_smp(Nsmp, Nstacksmp, Mmax_smp,     &
!!     &          Nfft, X_ispack, IT_ispack, T_ispack)
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint_gl), intent(in) :: M, Mmax_smp
!!        integer(kind = kint_gl), intent(in) :: Nfft
!!        integer(kind = kint_gl), intent(in) :: IT_ispack(Nfft/2)
!!        real(kind = 8), intent(in) :: T_ispack(Nfft+Nfft/2)
!!        real(kind = kreal), intent(inout) :: X(M, Nfft)
!!        real(kind = 8), intent(inout) :: X_ispack(Mmax_smp*Nfft,Nsmp)
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for forward Fourier transform by ISPACK
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
!!      subroutine multi_pout_FXRTBA(Nsmp, Nstacksmp, M, Nfft, X,       &
!!     &          X_ispack, Mmax_smp, IT_ispack, T_ispack,              &
!!     &          elapsed_fft, elapsed_cpy)
!!      subroutine multi_pout_FXRTBA_smp(Nsmp, Nstacksmp, Mmax_smp,     &
!!     &          Nfft, X_ispack, IT_ispack, T_ispack)
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint_gl), intent(in) :: M, Mmax_smp
!!        integer(kind = kint_gl), intent(in) :: Nfft
!!        integer(kind = kint_gl), intent(in) :: IT_ispack(Nfft/2)
!!        real(kind = 8), intent(in) :: T_ispack(Nfft+Nfft/2)
!!        real(kind = kreal), intent(inout) :: X(M,Nfft)
!!        real(kind = 8), intent(inout) :: X_ispack(Mmax_smp*Nfft,Nsmp)
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
!!
!!@n @param Mmax_smp    Maximum number of component for each SMP process
!!@n @param X_ispack(Mmax_smp*Nfft,Nsmp) 
!!                 Data for multiple Fourier transform
!!@n @param IT_ispack(5)              Work integer for ISPACK
!!@n @param T_ispack(itwo*Nfft)       Work constatnts for ISPACK
!
      module multi_pout_ISPACK3_smp
!
      use omp_lib
!
      use m_precision
      use m_constants
!
      implicit none
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine FXRINI_kemo(Nfft, IT_ispack, T_ispack)
!
      integer(kind = kint_gl), intent(in) :: Nfft
      integer(kind = kint_gl), intent(inout) :: IT_ispack(Nfft/2)
      real(kind = 8), intent(inout) :: T_ispack(Nfft+Nfft/2)
!
!
      call FXRINI(Nfft, IT_ispack, T_ispack(1))
!
      end subroutine FXRINI_kemo
!
! ------------------------------------------------------------------
!
      subroutine multi_pout_FXRTFA(Nsmp, Nstacksmp, M, Nfft, X,         &
     &          X_ispack, Mmax_smp, IT_ispack, T_ispack,                &
     &          elapsed_fft, elapsed_cpy)
!
      use normalize_for_ISPACK
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: M, Mmax_smp
      integer(kind = kint_gl), intent(in) :: Nfft
      integer(kind = kint_gl), intent(in) :: IT_ispack(Nfft/2)
      real(kind = 8), intent(in) :: T_ispack(Nfft+Nfft/2)
!
      real(kind = kreal), intent(inout) :: X(M, Nfft)
      real(kind = 8), intent(inout) :: X_ispack(Mmax_smp*Nfft,Nsmp)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
!
!
      start = OMP_GET_WTIME()
      call copy_rtp_fld_to_FXRTFA(Nsmp, Nstacksmp, Mmax_smp, Nfft,      &
     &                            M, X(1,1), X_ispack(1,1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
!$omp parallel
      call multi_pout_FXRTFA_smp(Nsmp, Nstacksmp, Mmax_smp,             &
     &                           Nfft, X_ispack, IT_ispack, T_ispack)
!$omp end parallel
      elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call norm_rtp_spectr_from_FXRTFA(Nsmp, Nstacksmp, Mmax_smp, Nfft, &
     &                                 X_ispack(1,1), M, X(1,1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine multi_pout_FXRTFA
!
! ------------------------------------------------------------------
!
      subroutine multi_pout_FXRTBA(Nsmp, Nstacksmp, M, Nfft, X,         &
     &          X_ispack, Mmax_smp, IT_ispack, T_ispack,                &
     &          elapsed_fft, elapsed_cpy)
!
      use normalize_for_ISPACK
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: M, Mmax_smp
      integer(kind = kint_gl), intent(in) :: Nfft
      integer(kind = kint_gl), intent(in) :: IT_ispack(Nfft/2)
      real(kind = 8), intent(in) :: T_ispack(Nfft+Nfft/2)
!
      real(kind = kreal), intent(inout) :: X(M,Nfft)
      real(kind = 8), intent(inout) :: X_ispack(Mmax_smp*Nfft,Nsmp)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
!
!
      start = OMP_GET_WTIME()
      call norm_rtp_spectr_to_FXRTBA(Nsmp, Nstacksmp, Mmax_smp, Nfft,   &
     &                               M, X(1,1), X_ispack(1,1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
!$omp parallel
      call multi_pout_FXRTBA_smp(Nsmp, Nstacksmp, Mmax_smp,             &
     &    Nfft, X_ispack, IT_ispack, T_ispack)
!$omp end parallel
      elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call copy_rtp_fld_from_FXRTBA(Nsmp, Nstacksmp, Mmax_smp, Nfft,    &
     &                              X_ispack(1,1), M, X(1,1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine multi_pout_FXRTBA
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine multi_pout_FXRTFA_smp(Nsmp, Nstacksmp, Mmax_smp,       &
     &          Nfft, X_ispack, IT_ispack, T_ispack)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: Mmax_smp
      integer(kind = kint_gl), intent(in) :: Nfft
      integer(kind = kint_gl), intent(in) :: IT_ispack(Nfft/2)
      real(kind = 8), intent(in) :: T_ispack(Nfft+Nfft/2)
!
      real(kind = 8), intent(inout) :: X_ispack(Mmax_smp*Nfft,Nsmp)
!
      integer(kind = kint_gl) :: ismp, num8
!
!
!$omp do private(num8)
      do ismp = 1, Nsmp
        num8 = Nstacksmp(ismp) - Nstacksmp(ismp-1)
        if(num8 .le. 0) cycle
!
        call FXRTFA(num8, Nfft, X_ispack(1,ismp),                       &
     &              IT_ispack(1), T_ispack(1))
      end do
!$omp end do nowait
!
      end subroutine multi_pout_FXRTFA_smp
!
! ------------------------------------------------------------------
!
      subroutine multi_pout_FXRTBA_smp(Nsmp, Nstacksmp, Mmax_smp,       &
     &          Nfft, X_ispack, IT_ispack, T_ispack)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: Mmax_smp
      integer(kind = kint_gl), intent(in) :: Nfft
      integer(kind = kint_gl), intent(in) :: IT_ispack(Nfft/2)
      real(kind = 8), intent(in) :: T_ispack(Nfft+Nfft/2)
!
      real(kind = 8), intent(inout) :: X_ispack(Mmax_smp*Nfft,Nsmp)
!
      integer(kind = kint_gl) :: ismp, num8
!
!
!$omp do private(num8)
      do ismp = 1, Nsmp
        num8 = Nstacksmp(ismp) - Nstacksmp(ismp-1)
        if(num8 .le. 0) cycle
!
        call FXRTBA(num8, Nfft, X_ispack(1,ismp),                       &
     &              IT_ispack(1), T_ispack(1))
      end do
!$omp end do nowait
!
      end subroutine multi_pout_FXRTBA_smp
!
! ------------------------------------------------------------------
!
      end module multi_pout_ISPACK3_smp
