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
!!      subroutine multi_pout_FXRTFA(M, Nfft, X, WK,                    &
!!     &                             elapsed_fft, elapsed_cpy)
!!        integer(kind = kint_gl), intent(in) :: M
!!        integer(kind = kint_gl), intent(in) :: Nfft
!!        type(working_ISPACK3), intent(inout) :: WK
!!        real(kind = kreal), intent(inout) :: X(M, Nfft)
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
!!      subroutine multi_pout_FXRTBA(M, Nfft, X, WK,                    &
!!     &                             elapsed_fft, elapsed_cpy)
!!        integer(kind = kint_gl), intent(in) :: M
!!        integer(kind = kint_gl), intent(in) :: Nfft
!!        type(working_ISPACK3), intent(inout) :: WK
!!        real(kind = kreal), intent(inout) :: X(M,Nfft)
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
      subroutine multi_pout_FXRTFA(M, Nfft, X, WK,                      &
     &                             elapsed_fft, elapsed_cpy)
!
      use t_ispack3_FFT_wrapper
      use calypso_multi_ispack3
      use normalize_for_ISPACK
!
      integer(kind = kint_gl), intent(in) :: M
      integer(kind = kint_gl), intent(in) :: Nfft
!
      type(working_ISPACK3), intent(inout) :: WK
      real(kind = kreal), intent(inout) :: X(M, Nfft)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
!
!
      start = OMP_GET_WTIME()
      call copy_rtp_fld_to_FXRTFA                                       &
     &   (WK%Nplan_ISPACK3, WK%istack_ISPACK3, WK%Mmax_smp,             &
     &    Nfft, M, X(1,1), WK%X_ispack(1,1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
!$omp parallel
      call multi_FXRTFA_smp                                             &
     &   (WK%Nplan_ISPACK3, WK%istack_ISPACK3, WK%Mmax_smp,             &
     &    Nfft, WK%X_ispack, WK%IT_ispack, WK%T_ispack)
!$omp end parallel
      elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call norm_rtp_spectr_from_FXRTFA                                  &
     &   (WK%Nplan_ISPACK3, WK%istack_ISPACK3, WK%Mmax_smp,             &
     &    Nfft, WK%X_ispack(1,1), M, X(1,1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine multi_pout_FXRTFA
!
! ------------------------------------------------------------------
!
      subroutine multi_pout_FXRTBA(M, Nfft, X, WK,                      &
     &                             elapsed_fft, elapsed_cpy)
!
      use t_ispack3_FFT_wrapper
      use calypso_multi_ispack3
      use normalize_for_ISPACK
!
      integer(kind = kint_gl), intent(in) :: M
      integer(kind = kint_gl), intent(in) :: Nfft
!
      type(working_ISPACK3), intent(inout) :: WK
      real(kind = kreal), intent(inout) :: X(M,Nfft)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
!
!
      start = OMP_GET_WTIME()
      call norm_rtp_spectr_to_FXRTBA                                    &
     &   (WK%Nplan_ISPACK3, WK%istack_ISPACK3, WK%Mmax_smp,             &
     &    Nfft, M, X(1,1), WK%X_ispack(1,1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
!$omp parallel
      call multi_FXRTBA_smp                                             &
     &   (WK%Nplan_ISPACK3, WK%istack_ISPACK3, WK%Mmax_smp,             &
     &    Nfft, WK%X_ispack, WK%IT_ispack, WK%T_ispack)
!$omp end parallel
      elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call copy_rtp_fld_from_FXRTBA                                     &
     &   (WK%Nplan_ISPACK3, WK%istack_ISPACK3, WK%Mmax_smp,             &
     &    Nfft, WK%X_ispack(1,1), M, X(1,1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine multi_pout_FXRTBA
!
! ------------------------------------------------------------------
!
      end module multi_pout_ISPACK3_smp
