!>@file   multi_pout_ISPACK1_smp.f90
!!@brief  module multi_pout_ISPACK1_smp
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
!!      subroutine FTTRUI_kemo(Nfft, IT_ispack, T_ispack)
!! ------------------------------------------------------------------
!! wrapper subroutine for initierize FFT for ISPACK
!! ------------------------------------------------------------------
!!
!!      subroutine multi_pout_FTTRUF(Nsmp, Nstacksmp, M, Nfft, X,       &
!!     &          X_ispack, Mmax_smp, IT_ispack, T_ispack, WORK_ispack, &
!!     &          elapsed_fft, elapsed_cpy)
!!      subroutine multi_pout_FTTRUF_smp(Nsmp, Nstacksmp, Mmax_smp,     &
!!     &          Nfft, X_ispack, IT_ispack, T_ispack, WORK_ispack)
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: M, Nfft, Mmax_smp
!!        integer(kind = 4), intent(in) :: IT_ispack(5)
!!        real(kind = 8), intent(in) :: T_ispack(itwo*Nfft)
!!        real(kind = kreal), intent(inout) :: X(M, Nfft)
!!        real(kind = kreal), intent(inout)                             &
!!     &                             :: X_ispack(Mmax_smp*Nfft,Nsmp)
!!        real(kind = 8), intent(inout)                                 &
!!     &                             :: WORK_ispack(Mmax_smp*Nfft,Nsmp)
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
!!      subroutine multi_pout_FTTRUB(Nsmp, Nstacksmp, M, Nfft, X,       &
!!     &          X_ispack, Mmax_smp, IT_ispack, T_ispack, WORK_ispack, &
!!     &          elapsed_fft, elapsed_cpy)
!!      subroutine multi_pout_FTTRUB_smp(Nsmp, Nstacksmp, Mmax_smp,     &
!!     &          Nfft, X_ispack, IT_ispack, T_ispack, WORK_ispack)
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: M, Nfft, Mmax_smp
!!        integer(kind = 4), intent(in) :: IT_ispack(5)
!!        real(kind = 8), intent(in) :: T_ispack(itwo*Nfft)
!!        real(kind = kreal), intent(inout) :: X(M,Nfft)
!!        real(kind = kreal), intent(inout)                             &
!!     &     :: X_ispack(Mmax_smp*Nfft,Nsmp)
!!        real(kind = 8), intent(inout)                                 &
!!     &     :: WORK_ispack(Mmax_smp*Nfft,Nsmp)
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
!!@n @param WORK_ispack(Mmax_smp*Nfft,Nsmp)  Work area for ISPACK
!
      module multi_pout_ISPACK1_smp
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
      subroutine FTTRUI_kemo(Nfft, IT_ispack, T_ispack)
!
      use ispack_0931
!
      integer(kind = kint), intent(in) :: Nfft
      integer(kind = 4), intent(inout) :: IT_ispack(5)
      real(kind = 8), intent(inout) :: T_ispack(itwo*Nfft)
!
!
      call FTTRUI( Nfft, IT_ispack, T_ispack(1) )
!
      end subroutine FTTRUI_kemo
!
! ------------------------------------------------------------------
!
      subroutine multi_pout_FTTRUF(Nsmp, Nstacksmp, M, Nfft, X,         &
     &          X_ispack, Mmax_smp, IT_ispack, T_ispack, WORK_ispack,   &
     &          elapsed_fft, elapsed_cpy)
!
      use ispack_0931
      use normalize_for_ISPACK
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft, Mmax_smp
      integer(kind = 4), intent(in) :: IT_ispack(5)
      real(kind = 8), intent(in) :: T_ispack(itwo*Nfft)
!
      real(kind = kreal), intent(inout) :: X(M, Nfft)
      real(kind = kreal), intent(inout) :: X_ispack(Mmax_smp*Nfft,Nsmp)
      real(kind = 8), intent(inout) :: WORK_ispack(Mmax_smp*Nfft,Nsmp)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
!
!
      start = OMP_GET_WTIME()
      call copy_rtp_fld_to_FXRTFA                                       &
     &   (Nsmp, Nstacksmp, cast_long(Mmax_smp), cast_long(Nfft),        &
     &    cast_long(M), X(1,1), X_ispack(1,1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
!$omp parallel
      call multi_pout_FTTRUF_smp(Nsmp, Nstacksmp, Mmax_smp, Nfft,       &
     &    X_ispack, IT_ispack, T_ispack, WORK_ispack)
!$omp end parallel
      elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call norm_rtp_spectr_from_FXRTFA                                  &
     &   (Nsmp, Nstacksmp, cast_long(Mmax_smp), cast_long(Nfft),        &
     &    X_ispack(1,1), cast_long(M), X(1,1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine multi_pout_FTTRUF
!
! ------------------------------------------------------------------
!
      subroutine multi_pout_FTTRUB(Nsmp, Nstacksmp, M, Nfft, X,         &
     &          X_ispack, Mmax_smp, IT_ispack, T_ispack, WORK_ispack,   &
     &          elapsed_fft, elapsed_cpy)
!
      use ispack_0931
      use normalize_for_ISPACK
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft, Mmax_smp
      integer(kind = 4), intent(in) :: IT_ispack(5)
      real(kind = 8), intent(in) :: T_ispack(itwo*Nfft)
!
      real(kind = kreal), intent(inout) :: X(M,Nfft)
      real(kind = kreal), intent(inout)                                 &
     &                              :: X_ispack(Mmax_smp*Nfft,Nsmp)
      real(kind = 8), intent(inout) :: WORK_ispack(Mmax_smp*Nfft,Nsmp)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
!
!
      start = OMP_GET_WTIME()
      call norm_rtp_spectr_to_FXRTBA                                    &
     &   (Nsmp, Nstacksmp, cast_long(Mmax_smp), cast_long(Nfft),        &
     &    cast_long(M), X(1,1), X_ispack(1,1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
!$omp parallel
      call multi_pout_FTTRUB_smp(Nsmp, Nstacksmp, Mmax_smp, Nfft,       &
     &    X_ispack, IT_ispack, T_ispack, WORK_ispack)
!$omp end parallel
      elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call copy_rtp_fld_from_FXRTBA                                     &
     &   (Nsmp, Nstacksmp, cast_long(Mmax_smp), cast_long(Nfft),        &
     &    X_ispack(1,1), cast_long(M), X(1,1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine multi_pout_FTTRUB
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine multi_pout_FTTRUF_smp(Nsmp, Nstacksmp, Mmax_smp,       &
     &          Nfft, X_ispack, IT_ispack, T_ispack, WORK_ispack)
!
      use ispack_0931
!
      integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Nfft, Mmax_smp
      integer(kind = 4), intent(in) :: IT_ispack(5)
      real(kind = 8), intent(in) :: T_ispack(itwo*Nfft)
!
      real(kind = kreal), intent(inout) :: X_ispack(Mmax_smp*Nfft,Nsmp)
      real(kind = 8), intent(inout) :: WORK_ispack(Mmax_smp*Nfft,Nsmp)
!
      integer(kind = kint_gl) :: ismp, ist, num8
!
!
!$omp do private(ist,num8)
      do ismp = 1, Nsmp
        ist = Nstacksmp(ismp-1)
        num8 = Nstacksmp(ismp) - Nstacksmp(ismp-1)
        if(num8 .le. 0) cycle
!
        call FTTRUF(int(num8), Nfft, X_ispack(1,ismp),                  &
     &      WORK_ispack(1,ismp), IT_ispack(1), T_ispack(1))
      end do
!$omp end do nowait
!
      end subroutine multi_pout_FTTRUF_smp
!
! ------------------------------------------------------------------
!
      subroutine multi_pout_FTTRUB_smp(Nsmp, Nstacksmp, Mmax_smp,       &
     &          Nfft, X_ispack, IT_ispack, T_ispack, WORK_ispack)
!
      use ispack_0931
!
      integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Nfft, Mmax_smp
      integer(kind = 4), intent(in) :: IT_ispack(5)
      real(kind = 8), intent(in) :: T_ispack(itwo*Nfft)
!
      real(kind = kreal), intent(inout)                                 &
     &                              :: X_ispack(Mmax_smp*Nfft,Nsmp)
      real(kind = 8), intent(inout) :: WORK_ispack(Mmax_smp*Nfft,Nsmp)
!
      integer(kind = kint_gl) :: ismp, ist, num8
!
!
!$omp do private(ist,num8)
      do ismp = 1, Nsmp
        ist = Nstacksmp(ismp-1)
        num8 = Nstacksmp(ismp) - Nstacksmp(ismp-1)
        if(num8 .le. 0) cycle
!
        call FTTRUB(int(num8), Nfft, X_ispack(1,ismp),                  &
     &      WORK_ispack(1,ismp), IT_ispack(1), T_ispack(1))
      end do
!$omp end do nowait
!
      end subroutine multi_pout_FTTRUB_smp
!
! ------------------------------------------------------------------
!
      end module multi_pout_ISPACK1_smp
