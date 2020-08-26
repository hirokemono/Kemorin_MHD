!>@file   ispack3_FFT_wrapper.f90
!!@brief  module ispack3_FFT_wrapper
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
!!      subroutine FXRTFA_kemo_smp(Nsmp, Nstacksmp, M, Nfft, X,         &
!!     &          X_ispack, Mmax_smp, IT_ispack, T_ispack)
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint_gl), intent(in) :: M, Mmax_smp
!!        integer(kind = kint_gl), intent(in) :: Nfft
!!        integer(kind = kint_gl), intent(in) :: IT_ispack(Nfft/2)
!!        real(kind = 8), intent(in) :: T_ispack(Nfft+Nfft/2)
!!        real(kind = kreal), intent(inout) :: X(M, Nfft)
!!        real(kind = 8), intent(inout) :: X_ispack(Mmax_smp*Nfft,Nfft)
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for forward Fourier transform by ISPACK
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
!!      subroutine FXRTBA_kemo_smp(Nsmp, Nstacksmp, M, Nfft, X,         &
!!     &          X_ispack, Mmax_smp, IT_ispack, T_ispack)
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint_gl), intent(in) :: M, Mmax_smp
!!        integer(kind = kint_gl), intent(in) :: Nfft
!!        integer(kind = kint_gl), intent(in) :: IT_ispack(Nfft/2)
!!        real(kind = 8), intent(in) :: T_ispack(Nfft+Nfft/2)
!!        real(kind = kreal), intent(inout) :: X(M,Nfft)
!!        real(kind = 8), intent(inout) :: X_ispack(Mmax_smp*Nfft,Nfft)
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
      module ispack3_FFT_wrapper
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
      subroutine FXRTFA_kemo_smp(Nsmp, Nstacksmp, M, Nfft, X,           &
     &          X_ispack, Mmax_smp, IT_ispack, T_ispack)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: M, Mmax_smp
      integer(kind = kint_gl), intent(in) :: Nfft
      integer(kind = kint_gl), intent(in) :: IT_ispack(Nfft/2)
      real(kind = 8), intent(in) :: T_ispack(Nfft+Nfft/2)
!
      real(kind = kreal), intent(inout) :: X(M, Nfft)
      real(kind = 8), intent(inout) :: X_ispack(Mmax_smp*Nfft,Nfft)
!
      integer(kind = kint_gl) :: num8, inum, i
      integer(kind = kint) :: j, ismp, ist
      integer(kind = kint_gl) :: inod_s, inod_c
!
!
!$omp parallel do private(i,j,ist,num8,inum,inod_s,inod_c)
      do ismp = 1, Nsmp
        ist = Nstacksmp(ismp-1)
        num8 = Nstacksmp(ismp) - Nstacksmp(ismp-1)
!
        do i = 1, Nfft/2
          do inum = 1, num8
            j = ist + inum
            inod_c = inum + (2*i-2) * num8
            inod_s = inum + (2*i-1) * num8
            X_ispack(inod_c,ismp) = X(j,2*i-1)
            X_ispack(inod_s,ismp) = X(j,2*i  )
          end do
        end do
!
        call FXRTFA(num8, Nfft, X_ispack(1,ismp),                       &
     &      IT_ispack(1), T_ispack(1))
!
        do inum = 1, num8
          j = ist + inum
          inod_c = inum
          inod_s = inum + num8
          X(j,1) = X_ispack(inod_c,ismp)
          X(j,2) = X_ispack(inod_s,ismp)
        end do
        do i = 2, Nfft/2
          do inum = 1, num8
            j = ist + inum
            inod_c = inum + (2*i-2) * num8
            inod_s = inum + (2*i-1) * num8
            X(j,2*i-1) =   two * X_ispack(inod_c,ismp)
            X(j,2*i  ) = - two * X_ispack(inod_s,ismp)
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine FXRTFA_kemo_smp
!
! ------------------------------------------------------------------
!
      subroutine FXRTBA_kemo_smp(Nsmp, Nstacksmp, M, Nfft, X,           &
     &          X_ispack, Mmax_smp, IT_ispack, T_ispack)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: M, Mmax_smp
      integer(kind = kint_gl), intent(in) :: Nfft
      integer(kind = kint_gl), intent(in) :: IT_ispack(Nfft/2)
      real(kind = 8), intent(in) :: T_ispack(Nfft+Nfft/2)
!
      real(kind = kreal), intent(inout) :: X(M,Nfft)
      real(kind = 8), intent(inout) :: X_ispack(Mmax_smp*Nfft,Nfft)
!
      integer(kind = kint_gl) :: num8, inum, i
      integer(kind = kint) ::  j, ismp, ist
      integer(kind = kint_gl) :: inod_s, inod_c
!
!
!$omp parallel do private(i,j,ist,num8,inum,inod_s,inod_c)
      do ismp = 1, Nsmp
        ist = Nstacksmp(ismp-1)
        num8 = Nstacksmp(ismp) - Nstacksmp(ismp-1)
!
        do inum = 1, num8
          j = ist + inum
            inod_c = inum
            inod_s = inum + num8
          X_ispack(inod_c,ismp) = X(j,1)
          X_ispack(inod_s,ismp) = X(j,2)
        end do
        do i = 2, Nfft/2
          do inum = 1, num8
            j = ist + inum
            inod_c = inum + (2*i-2) * num8
            inod_s = inum + (2*i-1) * num8
            X_ispack(inod_c,ismp) =  half * X(j,2*i-1)
            X_ispack(inod_s,ismp) = -half * X(j,2*i  )
          end do
        end do
!
        call FXRTBA(num8, Nfft, X_ispack(1,ismp),                       &
     &      IT_ispack(1), T_ispack(1) )
!
        do i = 1, Nfft/2
          do inum = 1, num8
            j = ist + inum
            inod_c = inum + (2*i-2) * num8
            inod_s = inum + (2*i-1) * num8
            X(j,2*i-1) = X_ispack(inod_c,ismp)
            X(j,2*i  ) = X_ispack(inod_s,ismp)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine FXRTBA_kemo_smp
!
! ------------------------------------------------------------------
!
      end module ispack3_FFT_wrapper
