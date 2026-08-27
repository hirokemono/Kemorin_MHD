!>@file   single_pin_ISPACK3_smp.f90
!!@brief  module single_pin_ISPACK3_smp
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2026
!!
!!
!>@brief Multiple Fourier transform with inner frequency loop by ISPACK1
!!
!!@verbatim
!!      subroutine single_pin_FXRTFA_smp(Nsmp, Nstacksmp, M, Nfft, X,   &
!!     &          IT_ispack, T_ispack, elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) :: Nsmp
!!        integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
!!        integer(kind = kint_gl), intent(in) :: M, Nfft
!!        integer(kind = kint_gl), intent(in) :: IT_ispack(Nfft/2)
!!        real(kind = 8), intent(in) :: T_ispack(Nfft+Nfft/2)
!!        real(kind = kreal), intent(inout) :: X(Nfft,M)
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
!!      subroutine single_pin_FXRTBA_smp(Nsmp, Nstacksmp, M, Nfft, X,   &
!!     &          IT_ispack, T_ispack, elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) :: Nsmp
!!        integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
!!        integer(kind = kint_gl), intent(in) :: M, Nfft
!!        integer(kind = kint_gl), intent(in) :: IT_ispack(Nfft/2)
!!        real(kind = 8), intent(in) :: T_ispack(Nfft+Nfft/2)
!!        real(kind = kreal), intent(inout) :: X(Nfft,M)
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
!
      module single_pin_ISPACK3_smp
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
      subroutine single_pin_FXRTFA_smp(Nsmp, Nstacksmp, M, Nfft, X,     &
     &          IT_ispack, T_ispack, elapsed_fft, elapsed_cpy)
!
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: Nsmp
      integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: M, Nfft
      integer(kind = kint_gl), intent(in) :: IT_ispack(Nfft/2)
      real(kind = 8), intent(in) :: T_ispack(Nfft+Nfft/2)
!
      real(kind = kreal), intent(inout) :: X(Nfft,M)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: st_c, ed_c, st_f, ed_f
      integer(kind = kint_gl) :: j, ismp, ist, ied
!
!
      ed_c = 0.0d0
      ed_f = 0.0d0
!$omp parallel do private(j,ist,ied,st_c,st_f) reduction(+:ed_c,ed_f)
      do ismp = 1, Nsmp
        ist = Nstacksmp(ismp-1) + 1
        ied = Nstacksmp(ismp)
!
        do j = ist, ied
          st_f = OMP_GET_WTIME()
          call FXRTFA(cast_long(ione), Nfft, X(1,j),                    &
     &                IT_ispack(1), T_ispack(1))
          ed_f = ed_f + OMP_GET_WTIME() - st_f
!
          st_c = OMP_GET_WTIME()
!          X(1:2,j) =    X(1:2,j)
          X(3:Nfft-1:2,j) =  two * X(3:Nfft-1:2,j)
          X(4:Nfft:  2,j) = -two * X(4:Nfft:  2,j)
          ed_c = ed_c + OMP_GET_WTIME() - st_c
        end do
      end do
!$omp end parallel do
!
      elapsed_fft = elapsed_fft + ed_f / dble(Nsmp)
      elapsed_cpy = elapsed_cpy + ed_c / dble(Nsmp)
!
      end subroutine single_pin_FXRTFA_smp
!
! ------------------------------------------------------------------
!
      subroutine single_pin_FXRTBA_smp(Nsmp, Nstacksmp, M, Nfft, X,     &
     &          IT_ispack, T_ispack, elapsed_fft, elapsed_cpy)
!
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: Nsmp
      integer(kind = kint_gl), intent(in) :: Nstacksmp(0:Nsmp)
      integer(kind = kint_gl), intent(in) :: M, Nfft
      integer(kind = kint_gl), intent(in) :: IT_ispack(Nfft/2)
      real(kind = 8), intent(in) :: T_ispack(Nfft+Nfft/2)
!
      real(kind = kreal), intent(inout) :: X(Nfft,M)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: st_c, ed_c, st_f, ed_f
      integer(kind = kint_gl) :: j, ismp, ist, ied
!
!
      ed_c = 0.0d0
      ed_f = 0.0d0
!$omp parallel do private(j,ist,ied,st_c,st_f) reduction(+:ed_c,ed_f)
      do ismp = 1, Nsmp
        ist = Nstacksmp(ismp-1) + 1
        ied = Nstacksmp(ismp)
        do j = ist, ied
!
          st_c = OMP_GET_WTIME()
!          X(1:2,j) = X(1:2,j)
          X(3:Nfft-2:2,j) =  half * X(3:Nfft-2:2,j)
          X(4:Nfft:  2,j) = -half * X(4:Nfft:  2,j)
          ed_c = st_c + OMP_GET_WTIME() - st_c
!
          st_f = OMP_GET_WTIME()
          call FXRTBA(cast_long(ione), Nfft, X(1,j),                    &
     &                IT_ispack(1), T_ispack(1))
          ed_f = OMP_GET_WTIME() - st_f
        end do
      end do
!$omp end parallel do
!
      elapsed_fft = elapsed_fft + ed_f / dble(Nsmp)
      elapsed_cpy = elapsed_cpy + ed_c / dble(Nsmp)
!
      end subroutine single_pin_FXRTBA_smp
!
! ------------------------------------------------------------------
!
      end module single_pin_ISPACK3_smp
