!>@file   multi_pin_ISPACK1_smp.f90
!!@brief  module multi_pin_ISPACK1_smp
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2026
!!
!!
!>@brief Multiple Fourier transform with inner frequency loop by ISPACK1
!!
!!@verbatim
!!      subroutine multi_pin_FTTRUF(Nsmp, Nstacksmp, M, Nfft, X,        &
!!     &          X_ispack, Mmax_smp, IT_ispack, T_ispack, WORK_ispack, &
!!     &          elapsed_fft, elapsed_cpy)
!!      subroutine multi_pin_FTTRUF_smp(Nsmp, Nstacksmp, Mmax_smp, Nfft,&
!!     &          X_ispack, IT_ispack, T_ispack, WORK_ispack)
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: M, Nfft, Mmax_smp
!!        integer(kind = 4), intent(in) :: IT_ispack(5)
!!        real(kind = 8), intent(in) :: T_ispack(itwo*Nfft)
!!        real(kind = kreal), intent(inout) :: X(Nfft,M)
!!        real(kind = kreal), intent(inout)                             &
!!     &                 :: X_ispack(Mmax_smp*Nfft,Nsmp)
!!        real(kind = 8), intent(inout)                                 &
!!     &                 :: WORK_ispack(Mmax_smp*Nfft,Nsmp)
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
!!      subroutine multi_pin_FTTRUB(Nsmp, Nstacksmp, M, Nfft, X,        &
!!     &          X_ispack, Mmax_smp, IT_ispack, T_ispack, WORK_ispack, &
!!     &          elapsed_fft, elapsed_cpy)
!!      subroutine multi_pin_FTTRUB_smp(Nsmp, Nstacksmp, Mmax_smp, Nfft,&
!!     &          X_ispack, IT_ispack, T_ispack, WORK_ispack)
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: M, Nfft, Mmax_smp
!!        integer(kind = 4), intent(in) :: IT_ispack(5)
!!        real(kind = 8), intent(in) :: T_ispack(itwo*Nfft)
!!        real(kind = kreal), intent(inout) :: X(Nfft,M)
!!        real(kind = kreal), intent(inout)                             &
!!     &                 :: X_ispack(Mmax_smp*Nfft,Nsmp)
!!        real(kind = 8), intent(inout)                                 &
!!     &                 :: WORK_ispack(Mmax_smp*Nfft,Nsmp)
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
      module multi_pin_ISPACK1_smp
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
      subroutine multi_pin_FTTRUF(Nsmp, Nstacksmp, M, Nfft, X,          &
     &          X_ispack, Mmax_smp, IT_ispack, T_ispack, WORK_ispack,   &
     &          elapsed_fft, elapsed_cpy)
!
      use swap_prt_data_for_ISPACK
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft, Mmax_smp
      integer(kind = 4), intent(in) :: IT_ispack(5)
      real(kind = 8), intent(in) :: T_ispack(itwo*Nfft)
!
      real(kind = kreal), intent(inout) :: X(Nfft,M)
      real(kind = kreal), intent(inout) :: X_ispack(Mmax_smp*Nfft,Nsmp)
      real(kind = 8), intent(inout) :: WORK_ispack(Mmax_smp*Nfft,Nsmp)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
!
!
      start = OMP_GET_WTIME()
      call swap_prt_fld_to_FXRTFA                                       &
     &   (Nsmp, Nstacksmp, cast_long(Mmax_smp), cast_long(Nfft),        &
     &    cast_long(M), X(1,1), X_ispack(1,1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
!$omp parallel
      call multi_pin_FTTRUF_smp(Nsmp, Nstacksmp, Mmax_smp, Nfft,        &
     &    X_ispack, IT_ispack, T_ispack, WORK_ispack)
!$omp end parallel
      elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call swap_prt_spectr_from_FXRTFA                                  &
     &   (Nsmp, Nstacksmp, cast_long(Mmax_smp), cast_long(Nfft),        &
     &    X_ispack(1,1), cast_long(M), X(1,1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine multi_pin_FTTRUF
!
! ------------------------------------------------------------------
!
      subroutine multi_pin_FTTRUB(Nsmp, Nstacksmp, M, Nfft, X,          &
     &          X_ispack, Mmax_smp, IT_ispack, T_ispack, WORK_ispack,   &
     &          elapsed_fft, elapsed_cpy)
!
      use swap_prt_data_for_ISPACK
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft, Mmax_smp
      integer(kind = 4), intent(in) :: IT_ispack(5)
      real(kind = 8), intent(in) :: T_ispack(itwo*Nfft)
!
      real(kind = kreal), intent(inout) :: X(Nfft,M)
      real(kind = kreal), intent(inout)                                 &
     &                              :: X_ispack(Mmax_smp*Nfft,Nsmp)
      real(kind = 8), intent(inout) :: WORK_ispack(Mmax_smp*Nfft,Nsmp)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
!
!
      start = OMP_GET_WTIME()
      call swap_prt_spectr_to_FXRTBA                                    &
     &   (Nsmp, Nstacksmp, cast_long(Mmax_smp), cast_long(Nfft),        &
     &    cast_long(M), X(1,1), X_ispack(1,1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
!$omp parallel
      call multi_pin_FTTRUB_smp(Nsmp, Nstacksmp, Mmax_smp, Nfft,        &
     &          X_ispack, IT_ispack, T_ispack, WORK_ispack)
!$omp end parallel
      elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call swap_prt_fld_from_FXRTBA                                     &
     &   (Nsmp, Nstacksmp, cast_long(Mmax_smp), cast_long(Nfft),        &
     &    X_ispack(1,1), cast_long(M), X(1,1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine multi_pin_FTTRUB
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine multi_pin_FTTRUF_smp(Nsmp, Nstacksmp, Mmax_smp, Nfft,  &
     &          X_ispack, IT_ispack, T_ispack, WORK_ispack)
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
      end subroutine multi_pin_FTTRUF_smp
!
! ------------------------------------------------------------------
!
      subroutine multi_pin_FTTRUB_smp(Nsmp, Nstacksmp, Mmax_smp, Nfft,  &
     &          X_ispack, IT_ispack, T_ispack, WORK_ispack)
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
      integer(kind = kint_gl) :: ismp,  num8
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
      end subroutine multi_pin_FTTRUB_smp
!
! ------------------------------------------------------------------
!
      end module multi_pin_ISPACK1_smp
