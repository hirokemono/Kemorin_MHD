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
!!      subroutine multi_pin_FTTRUF_smp(Nsmp, Nstacksmp, M, Nfft, X,    &
!!     &          X_ispack, Mmax_smp, IT_ispack, T_ispack, WORK_ispack, &
!!     &          elapsed_fft, elapsed_cpy)
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
!!      subroutine multi_pin_FTTRUB_smp(Nsmp, Nstacksmp, M, Nfft, X,    &
!!     &          X_ispack, Mmax_smp, IT_ispack, T_ispack, WORK_ispack, &
!!     &          elapsed_fft, elapsed_cpy)
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
      subroutine multi_pin_FTTRUF_smp(Nsmp, Nstacksmp, M, Nfft, X,      &
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
      real(kind = kreal), intent(inout) :: X(Nfft,M)
      real(kind = kreal), intent(inout) :: X_ispack(Mmax_smp*Nfft,Nsmp)
      real(kind = 8), intent(inout) :: WORK_ispack(Mmax_smp*Nfft,Nsmp)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: st_c, ed_c, st_f, ed_f
      integer(kind = kint_gl) :: ismp, ist, num8
!
!
      ed_c = 0.0d0
      ed_f = 0.0d0
!$omp parallel do private(ist,num8,st_c,st_f) reduction(+:ed_c,ed_f)
      do ismp = 1, Nsmp
        ist = Nstacksmp(ismp-1)
        num8 = Nstacksmp(ismp) - Nstacksmp(ismp-1)
!
        st_c = OMP_GET_WTIME()
        call swap_prt_fld_to_FXRTFA_smp                                 &
     &     (ist, num8, cast_long(Nfft), cast_long(M), X,                &
     &      cast_long(Mmax_smp), X_ispack(1,ismp))
        ed_c = OMP_GET_WTIME() - st_c
!
        st_f = OMP_GET_WTIME()
        call FTTRUF(int(num8), Nfft, X_ispack(1,ismp),                  &
     &      WORK_ispack(1,ismp), IT_ispack(1), T_ispack(1))
        ed_f = OMP_GET_WTIME() - st_f
!
        st_c = OMP_GET_WTIME()
        call swap_prt_spectr_from_FXRTFA_smp                            &
     &     (ist, num8, cast_long(Nfft), cast_long(Mmax_smp),            &
     &      X_ispack(1,ismp), cast_long(M), X)
        ed_c = ed_c + OMP_GET_WTIME() - st_c
      end do
!$omp end parallel do
!
      elapsed_fft = elapsed_fft + ed_f / dble(Nsmp)
      elapsed_cpy = elapsed_cpy + ed_c / dble(Nsmp)
!
      end subroutine multi_pin_FTTRUF_smp
!
! ------------------------------------------------------------------
!
      subroutine multi_pin_FTTRUB_smp(Nsmp, Nstacksmp, M, Nfft, X,      &
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
      real(kind = kreal), intent(inout) :: X(Nfft,M)
      real(kind = kreal), intent(inout)                                 &
     &                              :: X_ispack(Mmax_smp*Nfft,Nsmp)
      real(kind = 8), intent(inout) :: WORK_ispack(Mmax_smp*Nfft,Nsmp)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: st_c, ed_c, st_f, ed_f
      integer(kind = kint_gl) :: ismp, ist, num8
!
!
      ed_c = 0.0d0
      ed_f = 0.0d0
!$omp parallel do private(ist,num8,st_c,st_f) reduction(+:ed_c,ed_f)
      do ismp = 1, Nsmp
        ist = Nstacksmp(ismp-1)
        num8 = Nstacksmp(ismp) - Nstacksmp(ismp-1)
!
        st_c = OMP_GET_WTIME()
        call swap_prt_spectr_to_FXRTBA_smp                              &
     &     (ist, num8, cast_long(Nfft), cast_long(M), X,                &
     &      cast_long(Mmax_smp), X_ispack(1,ismp))
        ed_c = OMP_GET_WTIME() - st_c
!
        st_f = OMP_GET_WTIME()
        call FTTRUB(int(num8), Nfft, X_ispack(1,ismp),                  &
     &      WORK_ispack(1,ismp), IT_ispack(1), T_ispack(1))
        ed_f = OMP_GET_WTIME() - st_f
!
        st_c = OMP_GET_WTIME()
        call swap_prt_fld_from_FXRTBA_smp                               &
     &     (ist, num8, cast_long(Nfft), cast_long(Mmax_smp),            &
     &      X_ispack(1,ismp), cast_long(M), X)
        ed_c = ed_c + OMP_GET_WTIME() - st_c
      end do
!$omp end parallel do
!
      elapsed_fft = elapsed_fft + ed_f / dble(Nsmp)
      elapsed_cpy = elapsed_cpy + ed_c / dble(Nsmp)
!
      end subroutine multi_pin_FTTRUB_smp
!
! ------------------------------------------------------------------
!
      end module multi_pin_ISPACK1_smp
