!>@file   FFTW3_multi_wrapper.F90
!!@brief  module FFTW3_multi_wrapper
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine init_4_FFTW_mul_smp(Nsmp, Nstacksmp, Ncomp, Nfft,    &
!!     &          plan_forward_smp, plan_backward_smp,                  &
!!     &          aNfft, X_FFTW, C_FFTW)
!!
!!   wrapper subroutine for initierize FFTW plans
!! ------------------------------------------------------------------
!!      subroutine destroy_FFTW_mul_smp                                 &
!!     &         (Nsmp, plan_backward_smp, plan_backward)
!!        CAUTION!!  dfftw_destroy_plan oftern makes SEGMENTAION FAULT!!
!!
!!
!!   wrapper subroutine for clear FFTW plans
!! ------------------------------------------------------------------
!!
!!      subroutine FFTW_mul_forward_SMP(plan_forward_smp,               &
!!     &          Nsmp, Nstacksmp, Ncomp, Nfft, aNfft, X, X_FFTW, C_FFTW)
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for forward Fourier transform by FFTW3
!!
!!   a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!   b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!    K = Nfft/2....
!!   a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!! ------------------------------------------------------------------
!!
!!      subroutine FFTW_mul_backward_SMP(plan_backward_smp,             &
!!     &          Nsmp, Nstacksmp, Ncomp, Nfft, X, X_FFTW, C_FFTW)
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for backward Fourier transform by FFTW3
!!
!!   x_{k} = a_{0} + (-1)^{j} a_{Nfft/2} + sum_{k=1}^{Nfft/2-1}
!!          (a_{k} \cos(2\pijk/Nfft) + b_{k} \sin(2\pijk/Nfft))
!!
!! ------------------------------------------------------------------
!!
!!       i = 1:     a_{0}
!!       i = 2:     a_{Nfft/2}
!!       i = 3:     a_{1}
!!       i = 4:     b_{1}
!!       ...
!!       i = 2*k+1: a_{k}
!!       i = 2*k+2: b_{k}
!!       ...
!!       i = Nfft-1:   a_{Nfft/2-1}
!!       i = Nfft:     b_{Nfft/2-1}
!!
!! ------------------------------------------------------------------
!!@endverbatim
!!
!!@n @param Nsmp  Number of SMP processors
!!@n @param Nstacksmp(0:Nsmp)   End number for each SMP process
!!@n @param Ncomp           Number of components for Fourier transforms
!!@n @param Nfft        Data length for eadh FFT
!!@n @param X(Ncomp, Nfft)  Data for Fourier transform
!!
!!@n @param plan_forward   FFTW plan for forward transform
!!@n @param plan_backward  FFTW plan for backward transform
!!@n @param aNfft       normalization parameter for FFTW (= 1 / Nfft)
!!@n @param X_FFTW      real data for multiple Fourier transform
!!@n @param C_FFTW      spectrum data for multiple Fourier transform
!
      module FFTW3_multi_wrapper
!
      use m_precision
      use m_constants
      use m_fftw_parameters
!
      implicit none
!
      real(kind = kreal) :: elapsed_fftw(3) = (/0.0,0.0,0.0/)
!
      integer, parameter :: IONE_4 = 1
      integer, parameter :: inembed = 0
      integer, parameter :: istride = 1
!
      private :: IONE_4, inembed, istride
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_4_FFTW_mul_smp(Nsmp, Nstacksmp, Ncomp, Nfft,      &
     &          plan_forward_smp, plan_backward_smp,                    &
     &          aNfft, X_FFTW, C_FFTW)
!
      integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Nfft, Ncomp
!
      integer(kind = fftw_plan), intent(inout)                          &
     &                          :: plan_forward_smp(Nsmp)
      integer(kind = fftw_plan), intent(inout)                          &
     &                          :: plan_backward_smp(Nsmp)
      real(kind = kreal), intent(inout) :: aNfft
      real(kind = kreal), intent(inout) :: X_FFTW(Nfft,Ncomp)
      complex(kind = fftw_complex), intent(inout)                       &
     &              :: C_FFTW(Nfft/2+1,Ncomp)
!
      integer(kind = kint) :: ip, ist
      integer(kind = 4) :: Nfft4, howmany, idist_r, idist_c
!
!
      Nfft4 = int(Nfft,KIND(Nfft4))
      do ip = 1, Nsmp
        ist = Nstacksmp(ip-1) + 1
        howmany = int(Nstacksmp(ip  ) - Nstacksmp(ip-1), KIND(howmany))
        idist_r = int(Nfft, KIND(idist_r))
        idist_c = int(Nfft, KIND(idist_c))/2 + 1
!
        call dfftw_plan_many_dft_r2c                                    &
     &     (plan_forward_smp(ip), IONE_4, Nfft4, howmany,               &
     &      X_FFTW(1,ist), inembed, istride, idist_r,                   &
     &      C_FFTW(1,ist), inembed, istride, idist_c, FFTW_KEMO_EST)
        call dfftw_plan_many_dft_c2r                                    &
     &     (plan_backward_smp(ip), IONE_4, Nfft4, howmany,              &
     &      C_FFTW(1,ist), inembed, istride, idist_c,                   &
     &      X_FFTW(1,ist), inembed, istride, idist_r, FFTW_KEMO_EST)
      end do
      aNfft = one / dble(Nfft)
!
      end subroutine init_4_FFTW_mul_smp
!
! ------------------------------------------------------------------
!
      subroutine destroy_FFTW_mul_smp                                   &
     &          (Nsmp, plan_forward, plan_backward)
!
      integer(kind = kint), intent(in) ::  Nsmp
!
      integer(kind = fftw_plan), intent(in) :: plan_forward(Nsmp)
      integer(kind = fftw_plan), intent(in) :: plan_backward(Nsmp)
!
      integer(kind = kint) :: j
!
!
      do j = 1, Nsmp
        call dfftw_destroy_plan(plan_forward(j))
        call dfftw_destroy_plan(plan_backward(j))
        call dfftw_cleanup
      end do
!
      end subroutine destroy_FFTW_mul_smp
!
! ------------------------------------------------------------------
!
      subroutine FFTW_mul_forward_SMP(plan_forward_smp,                 &
     &          Nsmp, Nstacksmp, Ncomp, Nfft, aNfft, X, X_FFTW, C_FFTW)
!
      integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Ncomp, Nfft
      integer(kind = fftw_plan), intent(in) :: plan_forward_smp(Nsmp)
      real(kind = kreal), intent(in) :: aNfft
!
      real(kind = kreal), intent(inout) :: X(Ncomp, Nfft)
      real(kind = kreal), intent(inout) :: X_FFTW(Nfft,Ncomp)
      complex(kind = fftw_complex), intent(inout)                       &
     &              :: C_FFTW(Nfft/2+1,Ncomp)
!
      integer(kind = kint) ::  i, j, ip, ist, ied
      real :: dummy(3), rtmp(3)
!
!
!      call cpu_time(dummy(1))
!$omp parallel do private(i, j, ip,ist,ied)
      do ip = 1, Nsmp
        ist = Nstacksmp(ip-1) + 1
        ied = Nstacksmp(ip)
        do j = ist, ied
          X_FFTW(1:Nfft,j) = X(j,1:Nfft)
        end do
      end do
!$omp end parallel do
!      call cpu_time(rtmp(1))
!
!      call cpu_time(dummy(2))
!$omp parallel do private(ip,ist,ied)
      do ip = 1, Nsmp
        ist = Nstacksmp(ip-1) + 1
        ied = Nstacksmp(ip)
        call dfftw_execute_dft_r2c(plan_forward_smp(ip),                &
     &        X_FFTW(1,ist), C_FFTW(1,ist))
      end do
!$omp end parallel do
!      call cpu_time(rtmp(2))
!
!   normalization
!      call cpu_time(dummy(3))
!$omp parallel do private(i, j, ip,ist,ied)
      do ip = 1, Nsmp
        ist = Nstacksmp(ip-1) + 1
        ied = Nstacksmp(ip)
        do j = ist, ied
          X(j,1) = aNfft * real(C_FFTW(1,j))
          do i = 2, (Nfft+1)/2
            X(j,2*i-1) = two * aNfft * real(C_FFTW(i,j))
            X(j,2*i  ) = two * aNfft * real(C_FFTW(i,j)*iu)
          end do 
          i = (Nfft+1)/2 + 1
          X(j,2) = two * aNfft * real(C_FFTW(i,j))
        end do
      end do
!$omp end parallel do
!      call cpu_time(rtmp(3))
!      elapsed_fftw(1:3) = elapsed_fftw(1:3) + rtmp(1:3) - dummy(1:3)
!
      end subroutine FFTW_mul_forward_SMP
!
! ------------------------------------------------------------------
!
      subroutine FFTW_mul_backward_SMP(plan_backward_smp,               &
     &          Nsmp, Nstacksmp, Ncomp, Nfft, X, X_FFTW, C_FFTW)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Ncomp, Nfft
      integer(kind = fftw_plan), intent(in) :: plan_backward_smp(Nsmp)
!
      real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
      real(kind = kreal), intent(inout) :: X_FFTW(Nfft,Ncomp)
      complex(kind = fftw_complex), intent(inout)                       &
     &              :: C_FFTW(Nfft/2+1,Ncomp)
!
      integer(kind = kint) :: i, j, ip, ist, ied
      real :: dummy(3), rtmp(3)
!
!
!      call cpu_time(dummy(3))
!$omp parallel do private(i,j,ip,ist,ied)
      do ip = 1, Nsmp
        ist = Nstacksmp(ip-1) + 1
        ied = Nstacksmp(ip)
!   normalization
        do j = ist, ied
          C_FFTW(1,j) = cmplx(X(j,1), zero, kind(0d0))
          do i = 2, (Nfft+1)/2
            C_FFTW(i,j) = half * cmplx(X(j,2*i-1), -X(j,2*i),kind(0d0))
          end do
          i = (Nfft+1)/2 + 1
          C_FFTW(i,j) = half * cmplx(X(j,2), zero, kind(0d0))
        end do
      end do
!$omp end parallel do
!      call cpu_time(rtmp(3))
!
!      call cpu_time(dummy(2))
!$omp parallel do private(ip,ist,ied)
      do ip = 1, Nsmp
        ist = Nstacksmp(ip-1) + 1
        ied = Nstacksmp(ip)
        call dfftw_execute_dft_c2r(plan_backward_smp(ip),               &
     &        C_FFTW(1,ist), X_FFTW(1,ist))
      end do
!$omp end parallel do
!      call cpu_time(rtmp(2))
!
!      call cpu_time(dummy(1))
!$omp parallel do private(i,j,ip,ist,ied)
      do ip = 1, Nsmp
        ist = Nstacksmp(ip-1) + 1
        ied = Nstacksmp(ip)
        do j = ist, ied
          X(j,1:Nfft) = X_FFTW(1:Nfft,j)
        end do
      end do
!$omp end parallel do
!      call cpu_time(rtmp(1))
!
!      elapsed_fftw(1:3) = elapsed_fftw(1:3) + rtmp(1:3) - dummy(1:3)
!
      end subroutine FFTW_mul_backward_SMP
!
! ------------------------------------------------------------------
!
      end module FFTW3_multi_wrapper
