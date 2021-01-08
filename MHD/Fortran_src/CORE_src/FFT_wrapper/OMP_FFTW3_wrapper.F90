!>@file   OMP_FFTW3_wrapper.F90
!!@brief  module OMP_FFTW3_wrapper
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine init_OMP_FFTW(Ncomp, Nfft,                           &
!!     &          plan_forward, plan_backward, aNfft, X_FFTW, C_FFTW)
!!
!!   wrapper subroutine for initierize FFTW plans
!! ------------------------------------------------------------------
!!      subroutine destroy_OMP_FFTW(plan_forward, plan_backward)
!!        CAUTION!!  dfftw_destroy_plan oftern makes SEGMENTAION FAULT!!
!!
!!
!!   wrapper subroutine for clear FFTW plans
!! ------------------------------------------------------------------
!!
!!      subroutine forward_mul_OMP_FFTW                                 &
!!     &         (plan_forward, Ncomp, Nfft, aNfft, X, X_FFTW, C_FFTW)
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
!!      subroutine backward_mul_OMP_FFTW                                &
!!     &         (plan_backward, Ncomp, Nfft, X, X_FFTW, C_FFTW)
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
      module OMP_FFTW3_wrapper
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
!
      private :: IONE_4, inembed
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_OMP_FFTW(Ncomp, Nfft,                             &
     &          plan_forward, plan_backward, aNfft, X_FFTW, C_FFTW)
!
      use m_OMP_FFTW3_counter
!
      integer(kind = kint), intent(in) :: Nfft, Ncomp
!
      integer(kind = fftw_plan), intent(inout) :: plan_forward
      integer(kind = fftw_plan), intent(inout) :: plan_backward
      real(kind = kreal), intent(inout) :: aNfft
      real(kind = kreal), intent(inout) :: X_FFTW(Ncomp,Nfft)
      complex(kind = fftw_complex), intent(inout)                       &
     &              :: C_FFTW(Ncomp,Nfft/2+1)
!
      integer(kind = 4) :: Nfft4, howmany
!
!
      Nfft4 = int(Nfft,KIND(Nfft4))
      howmany = int(Ncomp, KIND(howmany))
!
      call chack_init_OMP_FFTW()
!
      call dfftw_plan_many_dft_r2c                                      &
     &   (plan_forward, IONE_4, Nfft4, howmany,                         &
     &    X_FFTW(1,1), inembed, howmany, IONE_4,                        &
     &    C_FFTW(1,1), inembed, howmany, IONE_4, FFTW_KEMO_EST)
      call dfftw_plan_many_dft_c2r                                      &
     &   (plan_backward, IONE_4, Nfft4, howmany,                        &
     &    C_FFTW(1,1), inembed, howmany, IONE_4,                        &
     &    X_FFTW(1,1), inembed, howmany, IONE_4, FFTW_KEMO_EST)
      aNfft = one / dble(Nfft)
!
      end subroutine init_OMP_FFTW
!
! ------------------------------------------------------------------
!
      subroutine destroy_OMP_FFTW(plan_forward, plan_backward)
!
      use m_OMP_FFTW3_counter
!
      integer(kind = fftw_plan), intent(in) :: plan_forward
      integer(kind = fftw_plan), intent(in) :: plan_backward
!
!
      call dfftw_destroy_plan(plan_forward)
      call dfftw_destroy_plan(plan_backward)
      call dfftw_cleanup
      call chack_clean_OMP_FFTW()
!
      end subroutine destroy_OMP_FFTW
!
! ------------------------------------------------------------------
!
      subroutine forward_mul_OMP_FFTW                                   &
     &         (plan_forward, Ncomp, Nfft, aNfft, X, X_FFTW, C_FFTW)
!
      integer(kind = kint), intent(in) :: Ncomp, Nfft
      integer(kind = fftw_plan), intent(in) :: plan_forward
      real(kind = kreal), intent(in) :: aNfft
!
      real(kind = kreal), intent(inout) :: X(Ncomp, Nfft)
      real(kind = kreal), intent(inout) :: X_FFTW(Ncomp,Nfft)
      complex(kind = fftw_complex), intent(inout)                       &
     &              :: C_FFTW(Ncomp,Nfft/2+1)
!
      integer(kind = kint) ::  i, j
      real :: dummy(3), rtmp(3)
!
!
!      call cpu_time(dummy(1))
!$omp parallel do private(i, j)
      do j = 1, Ncomp
        X_FFTW(j,1:Nfft) = X(j,1:Nfft)
      end do
!$omp end parallel do
!      call cpu_time(rtmp(1))
!
!      call cpu_time(dummy(2))
        call dfftw_execute_dft_r2c(plan_forward, X_FFTW, C_FFTW)
!      call cpu_time(rtmp(2))
!
!   normalization
!      call cpu_time(dummy(3))
!$omp parallel do private(i, j)
      do j = 1, Ncomp
        X(j,1) = aNfft * real(C_FFTW(j,1))
        do i = 2, (Nfft+1)/2
          X(j,2*i-1) = two * aNfft * real(C_FFTW(j,i))
          X(j,2*i  ) = two * aNfft * real(C_FFTW(j,i)*iu)
        end do 
        i = (Nfft+1)/2 + 1
        X(j,2) = two * aNfft * real(C_FFTW(j,i))
      end do
!$omp end parallel do
!      call cpu_time(rtmp(3))
!      elapsed_fftw(1:3) = elapsed_fftw(1:3) + rtmp(1:3) - dummy(1:3)
!
      end subroutine forward_mul_OMP_FFTW
!
! ------------------------------------------------------------------
!
      subroutine backward_mul_OMP_FFTW                                  &
     &         (plan_backward, Ncomp, Nfft, X, X_FFTW, C_FFTW)
!
      integer(kind = kint), intent(in) :: Ncomp, Nfft
      integer(kind = fftw_plan), intent(in) :: plan_backward
!
      real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
      real(kind = kreal), intent(inout) :: X_FFTW(Ncomp,Nfft)
      complex(kind = fftw_complex), intent(inout)                       &
     &              :: C_FFTW(Ncomp,Nfft/2+1)
!
      integer(kind = kint) :: i, j
      real :: dummy(3), rtmp(3)
!
!
!   normalization
!      call cpu_time(dummy(3))
!$omp parallel do private(i,j)
      do j = 1, Ncomp
        C_FFTW(j,1) = cmplx(X(j,1), zero, kind(0d0))
        do i = 2, (Nfft+1)/2
          C_FFTW(j,i) = half * cmplx(X(j,2*i-1), -X(j,2*i),kind(0d0))
        end do
        i = (Nfft+1)/2 + 1
        C_FFTW(j,i) = half * cmplx(X(j,2), zero, kind(0d0))
      end do
!$omp end parallel do
!      call cpu_time(rtmp(3))
!
!      call cpu_time(dummy(2))
        call dfftw_execute_dft_c2r(plan_backward, C_FFTW, X_FFTW)
!      call cpu_time(rtmp(2))
!
!      call cpu_time(dummy(1))
!$omp parallel do private(i,j)
      do j = 1, Ncomp
        X(j,1:Nfft) = X_FFTW(j,1:Nfft)
      end do
!$omp end parallel do
!      call cpu_time(rtmp(1))
!
!      elapsed_fftw(1:3) = elapsed_fftw(1:3) + rtmp(1:3) - dummy(1:3)
!
      end subroutine backward_mul_OMP_FFTW
!
! ------------------------------------------------------------------
!
      end module OMP_FFTW3_wrapper
