!>@file   multi_pout_FFTW3_smp.f90
!!@brief  module multi_pout_FFTW3_smp
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine init_4_FFTW_mul_smp                                  &
!!     &         (Nsmp, Nstacksmp, Ncomp, Nfft, Nfft_c,                 &
!!     &          plan_forward_smp, plan_backward_smp, X_FFTW, C_FFTW)
!!        integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: Nfft, Nfft_c, Ncomp
!!        integer(kind = fftw_plan), intent(inout)                      &
!!     &                          :: plan_forward_smp(Nsmp)
!!        integer(kind = fftw_plan), intent(inout)                      &
!!     &                          :: plan_backward_smp(Nsmp)
!!        real(kind = kreal), intent(inout) :: X_FFTW(Nfft,Ncomp)
!!        complex(kind = fftw_complex), intent(inout)                   &
!!     &                          :: C_FFTW(Nfft_c,Ncomp)
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
!!      subroutine multi_pout_fwd_FFTW3(plan_forward_smp,               &
!!     &          Nsmp, Nstacksmp, Ncomp, Nfft, aNfft, Nfft_c,          &
!!     &          X, X_FFTW, C_FFTW, elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft, Nfft_c
!!        integer(kind = fftw_plan), intent(in) :: plan_forward_smp(Nsmp)
!!        real(kind = kreal), intent(in) :: aNfft
!!        real(kind = kreal), intent(inout) :: X(Ncomp, Nfft)
!!        real(kind = kreal), intent(inout) :: X_FFTW(Nfft,Ncomp)
!!        complex(kind = fftw_complex), intent(inout)                   &
!!     &                                  :: C_FFTW(Nfft_c,Ncomp)
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!!      subroutine multi_pout_fwd_FFTW3_smp(plan_forward_smp,           &
!!     &          Nsmp, Nstacksmp, Ncomp_c, Nfft_r, X_FFTW,             &
!!     &          Nfft_c, C_FFTW)
!!        integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: Ncomp_c, Nfft_c, Nfft_r
!!        integer(kind = fftw_plan), intent(in) :: plan_forward_smp(Nsmp)
!!        real(kind = kreal), intent(in) :: X_FFTW(Nfft_r,Ncomp_c)
!!        complex(kind = fftw_complex), intent(inout)                   &
!!     &                                  :: C_FFTW(Nfft_c,Ncomp_c)
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for forward Fourier transform by FFTW3
!!
!!   a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!   b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \sin (\frac{2\pi j k}{Nfft})
!!
!!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!    K = Nfft/2....
!!   a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!! ------------------------------------------------------------------
!!
!!      subroutine multi_pout_bwd_FFTW3(plan_backward_smp,              &
!!     &          Nsmp, Nstacksmp, Ncomp, Nfft, Nfft_c,                 &
!!     &          X, X_FFTW, C_FFTW, elapsed_fft, elapsed_cpy)
!!        integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: Ncomp, Nfft, Nfft_c
!!        integer(kind = fftw_plan), intent(in):: plan_backward_smp(Nsmp)
!!        real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
!!        real(kind = kreal), intent(inout) :: X_FFTW(Nfft,Ncomp)
!!        complex(kind = fftw_complex), intent(inout)                   &
!!     &                                  :: C_FFTW(Nfft_c,Ncomp)
!!        real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!!      subroutine multi_pout_bwd_FFTW3_smp(plan_backward_smp,          &
!!     &          Nsmp, Nstacksmp, Ncomp_c, Nfft_c, C_FFTW,             &
!!     &          Nfft_r, X_FFTW)
!!        integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
!!        integer(kind = kint), intent(in) :: Ncomp_c, Nfft_c, Nfft_r
!!        integer(kind = fftw_plan), intent(in):: plan_backward_smp(Nsmp)
!!        complex(kind = fftw_complex), intent(in)                      &
!!     &                                  :: C_FFTW(Nfft_c,Ncomp_c)
!!        real(kind = kreal), intent(inout) :: X_FFTW(Nfft_r,Ncomp_c)
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
      module multi_pout_FFTW3_smp
!
      use omp_lib
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
      subroutine init_4_FFTW_mul_smp                                    &
     &         (Nsmp, Nstacksmp, Ncomp, Nfft, Nfft_c,                   &
     &          plan_forward_smp, plan_backward_smp, X_FFTW, C_FFTW)
!
      integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Nfft, Nfft_c, Ncomp
!
      integer(kind = fftw_plan), intent(inout)                          &
     &                          :: plan_forward_smp(Nsmp)
      integer(kind = fftw_plan), intent(inout)                          &
     &                          :: plan_backward_smp(Nsmp)
      real(kind = kreal), intent(inout) :: X_FFTW(Nfft,Ncomp)
      complex(kind = fftw_complex), intent(inout)                       &
     &                          :: C_FFTW(Nfft_c,Ncomp)
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
      end do
      call dfftw_cleanup
!
      end subroutine destroy_FFTW_mul_smp
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine multi_pout_fwd_FFTW3(plan_forward_smp,                 &
     &          Nsmp, Nstacksmp, Ncomp, Nfft, aNfft, Nfft_c,            &
     &          X, X_FFTW, C_FFTW, elapsed_fft, elapsed_cpy)
!
      use swap_rtp_data_for_FFTW
      use normalize_for_FFTW
!
      integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Ncomp, Nfft, Nfft_c
      integer(kind = fftw_plan), intent(in) :: plan_forward_smp(Nsmp)
      real(kind = kreal), intent(in) :: aNfft
!
      real(kind = kreal), intent(inout) :: X(Ncomp, Nfft)
      real(kind = kreal), intent(inout) :: X_FFTW(Nfft,Ncomp)
      complex(kind = fftw_complex), intent(inout)                       &
     &                                  :: C_FFTW(Nfft_c,Ncomp)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
      integer(kind = kint) :: j, ip, ist, num
!
!
      start = OMP_GET_WTIME()
      call swap_to_rtp_fwd_FFTW(ione, Ncomp, Nfft, X,                   &
     &                          Ncomp, Nfft, X_FFTW)
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
!$omp parallel
      call multi_pout_fwd_FFTW3_smp(plan_forward_smp, Nsmp, Nstacksmp,  &
     &    Ncomp, Nfft, X_FFTW(1,1), Nfft_c, C_FFTW(1,1))
!$omp end parallel
      elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
!   normalization
      start = OMP_GET_WTIME()
      call normalize_fwd_OMP_FFTW(aNfft, Ncomp, NFFT_c, C_FFTW(1,1))
      call swap_from_rtp_fwd_OMP_FFTW(Ncomp, NFFT_c, C_FFTW(1,1),       &
     &                                Ncomp, Nfft, ione, X(1,1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine multi_pout_fwd_FFTW3
!
! ------------------------------------------------------------------
!
      subroutine multi_pout_bwd_FFTW3(plan_backward_smp,                &
     &          Nsmp, Nstacksmp, Ncomp, Nfft, Nfft_c,                   &
     &          X, X_FFTW, C_FFTW, elapsed_fft, elapsed_cpy)
!
      use swap_rtp_data_for_FFTW
      use normalize_for_FFTW
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Ncomp, Nfft, Nfft_c
      integer(kind = fftw_plan), intent(in) :: plan_backward_smp(Nsmp)
!
      real(kind = kreal), intent(inout) :: X(Ncomp,Nfft)
      real(kind = kreal), intent(inout) :: X_FFTW(Nfft,Ncomp)
      complex(kind = fftw_complex), intent(inout)                       &
     &                                  :: C_FFTW(Nfft_c,Ncomp)
      real(kind = kreal), intent(inout) :: elapsed_fft, elapsed_cpy
!
      real(kind = kreal) :: start
!
!
!   normalization
      start = OMP_GET_WTIME()
      call swap_to_rtp_bwd_OMP_FFTW(Ncomp, Nfft, ione, X(1,1),          &
     &                              Ncomp, NFFT_c, C_FFTW(1,1))
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
!$omp parallel
      call multi_pout_bwd_FFTW3_smp(plan_backward_smp, Nsmp, Nstacksmp, &
     &    Ncomp, Nfft_c, C_FFTW(1,1), Nfft, X_FFTW(1,1))
!$omp end parallel
      elapsed_fft = elapsed_fft + OMP_GET_WTIME() - start
!
      start = OMP_GET_WTIME()
      call swap_from_rtp_bwd_FFTW(Ncomp, Nfft, X_FFTW,                  &
     &                            ione, Ncomp, Nfft, X)
      elapsed_cpy = elapsed_cpy + OMP_GET_WTIME() - start
!
      end subroutine multi_pout_bwd_FFTW3
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine multi_pout_fwd_FFTW3_smp(plan_forward_smp,             &
     &          Nsmp, Nstacksmp, Ncomp_c, Nfft_r, X_FFTW,               &
     &          Nfft_c, C_FFTW)
!
      integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Ncomp_c, Nfft_c, Nfft_r
      integer(kind = fftw_plan), intent(in) :: plan_forward_smp(Nsmp)
      real(kind = kreal), intent(in) :: X_FFTW(Nfft_r,Ncomp_c)
!
      complex(kind = fftw_complex), intent(inout)                       &
     &                                  :: C_FFTW(Nfft_c,Ncomp_c)
!
      integer(kind = kint) :: ip, ist, num
!
!
!$omp do private(ip,ist,num)
      do ip = 1, Nsmp
        num = Nstacksmp(ip  ) - Nstacksmp(ip-1)
        if(num .le. 0) cycle
!
        ist = Nstacksmp(ip-1) - Nstacksmp(0)
        call dfftw_execute_dft_r2c(plan_forward_smp(ip),                &
     &                             X_FFTW(1,ist+1), C_FFTW(1,ist+1))
      end do
!$omp end do nowait
!
      end subroutine multi_pout_fwd_FFTW3_smp
!
! ------------------------------------------------------------------
!
      subroutine multi_pout_bwd_FFTW3_smp(plan_backward_smp,            &
     &          Nsmp, Nstacksmp, Ncomp_c, Nfft_c, C_FFTW,               &
     &          Nfft_r, X_FFTW)
!
      integer(kind = kint), intent(in) :: Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: Ncomp_c, Nfft_c, Nfft_r
      integer(kind = fftw_plan), intent(in) :: plan_backward_smp(Nsmp)
      complex(kind = fftw_complex), intent(in)                          &
     &                                  :: C_FFTW(Nfft_c,Ncomp_c)
!
      real(kind = kreal), intent(inout) :: X_FFTW(Nfft_r,Ncomp_c)
!
      integer(kind = kint) :: ip, ist, num
!
!
!$omp do private(ip,ist,num)
      do ip = 1, Nsmp
        num = Nstacksmp(ip  ) - Nstacksmp(ip-1)
        if(num .le. 0) cycle
!
        ist = Nstacksmp(ip-1) - Nstacksmp(0)
        call dfftw_execute_dft_c2r(plan_backward_smp(ip),               &
     &                             C_FFTW(1,ist+1), X_FFTW(1,ist+1))
      end do
!$omp end do nowait
!
      end subroutine multi_pout_bwd_FFTW3_smp
!
! ------------------------------------------------------------------
!
      end module multi_pout_FFTW3_smp
