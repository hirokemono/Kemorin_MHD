!>@file   t_FFTW_wrapper.f90
!!@brief  module t_FFTW_wrapper
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine init_FFTW_type(Nsmp, Nstacksmp, Nfft, WK)
!!      subroutine verify_wk_FFTW_type(Nsmp, Nstacksmp, Nfft, WK)
!!
!!   wrapper subroutine for initierize FFT by FFTW
!! ------------------------------------------------------------------
!!
!!      subroutine FFTW_forward_type(Nsmp, Nstacksmp, M, Nfft, X, WK)
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for forward Fourier transform by FFTW3
!!
!!   a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\grac{2\pijk}{Nfft})
!!   b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\grac{2\pijk}{Nfft})
!!
!!   a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!!    K = Nfft/2....
!!   a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\grac{2\pijk}{Nfft})
!!
!! ------------------------------------------------------------------
!!
!!      subroutine FFTW_backward_type(Nsmp, Nstacksmp, M, Nfft, X, WK)
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
!!@n @param M           Number of components for Fourier transforms
!!@n @param Nfft        Data length for eadh FFT
!!@n @param X(M, Nfft)  Data for Fourier transform
!!@n @param WK          Work structure for FFTW3
!
      module t_FFTW_wrapper
!
      use m_precision
      use m_constants
!
      implicit none
!
!>      plan ID for fftw
      integer, parameter :: fftw_plan =    8
!>      data size of complex for FFTW3
      integer, parameter :: fftw_complex = 8
!
!>      Unit imaginary number
      complex(kind = fftw_complex), parameter :: iu = (0.0d0,1.0d0)
!
!>      estimation flag for FFTW
      integer(kind = 4), parameter :: FFTW_ESTIMATE = 64
!
!>      structure for working data for FFTW
      type working_FFTW
!>        plan ID for backward transform
        integer(kind = fftw_plan), allocatable :: plan_backward(:)
!>        plan ID for forward transform
        integer(kind = fftw_plan), allocatable :: plan_forward(:)
!
!>        normalization parameter for FFTW
        real(kind = kreal) :: aNfft
!>        real data for multiple Fourier transform
        real(kind = kreal), allocatable :: X_FFTW(:,:)
!>        spectrum data for multiple Fourier transform
        complex(kind = fftw_complex), allocatable :: C_FFTW(:,:)
!>        flag for number of components for Fourier transform
        integer(kind = kint) :: iflag_fft_len =  -1
      end type working_FFTW
!
      private :: fftw_plan, fftw_complex
      private :: iu
      private :: FFTW_ESTIMATE
!
      private :: alloc_work_4_FFTW_t, dealloc_work_4_FFTW_t
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_FFTW_type(Nsmp, Nstacksmp, Nfft, WK)
!
      integer(kind = kint), intent(in) ::  Nfft
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
      type(working_FFTW), intent(inout) :: WK
!
      integer(kind = kint) :: M, ip
!
!
      M = Nstacksmp(1)
      do ip = 1, Nsmp
        M = max(M, (Nstacksmp(ip) - Nstacksmp(ip-1)) )
      end do
!
      call alloc_work_4_FFTW_t(Nsmp, Nfft, WK)
!
      do ip = 1, Nsmp
        call dfftw_plan_dft_r2c_1d(WK%plan_forward(ip), Nfft,           &
     &      WK%X_FFTW(1:Nfft,ip), WK%C_FFTW(1:Nfft/2+1,ip),             &
     &      FFTW_ESTIMATE)
        call dfftw_plan_dft_c2r_1d(WK%plan_backward(ip), Nfft,          &
     &      WK%C_FFTW(1:Nfft/2+1,ip), WK%X_FFTW(1:Nfft,ip),             &
     &      FFTW_ESTIMATE)
      end do
!
      end subroutine init_FFTW_type
!
! ------------------------------------------------------------------
!
      subroutine verify_wk_FFTW_type(Nsmp, Nstacksmp, Nfft, WK)
!
      integer(kind = kint), intent(in) ::  Nfft
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
!
      type(working_FFTW), intent(inout) :: WK
!
      integer(kind = kint) :: M, ip
!
!
      if(WK%iflag_fft_len .lt. 0) then
        call init_FFTW_type(Nsmp, Nstacksmp, Nfft, WK)
        return
      end if
!
      M = Nstacksmp(1)
      do ip = 1, Nsmp
        M = max(M, (Nstacksmp(ip) - Nstacksmp(ip-1)) )
      end do
!
      if( WK%iflag_fft_len .ne. Nfft) then
        do ip = 1, Nsmp
          call dfftw_destroy_plan(WK%plan_forward(ip))
          call dfftw_destroy_plan(WK%plan_backward(ip))
        end do
        call dealloc_work_4_FFTW_t(WK)
!
        call alloc_work_4_FFTW_t(Nsmp, Nfft, WK)
!
        do ip = 1, Nsmp
          call dfftw_plan_dft_r2c_1d(WK%plan_forward(ip), Nfft,         &
     &        WK%X_FFTW(1:Nfft,ip), WK%C_FFTW(1:Nfft/2+1,ip),           &
     &        FFTW_ESTIMATE)
          call dfftw_plan_dft_c2r_1d(WK%plan_backward(ip), Nfft,        &
     &        WK%C_FFTW(1:Nfft/2+1,ip), WK%X_FFTW(1:Nfft,ip),           &
     &        FFTW_ESTIMATE)
        end do
      end if
!
      end subroutine verify_wk_FFTW_type
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine FFTW_forward_type(Nsmp, Nstacksmp, M, Nfft, X, WK)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(M, Nfft)
      type(working_FFTW), intent(inout) :: WK
!
      integer(kind = kint) ::  i, j, ismp, ist, num, inum
!
!
!   normalization
!
!$omp parallel do private(i,j,ist,num,inum)
      do ismp = 1, Nsmp
        ist = Nstacksmp(ismp-1)
        num = Nstacksmp(ismp) - Nstacksmp(ismp-1)
        do inum = 1, num
          j = ist + inum
!
          do i = 1, Nfft
            WK%X_FFTW(i,ismp) = X(j,i)
          end do
!
          call dfftw_execute_dft_r2c(WK%plan_forward,                   &
     &        WK%X_FFTW(1:Nfft,ismp), WK%C_FFTW(1:Nfft/2+1,ismp) )
!
          X(j,1) = WK%aNfft * real(WK%C_FFTW(1,ismp))
          do i = 2, (Nfft+1)/2
            X(j,2*i-1) = two * WK%aNfft * real(WK%C_FFTW(i,ismp))
            X(j,2*i  ) = two * WK%aNfft * real(WK%C_FFTW(i,ismp)*iu)
          end do
          i = (Nfft+1)/2 + 1
          X(j,2) = two * WK%aNfft * real(WK%C_FFTW(i,ismp))
        end do
!
      end do
!$omp end parallel do
!
      end subroutine FFTW_forward_type
!
! ------------------------------------------------------------------
!
      subroutine FFTW_backward_type(Nsmp, Nstacksmp, M, Nfft, X, WK)
!
      integer(kind = kint), intent(in) ::  Nsmp, Nstacksmp(0:Nsmp)
      integer(kind = kint), intent(in) :: M, Nfft
!
      real(kind = kreal), intent(inout) :: X(M,Nfft)
      type(working_FFTW), intent(inout) :: WK
!
      integer(kind = kint) ::  i, j, ismp, ist, inum, num
!
!
!   normalization
!
!$omp parallel do private(i,j,ist,num,inum)
      do ismp = 1, Nsmp
        ist = Nstacksmp(ismp-1)
        num = Nstacksmp(ismp) - Nstacksmp(ismp-1)
        do inum = 1, num
          j = ist + inum
!
          WK%C_FFTW(1,ismp) = cmplx(X(j,1), zero, kind(0d0))
          do i = 2, (Nfft+1)/2
            WK%C_FFTW(i,ismp) = half                                    &
     &                     * cmplx(X(j,2*i-1), -X(j,2*i  ), kind(0d0))
          end do
          i = (Nfft+1)/2 + 1
          WK%C_FFTW(i,ismp) = half * cmplx(X(j,2), zero, kind(0d0))
!
          call dfftw_execute_dft_c2r(WK%plan_backward,                  &
     &        WK%C_FFTW(1:Nfft/2+1,ismp),  WK%X_FFTW(1:Nfft,ismp) )
!
          do i = 1, Nfft
            X(j,i) = WK%X_FFTW(i,ismp)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine FFTW_backward_type
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_work_4_FFTW_t(Nsmp, Nfft, WK)
!
      integer(kind = kint), intent(in) :: Nsmp, Nfft
      type(working_FFTW), intent(inout) :: WK
!
!
      allocate(WK%plan_forward(Nsmp))
      allocate(WK%plan_backward(Nsmp))
!
      WK%iflag_fft_len = Nfft
      allocate( WK%X_FFTW(Nfft,Nsmp) )
      allocate( WK%C_FFTW(Nfft/2+1,Nsmp) )
      WK%aNfft = one / dble(Nfft)
      WK%X_FFTW = 0.0d0
      WK%C_FFTW = 0.0d0
!
      end subroutine alloc_work_4_FFTW_t
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine dealloc_work_4_FFTW_t(WK)
!
      type(working_FFTW), intent(inout) :: WK
!
      deallocate(WK%X_FFTW, WK%C_FFTW)
      deallocate(WK%plan_forward, WK%plan_backward)
      WK%iflag_fft_len = 0
!
      end subroutine dealloc_work_4_FFTW_t
!
! ------------------------------------------------------------------
!
      end module t_FFTW_wrapper
