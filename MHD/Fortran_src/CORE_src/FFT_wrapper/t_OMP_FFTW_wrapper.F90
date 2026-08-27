!>@file   t_OMP_FFTW_wrapper.f90
!!@brief  module t_OMP_FFTW_wrapper
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!   wrapper subroutine for initierize FFTW plans
!! ------------------------------------------------------------------
!!      subroutine init_OMP_FFTW_type(Ncomp, Nfft, WK)
!!        integer(kind = kint), intent(in) ::  Ncomp
!!        integer(kind = kint), intent(in) ::  Nfft
!!        type(working_mul_FFTW), intent(inout) :: WK
!!
!! ------------------------------------------------------------------
!!   wrapper subroutine for clear FFTW plans
!!        CAUTION!!  dfftw_destroy_plan oftern makes SEGMENTAION FAULT!!
!! ------------------------------------------------------------------
!!      subroutine finalize_OMP_FFTW_type(WK)
!!        type(working_mul_FFTW), intent(inout) :: WK
!!
!! ------------------------------------------------------------------
!!   wrapper subroutine for verify FFT by FFTW
!! ------------------------------------------------------------------
!!      subroutine verify_wk_OMP_FFTW_type(Ncomp, Nfft, WK)
!!
!! ------------------------------------------------------------------
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
!!@n @param WK          Work structure for FFTW3
!
      module t_OMP_FFTW_wrapper
!
      use m_precision
      use m_constants
      use m_fftw_parameters
!
      use t_multi_FFTW_wrapper
!
      implicit none
!
      integer, parameter, private :: IONE_4 = 1
      integer, parameter, private :: inembed = 0
!
      private :: init_OMP_FFTW, destroy_OMP_FFTW
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_OMP_FFTW_type(Ncomp, Nfft, WK)
!
      integer(kind = kint), intent(in) ::  Ncomp
      integer(kind = kint), intent(in) ::  Nfft
!
      type(working_mul_FFTW), intent(inout) :: WK
!
!
      call alloc_OMP_FFTW_plan_t(Ncomp, Nfft, WK)
      call init_OMP_FFTW(Ncomp, Nfft, WK%Nfft_c,                        &
     &    WK%plan_mul_fwd(1), WK%plan_mul_bwd(1),                       &
     &    WK%X_FFTW_mul(1,1), WK%C_FFTW_mul(1,1))
!
!
      end subroutine init_OMP_FFTW_type
!
! ------------------------------------------------------------------
!
      subroutine finalize_OMP_FFTW_type(WK)
!
      type(working_mul_FFTW), intent(inout) :: WK
!
!
      call destroy_OMP_FFTW(WK%plan_mul_fwd(1), WK%plan_mul_bwd(1))
      call dealloc_mul_FFTW_plan_t(WK)
!
      end subroutine finalize_OMP_FFTW_type
!
! ------------------------------------------------------------------
!
      subroutine verify_wk_OMP_FFTW_type(Ncomp, Nfft, WK)
!
      integer(kind = kint), intent(in) :: Ncomp, Nfft
!
      type(working_mul_FFTW), intent(inout) :: WK
!
!
      if(WK%iflag_fft_mul_len .lt. 0) then
        call init_OMP_FFTW_type(Ncomp, Nfft, WK)
        return
      end if
!
      if( WK%iflag_fft_mul_len .ne. Nfft*Ncomp) then
        call finalize_OMP_FFTW_type(WK)
        call init_OMP_FFTW_type(Ncomp, Nfft, WK)
      end if
!
      end subroutine verify_wk_OMP_FFTW_type
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine init_OMP_FFTW(Ncomp, Nfft, Nfft_c,                     &
     &          plan_forward, plan_backward, X_FFTW, C_FFTW)
!
      use m_OMP_FFTW3_counter
!
      integer(kind = kint), intent(in) :: Nfft, Nfft_c, Ncomp
!
      integer(kind = fftw_plan), intent(inout) :: plan_forward
      integer(kind = fftw_plan), intent(inout) :: plan_backward
      real(kind = kreal), intent(inout) :: X_FFTW(Ncomp,Nfft)
      complex(kind = fftw_complex), intent(inout)                       &
     &                                  :: C_FFTW(Ncomp,Nfft_c)
!
      integer(kind = 4) :: Nfft4, howmany
!
!
      Nfft4 = int(Nfft,KIND(Nfft4))
      howmany = int(Ncomp, KIND(howmany))
!
      call check_init_OMP_FFTW()
!
      call dfftw_plan_many_dft_r2c                                      &
     &   (plan_forward, IONE_4, Nfft4, howmany,                         &
     &    X_FFTW(1,1), inembed, howmany, IONE_4,                        &
     &    C_FFTW(1,1), inembed, howmany, IONE_4, FFTW_KEMO_EST)
      call dfftw_plan_many_dft_c2r                                      &
     &   (plan_backward, IONE_4, Nfft4, howmany,                        &
     &    C_FFTW(1,1), inembed, howmany, IONE_4,                        &
     &    X_FFTW(1,1), inembed, howmany, IONE_4, FFTW_KEMO_EST)
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
      call check_clean_OMP_FFTW()
!
      end subroutine destroy_OMP_FFTW
!
! ------------------------------------------------------------------
!
      end module t_OMP_FFTW_wrapper
