!>@file   m_sph_single_FFTW.F90
!!@brief  module m_sph_single_FFTW
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!!@n    Modified on Oct., 2014
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine init_sph_single_FFTW
!!      subroutine finalize_sph_single_FFTW
!!      subroutine verify_sph_single_FFTW
!!
!!   wrapper subroutine for initierize FFT by FFTW
!! ------------------------------------------------------------------
!!
!!      subroutine sph_single_fwd_FFTW_to_send                          &
!!     &         (ncomp, n_WS, irev_sr_rtp, X_rtp, WS)
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
!!      subroutine sph_single_back_FFTW_from_recv                       &
!!     &         (ncomp, n_WR, irev_sr_rtp, WR, X_rtp)
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
!
      module m_sph_single_FFTW
!
      use m_precision
      use m_constants
      use m_machine_parameter
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
!>      estimation flag for FFTW
      integer(kind = 4), parameter :: FFTW_ESTIMATE = 64
!
!>      plan ID for backward transform
      integer(kind = fftw_plan), allocatable :: plan_backward(:)
!>      plan ID for forward transform
      integer(kind = fftw_plan), allocatable :: plan_forward(:)
!
!>      normalization parameter for FFTW (= 1 / Nfft)
      real(kind = kreal) :: aNfft
!>      real data for multiple Fourier transform
      real(kind = kreal), allocatable :: X_FFTW(:,:)
!>      spectrum data for multiple Fourier transform
      complex(kind = fftw_complex), allocatable :: C_FFTW(:,:)
!>      flag for number of components for Fourier transform
      integer(kind = kint) :: iflag_fft_len =  -1
!
      private :: fftw_plan, fftw_complex, iu, FFTW_ESTIMATE
      private :: iflag_fft_len
      private :: plan_backward, plan_forward
      private :: X_FFTW, C_FFTW, aNfft
!
      private :: allocate_FFTW_plan, deallocate_FFTW_plan
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_single_FFTW
!
      use m_spheric_parameter
!
      integer(kind = kint) :: j
!
!
      call allocate_FFTW_plan(np_smp, nidx_rtp(3))
!
      do j = 1, np_smp
#ifdef FFTW3_C
        call kemo_fftw_plan_dft_r2c_1d(plan_forward(j), nidx_rtp(3),    &
     &      X_FFTW(1,j), C_FFTW(1,j) , FFTW_ESTIMATE)
        call kemo_fftw_plan_dft_c2r_1d(plan_backward(j), nidx_rtp(3),   &
     &      C_FFTW(1,j), X_FFTW(1,j) , FFTW_ESTIMATE)
#else
        call dfftw_plan_dft_r2c_1d(plan_forward(j), nidx_rtp(3),        &
     &      X_FFTW(1,j), C_FFTW(1,j) , FFTW_ESTIMATE)
        call dfftw_plan_dft_c2r_1d(plan_backward(j), nidx_rtp(3),       &
     &      C_FFTW(1,j), X_FFTW(1,j) , FFTW_ESTIMATE)
#endif
      end do
      aNfft = one / dble(nidx_rtp(3))
!
      end subroutine init_sph_single_FFTW
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_single_FFTW
!
      integer(kind = kint) :: j
!
!
#ifdef FFTW3_C
      do j = 1, np_smp
        call kemo_fftw_destroy_plan(plan_forward(j))
        call kemo_fftw_destroy_plan(plan_backward(j))
        call kemo_fftw_cleanup
      end do
#else
      do j = 1, np_smp
        call dfftw_destroy_plan(plan_forward(j))
        call dfftw_destroy_plan(plan_backward(j))
        call dfftw_cleanup
      end do
#endif
!
      call deallocate_FFTW_plan
!
      end subroutine finalize_sph_single_FFTW
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_single_FFTW
!
      use m_spheric_parameter
!
!
      if( iflag_fft_len .lt. 0) then
        call init_sph_single_FFTW
        return
      end if
!
      if( iflag_fft_len .ne. nidx_rtp(3)*np_smp) then
        call finalize_sph_single_FFTW
        call init_sph_single_FFTW
      end if
!
      end subroutine verify_sph_single_FFTW
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sph_single_fwd_FFTW_to_send                            &
     &         (ncomp, n_WS, irev_sr_rtp, X_rtp, WS)
!
      use m_spheric_parameter
      use m_spheric_param_smp
!
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(inout)                                 &
     &     :: X_rtp(irt_rtp_smp_stack(np_smp),nidx_rtp(3),ncomp)
!
      integer(kind = kint), intent(in) :: n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) ::  m, j, ip, ist, ied, nd
      integer(kind = kint) :: ic_rtp, is_rtp, ic_send, is_send
      real :: dummy(np_smp,3), rtmp(np_smp,3)
!
!
!!$omp parallel do schedule(static)                                     &
!!$omp&         private(nd,m,j,ip,ist,ied,ic_rtp,is_rtp,ic_send,is_send)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1) + 1
        ied = irt_rtp_smp_stack(ip) 
        do nd = 1, ncomp
!
!        call cpu_time(dummy(ip,1))
          do j = ist, ied
            X_FFTW(1:nidx_rtp(3),ip) = X_rtp(j,1:nidx_rtp(3),nd)
!
#ifdef FFTW3_C
            call kemo_fftw_execute(plan_forward(ip))
#else
            call dfftw_execute(plan_forward(ip))
#endif
!            call cpu_time(rtmp(ip,2))
!
!   normalization
!            call cpu_time(dummy(ip,3))
            ic_send = nd + (irev_sr_rtp(j) - 1) * ncomp
            WS(ic_send) = aNfft * real(C_FFTW(1,ip))
            do m = 2, (nidx_rtp(3)+1)/2
              ic_rtp = j + (2*m-2) * irt_rtp_smp_stack(np_smp)
              is_rtp = j + (2*m-1) * irt_rtp_smp_stack(np_smp)
              ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp
              is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp
              WS(ic_send) = two * aNfft * real(C_FFTW(m,ip))
              WS(is_send) = two * aNfft * real(C_FFTW(m,ip)*iu)
            end do 
            m = (nidx_rtp(3)+1)/2 + 1
            ic_rtp = j + irt_rtp_smp_stack(np_smp)
            ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp
            WS(ic_send) = two * aNfft * real(C_FFTW(m,ip))
!             call cpu_time(rtmp(ip,3))
          end do
        end do
      end do
!!$omp end parallel do
!
!      do ip = 1, np_smp
!        elapsed_fftw(1:3) = elapsed_fftw(1:3)                          &
!     &                     + rtmp(ip,1:3) - dummy(ip,1:3)
!      end do
!
      end subroutine sph_single_fwd_FFTW_to_send
!
! ------------------------------------------------------------------
!
      subroutine sph_single_back_FFTW_from_recv                         &
     &         (ncomp, n_WR, irev_sr_rtp, WR, X_rtp)
!
      use m_spheric_parameter
      use m_spheric_param_smp
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &     :: X_rtp(irt_rtp_smp_stack(np_smp),nidx_rtp(3),ncomp)
!
      integer(kind = kint) :: m, j, ip, ist, ied, nd
      integer(kind = kint) :: ic_rtp, is_rtp, ic_recv, is_recv
      real :: dummy(np_smp,3), rtmp(np_smp,3)
!
!
!$omp parallel do schedule(static)                                      &
!$omp&         private(nd,m,j,ip,ist,ied,ic_rtp,is_rtp,ic_recv,is_recv)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1) + 1
        ied = irt_rtp_smp_stack(ip)
        do nd = 1, ncomp
!
!   normalization
!        call cpu_time(dummy(ip,3))
!   normalization
          do j = ist, ied
            ic_recv = nd + (irev_sr_rtp(j) - 1) * ncomp
            C_FFTW(1,ip) = cmplx(WR(ic_recv), zero, kind(0d0))
            do m = 2, (nidx_rtp(3)+1)/2
              ic_rtp = j + (2*m-2) * irt_rtp_smp_stack(np_smp)
              is_rtp = j + (2*m-1) * irt_rtp_smp_stack(np_smp)
              ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp
              is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp
              C_FFTW(m,ip)                                              &
     &            = half * cmplx(WR(ic_recv), -WR(is_recv),kind(0d0))
            end do
            m = (nidx_rtp(3)+1)/2 + 1
            ic_rtp = j + irt_rtp_smp_stack(np_smp)
            ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp
            C_FFTW(m,ip) = half * cmplx(WR(ic_recv), zero, kind(0d0))
!          call cpu_time(rtmp(ip,3))
!
!          call cpu_time(dummy(ip,2))
#ifdef FFTW3_C
           call kemo_fftw_execute(plan_backward(ip))
#else
           call dfftw_execute(plan_backward(ip))
#endif
!        call cpu_time(rtmp(ip,2))
!
!        call cpu_time(dummy(ip,1))
            X_rtp(j,1:nidx_rtp(3),nd) = X_FFTW(1:nidx_rtp(3),ip)
!        call cpu_time(rtmp(ip,1))
          end do
        end do
      end do
!$omp end parallel do
!
!      do ip = 1, np_smp
!        elapsed_fftw(1:3) = elapsed_fftw(1:3)                          &
!     &                     + rtmp(ip,1:3) - dummy(ip,1:3)
!      end do
!
      end subroutine sph_single_back_FFTW_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine allocate_FFTW_plan(Ncomp, Nfft)
!
      integer(kind = kint), intent(in) :: Ncomp, Nfft
!
!
      allocate(plan_forward(Ncomp))
      allocate(plan_backward(Ncomp))
!
      iflag_fft_len = Nfft*Ncomp
      allocate( X_FFTW(Nfft,Ncomp) )
      allocate( C_FFTW(Nfft/2+1,Ncomp) )
      X_FFTW = 0.0d0
      C_FFTW = 0.0d0
!
      end subroutine allocate_FFTW_plan
!
! ------------------------------------------------------------------
!
      subroutine deallocate_FFTW_plan
!
!
      deallocate(plan_forward, plan_backward)
      deallocate(x_FFTW, C_FFTW)
      iflag_fft_len = 0
!
      end subroutine deallocate_FFTW_plan
!
! ------------------------------------------------------------------
!
      end module m_sph_single_FFTW
