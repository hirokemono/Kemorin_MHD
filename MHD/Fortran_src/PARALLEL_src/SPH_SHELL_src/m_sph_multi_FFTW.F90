!>@file   m_sph_multi_FFTW.F90
!!@brief  module m_sph_multi_FFTW
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2013
!!@n    Modified on Oct., 2014
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine init_sph_multi_FFTW
!!      subroutine finalize_sph_multi_FFTW
!!      subroutine verify_sph_multi_FFTW
!!
!!   wrapper subroutine for initierize FFT by FFTW
!! ------------------------------------------------------------------
!!
!!      subroutine sph_multi_fwd_FFTW_to_send                           &
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
!!      subroutine sph_multi_back_FFTW_from_recv                        &
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
!!@n @param Nstacksmp(0:np_smp)   End number for each SMP process
!!@n @param Ncomp           Number of components for Fourier transforms
!!@n @param Nfft        Data length for eadh FFT
!!@n @param X(Ncomp, Nfft)  Data for Fourier transform
!
      module m_sph_multi_FFTW
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
!>      Unit imaginary number
      complex(kind = fftw_complex), parameter :: iu = (0.0d0,1.0d0)
!>      estimation flag for FFTW
      integer(kind = 4), parameter :: FFTW_ESTIMATE = 64
!
!
!>      plan ID for multi backward transform
      integer(kind = fftw_plan), allocatable :: plan_back_mul(:)
!>      plan ID for multi forward transform
      integer(kind = fftw_plan), allocatable :: plan_fowd_mul(:)
!
!
!>      real data for multiple Fourier transform
      real(kind = kreal), allocatable :: X_FFTW_mul(:,:)
!>      spectrum data for multiple Fourier transform
      complex(kind = fftw_complex), allocatable :: C_FFTW_mul(:,:)
!>      flag for number of components for Fourier transform
      integer(kind = kint) :: iflag_fft_mul_len =  -1
!>      normalization parameter for FFTW (= 1 / Nfft)
      real(kind = kreal) :: aNfft
!
!
      real(kind = kreal) :: elapsed_fftw(3) = (/0.0,0.0,0.0/)
!
      private :: iu
      private :: FFTW_ESTIMATE
      private :: iflag_fft_mul_len
      private :: plan_back_mul, plan_fowd_mul
      private :: X_FFTW_mul, C_FFTW_mul, aNfft
!
      private :: allocate_mul_FFTW_plan, deallocate_mul_FFTW_plan
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_multi_FFTW
!
      use m_spheric_parameter
      use m_spheric_param_smp
!
      integer(kind = kint) :: ip, ist, howmany, inembed, istride
      integer(kind = kint) :: idist_r, idist_c
!
!
      call allocate_mul_FFTW_plan                                       &
     &   (irt_rtp_smp_stack(np_smp), nidx_rtp(3))
!
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1) + 1
        howmany = irt_rtp_smp_stack(ip  ) - irt_rtp_smp_stack(ip-1)
        inembed = 0
        istride = 1
        idist_r = nidx_rtp(3)
        idist_c = nidx_rtp(3)/2+1
!
#ifdef FFTW3_C
        call kemo_fftw_plan_many_dft_r2c                                &
     &     (plan_fowd_mul(ip), ione, nidx_rtp(3), howmany,              &
     &      X_FFTW_mul(1,ist), inembed, istride, idist_r,               &
     &      C_FFTW_mul(1,ist), inembed, istride, idist_c,               &
     &      FFTW_ESTIMATE)
        call kemo_fftw_plan_many_dft_c2r                                &
     &     (plan_back_mul(ip), ione, nidx_rtp(3), howmany,              &
     &      C_FFTW_mul(1,ist), inembed, istride, idist_c,               &
     &      X_FFTW_mul(1,ist), inembed, istride, idist_r,               &
     &      FFTW_ESTIMATE)
#else
        call dfftw_plan_many_dft_r2c                                    &
     &     (plan_fowd_mul(ip), ione, nidx_rtp(3), howmany,              &
     &      X_FFTW_mul(1,ist), inembed, istride, idist_r,               &
     &      C_FFTW_mul(1,ist), inembed, istride, idist_c,               &
     &      FFTW_ESTIMATE)
        call dfftw_plan_many_dft_c2r                                    &
     &     (plan_back_mul(ip), ione, nidx_rtp(3), howmany,              &
     &      C_FFTW_mul(1,ist), inembed, istride, idist_c,               &
     &      X_FFTW_mul(1,ist), inembed, istride, idist_r,               &
     &      FFTW_ESTIMATE)
#endif
      end do
      aNfft = one / dble(nidx_rtp(3))
!
      end subroutine init_sph_multi_FFTW
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_multi_FFTW
!
      integer(kind = kint) :: j
!
!
#ifdef FFTW3_C
      do j = 1, np_smp
        call kemo_fftw_destroy_plan(plan_fowd_mul(j))
        call kemo_fftw_destroy_plan(plan_back_mul(j))
        call kemo_fftw_cleanup
      end do
#else
      do j = 1, np_smp
        call dfftw_destroy_plan(plan_fowd_mul(j))
        call dfftw_destroy_plan(plan_back_mul(j))
        call dfftw_cleanup
      end do
#endif
!
      call deallocate_mul_FFTW_plan
!
      end subroutine finalize_sph_multi_FFTW
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_multi_FFTW
!
      use m_spheric_parameter
!
!
      if( iflag_fft_mul_len .lt. 0) then
        call init_sph_multi_FFTW
        return
      end if
!
      if( iflag_fft_mul_len .ne. nnod_rtp) then
        call finalize_sph_multi_FFTW
        call init_sph_multi_FFTW
      end if
!
      end subroutine verify_sph_multi_FFTW
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sph_multi_fwd_FFTW_to_send                             &
     &         (ncomp, n_WS, irev_sr_rtp, X_rtp, WS)
!
      use m_spheric_parameter
      use m_spheric_param_smp
!
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in)                                    &
     &         :: X_rtp(irt_rtp_smp_stack(np_smp),nidx_rtp(3),ncomp)
!
      integer(kind = kint), intent(in) :: n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) ::  m, j, ip, ist, ied, nd
      integer(kind = kint) :: ic_rtp, is_rtp, ic_send, is_send
      real :: dummy(3), rtmp(3)
!
!
!      call cpu_time(dummy(1))
!$omp parallel do schedule(static)                                      &
!$omp&         private(nd,m,j,ip,ist,ied,ic_rtp,is_rtp,ic_send,is_send)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1) + 1
        ied = irt_rtp_smp_stack(ip)
        do nd = 1, ncomp
          do j = ist, ied
            X_FFTW_mul(1:nidx_rtp(3),j) = X_rtp(j,1:nidx_rtp(3),nd)
          end do
!        call cpu_time(rtmp(1))
!
!        call cpu_time(dummy(2))
#ifdef FFTW3_C
          call kemo_fftw_execute_dft_r2c(plan_fowd_mul(ip),             &
     &        X_FFTW_mul(1,ist), C_FFTW_mul(1,ist))
#else
          call dfftw_execute_dft_r2c(plan_fowd_mul(ip),                 &
     &        X_FFTW_mul(1,ist), C_FFTW_mul(1,ist))
#endif
!
!      call cpu_time(rtmp(2))
!   normalization
!      call cpu_time(dummy(3))
          do j = ist, ied
            ic_send = nd + (irev_sr_rtp(j) - 1) * ncomp
            WS(ic_send) = aNfft * real(C_FFTW_mul(1,j))
            do m = 2, (nidx_rtp(3)+1)/2
              ic_rtp = j + (2*m-2) * irt_rtp_smp_stack(np_smp)
              is_rtp = j + (2*m-1) * irt_rtp_smp_stack(np_smp)
              ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp
              is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp
              WS(ic_send) = two * aNfft * real(C_FFTW_mul(m,j))
              WS(is_send) = two * aNfft * real(C_FFTW_mul(m,j)*iu)
            end do 
            m = (nidx_rtp(3)+1)/2 + 1
            ic_rtp = j + irt_rtp_smp_stack(np_smp)
            ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp
            WS(ic_send) = two * aNfft * real(C_FFTW_mul(m,j))
          end do
!      call cpu_time(rtmp(3))
        end do
      end do
!$omp end parallel do
!      elapsed_fftw(1:3) = elapsed_fftw(1:3) + rtmp(1:3) - dummy(1:3)
!
      end subroutine sph_multi_fwd_FFTW_to_send
!
! ------------------------------------------------------------------
!
      subroutine sph_multi_back_FFTW_from_recv                          &
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
     &          :: X_rtp(irt_rtp_smp_stack(np_smp),nidx_rtp(3),ncomp)
!
      integer(kind = kint) :: m, j, ip, ist, ied, nd
      integer(kind = kint) :: ic_rtp, is_rtp, ic_recv, is_recv
      real :: dummy(3), rtmp(3)
!
!
!      call cpu_time(dummy(3))
!$omp parallel do schedule(static)                                      &
!$omp&         private(nd,m,j,ip,ist,ied,ic_rtp,is_rtp,ic_recv,is_recv)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1) + 1
        ied = irt_rtp_smp_stack(ip)
        do nd = 1, ncomp
!   normalization
          do j = ist, ied
            ic_recv = nd + (irev_sr_rtp(j) - 1) * ncomp
            C_FFTW_mul(1,j) = cmplx(WR(ic_recv), zero, kind(0d0))
            do m = 2, (nidx_rtp(3)+1)/2
              ic_rtp = j + (2*m-2) * irt_rtp_smp_stack(np_smp)
              is_rtp = j + (2*m-1) * irt_rtp_smp_stack(np_smp)
              ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp
              is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp
              C_FFTW_mul(m,j)                                           &
     &            = half * cmplx(WR(ic_recv), -WR(is_recv),kind(0d0))
            end do
            m = (nidx_rtp(3)+1)/2 + 1
            ic_rtp = j + irt_rtp_smp_stack(np_smp)
            ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp
            C_FFTW_mul(m,j) = half * cmplx(WR(ic_recv), zero, kind(0d0))
          end do
!        call cpu_time(rtmp(3))
!
!        call cpu_time(dummy(2))
#ifdef FFTW3_C
          call kemo_fftw_execute_dft_c2r(plan_back_mul(ip),             &
     &        C_FFTW_mul(1,ist), X_FFTW_mul(1,ist))
#else
          call dfftw_execute_dft_c2r(plan_back_mul(ip),                 &
     &        C_FFTW_mul(1,ist), X_FFTW_mul(1,ist))
#endif
!        call cpu_time(rtmp(2))
!
!        call cpu_time(dummy(1))
          do j = ist, ied
            X_rtp(j,1:nidx_rtp(3),nd) = X_FFTW_mul(1:nidx_rtp(3),j)
          end do
        end do
!      call cpu_time(rtmp(1))
      end do
!$omp end parallel do
!
!      elapsed_fftw(1:3) = elapsed_fftw(1:3) + rtmp(1:3) - dummy(1:3)
!
      end subroutine sph_multi_back_FFTW_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine allocate_mul_FFTW_plan(nnod_rt, Nfft)
!
      integer(kind = kint), intent(in) :: nnod_rt, Nfft
!
!
      allocate(plan_back_mul(np_smp))
      allocate(plan_fowd_mul(np_smp))
!
      iflag_fft_mul_len = nnod_rt*Nfft
      allocate(X_FFTW_mul(Nfft,nnod_rt))
      allocate(C_FFTW_mul(Nfft/2+1,nnod_rt))
      X_FFTW_mul = 0.0d0
      C_FFTW_mul = 0.0d0
!
      end subroutine allocate_mul_FFTW_plan
!
! ------------------------------------------------------------------
!
      subroutine deallocate_mul_FFTW_plan
!
!
      deallocate(plan_back_mul, plan_fowd_mul)
      deallocate(X_FFTW_mul, C_FFTW_mul)
      iflag_fft_mul_len = 0
!
      end subroutine deallocate_mul_FFTW_plan
!
! ------------------------------------------------------------------
!
      end module m_sph_multi_FFTW
