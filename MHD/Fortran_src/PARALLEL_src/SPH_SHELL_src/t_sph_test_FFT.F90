!>@file   t_sph_test_FFT.F90
!!@brief  module t_sph_test_FFT
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!!@n    Modified on Oct., 2014
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine init_sph_test_FFT                                    &
!!     &         (nidx_rtp, ncomp_bwd, ncomp_fwd, FFT_t)
!!      subroutine finalize_sph_test_FFT(FFT_t)
!!      subroutine verify_sph_test_FFT                                  &
!!     &         (nidx_rtp, ncomp_bwd, ncomp_fwd, FFT_t)
!!
!!      subroutine alloc_test_ordering_FFT                              &
!!     &         (Nfft, ncomp_bwd, ncomp_fwd, FFT_t)
!!      subroutine dealloc_test_ordering_FFT(FFT_t)
!!
!!   wrapper subroutine for initierize FFT by FFTW
!! ------------------------------------------------------------------
!!
!!      subroutine sph_test_fwd_FFT_to_send(nnod_rtp, nidx_rtp,      &
!!     &          irt_rtp_smp_stack, ncomp_fwd, n_WS, irev_sr_rtp,   &
!!     &          X_rtp, WS, FFT_t)
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
!!      subroutine sph_test_back_FFT_from_recv(nnod_rtp, nidx_rtp,      &
!!     &          irt_rtp_smp_stack, ncomp_bwd, n_WR, irev_sr_rtp, WR,  &
!!     &          X_rtp, FFT_t)
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
      module t_sph_test_FFT
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_fftw_parameters
      use m_elapsed_labels_SPH_TRNS
      use calypso_mpi
!
      implicit none
!
!>      Structure to use SNGLE FFTW
      type work_for_test_FFT
!>        plan ID for backward transform
        integer(kind = fftw_plan), allocatable :: plan_bwd(:)
!>        plan ID for forward transform
        integer(kind = fftw_plan), allocatable :: plan_fwd(:)
!
!>        number of backward FFT
        integer(kind = kint_4b) :: howmany_bwd
!>        number of forward FFT
        integer(kind = kint_4b) :: howmany_fwd
!>        length of FFT for real
        integer(kind = kint) :: Nfft_r
!>        length of FFT for complex
        integer(kind = kint) :: Nfft_c
!>        normalization parameter for FFTW (= 1 / Nfft)
        real(kind = kreal) :: aNfft
!>        real data for multiple Fourier transform
        real(kind = kreal), allocatable :: X(:,:)
!>        spectrum data for multiple Fourier transform
        complex(kind = fftw_complex), allocatable :: C(:,:)
!
!>        temporal area for ordering
        real(kind = kreal), allocatable :: v_tmp(:)
!
!>        temporal area for time count
        real(kind = kreal), allocatable :: t_omp(:,:)
      end type work_for_test_FFT
!
      private :: alloc_FFTW_plan
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_test_FFT                                      &
     &         (nidx_rtp, ncomp_bwd, ncomp_fwd, FFT_t)
!
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      type(work_for_test_FFT), intent(inout) :: FFT_t
!
      integer(kind = kint) :: j
      integer(kind = 4) :: Nfft4
!
!
      call alloc_FFTW_plan(np_smp, nidx_rtp(3),                         &
     &                     ncomp_bwd, ncomp_fwd, FFT_t)
!
      Nfft4 = int(nidx_rtp(3))
      do j = 1, np_smp
        call dfftw_plan_dft_r2c_1d(FFT_t%plan_fwd(j), Nfft4,            &
     &      FFT_t%X(1,j), FFT_t%C(1,j) , FFTW_KEMO_EST)
        call dfftw_plan_dft_c2r_1d(FFT_t%plan_bwd(j), Nfft4,            &
     &      FFT_t%C(1,j), FFT_t%X(1,j) , FFTW_KEMO_EST)
      end do
      FFT_t%aNfft = one / dble(nidx_rtp(3))
!
      allocate(FFT_t%t_omp(np_smp,0:3))
      FFT_t%t_omp = 0.0d0
!
      end subroutine init_sph_test_FFT
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_test_FFT(FFT_t)
!
      type(work_for_test_FFT), intent(inout) :: FFT_t
!
      integer(kind = kint) :: j
!
!
      do j = 1, np_smp
        call dfftw_destroy_plan(FFT_t%plan_fwd(j))
        call dfftw_destroy_plan(FFT_t%plan_bwd(j))
        call dfftw_cleanup
      end do
!
      call dealloc_FFTW_plan(FFT_t)
      deallocate(FFT_t%t_omp)
!
      end subroutine finalize_sph_test_FFT
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_test_FFT                                    &
     &         (nidx_rtp, ncomp_bwd, ncomp_fwd, FFT_t)
!
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      type(work_for_test_FFT), intent(inout) :: FFT_t
!
      integer(kind = kint) :: Ncomp
!
!
      if(allocated(FFT_t%X) .eqv. .false.) then
        call init_sph_test_FFT(nidx_rtp, ncomp_bwd, ncomp_fwd, FFT_t)
        return
      end if
!
      Ncomp = max(ncomp_bwd, ncomp_fwd)
      if(size(FFT_t%X,1) .ne. Ncomp*nidx_rtp(3)) then
        call finalize_sph_test_FFT(FFT_t)
        call init_sph_test_FFT(nidx_rtp, ncomp_bwd, ncomp_fwd, FFT_t)
      end if
!
      end subroutine verify_sph_test_FFT
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sph_test_fwd_FFT_to_send(nnod_rtp, nidx_rtp,           &
     &          irt_rtp_smp_stack, ncomp_fwd, n_WS, irev_sr_rtp,        &
     &          X_rtp, WS, FFT_t)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_fwd
      real(kind = kreal), intent(in)                                    &
     &     :: X_rtp(irt_rtp_smp_stack(np_smp),nidx_rtp(3),ncomp_fwd)
!
      integer(kind = kint), intent(in) :: n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(work_for_test_FFT), intent(inout) :: FFT_t
!
      integer(kind = kint) ::  m, j, ip, ist, ied, nd
      integer(kind = kint) :: ic_rtp, is_rtp, ic_send, is_send
!
!
      if(iflag_FFT_time) then
!$omp parallel workshare
        FFT_t%t_omp(1:np_smp,0:3) = 0
!$omp end parallel workshare
      end if
!
!!$omp parallel do schedule(static)                                      &
!!$omp&         private(nd,m,j,ip,ist,ied,ic_rtp,is_rtp,ic_send,is_send)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1) + 1
        ied = irt_rtp_smp_stack(ip) 
        do nd = 1, ncomp_fwd
!
          do j = ist, ied
!            if(iflag_FFT_time) FFT_t%t_omp(ip,0) = MPI_WTIME()
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+4)
!$omp parallel do
            do m = 1, nidx_rtp(3)
              FFT_t%X(m,ip) = X_rtp(j,m,nd)
            end do
!$omp end parallel do
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+4)
!            if(iflag_FFT_time) FFT_t%t_omp(ip,1)= FFT_t%t_omp(ip,1)   &
!     &                       + MPI_WTIME() - FFT_t%t_omp(ip,0)
!
!            if(iflag_FFT_time) FFT_t%t_omp(ip,0) = MPI_WTIME()
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+5)
          call dfftw_execute_dft_r2c(FFT_t%plan_fwd(ip),               &
     &        FFT_t%X(1,ip), FFT_t%C(1,ip))
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+5)
!            if(iflag_FFT_time) FFT_t%t_omp(ip,2)= FFT_t%t_omp(ip,2)   &
!     &                       + MPI_WTIME() - FFT_t%t_omp(ip,0)
!
!   normalization
!            if(iflag_FFT_time) FFT_t%t_omp(ip,0) = MPI_WTIME()
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+6)
            ic_send = nd + (irev_sr_rtp(j) - 1) * ncomp_fwd
            WS(ic_send) = FFT_t%aNfft * real(FFT_t%C(1,ip))
            do m = 2, (nidx_rtp(3)+1)/2
              ic_rtp = j + (2*m-2) * irt_rtp_smp_stack(np_smp)
              is_rtp = j + (2*m-1) * irt_rtp_smp_stack(np_smp)
              ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
              is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
              WS(ic_send) = two*FFT_t%aNfft * real(FFT_t%C(m,ip))
              WS(is_send) = two*FFT_t%aNfft * real(FFT_t%C(m,ip)*iu)
            end do 
            m = (nidx_rtp(3)+1)/2 + 1
            ic_rtp = j + irt_rtp_smp_stack(np_smp)
            ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
            WS(ic_send) = two*FFT_t%aNfft * real(FFT_t%C(m,ip))
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+6)
!
!            if(iflag_FFT_time) FFT_t%t_omp(ip,3)= FFT_t%t_omp(ip,3)   &
!     &                       + MPI_WTIME() - FFT_t%t_omp(ip,0)
          end do
        end do
      end do
!!$omp end parallel do
!
!      if(iflag_FFT_time) then
!        do ip = 2, np_smp
!          FFT_t%t_omp(1,1) = FFT_t%t_omp(1,1) + FFT_t%t_omp(ip,1)
!          FFT_t%t_omp(1,2) = FFT_t%t_omp(1,2) + FFT_t%t_omp(ip,2)
!          FFT_t%t_omp(1,3) = FFT_t%t_omp(1,3) + FFT_t%t_omp(ip,3)
!        end do
!        elps1%elapsed(ist_elapsed_FFT+4)                               &
!     &        = elps1%elapsed(ist_elapsed_FFT+4)                       &
!     &         + FFT_t%t_omp(1,1) / dble(np_smp)
!        elps1%elapsed(ist_elapsed_FFT+5)                               &
!     &        = elps1%elapsed(ist_elapsed_FFT+5)                       &
!     &         + FFT_t%t_omp(1,2) / dble(np_smp)
!        elps1%elapsed(ist_elapsed_FFT+6)                               &
!     &        = elps1%elapsed(ist_elapsed_FFT+6)                       &
!     &         + FFT_t%t_omp(1,3) / dble(np_smp)
!      end if
!
      end subroutine sph_test_fwd_FFT_to_send
!
! ------------------------------------------------------------------
!
      subroutine sph_test_back_FFT_from_recv(nnod_rtp, nidx_rtp,        &
     &          irt_rtp_smp_stack, ncomp_bwd, n_WR, irev_sr_rtp, WR,    &
     &          X_rtp, FFT_t)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &     :: X_rtp(irt_rtp_smp_stack(np_smp),nidx_rtp(3),ncomp_bwd)
      type(work_for_test_FFT), intent(inout) :: FFT_t
!
      integer(kind = kint) :: m, j, ip, ist, ied, nd
      integer(kind = kint) :: ic_rtp, is_rtp, ic_recv, is_recv
!
!
!      if(iflag_FFT_time) then
!!$omp parallel workshare
!        FFT_t%t_omp(1:np_smp,0:3) = 0
!!$omp end parallel workshare
!      end if
!
!!$omp parallel do schedule(static)                                      &
!!$omp&         private(nd,m,j,ip,ist,ied,ic_rtp,is_rtp,ic_recv,is_recv)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1) + 1
        ied = irt_rtp_smp_stack(ip)
        do nd = 1, ncomp_bwd
!
          do j = ist, ied
!
!            if(iflag_FFT_time) FFT_t%t_omp(ip,0) = MPI_WTIME()
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+1)
            ic_recv = nd + (irev_sr_rtp(j) - 1) * ncomp_bwd
            FFT_t%C(1,ip) = cmplx(WR(ic_recv), zero, kind(0d0))
            do m = 2, (nidx_rtp(3)+1)/2
              ic_rtp = j + (2*m-2) * irt_rtp_smp_stack(np_smp)
              is_rtp = j + (2*m-1) * irt_rtp_smp_stack(np_smp)
              ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
              is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
              FFT_t%C(m,ip)                                            &
     &            = half * cmplx(WR(ic_recv), -WR(is_recv),kind(0d0))
            end do
            m = (nidx_rtp(3)+1)/2 + 1
            ic_rtp = j + irt_rtp_smp_stack(np_smp)
            ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
            FFT_t%C(m,ip)                                              &
     &              = half * cmplx(WR(ic_recv), zero, kind(0d0))
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+1)
!            if(iflag_FFT_time) FFT_t%t_omp(ip,1)= FFT_t%t_omp(ip,1)   &
!     &                       + MPI_WTIME() - FFT_t%t_omp(ip,0)
!
!            if(iflag_FFT_time) FFT_t%t_omp(ip,0) = MPI_WTIME()
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+2)
            call dfftw_execute_dft_c2r(FFT_t%plan_bwd(ip),              &
     &          FFT_t%C(1,ip), FFT_t%X(1,ip))
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+2)
!            if(iflag_FFT_time) FFT_t%t_omp(ip,2)= FFT_t%t_omp(ip,2)  &
!     &                       + MPI_WTIME() - FFT_t%t_omp(ip,0)
!
!            if(iflag_FFT_time) FFT_t%t_omp(ip,0) = MPI_WTIME()
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+3)
!$omp parallel do
            do m = 1, nidx_rtp(3)
              X_rtp(j,m,nd) = FFT_t%X(m,ip)
            end do
!$omp end parallel do
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+3)
!            if(iflag_FFT_time) FFT_t%t_omp(ip,3)= FFT_t%t_omp(ip,3)  &
!     &                       + MPI_WTIME() - FFT_t%t_omp(ip,0)
          end do
        end do
      end do
!!$omp end parallel do
!
!      if(iflag_FFT_time) then
!        do ip = 2, np_smp
!          FFT_t%t_omp(1,1) = FFT_t%t_omp(1,1) + FFT_t%t_omp(ip,1)
!          FFT_t%t_omp(1,2) = FFT_t%t_omp(1,2) + FFT_t%t_omp(ip,2)
!          FFT_t%t_omp(1,3) = FFT_t%t_omp(1,3) + FFT_t%t_omp(ip,3)
!        end do
!        elps1%elapsed(ist_elapsed_FFT+1)                               &
!     &        = elps1%elapsed(ist_elapsed_FFT+1)                       &
!     &         + FFT_t%t_omp(1,1) / dble(np_smp)
!        elps1%elapsed(ist_elapsed_FFT+2)                               &
!     &        = elps1%elapsed(ist_elapsed_FFT+2)                       &
!     &         + FFT_t%t_omp(1,2) / dble(np_smp)
!        elps1%elapsed(ist_elapsed_FFT+3)                               &
!     &        = elps1%elapsed(ist_elapsed_FFT+3)                       &
!     &         + FFT_t%t_omp(1,3) / dble(np_smp)
!      end if
!
      end subroutine sph_test_back_FFT_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_FFTW_plan                                        &
     &         (Nsmp, Nfft, ncomp_bwd, ncomp_fwd, FFT_t)
!
      integer(kind = kint), intent(in) :: Nsmp, Nfft
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      type(work_for_test_FFT), intent(inout) :: FFT_t
!
      integer(kind = kint) :: Ncomp
!
!
      Ncomp = max(ncomp_bwd, ncomp_fwd)
      FFT_t%howmany_bwd = int(ncomp_bwd)
      FFT_t%howmany_fwd = int(ncomp_fwd)
      FFT_t%Nfft_r = Nfft
      FFT_t%Nfft_c = Nfft/2 + 1
!
      allocate(FFT_t%plan_fwd(Nsmp))
      allocate(FFT_t%plan_bwd(Nsmp))
!
      allocate(FFT_t%X(Ncomp*FFT_t%Nfft_r,Nsmp))
      allocate(FFT_t%C(Ncomp*FFT_t%Nfft_c,Nsmp))
      FFT_t%X = 0.0d0
      FFT_t%C = 0.0d0
!
      end subroutine alloc_FFTW_plan
!
! ------------------------------------------------------------------
!
      subroutine dealloc_FFTW_plan(FFT_t)
!
      type(work_for_test_FFT), intent(inout) :: FFT_t
!
!
      deallocate(FFT_t%plan_fwd, FFT_t%plan_bwd)
      deallocate(FFT_t%X, FFT_t%C)
!
      end subroutine dealloc_FFTW_plan
!
! ------------------------------------------------------------------
!
      subroutine alloc_test_ordering_FFT                                &
     &         (Nfft, ncomp_bwd, ncomp_fwd, FFT_t)
!
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      integer(kind = kint), intent(in) :: Nfft
      type(work_for_test_FFT), intent(inout) :: FFT_t
!
!
      allocate(FFT_t%v_tmp(Nfft))
      FFT_t%v_tmp = 0.0d0
!
      end subroutine alloc_test_ordering_FFT
!
! ------------------------------------------------------------------
!
      subroutine dealloc_test_ordering_FFT(FFT_t)
!
      type(work_for_test_FFT), intent(inout) :: FFT_t
!
!
      deallocate(FFT_t%v_tmp)
!
      end subroutine dealloc_test_ordering_FFT
!
! ------------------------------------------------------------------
!
      end module t_sph_test_FFT
