!>@file   t_sph_field_OMP_FFTW.F90
!!@brief  module t_sph_field_OMP_FFTW
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2013
!!@n    Modified on Oct., 2014
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine init_sph_domain_OMP_FFTW                             &
!!     &         (nidx_rtp, irt_rtp_smp_stack, OFFTW_d)
!!      subroutine finalize_sph_domain_OMP_FFTW(OFFTW_d)
!!      subroutine verify_sph_domain_OMP_FFTW                           &
!!     &         (nidx_rtp, irt_rtp_smp_stack, OFFTW_d)
!!
!!   wrapper subroutine for initierize FFT by FFTW
!! ------------------------------------------------------------------
!!
!!      subroutine sph_domain_fwd_OFFTW_to_send                         &
!!     &         (nnod_rtp, nidx_rtp, irt_rtp_smp_stack, ncomp_fwd,     &
!!     &          n_WS, irev_sr_rtp, X_rtp, WS, OFFTW_d)
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
!!      subroutine sph_domain_back_OFFTW_from_recv                      &
!!     &         (nnod_rtp, nidx_rtp, irt_rtp_smp_stack, ncomp_bwd,     &
!!     &          n_WR, irev_sr_rtp, WR, X_rtp, OFFTW_d)
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
      module t_sph_field_OMP_FFTW
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
      type work_for_domain_OMP_FFTW
!>        plan ID for backward transform
        integer(kind = fftw_plan) :: plan_bwd
!>        plan ID for forward transform
        integer(kind = fftw_plan) :: plan_fwd
!
!>        length of FFT for real
        integer(kind = kint) :: Nfft_r
!>        length of FFT for complex
        integer(kind = kint) :: Nfft_c
!>        normalization parameter for FFTW (= 1 / Nfft)
        real(kind = kreal) :: aNfft
!
!>        real data for multiple Fourier transform
        real(kind = kreal), allocatable :: X(:)
!>        spectrum data for multiple Fourier transform
        complex(kind = fftw_complex), allocatable :: C(:)
!
!>        temporal area for ordering
        real(kind = kreal), allocatable :: v_tmp(:)
!
!>        temporal area for time count
        real(kind = kreal), allocatable :: t_omp(:,:)
      end type work_for_domain_OMP_FFTW
!
      private :: alloc_domain_OMP_FFTW_plan
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_domain_OMP_FFTW                               &
     &         (nidx_rtp, irt_rtp_smp_stack, OFFTW_d)
!
      use m_OMP_FFTW3_counter
!
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
      type(work_for_domain_OMP_FFTW), intent(inout) :: OFFTW_d
!
      integer(kind = 4) :: howmany
!
      integer, parameter :: IONE_4 = 1
      integer, parameter :: inembed = 0
!
!
!
      call alloc_domain_OMP_FFTW_plan                                   &
     &   (nidx_rtp(3), irt_rtp_smp_stack, OFFTW_d)
!
      howmany = int(irt_rtp_smp_stack(np_smp))
!
      call chack_init_OMP_FFTW()
!
      call dfftw_plan_many_dft_r2c                                      &
     &   (OFFTW_d%plan_fwd, IONE_4, int(OFFTW_d%Nfft_r), howmany,       &
     &    OFFTW_d%X(1), inembed, howmany, IONE_4,                       &
     &    OFFTW_d%C(1), inembed, howmany, IONE_4,                       &
     &    FFTW_ESTIMATE)
      call dfftw_plan_many_dft_c2r                                      &
     &   (OFFTW_d%plan_bwd, IONE_4, int(OFFTW_d%Nfft_r), howmany,       &
     &    OFFTW_d%C(1), inembed, howmany, IONE_4,                       &
     &    OFFTW_d%X(1), inembed, howmany, IONE_4,                       &
     &    FFTW_ESTIMATE)
      OFFTW_d%aNfft = one / dble(nidx_rtp(3))
!
      allocate(OFFTW_d%t_omp(np_smp,0:3))
      OFFTW_d%t_omp = 0.0d0
!
      end subroutine init_sph_domain_OMP_FFTW
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_domain_OMP_FFTW(OFFTW_d)
!
      use m_OMP_FFTW3_counter
!
      type(work_for_domain_OMP_FFTW), intent(inout) :: OFFTW_d
!
!
      call dfftw_destroy_plan(OFFTW_d%plan_fwd)
      call dfftw_destroy_plan(OFFTW_d%plan_bwd)
      call dfftw_cleanup
      call chack_clean_OMP_FFTW()
!
      call dealloc_domain_OMP_FFTW_plan(OFFTW_d)
      deallocate(OFFTW_d%t_omp)
!
      end subroutine finalize_sph_domain_OMP_FFTW
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_domain_OMP_FFTW                             &
     &         (nidx_rtp, irt_rtp_smp_stack, OFFTW_d)
!
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
      type(work_for_domain_OMP_FFTW), intent(inout) :: OFFTW_d
!
!
      if(allocated(OFFTW_d%X) .eqv. .false.) then
        call init_sph_domain_OMP_FFTW                                   &
     &     (nidx_rtp, irt_rtp_smp_stack, OFFTW_d)
        return
      end if
!
      if(size(OFFTW_d%X)                                                &
     &          .ne. nidx_rtp(3)*irt_rtp_smp_stack(np_smp)) then
        call finalize_sph_domain_OMP_FFTW(OFFTW_d)
        call init_sph_domain_OMP_FFTW                                   &
     &     (nidx_rtp, irt_rtp_smp_stack, OFFTW_d)
      end if
!
      end subroutine verify_sph_domain_OMP_FFTW
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sph_domain_fwd_OFFTW_to_send                           &
     &         (nnod_rtp, nidx_rtp, irt_rtp_smp_stack, ncomp_fwd,       &
     &          n_WS, irev_sr_rtp, X_rtp, WS, OFFTW_d)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_fwd
      real(kind = kreal), intent(in)                                    &
     &        :: X_rtp(irt_rtp_smp_stack(np_smp),nidx_rtp(3),ncomp_fwd)
!
      integer(kind = kint), intent(in) :: n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(work_for_domain_OMP_FFTW), intent(inout) :: OFFTW_d
!
      integer(kind = kint) ::  nd
!
!
      do nd = 1, ncomp_fwd
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+4)
        call copy_rtp_field_to_OMP_FFTW                                 &
     &       (OFFTW_d%Nfft_r, irt_rtp_smp_stack(np_smp),                &
     &        X_rtp(1,1,nd), OFFTW_d%X(1))
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+4)
!
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+5)
        call dfftw_execute_dft_r2c(OFFTW_d%plan_fwd,                    &
      &                            OFFTW_d%X, OFFTW_d%C)
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+5)
!
!   normalization
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+6)
          call set_fwd_OMP_FFTW_to_send                                 &
     &       (nd, irt_rtp_smp_stack(np_smp),                            &
     &        nnod_rtp, ncomp_fwd, n_WS, irev_sr_rtp, WS,               &
     &        OFFTW_d%Nfft_c, OFFTW_d%aNfft, OFFTW_d%C(1))
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+6)
      end do
!
      end subroutine sph_domain_fwd_OFFTW_to_send
!
! ------------------------------------------------------------------
!
      subroutine sph_domain_back_OFFTW_from_recv                        &
     &         (nnod_rtp, nidx_rtp, irt_rtp_smp_stack, ncomp_bwd,       &
     &          n_WR, irev_sr_rtp, WR, X_rtp, OFFTW_d)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(in):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &        :: X_rtp(irt_rtp_smp_stack(np_smp),nidx_rtp(3),ncomp_bwd)
      type(work_for_domain_OMP_FFTW), intent(inout) :: OFFTW_d
!
      integer(kind = kint) :: nd
!
!
      do nd = 1, ncomp_bwd
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+1)
        call set_back_OMP_FFTW_from_recv(nd, irt_rtp_smp_stack(np_smp), &
     &        nnod_rtp, ncomp_bwd, n_WR, irev_sr_rtp, WR,               &
     &        OFFTW_d%Nfft_c, OFFTW_d%C(1))
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+1)
!
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+2)
        call dfftw_execute_dft_c2r(OFFTW_d%plan_bwd,                    &
     &                             OFFTW_d%C, OFFTW_d%X)
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+2)
!
        if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+3)
          call copy_rtp_field_from_OMP_FFTW                             &
     &       (OFFTW_d%Nfft_r, irt_rtp_smp_stack(np_smp),                &
     &        X_rtp(1,1,nd), OFFTW_d%X(1))
        if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+3)
      end do
!
      end subroutine sph_domain_back_OFFTW_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_domain_OMP_FFTW_plan                             &
     &         (Nfft, irt_rtp_smp_stack, OFFTW_d)
!
      integer(kind = kint), intent(in) :: Nfft
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
      type(work_for_domain_OMP_FFTW), intent(inout) :: OFFTW_d
!
      integer(kind = kint_gl) :: nnod_rt
!
!
      OFFTW_d%Nfft_r = Nfft
      OFFTW_d%Nfft_c = Nfft/2 + 1
!
      nnod_rt = irt_rtp_smp_stack(np_smp)
      allocate(OFFTW_d%X(OFFTW_d%Nfft_r*nnod_rt))
      allocate(OFFTW_d%C(OFFTW_d%Nfft_c*nnod_rt))
      OFFTW_d%X = 0.0d0
      OFFTW_d%C = 0.0d0
!
      end subroutine alloc_domain_OMP_FFTW_plan
!
! ------------------------------------------------------------------
!
      subroutine dealloc_domain_OMP_FFTW_plan(OFFTW_d)
!
      type(work_for_domain_OMP_FFTW), intent(inout) :: OFFTW_d
!
!
      deallocate(OFFTW_d%X, OFFTW_d%C)
!
      end subroutine dealloc_domain_OMP_FFTW_plan
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_rtp_field_to_OMP_FFTW                             &
     &         (Nfft_r, nnod_rt, X_rtp, X_FFT)
!
      integer(kind = kint), intent(in) :: Nfft_r, nnod_rt
      real(kind = kreal), intent(in)  :: X_rtp(nnod_rt,Nfft_r)
!
      real(kind = kreal), intent(inout) :: X_FFT(nnod_rt,Nfft_r)
!
!
!$omp parallel workshare
      X_FFT(1:nnod_rt,1:Nfft_r) = X_rtp(1:nnod_rt,1:Nfft_r)
!$omp end parallel workshare
!
      end subroutine copy_rtp_field_to_OMP_FFTW
!
! ------------------------------------------------------------------
!
      subroutine set_fwd_OMP_FFTW_to_send                               &
     &         (nd, nnod_rt, nnod_rtp, ncomp_fwd, n_WS,                 &
     &          irev_sr_rtp, WS, Nfft_c, aNfft, C_fft)
!
      integer(kind = kint), intent(in) :: nd
      integer(kind = kint), intent(in) :: nnod_rtp
!
      integer(kind = kint), intent(in) :: ncomp_fwd
!
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!
      integer(kind = kint), intent(in) :: Nfft_c, nnod_rt
      real(kind = kreal), intent(in) :: aNfft
      complex(kind = fftw_complex), intent(in) :: C_fft(nnod_rt,Nfft_c)
!
      integer(kind = kint), intent(in) :: n_WS
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) ::  m, j, ic_rtp, is_rtp, ic_send, is_send
!
!
!$omp parallel do private(j,ic_send)
      do j = 1, nnod_rt
        ic_send = nd + (irev_sr_rtp(j) - 1) * ncomp_fwd
        WS(ic_send) = aNfft * real(C_fft(j,1))
      end do
!$omp end parallel do
!
      do m = 2, Nfft_c-1
!$omp parallel do private(j,ic_rtp,is_rtp,ic_send,is_send)
        do j = 1, nnod_rt
          ic_rtp = j + (2*m-2) * nnod_rt
          is_rtp = j + (2*m-1) * nnod_rt
          ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
          is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
          WS(ic_send) = two * aNfft * real(C_fft(j,m))
          WS(is_send) = two * aNfft * real(C_fft(j,m)*iu)
        end do 
!$omp end parallel do
      end do
!
!$omp parallel do private(j,ic_rtp,ic_send)
      do j = 1, nnod_rt
        ic_rtp = j + nnod_rt
        ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
        WS(ic_send) = two * aNfft * real(C_fft(j,Nfft_c))
      end do
!$omp end parallel do
!
      end subroutine set_fwd_OMP_FFTW_to_send
!
! ------------------------------------------------------------------
!
      subroutine set_back_OMP_FFTW_from_recv                            &
     &         (nd, nnod_rt, nnod_rtp, ncomp_bwd,                       &
     &          n_WR, irev_sr_rtp, WR, Nfft_c, C_fft)
!
      integer(kind = kint), intent(in) :: nd
      integer(kind = kint), intent(in) :: Nfft_c, nnod_rt
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real(kind=kreal), intent(in):: WR(n_WR)
!
      complex(kind = fftw_complex), intent(inout)                       &
     &                             :: C_fft(nnod_rt,Nfft_c)
!
      integer(kind = kint) :: m, j, ic_rtp, is_rtp, ic_recv, is_recv
!
!
!   normalization
!$omp parallel do private(j,ic_recv)
      do j = 1, nnod_rt
        ic_recv = nd + (irev_sr_rtp(j) - 1) * ncomp_bwd
        C_fft(j,1) = cmplx(WR(ic_recv), zero, kind(0d0))
      end do
!$omp end parallel do
!
      do m = 2, Nfft_c-1
!$omp parallel do private(j,ic_rtp,is_rtp,ic_recv,is_recv)
        do j = 1, nnod_rt
          ic_rtp = j + (2*m-2) * nnod_rt
          is_rtp = j + (2*m-1) * nnod_rt
          ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
          is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
          C_fft(j,m)                                                    &
     &            = half * cmplx(WR(ic_recv), -WR(is_recv),kind(0d0))
        end do
!$omp end parallel do
      end do
!
!$omp parallel do private(j,ic_rtp,ic_recv)
      do j = 1, nnod_rt
        ic_rtp = j + nnod_rt
        ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
        C_fft(j,Nfft_c) = half * cmplx(WR(ic_recv), zero, kind(0d0))
      end do
!$omp end parallel do
!
      end subroutine set_back_OMP_FFTW_from_recv
!
! ------------------------------------------------------------------
!
      subroutine copy_rtp_field_from_OMP_FFTW                           &
     &         (Nfft_r, nnod_rt, X_rtp, X_FFT)
!
      integer(kind = kint), intent(in) :: Nfft_r, nnod_rt
      real(kind = kreal), intent(in) :: X_FFT(nnod_rt,Nfft_r)
!
      real(kind = kreal), intent(inout) :: X_rtp(nnod_rt,Nfft_r)
!
!
!$omp parallel workshare
      X_rtp(1:nnod_rt,1:Nfft_r) = X_FFT(1:nnod_rt,1:Nfft_r)
!$omp end parallel workshare
!
      end subroutine copy_rtp_field_from_OMP_FFTW
!
! ------------------------------------------------------------------
!
      end module t_sph_field_OMP_FFTW
