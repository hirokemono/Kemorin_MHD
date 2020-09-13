!>@file   t_sph_field_FFTW.F90
!!@brief  module t_sph_field_FFTW
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2013
!!@n    Modified on Oct., 2014
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine init_sph_field_FFTW(nidx_rtp, irt_rtp_smp_stack,     &
!!     &          FFTW_f)
!!      subroutine finalize_sph_field_FFTW(FFTW_f)
!!      subroutine verify_sph_field_FFTW(nnod_rtp, nidx_rtp,            &
!!     &          irt_rtp_smp_stack, FFTW_f)
!!
!!   wrapper subroutine for initierize FFT by FFTW
!! ------------------------------------------------------------------
!!
!!      subroutine sph_field_fwd_FFTW_to_send                           &
!!     &         (nnod_rtp, nidx_rtp, irt_rtp_smp_stack, ncomp,         &
!!     &          n_WS, irev_sr_rtp, X_rtp, WS, FFTW_f)
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
!!      subroutine sph_field_back_FFTW_from_recv                        &
!!     &         (nnod_rtp, nidx_rtp, irt_rtp_smp_stack, ncomp,         &
!!     &          n_WR, irev_sr_rtp, WR, X_rtp, FFTW_f)
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
      module t_sph_field_FFTW
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_elapsed_labels_SPH_TRNS
      use calypso_mpi
!
      implicit none
!
!>      Structure to use SNGLE FFTW
      type work_for_field_FFTW
!>        plan ID for backward transform
        integer(kind = fftw_plan), allocatable :: plan_bwd(:)
!>        plan ID for forward transform
        integer(kind = fftw_plan), allocatable :: plan_fwd(:)
!
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
      end type work_for_field_FFTW
!
      private :: alloc_fld_FFTW_plan
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_field_FFTW(nidx_rtp, irt_rtp_smp_stack,       &
     &          FFTW_f)
!
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
      type(work_for_field_FFTW), intent(inout) :: FFTW_f
!
      integer(kind = kint) :: ip, ist
      integer(kind = 4) :: Nfft4, howmany, idist_r, idist_c
!
      integer, parameter :: IONE_4 = 1
      integer, parameter :: inembed = 0
      integer, parameter :: istride = 1
!
!
!
      Nfft4 = int(nidx_rtp(3))
      call alloc_fld_FFTW_plan                                          &
     &   (irt_rtp_smp_stack(np_smp), nidx_rtp(3), FFTW_f)
!
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1) + 1
        howmany = int(irt_rtp_smp_stack(ip  )                           &
     &           - irt_rtp_smp_stack(ip-1))
        idist_r = int(nidx_rtp(3))
        idist_c = int(nidx_rtp(3)/2+1)
!
        call dfftw_plan_many_dft_r2c                                    &
     &     (FFTW_f%plan_fwd(ip), IONE_4, Nfft4, howmany,                &
     &      FFTW_f%X(1,ist), inembed, istride, idist_r,                 &
     &      FFTW_f%C(1,ist), inembed, istride, idist_c, FFTW_ESTIMATE)
        call dfftw_plan_many_dft_c2r                                    &
     &     (FFTW_f%plan_bwd(ip), IONE_4, Nfft4, howmany,                &
     &      FFTW_f%C(1,ist), inembed, istride, idist_c,                 &
     &      FFTW_f%X(1,ist), inembed, istride, idist_r, FFTW_ESTIMATE)
      end do
      FFTW_f%aNfft = one / dble(nidx_rtp(3))
!
      allocate(FFTW_f%t_omp(np_smp,0:3))
      FFTW_f%t_omp = 0.0d0
!
      end subroutine init_sph_field_FFTW
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_field_FFTW(FFTW_f)
!
      type(work_for_field_FFTW), intent(inout) :: FFTW_f
!
      integer(kind = kint) :: j
!
!
      do j = 1, np_smp
        call dfftw_destroy_plan(FFTW_f%plan_fwd(j))
        call dfftw_destroy_plan(FFTW_f%plan_bwd(j))
        call dfftw_cleanup
      end do
!
      call dealloc_fld_FFTW_plan(FFTW_f)
      deallocate(FFTW_f%t_omp)
!
      end subroutine finalize_sph_field_FFTW
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_field_FFTW(nnod_rtp, nidx_rtp,              &
     &          irt_rtp_smp_stack, FFTW_f)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
      type(work_for_field_FFTW), intent(inout) :: FFTW_f
!
!
      if(allocated(FFTW_f%X) .eqv. .false.) then
        call init_sph_field_FFTW(nidx_rtp, irt_rtp_smp_stack, FFTW_f)
        return
      end if
!
      if(size(FFTW_f%X) .ne. nnod_rtp) then
        call finalize_sph_field_FFTW(FFTW_f)
        call init_sph_field_FFTW(nidx_rtp, irt_rtp_smp_stack, FFTW_f)
      end if
!
      end subroutine verify_sph_field_FFTW
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sph_field_fwd_FFTW_to_send                             &
     &         (nnod_rtp, nidx_rtp, irt_rtp_smp_stack, ncomp,           &
     &          n_WS, irev_sr_rtp, X_rtp, WS, FFTW_f)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in)                                    &
     &         :: X_rtp(irt_rtp_smp_stack(np_smp),nidx_rtp(3),ncomp)
!
      integer(kind = kint), intent(in) :: n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WS(n_WS)
      type(work_for_field_FFTW), intent(inout) :: FFTW_f
!
      integer(kind = kint) ::  m, j, ip, ist, ied, nd
      integer(kind = kint) :: ic_rtp, is_rtp, ic_send, is_send
!
!
      if(iflag_FFT_time) then
!$omp parallel workshare
        FFTW_f%t_omp(1:np_smp,0:3) = 0
!$omp end parallel workshare
      end if
!
!$omp parallel private(nd)
      do nd = 1, ncomp
!$omp do private(j,ip,ist,ied)
        do ip = 1, np_smp
          ist = irt_rtp_smp_stack(ip-1) + 1
          ied = irt_rtp_smp_stack(ip)
!
          if(iflag_FFT_time) FFTW_f%t_omp(ip,0) = MPI_WTIME()
          do j = ist, ied
            FFTW_f%X(1:nidx_rtp(3),j) = X_rtp(j,1:nidx_rtp(3),nd)
          end do
          if(iflag_FFT_time) FFTW_f%t_omp(ip,1)= FFTW_f%t_omp(ip,1)     &
     &                     + MPI_WTIME() - FFTW_f%t_omp(ip,0)
        end do
!$omp end do
!
!$omp do private(ip,ist,ied)
        do ip = 1, np_smp
          ist = irt_rtp_smp_stack(ip-1) + 1
          ied = irt_rtp_smp_stack(ip)
          if(iflag_FFT_time) FFTW_f%t_omp(ip,0) = MPI_WTIME()
          call dfftw_execute_dft_r2c(FFTW_f%plan_fwd(ip),               &
     &        FFTW_f%X(1,ist), FFTW_f%C(1,ist))
          if(iflag_FFT_time) FFTW_f%t_omp(ip,2)= FFTW_f%t_omp(ip,2)     &
     &                     + MPI_WTIME() - FFTW_f%t_omp(ip,0)
        end do
!$omp end do
!
!   normalization
!$omp do private(m,j,ip,ist,ied,ic_rtp,is_rtp,ic_send,is_send)
        do ip = 1, np_smp
          ist = irt_rtp_smp_stack(ip-1) + 1
          ied = irt_rtp_smp_stack(ip)
          if(iflag_FFT_time) FFTW_f%t_omp(ip,0) = MPI_WTIME()
          do j = ist, ied
            ic_send = nd + (irev_sr_rtp(j) - 1) * ncomp
            WS(ic_send) = FFTW_f%aNfft * real(FFTW_f%C(1,j))
            do m = 2, (nidx_rtp(3)+1)/2
              ic_rtp = j + (2*m-2) * irt_rtp_smp_stack(np_smp)
              is_rtp = j + (2*m-1) * irt_rtp_smp_stack(np_smp)
              ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp
              is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp
              WS(ic_send) = two * FFTW_f%aNfft * real(FFTW_f%C(m,j))
              WS(is_send) = two * FFTW_f%aNfft * real(FFTW_f%C(m,j)*iu)
            end do 
            m = (nidx_rtp(3)+1)/2 + 1
            ic_rtp = j + irt_rtp_smp_stack(np_smp)
            ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp
            WS(ic_send) = two * FFTW_f%aNfft * real(FFTW_f%C(m,j))
          end do
          if(iflag_FFT_time) FFTW_f%t_omp(ip,3)= FFTW_f%t_omp(ip,3)     &
     &                     + MPI_WTIME() - FFTW_f%t_omp(ip,0)
        end do
!$omp end do
      end do
!$omp end parallel
!
      if(iflag_FFT_time) then
        do ip = 2, np_smp
          FFTW_f%t_omp(1,1) = FFTW_f%t_omp(1,1) + FFTW_f%t_omp(ip,1)
          FFTW_f%t_omp(1,2) = FFTW_f%t_omp(1,2) + FFTW_f%t_omp(ip,2)
          FFTW_f%t_omp(1,3) = FFTW_f%t_omp(1,3) + FFTW_f%t_omp(ip,3)
        end do
        elps1%elapsed(ist_elapsed_FFT+4)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+4)                        &
     &         + FFTW_f%t_omp(1,1) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+5)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+5)                        &
     &         + FFTW_f%t_omp(1,2) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+6)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+6)                        &
     &         + FFTW_f%t_omp(1,3) / dble(np_smp)
      end if
!
      end subroutine sph_field_fwd_FFTW_to_send
!
! ------------------------------------------------------------------
!
      subroutine sph_field_back_FFTW_from_recv                          &
     &         (nnod_rtp, nidx_rtp, irt_rtp_smp_stack, ncomp,           &
     &          n_WR, irev_sr_rtp, WR, X_rtp, FFTW_f)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &          :: X_rtp(irt_rtp_smp_stack(np_smp),nidx_rtp(3),ncomp)
      type(work_for_field_FFTW), intent(inout) :: FFTW_f
!
      integer(kind = kint) :: m, j, ip, ist, ied, nd
      integer(kind = kint) :: ic_rtp, is_rtp, ic_recv, is_recv
!      real :: dummy(3), rtmp(3)
!
!
      if(iflag_FFT_time) then
!$omp parallel workshare
        FFTW_f%t_omp(1:np_smp,0:3) = 0
!$omp end parallel workshare
      end if
!
!$omp parallel private(nd)
      do nd = 1, ncomp
!$omp do private(m,j,ip,ist,ied,ic_rtp,is_rtp,ic_recv,is_recv)
        do ip = 1, np_smp
          ist = irt_rtp_smp_stack(ip-1) + 1
          ied = irt_rtp_smp_stack(ip)
!   normalization
          if(iflag_FFT_time) FFTW_f%t_omp(ip,0) = MPI_WTIME()
          do j = ist, ied
            ic_recv = nd + (irev_sr_rtp(j) - 1) * ncomp
            FFTW_f%C(1,j) = cmplx(WR(ic_recv), zero, kind(0d0))
            do m = 2, (nidx_rtp(3)+1)/2
              ic_rtp = j + (2*m-2) * irt_rtp_smp_stack(np_smp)
              is_rtp = j + (2*m-1) * irt_rtp_smp_stack(np_smp)
              ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp
              is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp
              FFTW_f%C(m,j)                                             &
     &            = half * cmplx(WR(ic_recv), -WR(is_recv),kind(0d0))
            end do
            m = (nidx_rtp(3)+1)/2 + 1
            ic_rtp = j + irt_rtp_smp_stack(np_smp)
            ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp
            FFTW_f%C(m,j) = half * cmplx(WR(ic_recv), zero, kind(0d0))
          end do
          if(iflag_FFT_time) FFTW_f%t_omp(ip,1)= FFTW_f%t_omp(ip,1)     &
     &                     + MPI_WTIME() - FFTW_f%t_omp(ip,0)
        end do
!$omp end do
!
!$omp do private(ip,ist,ied)
        do ip = 1, np_smp
          ist = irt_rtp_smp_stack(ip-1) + 1
          ied = irt_rtp_smp_stack(ip)
          if(iflag_FFT_time) FFTW_f%t_omp(ip,0) = MPI_WTIME()
          call dfftw_execute_dft_c2r(FFTW_f%plan_bwd(ip),               &
     &        FFTW_f%C(1,ist), FFTW_f%X(1,ist))
          if(iflag_FFT_time) FFTW_f%t_omp(ip,2)= FFTW_f%t_omp(ip,2)     &
     &                     + MPI_WTIME() - FFTW_f%t_omp(ip,0)
        end do
!$omp end do
!
!$omp do private(j,ip,ist,ied)
        do ip = 1, np_smp
          ist = irt_rtp_smp_stack(ip-1) + 1
          ied = irt_rtp_smp_stack(ip)
          if(iflag_FFT_time) FFTW_f%t_omp(ip,0) = MPI_WTIME()
          do j = ist, ied
            X_rtp(j,1:nidx_rtp(3),nd) = FFTW_f%X(1:nidx_rtp(3),j)
          end do
          if(iflag_FFT_time) FFTW_f%t_omp(ip,3)= FFTW_f%t_omp(ip,3)     &
     &                     + MPI_WTIME() - FFTW_f%t_omp(ip,0)
        end do
!$omp end do
      end do
!$omp end parallel
!
!
      if(iflag_FFT_time) then
        do ip = 2, np_smp
          FFTW_f%t_omp(1,1) = FFTW_f%t_omp(1,1) + FFTW_f%t_omp(ip,1)
          FFTW_f%t_omp(1,2) = FFTW_f%t_omp(1,2) + FFTW_f%t_omp(ip,2)
          FFTW_f%t_omp(1,3) = FFTW_f%t_omp(1,3) + FFTW_f%t_omp(ip,3)
        end do
        elps1%elapsed(ist_elapsed_FFT+1)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+1)                        &
     &         + FFTW_f%t_omp(1,1) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+2)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+2)                        &
     &         + FFTW_f%t_omp(1,2) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+3)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+3)                        &
     &         + FFTW_f%t_omp(1,3) / dble(np_smp)
      end if
!
      end subroutine sph_field_back_FFTW_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_fld_FFTW_plan(nnod_rt, Nfft, FFTW_f)
!
      integer(kind = kint), intent(in) :: nnod_rt, Nfft
      type(work_for_field_FFTW), intent(inout) :: FFTW_f
!
!
      allocate(FFTW_f%plan_bwd(np_smp))
      allocate(FFTW_f%plan_fwd(np_smp))
!
      allocate(FFTW_f%X(Nfft,nnod_rt))
      allocate(FFTW_f%C(Nfft/2+1,nnod_rt))
      FFTW_f%X = 0.0d0
      FFTW_f%C = 0.0d0
!
      end subroutine alloc_fld_FFTW_plan
!
! ------------------------------------------------------------------
!
      subroutine dealloc_fld_FFTW_plan(FFTW_f)
!
      type(work_for_field_FFTW), intent(inout) :: FFTW_f
!
!
      deallocate(FFTW_f%plan_fwd, FFTW_f%plan_bwd)
      deallocate(FFTW_f%X, FFTW_f%C)
!
      end subroutine dealloc_fld_FFTW_plan
!
! ------------------------------------------------------------------
!
      end module t_sph_field_FFTW
