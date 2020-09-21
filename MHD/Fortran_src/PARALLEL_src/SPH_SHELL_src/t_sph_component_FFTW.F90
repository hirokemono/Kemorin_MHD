!>@file   t_sph_component_FFTW.F90
!!@brief  module t_sph_component_FFTW
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!!@n    Modified on Oct., 2014
!
!>@brief  Fourier transform using FFTW Ver.3
!!
!!@verbatim
!! ------------------------------------------------------------------
!!      subroutine init_sph_component_FFTW(ncomp, nidx_rtp, FFTW_c)
!!      subroutine finalize_sph_component_FFTW(FFTW_c)
!!      subroutine verify_sph_component_FFTW(ncomp, nidx_rtp, FFTW_c)
!!
!!   wrapper subroutine for initierize FFT by FFTW
!! ------------------------------------------------------------------
!!
!!      subroutine sph_comp_fwd_FFTW_to_send(nnod_rtp, nidx_rtp,        &
!!     &          irt_rtp_smp_stack, ncomp, n_WS, irev_sr_rtp,          &
!!     &          X_rtp, WS, FFTW_c)
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
!!      subroutine sph_comp_back_FFTW_from_recv                         &
!!     &         (nnod_rtp, nidx_rtp, irt_rtp_smp_stack,                &
!!     &          ncomp, ntot_item_sr, irev_sr_rtp, WR, X_rtp, FFTW_c)
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
!!@n @param X(Ncomp,Nfft,np_smp)  Data for Fourier transform
!
      module t_sph_component_FFTW
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
      type work_for_comp_FFTW
!>        plan ID for backward transform
        integer(kind = fftw_plan), allocatable :: plan_bwd(:)
!>        plan ID for forward transform
        integer(kind = fftw_plan), allocatable :: plan_fwd(:)
!
!>        length of FFT for real
        integer(kind = kint) :: Nfft_r
!>        length of FFT for complex
        integer(kind = kint) :: Nfft_c
!>        normalization parameter for FFTW (= 1 / Nfft)
        real(kind = kreal) :: aNfft
!>        real data for multiple Fourier transform
        real(kind = kreal), allocatable :: X(:,:,:)
!>        spectrum data for multiple Fourier transform
        complex(kind = fftw_complex), allocatable :: C(:,:,:)
!
!>        temporal area for time count
        real(kind = kreal), allocatable :: t_omp(:,:)
      end type work_for_comp_FFTW
!
      private :: alloc_comp_FFTW_plan, dealloc_comp_FFTW_plan
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_component_FFTW(ncomp, nidx_rtp, FFTW_c)
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      type(work_for_comp_FFTW), intent(inout) :: FFTW_c
!
      integer(kind = kint) :: ip
      integer(kind = 4) :: Nfft4, howmany
!
      integer, parameter :: IONE_4 = 1
      integer, parameter :: inembed = 0
!
      call alloc_comp_FFTW_plan(np_smp, nidx_rtp(3), FFTW_c)
!
      Nfft4 = int(nidx_rtp(3))
      howmany = int(ncomp)
      do ip = 1, np_smp
        call dfftw_plan_many_dft_r2c                                    &
     &     (FFTW_c%plan_fwd(ip), IONE_4, int(FFTW_c%Nfft_r), howmany,   &
     &      FFTW_c%X(1,1,ip), inembed, howmany, IONE_4,                 &
     &      FFTW_c%C(1,1,ip), inembed, howmany, IONE_4,                 &
     &      FFTW_ESTIMATE)
        call dfftw_plan_many_dft_c2r                                    &
     &     (FFTW_c%plan_bwd(ip), IONE_4, int(FFTW_c%Nfft_r), howmany,   &
     &      FFTW_c%C(1,1,ip), inembed, howmany, IONE_4,                 &
     &      FFTW_c%X(1,1,ip), inembed, howmany, IONE_4,                 &
     &      FFTW_ESTIMATE)
      end do
      FFTW_c%aNfft = one / dble(nidx_rtp(3))
!
      allocate(FFTW_c%t_omp(np_smp,0:3))
      FFTW_c%t_omp = 0.0d0
!
      end subroutine init_sph_component_FFTW
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_component_FFTW(FFTW_c)
!
      type(work_for_comp_FFTW), intent(inout) :: FFTW_c
!
      integer(kind = kint) :: j
!
!
      do j = 1, np_smp
        call dfftw_destroy_plan(FFTW_c%plan_fwd(j))
        call dfftw_destroy_plan(FFTW_c%plan_bwd(j))
        call dfftw_cleanup
      end do
!
      call dealloc_comp_FFTW_plan(FFTW_c)
      deallocate(FFTW_c%t_omp)
!
      end subroutine finalize_sph_component_FFTW
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_component_FFTW(ncomp, nidx_rtp, FFTW_c)
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      type(work_for_comp_FFTW), intent(inout) :: FFTW_c
!
!
      if(allocated(FFTW_c%X) .eqv. .false.) then
        call init_sph_component_FFTW(ncomp, nidx_rtp, FFTW_c)
        return
      end if
!
      if(size(FFTW_c%X) .ne. ncomp*nidx_rtp(3)*np_smp) then
        call finalize_sph_component_FFTW(FFTW_c)
        call init_sph_component_FFTW(ncomp, nidx_rtp, FFTW_c)
      end if
!
      end subroutine verify_sph_component_FFTW
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sph_comp_fwd_FFTW_to_send(nnod_rtp, nidx_rtp,          &
     &          irt_rtp_smp_stack, ncomp, n_WS1, irev_sr_rtp,           &
     &          X_rtp, WS, FFTW_c)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in)                                    &
     &     :: X_rtp(irt_rtp_smp_stack(np_smp),nidx_rtp(3),ncomp)
!
      integer(kind = kint), intent(in) :: n_WS1
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WS(ncomp,n_WS1)
      type(work_for_comp_FFTW), intent(inout) :: FFTW_c
!
      integer(kind = kint) ::  m, j, ip, ist, ied, nd
      integer(kind = kint) :: ic_rtp, is_rtp, ic_send, is_send
!
!
      if(iflag_FFT_time) then
!$omp parallel workshare
        FFTW_c%t_omp(1:np_smp,0:3) = 0
!$omp end parallel workshare
      end if
!
!$omp parallel do schedule(static)                                      &
!$omp&         private(nd,m,j,ip,ist,ied,ic_rtp,is_rtp,ic_send,is_send)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1) + 1
        ied = irt_rtp_smp_stack(ip) 
        do j = ist, ied
          if(iflag_FFT_time) FFTW_c%t_omp(ip,0) = MPI_WTIME()
          do nd = 1, ncomp
            do m = 1, nidx_rtp(3)
              ic_rtp = nd + (m-1) * ncomp
              FFTW_c%X(nd,m,ip) = X_rtp(j,m,nd)
            end do
          end do
          if(iflag_FFT_time) FFTW_c%t_omp(ip,1) = FFTW_c%t_omp(ip,1)    &
     &                       + MPI_WTIME() - FFTW_c%t_omp(ip,0)
!
          if(iflag_FFT_time) FFTW_c%t_omp(ip,0) = MPI_WTIME()
          call dfftw_execute_dft_r2c(FFTW_c%plan_fwd(ip),               &
     &        FFTW_c%X(1,1,ip), FFTW_c%C(1,1,ip))
          if(iflag_FFT_time) FFTW_c%t_omp(ip,2) = FFTW_c%t_omp(ip,2)    &
     &                       + MPI_WTIME() - FFTW_c%t_omp(ip,0)
!
!   normalization
          if(iflag_FFT_time) FFTW_c%t_omp(ip,0) = MPI_WTIME()
          do nd = 1, ncomp
            ic_send = irev_sr_rtp(j)
            WS(nd,ic_send) = FFTW_c%aNfft * real(FFTW_c%C(nd,1,ip))
          end do
          do m = 2, FFTW_c%Nfft_c-1
            ic_rtp = j + (2*m-2) * irt_rtp_smp_stack(np_smp)
            is_rtp = j + (2*m-1) * irt_rtp_smp_stack(np_smp)
            do nd = 1, ncomp
              ic_send = irev_sr_rtp(ic_rtp)
              is_send = irev_sr_rtp(is_rtp)
              WS(nd,ic_send)                                            &
     &                = two*FFTW_c%aNfft * real(FFTW_c%C(nd,m,ip))
              WS(nd,is_send)                                            &
     &                = two*FFTW_c%aNfft * real(FFTW_c%C(nd,m,ip)*iu)
            end do
          end do 
          ic_rtp = j + irt_rtp_smp_stack(np_smp)
          do nd = 1, ncomp
            ic_send = irev_sr_rtp(ic_rtp)
            WS(nd,ic_send)                                              &
     &         = two*FFTW_c%aNfft * real(FFTW_c%C(nd,FFTW_c%Nfft_c,ip))
          end do
          if(iflag_FFT_time) FFTW_c%t_omp(ip,3)= FFTW_c%t_omp(ip,3)   &
     &                       + MPI_WTIME() - FFTW_c%t_omp(ip,0)
        end do
      end do
!$omp end parallel do
!
      if(iflag_FFT_time) then
        do ip = 2, np_smp
          FFTW_c%t_omp(1,1) = FFTW_c%t_omp(1,1) + FFTW_c%t_omp(ip,1)
          FFTW_c%t_omp(1,2) = FFTW_c%t_omp(1,2) + FFTW_c%t_omp(ip,2)
          FFTW_c%t_omp(1,3) = FFTW_c%t_omp(1,3) + FFTW_c%t_omp(ip,3)
        end do
        elps1%elapsed(ist_elapsed_FFT+4)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+4)                        &
     &         + FFTW_c%t_omp(1,1) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+5)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+5)                        &
     &         + FFTW_c%t_omp(1,2) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+6)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+6)                        &
     &         + FFTW_c%t_omp(1,3) / dble(np_smp)
      end if
!
      end subroutine sph_comp_fwd_FFTW_to_send
!
! ------------------------------------------------------------------
!
      subroutine sph_comp_back_FFTW_from_recv                           &
     &         (nnod_rtp, nidx_rtp, irt_rtp_smp_stack,                  &
     &          ncomp, ntot_item_sr, irev_sr_rtp, WR, X_rtp, FFTW_c)
!
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nidx_rtp(3)
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: ntot_item_sr
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WR(ncomp,ntot_item_sr)
!
      real(kind = kreal), intent(inout)                                 &
     &     :: X_rtp(irt_rtp_smp_stack(np_smp),nidx_rtp(3),ncomp)
      type(work_for_comp_FFTW), intent(inout) :: FFTW_c
!
      integer(kind = kint) :: m, j, ip, ist, ied, nd
      integer(kind = kint) :: ic_rtp, is_rtp, ic_recv, is_recv
!
!
      if(iflag_FFT_time) then
!$omp parallel workshare
        FFTW_c%t_omp(1:np_smp,0:3) = 0
!$omp end parallel workshare
      end if
!
!$omp parallel do schedule(static)                                      &
!$omp&         private(nd,m,j,ip,ist,ied,ic_rtp,is_rtp,ic_recv,is_recv)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1) + 1
        ied = irt_rtp_smp_stack(ip)
        do j = ist, ied
!
          if(iflag_FFT_time) FFTW_c%t_omp(ip,0) = MPI_WTIME()
          do nd = 1, ncomp
            ic_recv = irev_sr_rtp(j)
            FFTW_c%C(nd,1,ip) = cmplx(WR(nd,ic_recv), zero, kind(0d0))
          end do
          do m = 2, FFTW_c%Nfft_c-1
            ic_rtp = j + (2*m-2) * irt_rtp_smp_stack(np_smp)
            is_rtp = j + (2*m-1) * irt_rtp_smp_stack(np_smp)
            do nd = 1, ncomp
              ic_recv = irev_sr_rtp(ic_rtp)
              is_recv = irev_sr_rtp(is_rtp)
              FFTW_c%C(nd,m,ip)                                         &
     &          = half*cmplx(WR(nd,ic_recv), -WR(nd,is_recv),kind(0d0))
            end do
          end do
          ic_rtp = j + irt_rtp_smp_stack(np_smp)
          do nd = 1, ncomp
            ic_recv = irev_sr_rtp(ic_rtp)
            FFTW_c%C(nd,FFTW_c%Nfft_c,ip)                               &
     &              = half * cmplx(WR(nd,ic_recv), zero, kind(0d0))
          end do
          if(iflag_FFT_time) FFTW_c%t_omp(ip,1) = FFTW_c%t_omp(ip,1)    &
     &                       + MPI_WTIME() - FFTW_c%t_omp(ip,0)
!
          if(iflag_FFT_time) FFTW_c%t_omp(ip,0) = MPI_WTIME()
          call dfftw_execute_dft_c2r(FFTW_c%plan_bwd(ip),               &
     &        FFTW_c%C(1,1,ip), FFTW_c%X(1,1,ip))
          if(iflag_FFT_time) FFTW_c%t_omp(ip,2)= FFTW_c%t_omp(ip,2)   &
     &                       + MPI_WTIME() - FFTW_c%t_omp(ip,0)
!
          if(iflag_FFT_time) FFTW_c%t_omp(ip,0) = MPI_WTIME()
          do nd = 1, ncomp
            do m = 1, nidx_rtp(3)
              ic_rtp = nd + (m-1) * ncomp
              X_rtp(j,m,nd) = FFTW_c%X(nd,ic_rtp,ip)
            end do  
          end do
          if(iflag_FFT_time) FFTW_c%t_omp(ip,3) = FFTW_c%t_omp(ip,3)    &
     &                       + MPI_WTIME() - FFTW_c%t_omp(ip,0)
        end do
      end do
!$omp end parallel do
!
      if(iflag_FFT_time) then
        do ip = 2, np_smp
          FFTW_c%t_omp(1,1) = FFTW_c%t_omp(1,1) + FFTW_c%t_omp(ip,1)
          FFTW_c%t_omp(1,2) = FFTW_c%t_omp(1,2) + FFTW_c%t_omp(ip,2)
          FFTW_c%t_omp(1,3) = FFTW_c%t_omp(1,3) + FFTW_c%t_omp(ip,3)
        end do
        elps1%elapsed(ist_elapsed_FFT+1)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+1)                        &
     &         + FFTW_c%t_omp(1,1) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+2)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+2)                        &
     &         + FFTW_c%t_omp(1,2) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+3)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+3)                        &
     &         + FFTW_c%t_omp(1,3) / dble(np_smp)
      end if
!
      end subroutine sph_comp_back_FFTW_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_comp_FFTW_plan(Ncomp, Nfft, FFTW_c)
!
      integer(kind = kint), intent(in) :: Ncomp, Nfft
      type(work_for_comp_FFTW), intent(inout) :: FFTW_c
!
!
      FFTW_c%Nfft_r = Nfft
      FFTW_c%Nfft_c = Nfft/2 + 1
!
      allocate(FFTW_c%plan_fwd(np_smp))
      allocate(FFTW_c%plan_bwd(np_smp))
!
      allocate( FFTW_c%X(Ncomp,Nfft,np_smp) )
      allocate( FFTW_c%C(Ncomp,Nfft/2+1,np_smp) )
      FFTW_c%X = 0.0d0
      FFTW_c%C = 0.0d0
!
      end subroutine alloc_comp_FFTW_plan
!
! ------------------------------------------------------------------
!
      subroutine dealloc_comp_FFTW_plan(FFTW_c)
!
      type(work_for_comp_FFTW), intent(inout) :: FFTW_c
!
!
      deallocate(FFTW_c%plan_fwd, FFTW_c%plan_bwd)
      deallocate(FFTW_c%X, FFTW_c%C)
!
      end subroutine dealloc_comp_FFTW_plan
!
! ------------------------------------------------------------------
!
      end module t_sph_component_FFTW
