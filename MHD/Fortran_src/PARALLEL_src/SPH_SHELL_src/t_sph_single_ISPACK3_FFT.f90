!>@file   t_sph_single_ISPACK3_FFT.f90
!!@brief  module t_sph_single_ISPACK3_FFT
!!
!!@author H. Matsui
!!@date Programmed in 2008
!
!
!>@brief  Fourier transform for spherical harmonics transform 
!!@n      using ISPACK
!!
!!@verbatim
!!  ---------------------------------------------------------------------
!!
!!      subroutine init_sph_single_ISPACK3(nphi_rtp, ispack3_s)
!!      subroutine finalize_sph_single_ISPACK3(ispack3_s)
!!      subroutine verify_sph_single_ISPACK3(nphi_rtp, ispack3_s)
!!        integer(kind = kint_gl), intent(in) :: nphi_rtp
!!        integer(kind = kint), intent(in) :: maxirt_rtp_smp
!!        type(work_for_single_ispack3), intent(inout) :: ispack3_s
!! ------------------------------------------------------------------
!! wrapper subroutine for initierize FFT for ISPACK
!! ------------------------------------------------------------------
!!
!!        subroutine sph_single_FXRTFA_to_send(nnod_rtp, nphi_rtp,      &
!!       &          irt_rtp_smp_stack, ncomp_fwd, n_WS, irev_sr_rtp,    &
!!       &          X_rtp, WS, ispack3_s)
!!        integer(kind = kint_gl), intent(in) :: ncomp_fwd
!!        integer(kind = kint_gl), intent(in) :: nnod_rtp, nphi_rtp
!!        integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!!        real(kind = kreal), intent(in)                                &
!!       &     :: X_rtp(irt_rtp_smp_stack(np_smp),nphi_rtp,ncomp_fwd)
!!        integer(kind = kint), intent(in) :: n_WS
!!        integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!!        real (kind=kreal), intent(inout):: WS(n_WS)
!!        type(work_for_single_ispack3), intent(inout) :: ispack3_s
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for forward Fourier transform by ISPACK
!!
!! a_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!! b_{k} = \frac{2}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!! a_{0} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j}
!! K = Nfft/2....
!! a_{k} = \frac{1}{Nfft} \sum_{j=0}^{Nfft-1} x_{j} \cos (\frac{2\pi j k}{Nfft})
!!
!! ------------------------------------------------------------------
!!
!!      subroutine sph_single_FXRTBA_from_recv(nnod_rtp, nidx_rtp,      &
!!     &          irt_rtp_smp_stack, ncomp_bwd, n_WR, irev_sr_rtp,      &
!!     &          WR, X_rtp, ispack3_s)
!!        integer(kind = kint_gl), intent(in) :: ncomp_bwd
!!        integer(kind = kint_gl), intent(in) :: nnod_rtp, nphi_rtp
!!        integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!!        integer(kind = kint), intent(in) :: n_WR
!!        integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!!        real (kind=kreal), intent(inout):: WR(n_WR)
!!        real(kind = kreal), intent(inout)                             &
!!       &     :: X_rtp(irt_rtp_smp_stack(np_smp),nphi_rtp,ncomp_bwd)
!!        type(work_for_single_ispack3), intent(inout) :: ispack3_s
!! ------------------------------------------------------------------
!!
!! wrapper subroutine for backward Fourier transform by ISPACK
!!
!! x_{k} = a_{0} + (-1)^{j} a_{Nfft/2} + sum_{k=1}^{Nfft/2-1}
!! (a_{k} \cos(2\pijk/Nfft) + b_{k} \sin(2\pijk/Nfft))
!!
!! ------------------------------------------------------------------
!!
!! i = 1:     a_{0}
!! i = 2:     a_{Nfft/2}
!! i = 3:     a_{1}
!! i = 4:     b_{1}
!! ...
!! i = 2*k+1: a_{k}
!! i = 2*k+2: b_{k}
!! ...
!! i = Nfft-1:   a_{Nfft/2-1}
!! i = Nfft:     b_{Nfft/2-1}
!!
!! ------------------------------------------------------------------
!!@endverbatim
!!
!!@n @param Nsmp  Number of SMP processors
!!@n @param Nstacksmp(0:Nsmp)   End number for each SMP process
!!@n @param M           Number of components for Fourier transforms
!!@n @param Nfft        Data length for eadh FFT
!!@n @param X(M, Nfft)  Data for Fourier transform
!
      module t_sph_single_ISPACK3_FFT
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_elapsed_labels_SPH_TRNS
      use calypso_mpi
!
      implicit none
!
!
!>      Structure to use ISPACK
      type work_for_single_ispack3
!>        Data for multiple Fourier transform
        real(kind = 8), allocatable :: X(:,:)
!>        Work constants for ISPACK
        real(kind = 8), allocatable :: T(:)
!>        Work area for ISPACK
        integer(kind = kint_gl), allocatable :: IT(:)
!
!>        temporal area for time count
        real(kind = kreal), allocatable :: t_omp(:,:)
      end type work_for_single_ispack3
!
      private :: alloc_work_single_ispack3
      private :: dealloc_work_single_ispack3
      private :: copy_single_ISPACK3_to_send
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_single_ISPACK3(nphi_rtp, ispack3_s)
!
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: nphi_rtp
!
      type(work_for_single_ispack3), intent(inout) :: ispack3_s
!
!
      call alloc_work_single_ispack3(nphi_rtp, ispack3_s)
      call FXRINI(cast_long(nphi_rtp), ispack3_s%IT, ispack3_s%T)
!
      allocate(ispack3_s%t_omp(np_smp,0:3))
      ispack3_s%t_omp = 0.0d0
!
      end subroutine init_sph_single_ISPACK3
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_single_ISPACK3(ispack3_s)
!
      type(work_for_single_ispack3), intent(inout) :: ispack3_s
!
!
      call dealloc_work_single_ispack3(ispack3_s)
      deallocate(ispack3_s%t_omp)
!
      end subroutine finalize_sph_single_ISPACK3
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_single_ISPACK3(nphi_rtp, ispack3_s)
!
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: nphi_rtp
!
      type(work_for_single_ispack3), intent(inout) :: ispack3_s
!
!
      if((2*nphi_rtp) .ne. size(ispack3_s%T)) then
!
        if(allocated(ispack3_s%T) .eqv. .false.) then
          call alloc_work_single_ispack3(nphi_rtp, ispack3_s)
        else if( (2*nphi_rtp) .gt. size(ispack3_s%T) ) then
          call dealloc_work_single_ispack3(ispack3_s)
          call alloc_work_single_ispack3(nphi_rtp, ispack3_s)
        end if
!
        call FXRINI(cast_long(nphi_rtp), ispack3_s%IT, ispack3_s%T)
      end if
!
      end subroutine verify_sph_single_ISPACK3
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sph_single_FXRTFA_to_send(nnod_rtp, nphi_rtp,          &
     &          irt_rtp_smp_stack, ncomp_fwd, n_WS, irev_sr_rtp,        &
     &          X_rtp, WS, ispack3_s)
!
      use transfer_to_long_integers
      use copy_single_FFT_and_rtp
!
      integer(kind = kint), intent(in) :: ncomp_fwd
      integer(kind = kint), intent(in) :: nnod_rtp, nphi_rtp
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      real(kind = kreal), intent(in)                                    &
     &     :: X_rtp(irt_rtp_smp_stack(np_smp),nphi_rtp,ncomp_fwd)
!
      integer(kind = kint), intent(in) :: n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      type(work_for_single_ispack3), intent(inout) :: ispack3_s
!
      integer(kind = kint) :: j, nd
      integer(kind = kint) :: m, ip, ist, ied
!
!
      if(iflag_FFT_time) then
!$omp parallel workshare
        ispack3_s%t_omp(1:np_smp,0:3) = 0
!$omp end parallel workshare
      end if
!
!$omp parallel do private(ip,m,j,nd,ist,ied)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1) + 1
        ied = irt_rtp_smp_stack(ip)
        do j = ist, ied
          do nd = 1, ncomp_fwd
!
            if(iflag_FFT_time) ispack3_s%t_omp(ip,0) = MPI_WTIME()
            do m = 1, nphi_rtp/2
              ispack3_s%X(2*m-1,ip) = X_rtp(j,2*m-1,nd)
              ispack3_s%X(2*m,  ip) = X_rtp(j,2*m,  nd)
            end do
            if(iflag_FFT_time) ispack3_s%t_omp(ip,1)                    &
     &                        = ispack3_s%t_omp(ip,1)                   &
     &                         + MPI_WTIME() - ispack3_s%t_omp(ip,0)
!
            if(iflag_FFT_time) ispack3_s%t_omp(ip,0) = MPI_WTIME()
            call FXRTFA(cast_long(1), cast_long(nphi_rtp),              &
     &          ispack3_s%X(1,ip), ispack3_s%IT(1), ispack3_s%T(1))
            if(iflag_FFT_time) ispack3_s%t_omp(ip,2)                    &
     &                        = ispack3_s%t_omp(ip,2)                   &
     &                         + MPI_WTIME() - ispack3_s%t_omp(ip,0)
!
            if(iflag_FFT_time) ispack3_s%t_omp(ip,0) = MPI_WTIME()
            call copy_single_ISPACK3_to_send                            &
     &         (nd, j, nnod_rtp, irev_sr_rtp,         &
     &          nphi_rtp, irt_rtp_smp_stack(np_smp),        &
     &          ncomp_fwd, ispack3_s%X(1,ip), n_WS, WS)
            if(iflag_FFT_time) ispack3_s%t_omp(ip,3)                    &
     &                        = ispack3_s%t_omp(ip,3)                   &
     &                         + MPI_WTIME() - ispack3_s%t_omp(ip,0)
!
          end do
        end do
      end do
!$omp end parallel do
!
      if(iflag_FFT_time) then
        call sum_omp_elapsed_4_FFT(np_smp, ispack3_s%t_omp(1,1),        &
     &                             elps1%elapsed(ist_elapsed_FFT+4))
      end if
!
      end subroutine sph_single_FXRTFA_to_send
!
! ------------------------------------------------------------------
!
      subroutine sph_single_FXRTBA_from_recv(nnod_rtp, nphi_rtp,        &
     &          irt_rtp_smp_stack, ncomp_bwd, n_WR, irev_sr_rtp,        &
     &          WR, X_rtp, ispack3_s)
!
      use transfer_to_long_integers
      use copy_single_FFT_and_rtp
!
      integer(kind = kint), intent(in) :: ncomp_bwd
      integer(kind = kint), intent(in) :: nnod_rtp, nphi_rtp
      integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: n_WR
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WR(n_WR)
!
      real(kind = kreal), intent(inout)                                 &
     &     :: X_rtp(irt_rtp_smp_stack(np_smp),nphi_rtp,ncomp_bwd)
!
      type(work_for_single_ispack3), intent(inout) :: ispack3_s
!
      integer(kind = kint_gl) ::  m, j, ip, ist, ied, nd
      integer(kind = kint_gl) :: ic_rtp, is_rtp, ic_recv, is_recv
!
!
      if(iflag_FFT_time) then
!$omp parallel workshare
        ispack3_s%t_omp(1:np_smp,0:3) = 0
!$omp end parallel workshare
      end if
!
!$omp parallel do private(ip,m,j,ist,ied,nd,                            &
!$omp&                    ic_rtp,is_rtp,ic_recv,is_recv)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1) + 1
        ied = irt_rtp_smp_stack(ip)
        do j = ist, ied
          do nd = 1, ncomp_bwd
!
            if(iflag_FFT_time) ispack3_s%t_omp(ip,0) = MPI_WTIME()
            ic_rtp = j
            is_rtp = j + irt_rtp_smp_stack(np_smp)
            ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
            is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
            ispack3_s%X(1,ip) = WR(ic_recv)
            ispack3_s%X(2,ip) = WR(is_recv)
            do m = 2, nphi_rtp/2
              ic_rtp = j + (2*m-2) * irt_rtp_smp_stack(np_smp)
              is_rtp = j + (2*m-1) * irt_rtp_smp_stack(np_smp)
              ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
              is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
              ispack3_s%X(2*m-1,ip) =  half * WR(ic_recv)
              ispack3_s%X(2*m,  ip) = -half * WR(is_recv)
            end do
            if(iflag_FFT_time) ispack3_s%t_omp(ip,1)                    &
     &                        = ispack3_s%t_omp(ip,1)                   &
     &                         + MPI_WTIME() - ispack3_s%t_omp(ip,0)
!
            if(iflag_FFT_time) ispack3_s%t_omp(ip,0) = MPI_WTIME()
            call FXRTBA(cast_long(1), cast_long(nphi_rtp),              &
     &          ispack3_s%X(1,ip), ispack3_s%IT(1), ispack3_s%T(1))
            if(iflag_FFT_time) ispack3_s%t_omp(ip,2)                    &
     &                        = ispack3_s%t_omp(ip,2)                   &
     &                         + MPI_WTIME() - ispack3_s%t_omp(ip,0)
!
            if(iflag_FFT_time) ispack3_s%t_omp(ip,0) = MPI_WTIME()
            do m = 1, nphi_rtp/2
              X_rtp(j,2*m-1,nd) = ispack3_s%X(2*m-1,ip)
              X_rtp(j,2*m,  nd) = ispack3_s%X(2*m,  ip)
            end do
            if(iflag_FFT_time) ispack3_s%t_omp(ip,3)                    &
     &                        = ispack3_s%t_omp(ip,3)                   &
     &                         + MPI_WTIME() - ispack3_s%t_omp(ip,0)
          end do
        end do
      end do
!$omp end parallel do
!
      if(iflag_FFT_time) then
        call sum_omp_elapsed_4_FFT(np_smp, ispack3_s%t_omp(1,1),        &
     &                             elps1%elapsed(ist_elapsed_FFT+1))
      end if
!
      end subroutine sph_single_FXRTBA_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_work_single_ispack3(Nfft, ispack3_s)
!
      integer(kind = kint), intent(in) :: Nfft
      type(work_for_single_ispack3), intent(inout) :: ispack3_s
!
!
      allocate( ispack3_s%X(Nfft,np_smp) )
      ispack3_s%X = 0.0d0
!
      allocate( ispack3_s%T(Nfft+Nfft/2) )
      allocate( ispack3_s%IT(Nfft/2) )
      ispack3_s%T =  0.0d0
      ispack3_s%IT = 0
!
      end subroutine alloc_work_single_ispack3
!
! ------------------------------------------------------------------
!
      subroutine dealloc_work_single_ispack3(ispack3_s)
!
      type(work_for_single_ispack3), intent(inout) :: ispack3_s
!
!
      deallocate(ispack3_s%X)
      deallocate(ispack3_s%T, ispack3_s%IT)
!
      end subroutine dealloc_work_single_ispack3
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine copy_single_ISPACK3_to_send                            &
     &         (nd, j, nnod_rtp, irev_sr_rtp, nphi_rtp, nnod_rt,        &
     &          ncomp_fwd, X_fft, n_WS, WS)
!
      integer(kind = kint), intent(in) :: nd, j
      integer(kind = kint), intent(in) :: nnod_rtp
      integer(kind = kint), intent(in) :: nphi_rtp, nnod_rt
!
      integer(kind = kint), intent(in) :: ncomp_fwd
      real(kind = kreal), intent(in) :: X_fft(nphi_rtp)
!
      integer(kind = kint), intent(in) :: n_WS
      integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
      real (kind=kreal), intent(inout):: WS(n_WS)
!
      integer(kind = kint) :: m, ic_rtp, is_rtp, ic_send, is_send
!
!
      ic_rtp = j
      is_rtp = j + nnod_rt
      ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
      is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
      WS(ic_send) = X_fft(1)
      WS(is_send) = X_fft(2)
      do m = 2, nphi_rtp/2
        ic_rtp = j + (2*m-2) * nnod_rt
        is_rtp = j + (2*m-1) * nnod_rt
        ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
        is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
        WS(ic_send) =   two * X_fft(2*m-1)
        WS(is_send) = - two * X_fft(2*m  )
      end do
!
      end subroutine copy_single_ISPACK3_to_send
!
! ------------------------------------------------------------------
!
      end module t_sph_single_ISPACK3_FFT
