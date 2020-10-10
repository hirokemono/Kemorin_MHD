!>@file   t_sph_ISPACK3_FFT.f90
!!@brief  module t_sph_ISPACK3_FFT
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
!!      subroutine init_sph_ISPACK3(nnod_rtp, nphi_rtp,                 &
!!     &                            ncomp_bwd, ncomp_fwd, ispack3_t)
!!      subroutine finalize_sph_ISPACK3(ispack3_t)
!!      subroutine verify_sph_ISPACK3(nnod_rtp, nphi_rtp,               &
!!     &          ncomp_bwd, ncomp_fwd, ispack3_t)
!!        type(work_for_ispack3), intent(inout) :: ispack3_t
!! ------------------------------------------------------------------
!! wrapper subroutine for initierize FFT for ISPACK
!! ------------------------------------------------------------------
!!
!!        subroutine sph_FXRTFA_to_send(nnod_rtp, nphi_rtp,             &
!!       &          irt_rtp_smp_stack, ncomp_fwd, n_WS, irev_sr_rtp,    &
!!       &          X_rtp, WS, ispack3_t)
!!        integer(kind = kint), intent(in) :: ncomp_fwd
!!        integer(kind = kint), intent(in) :: nnod_rtp, nphi_rtp
!!        integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!!        real(kind = kreal), intent(in)                                &
!!       &     :: X_rtp(irt_rtp_smp_stack(np_smp),nphi_rtp,ncomp_fwd)
!!        integer(kind = kint), intent(in) :: n_WS
!!        integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!!        real (kind=kreal), intent(inout):: WS(n_WS)
!!        type(work_for_ispack3), intent(inout) :: ispack3_t
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
!!      subroutine sph_FXRTBA_from_recv(nnod_rtp, nidx_rtp,             &
!!     &          irt_rtp_smp_stack, ncomp_bwd, n_WR, irev_sr_rtp,      &
!!     &          WR, X_rtp, ispack3_t)
!!        integer(kind = kint_gl), intent(in) :: ncomp_bwd
!!        integer(kind = kint_gl), intent(in) :: nnod_rtp, nphi_rtp
!!        integer(kind = kint), intent(in) :: irt_rtp_smp_stack(0:np_smp)
!!        integer(kind = kint), intent(in) :: n_WR
!!        integer(kind = kint), intent(in) :: irev_sr_rtp(nnod_rtp)
!!        real (kind=kreal), intent(inout):: WR(n_WR)
!!        real(kind = kreal), intent(inout)                             &
!!       &     :: X_rtp(irt_rtp_smp_stack(np_smp),nphi_rtp,ncomp_bwd)
!!        type(work_for_ispack3), intent(inout) :: ispack3_t
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
      module t_sph_ISPACK3_FFT
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_elapsed_labels_SPH_TRNS
      use calypso_mpi
!
      use t_spheric_rtp_data
      use t_sph_trans_comm_tbl
!
      implicit none
!
!
!>      Structure to use ISPACK
      type work_for_ispack3
!>        Work constants for ISPACK
        real(kind = 8), allocatable :: T(:)
!>        Work area for ISPACK
        integer(kind = kint_gl), allocatable :: IT(:)
!
!>        Data for multiple Fourier transform
        real(kind = 8), allocatable :: X(:)
!
!>        temporal area for time count
        real(kind = kreal), allocatable :: t_omp(:,:)
      end type work_for_ispack3
!
      private :: alloc_work_4_ispack3, alloc_const_4_ispack3
      private :: dealloc_work_4_ispack3, dealloc_const_4_ispack3
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_ISPACK3(nnod_rtp, nphi_rtp,                   &
     &                            ncomp_bwd, ncomp_fwd, ispack3_t)
!
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      integer(kind = kint), intent(in) :: nnod_rtp, nphi_rtp
!
      type(work_for_ispack3), intent(inout) :: ispack3_t
!
      integer(kind = kint) :: ncomp
!
!
      ncomp = max(ncomp_bwd, ncomp_fwd)
      call alloc_const_4_ispack3(nphi_rtp, ispack3_t)
      call FXRINI(cast_long(nphi_rtp), ispack3_t%IT, ispack3_t%T)
!
      call alloc_work_4_ispack3(nnod_rtp, ncomp, ispack3_t)
!
      allocate(ispack3_t%t_omp(np_smp,0:3))
      ispack3_t%t_omp = 0.0d0
!
      end subroutine init_sph_ISPACK3
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_ISPACK3(ispack3_t)
!
      type(work_for_ispack3), intent(inout) :: ispack3_t
!
!
      call dealloc_const_4_ispack3(ispack3_t)
      call dealloc_work_4_ispack3(ispack3_t)
      deallocate(ispack3_t%t_omp)
!
      end subroutine finalize_sph_ISPACK3
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_ISPACK3(nnod_rtp, nphi_rtp,                 &
     &          ncomp_bwd, ncomp_fwd, ispack3_t)
!
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      integer(kind = kint), intent(in) :: nnod_rtp, nphi_rtp
!
      type(work_for_ispack3), intent(inout) :: ispack3_t
!
      integer(kind = kint) :: ncomp
!
!
      ncomp = max(ncomp_bwd, ncomp_fwd)
!
      if((2*nphi_rtp) .ne. size(ispack3_t%T)) then
!
        if(allocated(ispack3_t%T) .eqv. .false.) then
          call alloc_const_4_ispack3(nphi_rtp, ispack3_t)
        else if( (2*nphi_rtp) .gt. size(ispack3_t%T) ) then
          call dealloc_const_4_ispack3(ispack3_t)
          call alloc_const_4_ispack3(nphi_rtp, ispack3_t)
        end if
!
        call FXRINI(cast_long(nphi_rtp),                                &
     &              ispack3_t%IT(1), ispack3_t%T(1))
      end if
!
      if(ALLOCATED(ispack3_t%X) .eqv. .false.) then
        call alloc_work_4_ispack3(nnod_rtp, ncomp, ispack3_t)
      else if( (ncomp*nnod_rtp) .gt. size(ispack3_t%X,1) ) then
        call dealloc_work_4_ispack3(ispack3_t)
        call alloc_work_4_ispack3(nnod_rtp, ncomp, ispack3_t)
      end if
!
      end subroutine verify_sph_ISPACK3
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sph_FXRTFA_to_send(nnod_rtp, nphi_rtp,                 &
     &          irt_rtp_smp_stack, ncomp_fwd, n_WS, irev_sr_rtp,        &
     &          X_rtp, WS, ispack3_t)
!
      use transfer_to_long_integers
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
      type(work_for_ispack3), intent(inout) :: ispack3_t
!
      integer(kind = kint) :: m, j, ip, ist, nd
      integer(kind = kint) :: ic_rtp, is_rtp, ic_send, is_send
      integer(kind = kint) :: inod_s, inod_c
      integer(kind = kint) :: num8, inum, ntot, ist_fft
!
!
!$omp parallel do schedule(static)                                      &
!$omp&         private(ip,m,j,nd,ist,num8,ntot,inum,inod_s,inod_c,      &
!$omp&                 ic_rtp,is_rtp,ic_send,is_send,ist_fft)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num8 = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        ntot = ncomp_fwd * num8
        ist_fft = ncomp_fwd*nphi_rtp*irt_rtp_smp_stack(ip-1)
!
        if(iflag_FFT_time) ispack3_t%t_omp(ip,0) = MPI_WTIME()
        do m = 1, nphi_rtp
          do j = 1, num8
            do nd = 1, ncomp_fwd
              inum = nd + (j-1) * ncomp_fwd
              inod_c = inum + (m-1) * ncomp_fwd * num8 + ist_fft
              ispack3_t%X(inod_c) = X_rtp(j+ist,m,nd)
            end do
          end do
        end do
        if(iflag_FFT_time) ispack3_t%t_omp(ip,1)= ispack3_t%t_omp(ip,1) &
     &                    + MPI_WTIME() - ispack3_t%t_omp(ip,0)
      end do
!$omp end parallel do
!
!$omp parallel do schedule(static)                                      &
!$omp&         private(ip,m,j,nd,ist,num8,ntot,inum,inod_s,inod_c,      &
!$omp&                 ic_rtp,is_rtp,ic_send,is_send,ist_fft)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num8 = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        ntot = ncomp_fwd * num8
        ist_fft = ncomp_fwd*nphi_rtp*irt_rtp_smp_stack(ip-1)
        if(iflag_FFT_time) ispack3_t%t_omp(ip,0) = MPI_WTIME()
        call FXRTFA(cast_long(ntot), cast_long(nphi_rtp),               &
     &      ispack3_t%X(ist_fft+1), ispack3_t%IT(1), ispack3_t%T(1))
        if(iflag_FFT_time) ispack3_t%t_omp(ip,2)= ispack3_t%t_omp(ip,2) &
     &                    + MPI_WTIME() - ispack3_t%t_omp(ip,0)
      end do
!$omp end parallel do
!
!$omp parallel do schedule(static)                                      &
!$omp&         private(ip,m,j,nd,ist,num8,ntot,inum,inod_s,inod_c,      &
!$omp&                 ic_rtp,is_rtp,ic_send,is_send,ist_fft)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num8 = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        ntot = ncomp_fwd * num8
        ist_fft = ncomp_fwd*nphi_rtp*irt_rtp_smp_stack(ip-1)
        if(iflag_FFT_time) ispack3_t%t_omp(ip,0) = MPI_WTIME()
        do j = 1, num8
          do nd = 1, ncomp_fwd
            inum = nd + (j-1) * ncomp_fwd
            ic_rtp = j+ist
            is_rtp = j+ist + irt_rtp_smp_stack(np_smp)
            ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
            is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
            inod_c = inum + ist_fft
            inod_s = inum + ncomp_fwd * num8 + ist_fft
            WS(ic_send) = ispack3_t%X(inod_c)
            WS(is_send) = ispack3_t%X(inod_s)
          end do
        end do
        do m = 2, nphi_rtp/2
          do j = 1, num8
            do nd = 1, ncomp_fwd
              inum = nd + (j-1) * ncomp_fwd
              ic_rtp = j+ist + (2*m-2) * irt_rtp_smp_stack(np_smp)
              is_rtp = j+ist + (2*m-1) * irt_rtp_smp_stack(np_smp)
              ic_send = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_fwd
              is_send = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_fwd
              inod_c = inum + (2*m-2) * ncomp_fwd * num8 + ist_fft
              inod_s = inum + (2*m-1) * ncomp_fwd * num8 + ist_fft
              WS(ic_send) =   two * ispack3_t%X(inod_c)
              WS(is_send) = - two * ispack3_t%X(inod_s)
            end do
          end do
        end do
        if(iflag_FFT_time) ispack3_t%t_omp(ip,3)= ispack3_t%t_omp(ip,3) &
     &                    + MPI_WTIME() - ispack3_t%t_omp(ip,0)
!
      end do
!$omp end parallel do
!
      if(iflag_FFT_time) then
        do ip = 2, np_smp
          ispack3_t%t_omp(1,1) = ispack3_t%t_omp(1,1)                   &
     &                         + ispack3_t%t_omp(ip,1)
          ispack3_t%t_omp(1,2) = ispack3_t%t_omp(1,2)                   &
     &                         + ispack3_t%t_omp(ip,2)
          ispack3_t%t_omp(1,3) = ispack3_t%t_omp(1,3)                   &
     &                         + ispack3_t%t_omp(ip,3)
        end do
        elps1%elapsed(ist_elapsed_FFT+4)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+4)                        &
     &         + ispack3_t%t_omp(1,1) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+5)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+5)                        &
     &         + ispack3_t%t_omp(1,2) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+6)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+6)                        &
     &         + ispack3_t%t_omp(1,3) / dble(np_smp)
      end if
!
      end subroutine sph_FXRTFA_to_send
!
! ------------------------------------------------------------------
!
      subroutine sph_FXRTBA_from_recv(nnod_rtp, nphi_rtp,               &
     &          irt_rtp_smp_stack, ncomp_bwd, n_WR, irev_sr_rtp,        &
     &          WR, X_rtp, ispack3_t)
!
      use transfer_to_long_integers
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
      type(work_for_ispack3), intent(inout) :: ispack3_t
!
      integer(kind = kint) ::  m, j, ip, ist, nd
      integer(kind = kint) ::  inod_s, inod_c
      integer(kind = kint) :: ic_rtp, is_rtp, ic_recv, is_recv
      integer(kind = kint) :: num8, inum, ntot, ist_fft
!
!
      if(iflag_FFT_time) then
!$omp parallel workshare
        ispack3_t%t_omp(1:np_smp,0:3) = 0
!$omp end parallel workshare
      end if
!
!$omp parallel do private(ip,m,j,ist,num8,ntot,inum,nd,inod_s,inod_c,   &
!$omp&                    ic_rtp,is_rtp,ic_recv,is_recv,ist_fft)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num8 = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        ntot = ncomp_bwd * num8
        ist_fft = ncomp_bwd*nphi_rtp*irt_rtp_smp_stack(ip-1)
!
        if(iflag_FFT_time) ispack3_t%t_omp(ip,0) = MPI_WTIME()
        do j = 1, num8
          do nd = 1, ncomp_bwd
            inum = nd + (j-1) * ncomp_bwd
            ic_rtp = j+ist
            is_rtp = j+ist + irt_rtp_smp_stack(np_smp)
            ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
            is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
            inod_c = inum +                    ist_fft
            inod_s = inum + ncomp_bwd * num8 + ist_fft
            ispack3_t%X(inod_c) = WR(ic_recv)
            ispack3_t%X(inod_s) = WR(is_recv)
          end do
        end do
        do m = 2, nphi_rtp/2
          do j = 1, num8
            do nd = 1, ncomp_bwd
              inum = nd + (j-1) * ncomp_bwd
              inod_c = inum + (2*m-2) * ncomp_bwd * num8 + ist_fft
              inod_s = inum + (2*m-1) * ncomp_bwd * num8 + ist_fft
              ic_rtp = j+ist + (2*m-2) * irt_rtp_smp_stack(np_smp)
              is_rtp = j+ist + (2*m-1) * irt_rtp_smp_stack(np_smp)
              ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
              is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
              ispack3_t%X(inod_c) =  half * WR(ic_recv)
              ispack3_t%X(inod_s) = -half * WR(is_recv)
            end do
          end do
        end do
        if(iflag_FFT_time) ispack3_t%t_omp(ip,1)= ispack3_t%t_omp(ip,1) &
     &                    + MPI_WTIME() - ispack3_t%t_omp(ip,0)
      end do
!$omp end parallel do
!
!$omp parallel do private(ip,m,j,ist,num8,ntot,inum,nd,inod_s,inod_c,   &
!$omp&                    ic_rtp,is_rtp,ic_recv,is_recv,ist_fft)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num8 = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        ntot = ncomp_bwd * num8
        ist_fft = ncomp_bwd*nphi_rtp*irt_rtp_smp_stack(ip-1)
        if(iflag_FFT_time) ispack3_t%t_omp(ip,0) = MPI_WTIME()
        call FXRTBA(cast_long(ntot), cast_long(nphi_rtp),               &
     &      ispack3_t%X(ist_fft+1), ispack3_t%IT(1), ispack3_t%T(1))
        if(iflag_FFT_time) ispack3_t%t_omp(ip,2)= ispack3_t%t_omp(ip,2) &
     &                    + MPI_WTIME() - ispack3_t%t_omp(ip,0)
      end do
!$omp end parallel do
!
!$omp parallel do private(ip,m,j,ist,num8,ntot,inum,nd,inod_s,inod_c,   &
!$omp&                    ic_rtp,is_rtp,ic_recv,is_recv,ist_fft)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num8 = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        ntot = ncomp_bwd * num8
        ist_fft = ncomp_bwd*nphi_rtp*irt_rtp_smp_stack(ip-1)
        if(iflag_FFT_time) ispack3_t%t_omp(ip,0) = MPI_WTIME()
        do m = 1, nphi_rtp
          do j = 1, num8
            do nd = 1, ncomp_bwd
              inum = nd + (j-1) * ncomp_bwd
              inod_c = inum + (m-1) * ncomp_bwd * num8 + ist_fft
              X_rtp(j+ist,m,nd) = ispack3_t%X(inod_c)
            end do
          end do
        end do
        if(iflag_FFT_time) ispack3_t%t_omp(ip,3)= ispack3_t%t_omp(ip,3) &
     &                    + MPI_WTIME() - ispack3_t%t_omp(ip,0)
      end do
!$omp end parallel do
!
      if(iflag_FFT_time) then
        do ip = 2, np_smp
          ispack3_t%t_omp(1,1) = ispack3_t%t_omp(1,1)                   &
     &                         + ispack3_t%t_omp(ip,1)
          ispack3_t%t_omp(1,2) = ispack3_t%t_omp(1,2)                   &
     &                         + ispack3_t%t_omp(ip,2)
          ispack3_t%t_omp(1,3) = ispack3_t%t_omp(1,3)                   &
     &                         + ispack3_t%t_omp(ip,3)
        end do
        elps1%elapsed(ist_elapsed_FFT+1)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+1)                        &
     &         + ispack3_t%t_omp(1,1) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+2)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+2)                        &
     &         + ispack3_t%t_omp(1,2) / dble(np_smp)
        elps1%elapsed(ist_elapsed_FFT+3)                                &
     &        = elps1%elapsed(ist_elapsed_FFT+3)                        &
     &         + ispack3_t%t_omp(1,3) / dble(np_smp)
      end if
!
      end subroutine sph_FXRTBA_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_work_4_ispack3(nnod_rtp, ncomp, ispack3_t)
!
      integer(kind = kint), intent(in) :: nnod_rtp, ncomp
      type(work_for_ispack3), intent(inout) :: ispack3_t
!
!
      allocate( ispack3_t%X(nnod_rtp*ncomp) )
!
      end subroutine alloc_work_4_ispack3
!
! ------------------------------------------------------------------
!
      subroutine alloc_const_4_ispack3(nfft, ispack3_t)
!
      integer(kind = kint), intent(in) :: nfft
      type(work_for_ispack3), intent(inout) :: ispack3_t
!
!
      allocate( ispack3_t%T(nfft+nfft/2) )
      allocate( ispack3_t%IT(nfft/2) )
      ispack3_t%T =  0.0d0
      ispack3_t%IT = 0
!
      end subroutine alloc_const_4_ispack3
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine dealloc_work_4_ispack3(ispack3_t)
!
      type(work_for_ispack3), intent(inout) :: ispack3_t
!
!
      deallocate(ispack3_t%X)
!
      end subroutine dealloc_work_4_ispack3
!
! ------------------------------------------------------------------
!
      subroutine dealloc_const_4_ispack3(ispack3_t)
!
      type(work_for_ispack3), intent(inout) :: ispack3_t
!
!
      deallocate(ispack3_t%T, ispack3_t%IT)
!
      end subroutine dealloc_const_4_ispack3
!
! ------------------------------------------------------------------
!
      end module t_sph_ISPACK3_FFT
