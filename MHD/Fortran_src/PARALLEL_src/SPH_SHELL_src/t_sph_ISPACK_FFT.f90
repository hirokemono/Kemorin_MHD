!>@file   t_sph_ISPACK_FFT.f90
!!@brief  module t_sph_ISPACK_FFT
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
!!      subroutine init_sph_ISPACK(nnod_rtp, nphi_rtp,                  &
!!     &                           ncomp_bwd, ncomp_fwd, ispack_t)
!!      subroutine finalize_sph_ISPACK(ispack_t)
!!      subroutine verify_sph_ISPACK(nnod_rtp, nphi_rtp,                &
!!     &                             ncomp_bwd, ncomp_fwd, ispack_t)
!! ------------------------------------------------------------------
!! wrapper subroutine for initierize FFT for ISPACK
!! ------------------------------------------------------------------
!!
!!      subroutine sph_FTTRUF_to_send(nnod_rtp, nidx_rtp,               &
!!     &          irt_rtp_smp_stack, ncomp_fwd, n_WS, irev_sr_rtp,      &
!!     &          X_rtp, WS, ispack_t)
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
!!      subroutine sph_FTTRUB_from_recv(nnod_rtp, nidx_rtp,             &
!!     &          irt_rtp_smp_stack, ncomp_bwd, n_WR, irev_sr_rtp,      &
!!     &          WR, X_rtp, ispack_t)
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
      module t_sph_ISPACK_FFT
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
      type work_for_ispack
!>        Work constants for ISPACK
        real(kind = 8), allocatable :: T(:)
!>        Work area for ISPACK
        integer(kind = 4) :: IT(5)
!
!>        Data for multiple Fourier transform
        real(kind = 8), allocatable :: X(:)
!>        Work area for ISPACK
        real(kind = 8), allocatable :: WK(:)
      end type work_for_ispack
!
      private :: alloc_work_4_ispack, alloc_const_4_ispack
      private :: dealloc_work_4_ispack, dealloc_const_4_ispack
!
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine init_sph_ISPACK(nnod_rtp, nphi_rtp,                    &
     &                           ncomp_bwd, ncomp_fwd, ispack_t)
!
      use ispack_0931
!
      integer(kind = kint), intent(in) :: nnod_rtp, nphi_rtp
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
!
      type(work_for_ispack), intent(inout) :: ispack_t
!
      integer(kind = kint) :: ncomp
!
!
      ncomp = max(ncomp_bwd, ncomp_fwd)
      call alloc_const_4_ispack(nphi_rtp, ispack_t)
      call FTTRUI(nphi_rtp, ispack_t%IT, ispack_t%T)
!
      call alloc_work_4_ispack(nnod_rtp, ncomp, ispack_t)
!
      end subroutine init_sph_ISPACK
!
! ------------------------------------------------------------------
!
      subroutine finalize_sph_ISPACK(ispack_t)
!
      type(work_for_ispack), intent(inout) :: ispack_t
!
!
      call dealloc_const_4_ispack(ispack_t)
      call dealloc_work_4_ispack(ispack_t)
!
      end subroutine finalize_sph_ISPACK
!
! ------------------------------------------------------------------
!
      subroutine verify_sph_ISPACK(nnod_rtp, nphi_rtp,                  &
     &                             ncomp_bwd, ncomp_fwd, ispack_t)
!
      use ispack_0931
!
      integer(kind = kint), intent(in) :: ncomp_bwd, ncomp_fwd
      integer(kind = kint), intent(in) :: nnod_rtp, nphi_rtp
!
      type(work_for_ispack), intent(inout) :: ispack_t
!
      integer(kind = kint) :: ncomp
!
!
      ncomp = max(ncomp_bwd, ncomp_fwd)
!
      if((2*nphi_rtp) .ne. size(ispack_t%T)) then
!
        if(allocated(ispack_t%T) .eqv. .false.) then
          call alloc_const_4_ispack(nphi_rtp, ispack_t)
        else if( (2*nphi_rtp) .gt. size(ispack_t%T) ) then
          call dealloc_const_4_ispack(ispack_t)
          call alloc_const_4_ispack(nphi_rtp, ispack_t)
        end if
!
        call FTTRUI( nphi_rtp, ispack_t%IT, ispack_t%T )
      end if
!
      if(ALLOCATED(ispack_t%X) .eqv. .false.) then
        call alloc_work_4_ispack(nnod_rtp, ncomp, ispack_t)
      else if( (ncomp*nnod_rtp) .gt. size(ispack_t%X,1) ) then
        call dealloc_work_4_ispack(ispack_t)
        call alloc_work_4_ispack(nnod_rtp, ncomp, ispack_t)
      end if
!
      end subroutine verify_sph_ISPACK
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine sph_FTTRUF_to_send(nnod_rtp, nidx_rtp,                 &
     &          irt_rtp_smp_stack, ncomp_fwd, n_WS, irev_sr_rtp,        &
     &          X_rtp, WS, ispack_t)
!
      use ispack_0931
      use set_comm_table_rtp_ISPACK
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
!
      type(work_for_ispack), intent(inout) :: ispack_t
!
      integer(kind = kint) :: m, j, ip, ist, num, ntot, nd, ist_fft
      integer(kind = kint) :: ic_rtp, is_rtp, ic_send, is_send
      integer(kind = kint) :: inum, inod_s, inod_c
!
!
      if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+4)
!$omp parallel do private(ip,m,j,nd,ist,num,ntot,inum,inod_s,inod_c,    &
!$omp&                    ic_rtp,is_rtp,ic_send,is_send,ist_fft)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        ntot = ncomp_fwd * num
        ist_fft = ncomp_fwd*nidx_rtp(3)*irt_rtp_smp_stack(ip-1)
!
        do m = 1, nidx_rtp(3)/2
          do j = 1, num
            do nd = 1, ncomp_fwd
              inum = nd + (j-1) * ncomp_fwd
              inod_c = inum + (2*m-2) * ncomp_fwd * num + ist_fft
              inod_s = inum + (2*m-1) * ncomp_fwd * num + ist_fft
              ispack_t%X(inod_c) = X_rtp(j+ist,2*m-1,nd)
              ispack_t%X(inod_s) = X_rtp(j+ist,2*m,  nd)
            end do
          end do
        end do
      end do
!$omp end parallel do
      if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+4)
!
      if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+5)
      call copy_ISPACK_field_to_send(nnod_rtp, nidx_rtp(3),          &
     &    irt_rtp_smp_stack, ncomp_fwd, irev_sr_rtp, ispack_t%X,       &
     &    n_WS, WS)
      if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+6)
!
      end subroutine sph_FTTRUF_to_send
!
! ------------------------------------------------------------------
!
      subroutine sph_FTTRUB_from_recv(nnod_rtp, nidx_rtp,               &
     &          irt_rtp_smp_stack, ncomp_bwd, n_WR, irev_sr_rtp,        &
     &          WR, X_rtp, ispack_t)
!
      use ispack_0931
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
!
      type(work_for_ispack), intent(inout) :: ispack_t
!
      integer(kind = kint) ::  m, j, ip, ist, num, ntot, nd, ist_fft
      integer(kind = kint) :: inum, inod_s, inod_c
      integer(kind = kint) :: ic_rtp, is_rtp, ic_recv, is_recv
!
!
      if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+1)
!$omp parallel do schedule(static)                                      &
!$omp&         private(ip,m,j,ist,num,ntot,inum,nd,inod_s,inod_c,       &
!$omp&                 ic_rtp,is_rtp,ic_recv,is_recv,ist_fft)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        ntot = ncomp_bwd * num
        ist_fft = ncomp_bwd*nidx_rtp(3)*irt_rtp_smp_stack(ip-1)
!
        do j = 1, num
          do nd = 1, ncomp_bwd
            inum = nd + (j-1) * ncomp_bwd
            ic_rtp = j+ist
            is_rtp = j+ist + irt_rtp_smp_stack(np_smp)
            ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
            is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
            inod_c = inum +                   ist_fft
            inod_s = inum + ncomp_bwd * num + ist_fft
            ispack_t%X(inod_c) = WR(ic_recv)
            ispack_t%X(inod_s) = WR(is_recv)
          end do
        end do
        do m = 2, nidx_rtp(3)/2
          do j = 1, num
            do nd = 1, ncomp_bwd
              inum = nd + (j-1) * ncomp_bwd
              inod_c = inum + (2*m-2) * ncomp_bwd * num + ist_fft
              inod_s = inum + (2*m-1) * ncomp_bwd * num + ist_fft
              ic_rtp = j+ist + (2*m-2) * irt_rtp_smp_stack(np_smp)
              is_rtp = j+ist + (2*m-1) * irt_rtp_smp_stack(np_smp)
              ic_recv = nd + (irev_sr_rtp(ic_rtp) - 1) * ncomp_bwd
              is_recv = nd + (irev_sr_rtp(is_rtp) - 1) * ncomp_bwd
              ispack_t%X(inod_c) =  half * WR(ic_recv)
              ispack_t%X(inod_s) = -half * WR(is_recv)
            end do
          end do
        end do
      end do
!$omp end parallel do
      if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+1)
!
      if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+2)
!$omp parallel do schedule(static)                                      &
!$omp&         private(ip,m,j,ist,num,ntot,inum,nd,inod_s,inod_c,       &
!$omp&                 ic_rtp,is_rtp,ic_recv,is_recv,ist_fft)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        ntot = ncomp_bwd * num
        ist_fft = ncomp_bwd*nidx_rtp(3)*irt_rtp_smp_stack(ip-1)
        call FTTRUB(ntot, nidx_rtp(3), ispack_t%X(ist_fft+1),           &
     &      ispack_t%WK(ist_fft+1), ispack_t%IT, ispack_t%T)
      end do
!$omp end parallel do
      if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+2)
!
      if(iflag_FFT_time) call start_elapsed_time(ist_elapsed_FFT+3)
!$omp parallel do schedule(static)                                      &
!$omp&         private(ip,m,j,ist,num,ntot,inum,nd,inod_s,inod_c,       &
!$omp&                 ic_rtp,is_rtp,ic_recv,is_recv,ist_fft)
      do ip = 1, np_smp
        ist = irt_rtp_smp_stack(ip-1)
        num = irt_rtp_smp_stack(ip) - irt_rtp_smp_stack(ip-1)
        ntot = ncomp_bwd * num
        ist_fft = ncomp_bwd*nidx_rtp(3)*irt_rtp_smp_stack(ip-1)
        do m = 1, nidx_rtp(3)/2
          do j = 1, num
            do nd = 1, ncomp_bwd
              inum = nd + (j-1) * ncomp_bwd
              inod_c = inum + (2*m-2) * ncomp_bwd * num + ist_fft
              inod_s = inum + (2*m-1) * ncomp_bwd * num + ist_fft
              X_rtp(j+ist,2*m-1,nd) = ispack_t%X(inod_c)
              X_rtp(j+ist,2*m,  nd) = ispack_t%X(inod_s)
            end do
          end do
        end do
      end do
!$omp end parallel do
      if(iflag_FFT_time) call end_elapsed_time(ist_elapsed_FFT+3)
!
      end subroutine sph_FTTRUB_from_recv
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine alloc_work_4_ispack(nnod_rtp, ncomp, ispack_t)
!
      integer(kind = kint), intent(in) :: nnod_rtp, ncomp
      type(work_for_ispack), intent(inout) :: ispack_t
!
!
      allocate( ispack_t%X(nnod_rtp*ncomp) )
      allocate( ispack_t%WK(nnod_rtp*ncomp) )
!
!$omp parallel workshare
      ispack_t%X =  0.0d0
      ispack_t%WK = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_work_4_ispack
!
! ------------------------------------------------------------------
!
      subroutine alloc_const_4_ispack(nfft, ispack_t)
!
      integer(kind = kint), intent(in) :: nfft
      type(work_for_ispack), intent(inout) :: ispack_t
!
!
      allocate( ispack_t%T(2*nfft) )
      ispack_t%T = 0.0d0
!
      end subroutine alloc_const_4_ispack
!
! ------------------------------------------------------------------
! ------------------------------------------------------------------
!
      subroutine dealloc_work_4_ispack(ispack_t)
!
      type(work_for_ispack), intent(inout) :: ispack_t
!
!
      deallocate(ispack_t%X, ispack_t%WK)
!
      end subroutine dealloc_work_4_ispack
!
! ------------------------------------------------------------------
!
      subroutine dealloc_const_4_ispack(ispack_t)
!
      type(work_for_ispack), intent(inout) :: ispack_t
!
!
      deallocate(ispack_t%T)
!
      end subroutine dealloc_const_4_ispack
!
! ------------------------------------------------------------------
!
      end module t_sph_ISPACK_FFT
