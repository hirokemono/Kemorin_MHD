!>@file   interpolate_SR_N.f90
!!@brief  module interpolate_SR_N
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2006
!
!>@brief  Arbitrary components data communication
!!@n      for interpolation between two meshes
!!
!!@verbatim
!!      subroutine interpolate_send_recv_N                              &
!!     &      (npe_send, isend_self, nnod_send, id_pe_send, istack_send,&
!!     &       npe_recv, irecv_self, nnod_recv, id_pe_recv, istack_recv,&
!!     &       inod_import, nnod_org, NB, X_org, nnod_new, X_new)
!!@endverbatim
!!
!!@n @param  NB    Number of components for communication
!!@n @param  nnod_org    Number of data points for origin
!!@n @param  nnod_new    Number of components for destination
!!
!!@n @param  npe_send    Number of processses to send
!!@n @param  isend_self  Integer flag to copy within own process
!!@n @param  nnod_send   Number of data points to send
!!@n @param  id_pe_send(npe_send)      Process ID to send
!!@n @param  istack_send(0:npe_send)
!!                    End points of send buffer for each process
!!
!!@n @param  npe_recv    Number of processses to receive
!!@n @param  irecv_self  Integer flag to copy within own process
!!@n @param  nnod_recv   Number of data points to receive
!!@n @param  id_pe_recv(npe_send)      Process ID to receive
!!@n @param  istack_recv(0:npe_send)
!!                    End points of receive buffer for each process
!!@n @param  inod_import(nnod_recv)
!!                    local node ID to copy from receive buffer
!!
!!@n @param  X_org(NB*nnod_org)   Send data
!!@n @param  X_new(NB*nnod_new)   Received data
!
      module interpolate_SR_N
!
      use calypso_mpi
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine interpolate_send_recv_N                                &
     &       (npe_send, isend_self, nnod_send, id_pe_send, istack_send, &
     &        npe_recv, irecv_self, nnod_recv, id_pe_recv, istack_recv, &
     &        inod_import, nnod_org, NB, X_org, nnod_new, X_new)
!
      use m_solver_SR
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in) :: npe_send
      integer(kind = kint), intent(in) :: isend_self
      integer(kind = kint), intent(in) :: nnod_send
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
      integer(kind = kint ), intent(in) :: istack_send(0:npe_send)
!
      integer(kind = kint), intent(in) :: npe_recv
      integer(kind = kint), intent(in) :: irecv_self
      integer(kind = kint), intent(in) :: nnod_recv
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind=kint ), intent(in) :: inod_import(nnod_recv)
!
      real   (kind=kreal), intent(in):: X_org(NB*nnod_org)
      real   (kind=kreal), intent(inout):: X_new(NB*nnod_new)
!
      integer (kind = kint) :: neib, istart, inum, ierr
      integer (kind = kint) :: i, ist, ied, j, jst, jed, k, kst, ked
      integer (kind = kint) :: ncomm_send, ncomm_recv
      integer (kind = kint) :: ist_send, ist_recv
!
!
      call resize_work_itp_SR(NB, npe_send, npe_recv, nnod_recv)
!
      ncomm_send = npe_send - isend_self
      ncomm_recv = npe_recv - irecv_self
!
!C-- SEND
!
      do neib = 1, ncomm_send
        istart= NB *  istack_send(neib-1) + 1
        inum  = NB * (istack_send(neib  ) - istack_send(neib-1) )
        call MPI_ISEND(X_org(istart), inum, CALYPSO_REAL,               &
     &      id_pe_send(neib), 0, CALYPSO_COMM, req1(neib), ierr)
      end do
!C
!C-- RECEIVE
      do neib= 1, ncomm_recv
        istart= NB *  istack_recv(neib-1) + 1
        inum  = NB * (istack_recv(neib  ) - istack_recv(neib-1) )
        call MPI_IRECV(WR(istart), inum, CALYPSO_REAL,                  &
     &      id_pe_recv(neib), 0, CALYPSO_COMM, req2(neib), ierr)
      end do
!
      call MPI_WAITALL (ncomm_recv, req2, sta2, ierr)
!
!    copy in same domain
!
      if (isend_self .eq. 1) then
        ist_send= NB *  istack_send(npe_send-1)
        ist_recv= NB *  istack_recv(npe_recv-1)
        inum  = NB * (istack_send(npe_send  )                           &
     &         - istack_send(npe_send-1) )
        do i = 1, inum
          WR(ist_recv+i) = X_org(ist_send+i)
        end do
      end if

      do neib = 1, npe_recv
        ist = istack_recv(neib-1) + 1
        ied  = istack_recv(neib  )
        do k= ist, ied
          j = inod_import(k)
          jst = NB * (j-1) + 1
          jed = NB * j
          kst = NB * (k-1) + 1
          ked = NB * k
          X_new(jst:jed) = WR(kst:ked)
        end do
      end do

      call MPI_WAITALL (ncomm_send, req1, sta1, ierr)
!
      end subroutine interpolate_send_recv_N
!
! ----------------------------------------------------------------------
!
      end module interpolate_SR_N
