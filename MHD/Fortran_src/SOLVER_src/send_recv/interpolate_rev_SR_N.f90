!>@file   interpolate_rev_SR_N.f90
!!@brief  module interpolate_rev_SR_N
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2009
!
!>@brief  Arbitrary components data communication
!!@n      for reverse interpolation between two meshes
!!
!!@verbatim
!!      subroutine interpolate_reverse_SR_N                             &
!!     &      (npe_send, isend_self, nnod_send, id_pe_send, istack_send,&
!!     &       npe_recv, irecv_self, nnod_recv, id_pe_recv, istack_recv,&
!!     &       inod_import, nnod_org, NB, X_org, nnod_new, X_new,       &
!!     &       SOLVER_COMM)
!!@endverbatim
!!
!!@n @param  NB    Number of components for communication
!!@n @param  nnod_org    Number of data points for origin
!!@n @param  nnod_new    Number of components for destination
!!
!!@n @param  npe_send    Number of processses to receive
!!@n @param  isend_self  Integer flag to copy within own process
!!@n @param  nnod_send   Number of data points to receive
!!@n @param  id_pe_send(npe_send)      Process ID to receive
!!@n @param  istack_send(0:npe_send)
!!                    End points of receive buffer for each process
!!
!!@n @param  npe_recv    Number of processses to send
!!@n @param  irecv_self  Integer flag to copy within own process
!!@n @param  nnod_recv   Number of data points to send
!!@n @param  id_pe_recv(npe_send)      Process ID to send
!!@n @param  istack_recv(0:npe_send)
!!                    End points of send buffer for each process
!!@n @param  inod_import(nnod_recv)
!!                    local node ID to copy to send buffer
!!
!!@n @param  X_new(NB*nnod_new)   Send data
!!@n @param  X_org(NB*nnod_org)   Received data
!!@n
!!@n @param  SOLVER_COMM          MPI communicator
!
      module interpolate_rev_SR_N
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
      subroutine interpolate_reverse_SR_N                               &
     &       (npe_send, isend_self, nnod_send, id_pe_send, istack_send, &
     &        npe_recv, irecv_self, nnod_recv, id_pe_recv, istack_recv, &
     &        inod_import, nnod_org, NB, X_org, nnod_new, X_new,        &
     &        SOLVER_COMM)
!
      use m_solver_SR
!
      integer, intent(in)   :: SOLVER_COMM
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: nnod_send
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
      integer(kind = kint ), intent(in) :: istack_send(0:npe_send)
!
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: nnod_recv
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind=kint ), intent(in) :: inod_import(nnod_recv)
!
      real   (kind=kreal), intent(in):: X_new(NB*nnod_new)
      real   (kind=kreal), intent(inout):: X_org(NB*nnod_org)
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
      do neib = 1, npe_recv
        ist = istack_recv(neib-1) + 1
        ied  = istack_recv(neib  )
        do k= ist, ied
          j = inod_import(k)
          jst = NB * (j-1) + 1
          jed = NB * j
          kst = NB * (k-1) + 1
          ked = NB * k
          WR(kst:ked) = X_new(jst:jed)
        end do
      end do
!
      do neib= 1, ncomm_recv
        istart= NB *  istack_recv(neib-1) + 1
        inum  = NB * (istack_recv(neib  ) - istack_recv(neib-1) )
        call MPI_ISEND(WR(istart), inum, MPI_DOUBLE_PRECISION,          &
     &      id_pe_recv(neib), 0, SOLVER_COMM, req2(neib), ierr)
      end do
!
!C
!C-- RECEIVE
      do neib = 1, ncomm_send
        istart= NB *  istack_send(neib-1) + 1
        inum  = NB * (istack_send(neib  ) - istack_send(neib-1) )
        call MPI_IRECV(X_org(istart), inum, MPI_DOUBLE_PRECISION,       &
     &      id_pe_send(neib), 0, SOLVER_COMM, req1(neib), ierr)
      end do
!
      call MPI_WAITALL (ncomm_send, req1, sta1, ierr)
!
!    copy in same domain
!
      if (isend_self .eq. 1) then
        ist_send= NB *  istack_send(npe_send-1)
        ist_recv= NB *  istack_recv(npe_recv-1)
        inum  = NB * (istack_send(npe_send  )                           &
     &         - istack_send(npe_send-1) )
        do i = 1, inum
          X_org(ist_send+i) = WR(ist_recv+i)
        end do
      end if
!
      call MPI_WAITALL (ncomm_recv, req2, sta2, ierr)
!
      end subroutine interpolate_reverse_SR_N
!
! ----------------------------------------------------------------------
!
      end module interpolate_rev_SR_N
