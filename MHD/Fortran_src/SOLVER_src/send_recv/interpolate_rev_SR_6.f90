!
!      module interpolate_rev_SR_6
!
!      Written by H. Matsui on Jan., 2009
!
!      subroutine interpolate_reverse_SR_6                              &
!     &       (npe_send, isend_self, nnod_send, id_pe_send, istack_send,&
!     &        npe_recv, irecv_self, nnod_recv, id_pe_recv, istack_recv,&
!     &        inod_import, numnod, X_org, nnod_2nd, X,                 &
!     &        SOLVER_COMM, my_rank)
!
      module interpolate_rev_SR_6
!
      use calypso_mpi
!
      use m_precision
      use m_constants
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine interpolate_reverse_SR_6                               &
     &       (npe_send, isend_self, nnod_send, id_pe_send, istack_send, &
     &        npe_recv, irecv_self, nnod_recv, id_pe_recv, istack_recv, &
     &        inod_import, numnod, X_org, nnod_2nd, X,                  &
     &        SOLVER_COMM, my_rank)
!
      use m_solver_SR
!
!
      integer, intent(in)   :: SOLVER_COMM
!       communicator for mpi
      integer(kind = kint), intent(in) :: my_rank
!
      integer(kind = kint), intent(in) :: numnod
!  number of node and component of field
      integer(kind = kint), intent(in) :: nnod_2nd
!  number of node and component of field
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: nnod_send
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
!     destination domiain ID to send    (i-th pe)
      integer(kind = kint ), intent(in) :: istack_send(0:npe_send)
!
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: nnod_recv
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
!     originate domiain ID to get   (i-th pe)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
!       imported node count for each neighbor pe (i-th pe)
      integer(kind=kint ), intent(in) :: inod_import(nnod_recv)
!       imported node                            (i-th dof)
!
      real   (kind=kreal), intent(in):: X(isix*nnod_2nd)
!       interpolated result vector
      real   (kind=kreal), intent(inout):: X_org(isix*numnod)
!       interpolated result vector
!
      integer (kind = kint) :: neib, istart, inum, iend, ierr
      integer (kind = kint) :: i, j, k
      integer (kind = kint) :: ncomm_send, ncomm_recv
      integer (kind = kint) :: ist_send, ist_recv
!
!
!
      call resize_work_itp_SR(isix, npe_send, npe_recv, nnod_recv)
!
      ncomm_send = npe_send - isend_self
      ncomm_recv = npe_recv - irecv_self
!
!C-- SEND
!
      do neib = 1, npe_recv
        istart = istack_recv(neib-1) + 1
        iend  = istack_recv(neib  )
        do k= istart, iend
          j = inod_import(k)
          WR(isix*k-5) = X(isix*j-5)
          WR(isix*k-4) = X(isix*j-4)
          WR(isix*k-3) = X(isix*j-3)
          WR(isix*k-2) = X(isix*j-2)
          WR(isix*k-1) = X(isix*j-1)
          WR(isix*k  ) = X(isix*j  )
        end do
      end do

      do neib= 1, ncomm_recv
        istart= isix *  istack_recv(neib-1) + 1
        inum  = isix * (istack_recv(neib  ) - istack_recv(neib-1) )
        call MPI_ISEND(WR(istart), inum, MPI_DOUBLE_PRECISION,          &
     &      id_pe_recv(neib), 0, SOLVER_COMM, req2(neib), ierr)
      end do
!C
!C-- RECEIVE
!
      do neib = 1, ncomm_send
        istart= isix *  istack_send(neib-1) + 1
        inum  = isix * (istack_send(neib  ) - istack_send(neib-1) )
        call MPI_IRECV(X_org(istart), inum, MPI_DOUBLE_PRECISION,       &
     &      id_pe_send(neib), 0, SOLVER_COMM, req1(neib), ierr)
      end do
!
      call MPI_WAITALL (ncomm_send, req1, sta1, ierr)
!
!    copy in same domain
!
      if (isend_self .eq. 1) then
        ist_send= isix *  istack_send(npe_send-1)
        ist_recv= isix *  istack_recv(npe_recv-1)
        inum  = istack_send(npe_send  ) - istack_send(npe_send-1)
        do i = 1, inum
          X_org(ist_send+isix*i-5) = WR(ist_recv+isix*i-5)
          X_org(ist_send+isix*i-4) = WR(ist_recv+isix*i-4)
          X_org(ist_send+isix*i-3) = WR(ist_recv+isix*i-3)
          X_org(ist_send+isix*i-2) = WR(ist_recv+isix*i-2)
          X_org(ist_send+isix*i-1) = WR(ist_recv+isix*i-1)
          X_org(ist_send+isix*i  ) = WR(ist_recv+isix*i  )
        end do
      end if
!
      call MPI_WAITALL (ncomm_recv, req2, sta2, ierr)
!
      end subroutine interpolate_reverse_SR_6
!
! ----------------------------------------------------------------------
!
      end module interpolate_rev_SR_6
