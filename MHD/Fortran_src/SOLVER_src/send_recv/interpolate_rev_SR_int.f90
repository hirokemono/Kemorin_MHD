!
!      module interpolate_rev_SR_int
!
!      Written by H. Matsui on Jan., 2009
!
!      subroutine interpolate_reverse_SR_int                            &
!     &       (npe_send, isend_self, nnod_send, id_pe_send, istack_send,&
!     &        npe_recv, irecv_self, nnod_recv, id_pe_recv, istack_recv,&
!     &        inod_import, numnod, idx_org, nnod_2nd, idx,             &
!     &        SOLVER_COMM, my_rank)
!
      module interpolate_rev_SR_int
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
      subroutine interpolate_reverse_SR_int                             &
     &       (npe_send, isend_self, nnod_send, id_pe_send, istack_send, &
     &        npe_recv, irecv_self, nnod_recv, id_pe_recv, istack_recv, &
     &        inod_import, numnod, idx_org, nnod_2nd, idx,              &
     &        SOLVER_COMM, my_rank)
!
      use m_solver_SR
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
      integer (kind=kint), intent(in):: idx(nnod_2nd)
!       interpolated result vector
      integer (kind=kint), intent(inout):: idx_org(numnod)
!       interpolated result vector
!
      integer (kind = kint) :: neib, istart, inum, iend, ierr
      integer (kind = kint) :: i, j, k
      integer (kind = kint) :: ncomm_send, ncomm_recv
      integer (kind = kint) :: ist_send, ist_recv
!
!
      call resize_iwork_itp_SR(npe_send, npe_recv, nnod_recv)
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
          iWR(k) = idx(j)
        end do
      end do

      do neib= 1, ncomm_recv
        istart= istack_recv(neib-1) + 1
        inum  = istack_recv(neib  ) - istack_recv(neib-1)
        call MPI_ISEND(iWR(istart), inum, MPI_INTEGER,                  &
     &      id_pe_recv(neib), 0, SOLVER_COMM, req2(neib), ierr)
      end do
!C
!C-- RECEIVE
      do neib = 1, ncomm_send
        istart= istack_send(neib-1) + 1
        inum  = istack_send(neib  ) - istack_send(neib-1)
        call MPI_IRECV(idx_org(istart), inum, MPI_INTEGER,              &
     &      id_pe_send(neib), 0, SOLVER_COMM, req1(neib), ierr)
      end do
!
      call MPI_WAITALL (ncomm_send, req1, sta1, ierr)
!
      if (isend_self .eq. 1) then
        ist_send= istack_send(npe_send-1)
        ist_recv= istack_recv(npe_recv-1)
        inum  = istack_send(npe_send  ) - istack_send(npe_send-1) 
        do i = 1, inum
          idx_org(ist_send+i) = iWR(ist_recv+i)
        end do
      end if
!
      call MPI_WAITALL (ncomm_recv, req2, sta2, ierr)
!
      end subroutine interpolate_reverse_SR_int
!
! ----------------------------------------------------------------------
!
      end module interpolate_rev_SR_int
