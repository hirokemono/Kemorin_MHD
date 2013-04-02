!
!      module interpolate_SR_N
!
!      Written by H. Matsui on Sep., 2006
!
!      subroutine interpolate_send_recv_N                               &
!     &       (npe_send, isend_self, nnod_send, id_pe_send, istack_send,&
!     &        npe_recv, irecv_self, nnod_recv, id_pe_recv, istack_recv,&
!     &        inod_import, numnod, numdir, X_org, nnod_2nd, X,         &
!     &        SOLVER_COMM, my_rank)
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
     &        inod_import, numnod, numdir, X_org, nnod_2nd, X,          &
     &        SOLVER_COMM, my_rank)
!
      use m_solver_SR
!
      integer, intent(in)   :: SOLVER_COMM
!<       communicator for mpi
      integer(kind = kint), intent(in) :: my_rank
!<
      integer(kind = kint), intent(in) :: numnod
!<  number of node and component of field
      integer(kind = kint), intent(in) :: nnod_2nd
!<  number of node and component of field
!
      integer(kind = kint), intent(in) :: numdir
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: nnod_send
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
!<     destination domiain ID to send    (i-th pe)
      integer(kind = kint ), intent(in) :: istack_send(0:npe_send)
!<
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: nnod_recv
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
!<     originate domiain ID to get   (i-th pe)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
!<       imported node count for each neighbor pe (i-th pe)
      integer(kind=kint ), intent(in) :: inod_import(nnod_recv)
!<       imported node                            (i-th dof)
!
      real   (kind=kreal), intent(in):: X_org(numdir*numdir)
!<       interpolated result vector
      real   (kind=kreal), intent(inout):: X(numdir*nnod_2nd)
!<       interpolated result vector
!
      integer (kind = kint) :: neib, istart, inum, ierr
      integer (kind = kint) :: i, ist, ied, j, jst, jed, k, kst, ked
      integer (kind = kint) :: ncomm_send, ncomm_recv
      integer (kind = kint) :: ist_send, ist_recv
!
!
      call resize_work_itp_SR(numdir, npe_send, npe_recv, nnod_recv)
!
      ncomm_send = npe_send - isend_self
      ncomm_recv = npe_recv - irecv_self
!
!C-- SEND
!
      do neib = 1, ncomm_send
        istart= numdir *  istack_send(neib-1) + 1
        inum  = numdir * (istack_send(neib  ) - istack_send(neib-1) )
        call MPI_ISEND(X_org(istart), inum, MPI_DOUBLE_PRECISION,       &
     &      id_pe_send(neib), 0, SOLVER_COMM, req1(neib), ierr)
      end do
!C
!C-- RECEIVE
      do neib= 1, ncomm_recv
        istart= numdir *  istack_recv(neib-1) + 1
        inum  = numdir * (istack_recv(neib  ) - istack_recv(neib-1) )
        call MPI_IRECV(WR(istart), inum, MPI_DOUBLE_PRECISION,          &
     &      id_pe_recv(neib), 0, SOLVER_COMM, req2(neib), ierr)
      end do
!
      call MPI_WAITALL (ncomm_recv, req2, sta2, ierr)
!
!    copy in same domain
!
      if (isend_self .eq. 1) then
        ist_send= numdir *  istack_send(npe_send-1)
        ist_recv= numdir *  istack_recv(npe_recv-1)
        inum  = numdir * (istack_send(npe_send  )                       &
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
          jst = numdir * (j-1) + 1
          jed = numdir * j
          kst = numdir * (k-1) + 1
          ked = numdir * k
          X(jst:jed) = WR(kst:ked)
        end do
      end do

      call MPI_WAITALL (ncomm_send, req1, sta1, ierr)
!
      end subroutine interpolate_send_recv_N
!
! ----------------------------------------------------------------------
!
      end module interpolate_SR_N
