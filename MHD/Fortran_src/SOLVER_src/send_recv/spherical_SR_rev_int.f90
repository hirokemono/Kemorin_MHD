!
!     module spherical_SR_rev_int
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine sph_send_recv_by_rev_int(nnod_org, nnod_new,          &
!     &                           npe_send, isend_self, nnod_send,      &
!     &                           id_pe_send, istack_send, inod_export, &
!     &                           npe_recv, irecv_self, nnod_recv,      &
!     &                           id_pe_recv, istack_recv, irev_import, &
!     &                           iX_org, iX_new, SOLVER_COMM)
!
      module spherical_SR_rev_int
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sph_send_recv_by_rev_int(nnod_org, nnod_new,           &
     &                            npe_send, isend_self, nnod_send,      &
     &                            id_pe_send, istack_send, inod_export, &
     &                            npe_recv, irecv_self, nnod_recv,      &
     &                            id_pe_recv, istack_recv, irev_import, &
     &                            iX_org, iX_new, SOLVER_COMM)
!
      use calypso_mpi
      use m_solver_SR
!
      integer, intent(in)   :: SOLVER_COMM
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: nnod_new
!
      integer(kind = kint), intent(in) :: npe_send, isend_self
      integer(kind = kint), intent(in) :: nnod_send
      integer(kind = kint), intent(in) :: id_pe_send(npe_send)
      integer(kind = kint), intent(in) :: istack_send(0:npe_send)
      integer(kind = kint), intent(in) :: inod_export(nnod_send)
!
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: nnod_recv
      integer(kind = kint), intent(in) :: id_pe_recv(npe_recv)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_recv)
      integer(kind = kint), intent(in) :: irev_import(nnod_new)
!
      integer (kind=kint), intent(in):: iX_org(nnod_org)
!
      integer (kind=kint), intent(inout):: iX_new(nnod_new)
!
      integer (kind = kint) :: neib, ist, inum, ierr
      integer (kind = kint) :: i, j, k
      integer (kind = kint) :: ncomm_send, ncomm_recv
      integer (kind = kint) :: ist_send, ist_recv
!
!
      call resize_iwork_sph_SR(npe_send, npe_recv,                      &
     &    nnod_send, nnod_recv)
!
      ncomm_send = npe_send - isend_self
      ncomm_recv = npe_recv - irecv_self
!
!C-- SEND
!
!$omp parallel do private(k,j)
      do k = 1, istack_send(npe_send)
        j = inod_export(k)
        iWS(k)= iX_org(j)
      end do
!$omp end parallel do
!C
      do neib = 1, ncomm_send
        ist= istack_send(neib-1) + 1
        inum  = istack_send(neib  ) - istack_send(neib-1)
        call MPI_ISEND(iWS(ist), inum, MPI_INTEGER,                     &
     &      id_pe_send(neib), 0, SOLVER_COMM, req1(neib), ierr)
      end do
!C
!C-- RECEIVE
      if(ncomm_recv .gt. 0) then
        do neib= 1, ncomm_recv
          ist= istack_recv(neib-1) + 1
          inum  = istack_recv(neib  ) - istack_recv(neib-1)
          call MPI_IRECV(iWR(ist), inum, MPI_INTEGER,                   &
     &        id_pe_recv(neib), 0, SOLVER_COMM, req2(neib), ierr)
        end do
!
        call MPI_WAITALL (ncomm_recv, req2, sta2, ierr)
      end if
!
      if (isend_self .eq. 1) then
        ist_send= istack_send(npe_send-1)
        ist_recv= istack_recv(npe_recv-1)
        inum  =   istack_send(npe_send  ) - istack_send(npe_send-1) 
!$omp parallel do
        do i = 1, inum
          iWR(ist_recv+i) = iWS(ist_send+i)
        end do
!$omp end parallel do
      end if
!
      ist = istack_recv(npe_recv)
      iWR(ist+1) = 0
!
!
!$omp parallel do private(j,k)
      do k = 1, nnod_new
        j = irev_import(k)
        iX_new(k) = iWR(j)
      end do
!$omp end parallel do
!
      if(ncomm_send .gt. 0) then
        call MPI_WAITALL (ncomm_send, req1, sta1, ierr)
      end if
!
      end subroutine sph_send_recv_by_rev_int
!
! ----------------------------------------------------------------------
!
      end module spherical_SR_rev_int
