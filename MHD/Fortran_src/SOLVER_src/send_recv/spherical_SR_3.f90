!
!     module spherical_SR_3
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine sph_send_recv_3(nnod_org, nnod_new,                   &
!     &                           npe_send, isend_self, nnod_send,      &
!     &                           id_pe_send, istack_send, inod_export, &
!     &                           npe_recv, irecv_self, nnod_recv,      &
!     &                           id_pe_recv, istack_recv, inod_import, &
!     &                           X_org, X_new, SOLVER_COMM)
!
      module spherical_SR_3
!
      use m_precision
      use m_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sph_send_recv_3(nnod_org, nnod_new,                    &
     &                           npe_send, isend_self, nnod_send,       &
     &                           id_pe_send, istack_send, inod_export,  &
     &                           npe_recv, irecv_self, nnod_recv,       &
     &                           id_pe_recv, istack_recv, inod_import,  &
     &                           X_org, X_new, SOLVER_COMM)
!
      use calypso_mpi
!
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
      integer(kind = kint), intent(in) :: inod_import(nnod_recv)
!
      real (kind=kreal), intent(in)::    X_org(ithree*nnod_org)
!
      real (kind=kreal), intent(inout):: X_new(ithree*nnod_new)
!
      integer (kind = kint) :: neib, istart, inum, iend, ierr
      integer (kind = kint) :: i, j, k
      integer (kind = kint) :: ncomm_send, ncomm_recv
      integer (kind = kint) :: ist_send, ist_recv
!
!
      call resize_work_sph_SR(ithree, npe_send, npe_recv,               &
     &    nnod_send, nnod_recv)
!
      ncomm_send = npe_send - isend_self
      ncomm_recv = npe_recv - irecv_self
!
!C-- SEND
!
!$omp parallel private(neib,istart,iend)
      do neib = 1, npe_send
        istart= istack_send(neib-1) + 1
        iend  = istack_send(neib  )
!$omp do private(k,j)
        do k= istart, iend
          j = inod_export(k)
          WS(ithree*k-2)= X_org(ithree*j-2)
          WS(ithree*k-1)= X_org(ithree*j-1)
          WS(ithree*k  )= X_org(ithree*j  )
        end do
!$omp end do nowait
      end do
!$omp end parallel
!C
      do neib = 1, ncomm_send
        istart= ithree * istack_send(neib-1) + 1
        inum  = ithree * (istack_send(neib  ) - istack_send(neib-1))
        call MPI_ISEND(WS(istart), inum, MPI_DOUBLE_PRECISION,          &
     &      id_pe_send(neib), 0, SOLVER_COMM, req1(neib), ierr)
      end do
!C
!C-- RECEIVE
      if(ncomm_recv .gt. 0) then
        do neib= 1, ncomm_recv
          istart= ithree * istack_recv(neib-1) + 1
          inum  = ithree * (istack_recv(neib  ) - istack_recv(neib-1))
          call MPI_IRECV(WR(istart), inum, MPI_DOUBLE_PRECISION,        &
     &      id_pe_recv(neib), 0, SOLVER_COMM, req2(neib), ierr)
        end do
!
        call MPI_WAITALL (ncomm_recv, req2, sta2, ierr)
      end if
!
      if (isend_self .eq. 1) then
        ist_send= ithree * istack_send(npe_send-1)
        ist_recv= ithree * istack_recv(npe_recv-1)
        inum = ithree*(istack_send(npe_send) - istack_send(npe_send-1))
!$omp parallel do
        do i = 1, inum
          WR(ist_recv+i) = WS(ist_send+i)
        end do
!$omp end parallel do
      end if
!
!$omp parallel private(neib,istart,iend)
      do neib = 1, npe_recv
        istart = istack_recv(neib-1) + 1
        iend  =  istack_recv(neib  )
!$omp do private(k,j)
        do k= istart, iend
          j = inod_import(k)
          X_new(ithree*j-2) = WR(ithree*k-2)
          X_new(ithree*j-1) = WR(ithree*k-1)
          X_new(ithree*j  ) = WR(ithree*k  )
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      if(ncomm_send .gt. 0) then
        call MPI_WAITALL (ncomm_send, req1, sta1, ierr)
      end if
!
      end subroutine sph_send_recv_3
!
! ----------------------------------------------------------------------
!
      end module spherical_SR_3
