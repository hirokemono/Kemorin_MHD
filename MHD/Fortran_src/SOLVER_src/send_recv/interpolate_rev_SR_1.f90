!>@file   interpolate_rev_SR_1.f90
!!@brief  module interpolate_rev_SR_1
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2009
!
!>@brief  Scalar data communication
!!@n      for reverse interpolation between two meshes
!!
!!@verbatim
!!      subroutine interpolate_reverse_SR_1                             &
!!     &      (npe_send, isend_self, nnod_send, id_pe_send, istack_send,&
!!     &       npe_recv, irecv_self, nnod_recv, id_pe_recv, istack_recv,&
!!     &       inod_import, nnod_org, X_org, nnod_new, X_new)
!!@endverbatim
!!
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
!!@n @param  X_new(nnod_new)   Send data
!!@n @param  X_org(nnod_org)   Received data
!
      module interpolate_rev_SR_1
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
      subroutine interpolate_reverse_SR_1                               &
     &       (npe_send, isend_self, nnod_send, id_pe_send, istack_send, &
     &        npe_recv, irecv_self, nnod_recv, id_pe_recv, istack_recv, &
     &        inod_import, nnod_org, X_org, nnod_new, X_new)
!
      use m_solver_SR
!
!
      integer(kind = kint), intent(in) :: nnod_org
      integer(kind = kint), intent(in) :: nnod_new
!
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
      real   (kind=kreal), intent(in):: X_new(nnod_new)
      real   (kind=kreal), intent(inout):: X_org(nnod_org)
!
      integer (kind = kint) :: neib, istart, inum, iend
      integer (kind = kint) :: i, j, k
      integer (kind = kint) :: ncomm_send, ncomm_recv
      integer (kind = kint) :: ist_send, ist_recv
!
!
!
      call resize_work_itp_SR(ione, npe_send, npe_recv, nnod_recv)
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
          WR(k) = X_new(j)
        end do
      end do

      do neib= 1, ncomm_recv
        istart= istack_recv(neib-1) + 1
        inum  = istack_recv(neib  ) - istack_recv(neib-1)
        call MPI_ISEND(WR(istart), inum, CALYPSO_REAL,                  &
     &      id_pe_recv(neib), 0, CALYPSO_COMM, req2(neib), ierr_MPI)
      end do
!C
!C-- RECEIVE
!
      do neib = 1, ncomm_send
        istart= istack_send(neib-1) + 1
        inum  = istack_send(neib  ) - istack_send(neib-1)
        call MPI_IRECV(X_org(istart), inum, CALYPSO_REAL,               &
     &      id_pe_send(neib), 0, CALYPSO_COMM, req1(neib), ierr_MPI)
      end do
!
      call MPI_WAITALL (ncomm_send, req1, sta1, ierr_MPI)
!
      if (isend_self .eq. 1) then
        ist_send= istack_send(npe_send-1)
        ist_recv= istack_recv(npe_recv-1)
        inum  = istack_send(npe_send  ) - istack_send(npe_send-1)
        do i = 1, inum
          X_org(ist_send+i) = WR(ist_recv+i)
        end do
      end if

      call MPI_WAITALL (ncomm_recv, req2, sta2, ierr_MPI)

      end subroutine interpolate_reverse_SR_1
!
! ----------------------------------------------------------------------
!
      end module interpolate_rev_SR_1
