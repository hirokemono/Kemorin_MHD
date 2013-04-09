!>@file   interpolate_SR_1.f90
!!@brief  module interpolate_SR_1
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2006
!
!>@brief  Scalar data communication
!!@n      for interpolation between two meshes
!!
!!@verbatim
!!      subroutine interpolate_send_recv_1                              &
!!     &      (npe_send, isend_self, nnod_send, id_pe_send, istack_send,&
!!     &       npe_recv, irecv_self, nnod_recv, id_pe_recv, istack_recv,&
!!     &       inod_import, nnod_org, X_org, nnod_new, X_new,           &
!!     &       SOLVER_COMM)
!!@endverbatim
!!
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
!!@n @param  X_org(nnod_org)   Send data
!!@n @param  X_new(nnod_new)   Received data
!!@n
!!@n @param  SOLVER_COMM          MPI communicator
!
      module interpolate_SR_1
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
      subroutine interpolate_send_recv_1                                &
     &       (npe_send, isend_self, nnod_send, id_pe_send, istack_send, &
     &        npe_recv, irecv_self, nnod_recv, id_pe_recv, istack_recv, &
     &        inod_import, nnod_org, X_org, nnod_new, X_new,            &
     &        SOLVER_COMM)
!
      use m_solver_SR
!
!
      integer, intent(in)   :: SOLVER_COMM
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
      real   (kind=kreal), intent(in):: X_org(nnod_org)
      real   (kind=kreal), intent(inout):: X_new(nnod_new)
!
      integer (kind = kint) :: neib, istart, inum, iend, ierr
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
      do neib = 1, ncomm_send
        istart= istack_send(neib-1) + 1
        inum  = istack_send(neib  ) - istack_send(neib-1)
        call MPI_ISEND(X_org(istart), inum, MPI_DOUBLE_PRECISION,       &
     &      id_pe_send(neib), 0, SOLVER_COMM, req1(neib), ierr)
      end do
!C
!C-- RECEIVE
      do neib= 1, ncomm_recv
        istart= istack_recv(neib-1) + 1
        inum  = istack_recv(neib  ) - istack_recv(neib-1)
        call MPI_IRECV(WR(istart), inum, MPI_DOUBLE_PRECISION,          &
     &      id_pe_recv(neib), 0, SOLVER_COMM, req2(neib), ierr)
      end do
!
      call MPI_WAITALL (ncomm_recv, req2, sta2, ierr)
!
      if (isend_self .eq. 1) then
        ist_send= istack_send(npe_send-1)
        ist_recv= istack_recv(npe_recv-1)
        inum  = istack_send(npe_send  ) - istack_send(npe_send-1)
        do i = 1, inum
          WR(ist_recv+i) = X_org(ist_send+i)
        end do
      end if

      do neib = 1, npe_recv
        istart = istack_recv(neib-1) + 1
        iend  = istack_recv(neib  )
        do k= istart, iend
          j = inod_import(k)
          X_new(j) = WR(k)
        end do
      end do

      call MPI_WAITALL (ncomm_send, req1, sta1, ierr)

      end subroutine interpolate_send_recv_1
!
! ----------------------------------------------------------------------
!
      end module interpolate_SR_1
