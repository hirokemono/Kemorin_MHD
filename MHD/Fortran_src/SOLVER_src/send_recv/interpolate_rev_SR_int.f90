!>@file   interpolate_rev_SR_int.f90
!!@brief  module interpolate_rev_SR_int
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2009
!
!>@brief  Integer data communication
!!@n      for reverse interpolation between two meshes
!!
!!@verbatim
!!      subroutine interpolate_reverse_SR_int                           &
!!     &      (npe_send, isend_self, nnod_send, id_pe_send, istack_send,&
!!     &       npe_recv, irecv_self, nnod_recv, id_pe_recv, istack_recv,&
!!     &       inod_import, nnod_org, iX_org, nnod_new, iX_new)
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
!!@n @param  iX_new(nnod_new)   Send data
!!@n @param  iX_org(nnod_org)   Received data
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
     &        inod_import, nnod_org, iX_org, nnod_new, iX_new)
!
      use m_solver_SR
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
      integer (kind=kint), intent(in):: iX_new(nnod_new)
      integer (kind=kint), intent(inout):: iX_org(nnod_org)
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
          iWR(k) = iX_new(j)
        end do
      end do

      do neib= 1, ncomm_recv
        istart= istack_recv(neib-1) + 1
        inum  = istack_recv(neib  ) - istack_recv(neib-1)
        call MPI_ISEND(iWR(istart), inum, MPI_INTEGER,                  &
     &      id_pe_recv(neib), 0, CALYPSO_COMM, req2(neib), ierr)
      end do
!C
!C-- RECEIVE
      do neib = 1, ncomm_send
        istart= istack_send(neib-1) + 1
        inum  = istack_send(neib  ) - istack_send(neib-1)
        call MPI_IRECV(iX_org(istart), inum, MPI_INTEGER,               &
     &      id_pe_send(neib), 0, CALYPSO_COMM, req1(neib), ierr)
      end do
!
      call MPI_WAITALL (ncomm_send, req1, sta1, ierr)
!
      if (isend_self .eq. 1) then
        ist_send= istack_send(npe_send-1)
        ist_recv= istack_recv(npe_recv-1)
        inum  = istack_send(npe_send  ) - istack_send(npe_send-1) 
        do i = 1, inum
          iX_org(ist_send+i) = iWR(ist_recv+i)
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
