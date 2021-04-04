!>@file   reverse_SR_int.f90
!!@brief  module reverse_SR_int
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2008
!
!>@brief  Routines to construca element communication table
!!
!!@verbatim
!!      subroutine num_items_send_recv(npe_send, irank_send, num_send,  &
!!     &                               npe_recv, irank_recv, irecv_self,&
!!     &                               num_recv, istack_recv, ntot_recv)
!!      subroutine comm_items_send_recv(num_neib, id_neib,              &
!!     &          istack_send, istack_recv, item_send, item_recv)
!!
!!      subroutine local_node_id_reverse_SR(numnod, num_neib, id_neib,  &
!!     &         istack_import, item_import, istack_export, item_export,&
!!     &         item_local, inod_local)
!!@endverbatim
!!
      module reverse_SR_int
!
      use m_precision
      use m_constants
      use calypso_mpi
      use t_solver_SR
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine num_items_send_recv(npe_send, irank_send, num_send,    &
     &                               npe_recv, irank_recv, irecv_self,  &
     &                               num_recv, istack_recv, ntot_recv)
!
      integer(kind = kint), intent(in) :: npe_send
      integer(kind = kint), intent(in) :: npe_recv, irecv_self
      integer(kind = kint), intent(in) :: irank_send(npe_send)
      integer(kind = kint), intent(in) :: irank_recv(npe_recv)
!
      integer(kind = kint), intent(in) :: num_send(npe_send)
!
      integer(kind = kint), intent(inout) :: ntot_recv
      integer(kind = kint), intent(inout) :: num_recv(npe_recv)
      integer(kind = kint), intent(inout) :: istack_recv(0:npe_recv)
!
      type(send_recv_status) :: iSR_sig
      integer :: ncomm_send, ncomm_recv, ip
!
!
      call resize_SR_flag(npe_send, npe_recv, iSR_sig)
!
      ncomm_send = int(npe_send - irecv_self)
      ncomm_recv = int(npe_recv - irecv_self)
!
      do ip = 1, ncomm_send
        call MPI_ISEND(num_send(ip), 1, CALYPSO_INTEGER,                &
     &                 int(irank_send(ip)), 0, CALYPSO_COMM,            &
     &                 iSR_sig%req1(ip), ierr_MPI)
      end do
!
      do ip = 1, ncomm_recv
        call MPI_IRECV (num_recv(ip), 1, CALYPSO_INTEGER,               &
     &                 int(irank_recv(ip)), 0, CALYPSO_COMM,            &
     &                 iSR_sig%req2(ip), ierr_MPI)
      end do
      write(*,*) my_rank, my_rank, 'MPI_WAITALL_2'
      call MPI_WAITALL                                                  &
     &   (ncomm_recv, iSR_sig%req2(1), iSR_sig%sta2(1,1), ierr_MPI)
      write(*,*) my_rank, my_rank, 'MPI_WAITALL_1'
      call MPI_WAITALL                                                  &
     &   (ncomm_send, iSR_sig%req1(1), iSR_sig%sta1(1,1), ierr_MPI)
      call dealloc_SR_flag(iSR_sig)
!
      if(irecv_self .gt. 0) num_recv(npe_recv) = num_send(npe_send)
!
      istack_recv(0) = 0
      do ip = 1, ncomm_recv
        istack_recv(ip) = istack_recv(ip-1) + num_recv(ip)
      end do
      ntot_recv = istack_recv(npe_recv)
!
      end subroutine  num_items_send_recv
!
!-----------------------------------------------------------------------
!
      subroutine comm_items_send_recv(num_neib, id_neib,                &
     &          istack_send, istack_recv, item_send, item_recv)
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: id_neib(num_neib)
!
      integer(kind = kint), intent(in) :: istack_send(0:num_neib)
      integer(kind = kint), intent(in) :: istack_recv(0:num_neib)
!
      integer(kind = kint), intent(in)                                  &
     &                 :: item_send(istack_send(num_neib))
!
      integer(kind = kint), intent(inout)                               &
     &                 :: item_recv(istack_recv(num_neib))
!
      type(send_recv_status) :: iSR_sig
      integer(kind = kint) :: ip, ist
      integer :: num
!
!
      call resize_SR_flag(num_neib, num_neib, iSR_sig)
!
      do ip = 1, num_neib
        ist = istack_send(ip-1)
        num = int(istack_send(ip  ) - istack_send(ip-1))
        call MPI_ISEND(item_send(ist+1), num,                           &
     &                 CALYPSO_INTEGER,int(id_neib(ip)), 0,             &
     &                 CALYPSO_COMM, iSR_sig%req1(ip), ierr_MPI)
      end do
!
      do ip = 1, num_neib
        ist = istack_recv(ip-1)
        num = int(istack_recv(ip  ) - istack_recv(ip-1))
        call MPI_IRECV(item_recv(ist+1), num,                           &
     &                 CALYPSO_INTEGER, int(id_neib(ip)), 0,            &
     &                 CALYPSO_COMM, iSR_sig%req2(ip), ierr_MPI)
      end do
      call MPI_WAITALL                                                  &
     &   (int(num_neib), iSR_sig%req2, iSR_sig%sta2, ierr_MPI)
      call MPI_WAITALL                                                  &
     &   (int(num_neib), iSR_sig%req1, iSR_sig%sta1, ierr_MPI)
      call dealloc_SR_flag(iSR_sig)
!
      end subroutine comm_items_send_recv
!
!-----------------------------------------------------------------------
!
      subroutine local_node_id_reverse_SR(numnod, num_neib, id_neib,    &
     &         istack_import, item_import, istack_export, item_export,  &
     &         item_local, inod_local)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: id_neib(num_neib)
!
      integer(kind = kint), intent(in) :: istack_import(0:num_neib)
      integer(kind = kint), intent(in)                                  &
     &                 :: item_import(istack_import(num_neib))
!
      integer(kind = kint), intent(in) :: istack_export(0:num_neib)
      integer(kind = kint), intent(in)                                  &
     &                 :: item_export(istack_export(num_neib))
!
      integer(kind = kint), intent(inout)                               &
     &                 :: item_local(istack_export(num_neib))
      integer(kind = kint), intent(inout) :: inod_local(numnod)
!
      type(send_recv_status) :: iSR_sig
      integer(kind = kint) :: ip, ist, i, inod
      integer :: num
!
!
      call resize_SR_flag(num_neib, num_neib, iSR_sig)
!
      do ip = 1, num_neib
        ist = istack_import(ip-1)
        num = int(istack_import(ip  ) - istack_import(ip-1))
        call MPI_ISEND(item_import(ist+1), num,                         &
     &                 CALYPSO_INTEGER, int(id_neib(ip)), 0,            &
     &                 CALYPSO_COMM, iSR_sig%req1(ip), ierr_MPI)
      end do
!
      do ip = 1, num_neib
        ist = istack_export(ip-1)
        num = int(istack_export(ip  ) - istack_export(ip-1))
        call MPI_IRECV(item_local(ist+1), num,                          &
     &                 CALYPSO_INTEGER, int(id_neib(ip)), 0,            &
     &                 CALYPSO_COMM, iSR_sig%req2(ip), ierr_MPI)
      end do
      call MPI_WAITALL                                                  &
     &   (int(num_neib), iSR_sig%req2, iSR_sig%sta2, ierr_MPI)
      call MPI_WAITALL                                                  &
     &   (int(num_neib), iSR_sig%req1, iSR_sig%sta1, ierr_MPI)
      call dealloc_SR_flag(iSR_sig)
!
      inod_local = 0
      do i = 1, istack_export(num_neib)
        inod = item_export(i)
        inod_local(inod) = item_local(i)
      end do
!
      end subroutine local_node_id_reverse_SR
!
!-----------------------------------------------------------------------
!
      end module reverse_SR_int
