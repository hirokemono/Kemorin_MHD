!>@file   reverse_SR_int8.f90
!!@brief  module reverse_SR_int8
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!!@n     Modified in Aug., 2007
!!@n     Modified in Sep., 2013
!!@n     Modified in July, 2020
!
!>@brief  Data communication for 8-byte integer
!!
!!@verbatim
!!      subroutine int8_items_send_recv                                 &
!!     &         (num_neib, id_neib, istack_send, istack_recv,          &
!!     &          item8_send, item8_recv)
!!@endverbatim
!
      module reverse_SR_int8
!
      use m_precision
      use calypso_mpi
      use t_solver_SR
      use t_solver_SR_int8
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int8_items_send_recv                                   &
     &         (num_neib, id_neib, istack_send, istack_recv,            &
     &          item8_send, item8_recv)
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: id_neib(num_neib)
!
      integer(kind = kint), intent(in) :: istack_send(0:num_neib)
      integer(kind = kint), intent(in) :: istack_recv(0:num_neib)
!
      integer(kind = kint_gl), intent(in)                               &
     &                 :: item8_send(istack_send(num_neib))
!
      integer(kind = kint_gl), intent(inout)                            &
     &                 :: item8_recv(istack_recv(num_neib))
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
        call MPI_ISEND(item8_send(ist+1), num,                          &
     &                 CALYPSO_GLOBAL_INT, int(id_neib(ip)), 0,         &
     &                 CALYPSO_COMM, iSR_sig%req1(ip), ierr_MPI)
      end do
!
      do ip = 1, num_neib
        ist = istack_recv(ip-1)
        num = int(istack_recv(ip  ) - istack_recv(ip-1))
        call MPI_IRECV(item8_recv(ist+1), num,                          &
     &                 CALYPSO_GLOBAL_INT, int(id_neib(ip)), 0,         &
     &                 CALYPSO_COMM, iSR_sig%req2(ip), ierr_MPI)
      end do
      call MPI_WAITALL                                                  &
     &   (int(num_neib), iSR_sig%req2, iSR_sig%sta2, ierr_MPI)
      call MPI_WAITALL                                                  &
     &   (int(num_neib), iSR_sig%req1, iSR_sig%sta1, ierr_MPI)
      call dealloc_SR_flag(iSR_sig)
!
      end subroutine int8_items_send_recv
!
!-----------------------------------------------------------------------
!
      end module reverse_SR_int8
