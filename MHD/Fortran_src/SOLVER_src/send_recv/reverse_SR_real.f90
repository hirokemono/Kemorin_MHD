!>@file   reverse_SR_real.f90
!!@brief  module reverse_SR_real
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
!!      subroutine real_items_send_recv(num_neib, id_neib,              &
!!     &          istack_send, istack_recv, x_send, x_recv)
!!        integer(kind = kint), intent(in) :: num_neib
!!        integer(kind = kint), intent(in) :: id_neib(num_neib)
!!        integer(kind = kint), intent(in) :: istack_send(0:num_neib)
!!        integer(kind = kint), intent(in) :: istack_recv(0:num_neib)
!!        real(kind = kreal), intent(in) :: x_send(istack_send(num_neib))
!!        real(kind = kreal), intent(inout)                             &
!!      &                 :: x_recv(istack_recv(num_neib))
!!      subroutine real_items_send_recv_3(num_neib, id_neib,            &
!!     &          istack_send, istack_recv, x_send, x_recv)
!!        integer(kind = kint), intent(in) :: num_neib
!!        integer(kind = kint), intent(in) :: id_neib(num_neib)
!!        integer(kind = kint), intent(in) :: istack_send(0:num_neib)
!!        integer(kind = kint), intent(in) :: istack_recv(0:num_neib)
!!        real(kind = kreal), intent(in)                                &
!!           &                 :: x_send(3*istack_send(num_neib))
!!        real(kind = kreal), intent(inout)                             &
!!      &                 :: x_recv(3*istack_recv(num_neib))
!!@endverbatim
!
      module reverse_SR_real
!
      use m_precision
      use calypso_mpi
      use t_solver_SR
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine real_items_send_recv(num_neib, id_neib,                &
     &          istack_send, istack_recv, x_send, x_recv)
!
      use t_solver_SR
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: id_neib(num_neib)
!
      integer(kind = kint), intent(in) :: istack_send(0:num_neib)
      integer(kind = kint), intent(in) :: istack_recv(0:num_neib)
!
      real(kind = kreal), intent(in) :: x_send(istack_send(num_neib))
!
      real(kind = kreal), intent(inout)                                 &
     &                 :: x_recv(istack_recv(num_neib))
!
      type(send_recv_status) :: rSR_sig
      integer(kind = kint) :: ip, ist
      integer :: num
!
!
      call resize_SR_flag(num_neib, num_neib, rSR_sig)
!
      do ip = 1, num_neib
        ist = istack_send(ip-1)
        num = int((istack_send(ip  ) - istack_send(ip-1)))
        call MPI_ISEND(x_send(ist+1), num, CALYPSO_REAL,                &
     &                 int(id_neib(ip)), 0, CALYPSO_COMM,               &
     &                 rSR_sig%req1(ip), ierr_MPI)
      end do
!
      do ip = 1, num_neib
        ist = istack_recv(ip-1)
        num = int((istack_recv(ip  ) - istack_recv(ip-1)))
        call MPI_IRECV(x_recv(ist+1), num, CALYPSO_REAL,                &
     &                 int(id_neib(ip)), 0, CALYPSO_COMM,               &
     &                 rSR_sig%req2(ip), ierr_MPI)
      end do
      call MPI_WAITALL                                                  &
     &   (int(num_neib), rSR_sig%req2, rSR_sig%sta2, ierr_MPI)
      call MPI_WAITALL                                                  &
     &   (int(num_neib), rSR_sig%req1, rSR_sig%sta1, ierr_MPI)
      call dealloc_SR_flag(rSR_sig)
!
      end subroutine real_items_send_recv
!
!-----------------------------------------------------------------------
!
      subroutine real_items_send_recv_3(num_neib, id_neib,              &
     &          istack_send, istack_recv, x_send, x_recv)
!
      use t_solver_SR
!
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: id_neib(num_neib)
!
      integer(kind = kint), intent(in) :: istack_send(0:num_neib)
      integer(kind = kint), intent(in) :: istack_recv(0:num_neib)
!
      real(kind = kreal), intent(in)                                    &
     &                 :: x_send(3*istack_send(num_neib))
!
      real(kind = kreal), intent(inout)                                 &
     &                 :: x_recv(3*istack_recv(num_neib))
!
      type(send_recv_status) :: rSR_sig
      integer(kind = kint) :: ip, ist
      integer :: num
!
!
      call resize_SR_flag(num_neib, num_neib, rSR_sig)
!
      do ip = 1, num_neib
        ist = 3*istack_send(ip-1)
        num = int(3*(istack_send(ip  ) - istack_send(ip-1)))
        call MPI_ISEND(x_send(ist+1), num, CALYPSO_REAL,                &
     &                 int(id_neib(ip)), 0, CALYPSO_COMM,               &
     &                 rSR_sig%req1(ip), ierr_MPI)
      end do
!
      do ip = 1, num_neib
        ist = 3* istack_recv(ip-1)
        num = int(3*(istack_recv(ip  ) - istack_recv(ip-1)))
        call MPI_IRECV(x_recv(ist+1), num, CALYPSO_REAL,                &
     &                 int(id_neib(ip)), 0, CALYPSO_COMM,               &
     &                 rSR_sig%req2(ip), ierr_MPI)
      end do
      call MPI_WAITALL                                                  &
     &   (int(num_neib), rSR_sig%req2, rSR_sig%sta2, ierr_MPI)
      call MPI_WAITALL                                                  &
     &   (int(num_neib), rSR_sig%req1, rSR_sig%sta1, ierr_MPI)
      call dealloc_SR_flag(rSR_sig)
!
      end subroutine real_items_send_recv_3
!
!-----------------------------------------------------------------------
!
      end module reverse_SR_real
