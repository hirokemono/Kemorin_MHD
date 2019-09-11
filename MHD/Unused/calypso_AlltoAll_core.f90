!>@file   calypso_AlltoAll_core.f90
!!@brief  module calypso_AlltoAll_core
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief  Arbitrary components data communication
!!@n      using MPI_AllToAll
!!
!!@verbatim
!!      subroutine calypso_AllToAllv_Ncore                              &
!!     &   (NB, npe_sr, istack_send, istack_recv, CALYPSO_SUB_COMM)
!!      subroutine calypso_AllToAllv_intcore                            &
!!     &         (npe_sr, istack_send, istack_recv, CALYPSO_SUB_COMM)
!!
!!      subroutine calypso_AllToAll_Ncore(NB, nitem_SR, CALYPSO_SUB_COMM)
!!      subroutine calypso_AllToAll_intcore(nitem_SR, CALYPSO_SUB_COMM)
!!@endverbatim
!!
!!@n @param  NB    Number of components for communication
!!@n
!!@n @param  npe_sr    Number of processses to send
!!@n @param  istack_send(0:npe_sr)
!!                    End points of send buffer for each process
!!@n @param  istack_recv(0:npe_sr)
!!                    End points of receive buffer for each process
!!@n @param  CALYPSO_SUB_COMM
!!                    Sub MPI communicator
!
      module calypso_AlltoAll_core
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
      subroutine calypso_AllToAllv_Ncore                                &
     &   (NB, npe_sr, istack_send, istack_recv, CALYPSO_SUB_COMM)
!
      use calypso_mpi
      use m_solver_SR
!
      integer, intent(in)  :: CALYPSO_SUB_COMM
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in) :: npe_sr
      integer(kind = kint), intent(in) :: istack_send(0:npe_sr)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_sr)
!
      integer(kind = kint) :: num_send(npe_sr), num_recv(npe_sr)
      integer(kind = kint) :: ist_send(npe_sr), ist_recv(npe_sr)
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, npe_sr
        num_send(ip) = NB*(istack_send(ip) - istack_send(ip-1))
        num_recv(ip) = NB*(istack_recv(ip) - istack_recv(ip-1))
        ist_send(ip) = NB*istack_send(ip-1)
        ist_recv(ip) = NB*istack_recv(ip-1)
      end do
!
      call MPI_AllToAllV(WS(1), num_send, ist_send, CALYPSO_REAL,       &
     &                   WR(1), num_recv, ist_recv, CALYPSO_REAL,       &
     &                   CALYPSO_SUB_COMM, ierr_MPI)
!
      end subroutine calypso_AllToAllv_Ncore
!
! ----------------------------------------------------------------------
!
      subroutine calypso_AllToAllv_intcore                              &
     &         (npe_sr, istack_send, istack_recv, CALYPSO_SUB_COMM)
!
      use calypso_mpi
      use m_solver_SR
!
      integer, intent(in)  :: CALYPSO_SUB_COMM
      integer(kind = kint), intent(in) :: npe_sr
      integer(kind = kint), intent(in) :: istack_send(0:npe_sr)
      integer(kind = kint), intent(in) :: istack_recv(0:npe_sr)
!
      integer(kind = kint) :: num_send(npe_sr), num_recv(npe_sr)
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, npe_sr
        num_send(ip) = (istack_send(ip) - istack_send(ip-1))
        num_recv(ip) = (istack_recv(ip) - istack_recv(ip-1))
      end do
!
      call MPI_AllToAllV                                                &
     &             (iWS(1), num_send, istack_send(0), CALYPSO_INTEGER,  &
     &              iWR(1), num_recv, istack_recv(0), CALYPSO_INTEGER,  &
     &              CALYPSO_SUB_COMM, ierr_MPI)
!
      end subroutine calypso_AllToAllv_intcore
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine calypso_AllToAll_Ncore(NB, nitem_SR, CALYPSO_SUB_COMM)
!
      use calypso_mpi
      use m_solver_SR
!
      integer, intent(in)  :: CALYPSO_SUB_COMM
      integer(kind = kint), intent(in) :: NB
      integer(kind = kint), intent(in) :: nitem_SR
!
!
      call MPI_AllToAll(WS(1), (NB*nitem_SR), CALYPSO_REAL,             &
     &                  WR(1), (NB*nitem_SR), CALYPSO_REAL,             &
     &                  CALYPSO_SUB_COMM, ierr_MPI)
!
      end subroutine calypso_AllToAll_Ncore
!
! ----------------------------------------------------------------------
!
      subroutine calypso_AllToAll_intcore(nitem_SR, CALYPSO_SUB_COMM)
!
      use calypso_mpi
      use m_solver_SR
!
      integer, intent(in)  :: CALYPSO_SUB_COMM
      integer(kind = kint), intent(in) :: nitem_SR
!
!
      call MPI_AllToAll(iWS(1), nitem_SR, CALYPSO_INTEGER,              &
     &                  iWR(1), nitem_SR, CALYPSO_INTEGER,              &
     &                  CALYPSO_SUB_COMM, ierr_MPI)
!
      end subroutine calypso_AllToAll_intcore
!
! ----------------------------------------------------------------------
!
      end module calypso_AlltoAll_core
