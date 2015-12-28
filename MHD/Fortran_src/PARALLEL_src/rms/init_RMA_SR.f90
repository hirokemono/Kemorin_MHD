!
!      module init_RMA_SR
!
!        programmed by H.Matsui on Feb. 2003
!        modified by H. Matsui on June. 2006
!
!      subroutine init_RMA_SendRecv(nod_comm)
!
      module init_RMA_SR
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
      subroutine init_RMA_SendRecv(nod_comm)
!
      use calypso_mpi
      use t_comm_table
      use m_RMA_SR
!
      type(communication_table), intent(in) :: nod_comm
      integer(kind = kint), parameter :: isix = 6
!
!
      call init_work_4_SR                                               &
     &   (nod_comm%num_neib, nod_comm%id_neib, nod_comm%istack_import)
      call init_window_4_SR                                             &
     &   (isix, nod_comm%num_neib, nod_comm%istack_import)
!
      end subroutine init_RMA_SendRecv
!
!-----------------------------------------------------------------------
!
      end module init_RMA_SR
