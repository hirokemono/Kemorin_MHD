!
!      module init_RMA_SR
!
!        programmed by H.Matsui on Feb. 2003
!        modified by H. Matsui on June. 2006
!
!      subroutine init_RMA_SendRecv
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
      subroutine init_RMA_SendRecv
!
      use m_parallel_var_dof
      use m_nod_comm_table
      use m_RMA_SR
!
      integer(kind = kint), parameter :: isix = 6
!
!
      call init_work_4_SR( num_neib, id_neib,  istack_import,           &
     &       SOLVER_COMM, my_rank )
      call init_window_4_SR(isix, num_neib, istack_import)
!
      end subroutine init_RMA_SendRecv
!
!-----------------------------------------------------------------------
!
      end module init_RMA_SR
