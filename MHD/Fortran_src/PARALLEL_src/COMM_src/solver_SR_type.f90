!solver_SR_type.f90
!     module solver_SR_type
!
!     Written by H. Matsui on Jan., 2009
!
!        subroutine SOLVER_SEND_RECV_type(NP, comm_tbl, X, my_rank,     &
!     &           SOLVER_COMM)
!        type(communication_table), intent(in) :: comm_tbl
!        integer(kind = kint), intent(in) :: NP
!        integer(kind = kint), intent(in) :: SOLVER_COMM, my_rank
!        real(kind = kreal), intent(inout) :: X(NP)
!
      module solver_SR_type
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine SOLVER_SEND_RECV_type(NP, comm_tbl, X, my_rank,        &
     &          SOLVER_COMM)
!
      use t_comm_table
      use solver_SR
!
      type(communication_table), intent(in) :: comm_tbl
      integer(kind = kint), intent(in) :: NP
      integer(kind = kint), intent(in) :: SOLVER_COMM, my_rank
!
      real(kind = kreal), intent(inout) :: X(NP)
!
!
      if (comm_tbl%num_neib .gt. 0) then
        call SOLVER_SEND_RECV                                           &
     &     (NP, comm_tbl%num_neib, comm_tbl%id_neib,                    &
     &      comm_tbl%istack_import, comm_tbl%item_import,               &
     &      comm_tbl%istack_export, comm_tbl%item_export,               &
     &      X(1), SOLVER_COMM, my_rank)
      end if
!
      end subroutine SOLVER_SEND_RECV_type
!
! ----------------------------------------------------------------------
!
      end module solver_SR_type
