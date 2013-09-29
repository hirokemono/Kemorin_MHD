!solver_SR_6_type.f90
!     module solver_SR_6_type
!
!     Written by H. Matsui on Jan., 2009
!
!      subroutine SOLVER_SEND_RECV_6_type(NP, comm_tbl, X)
!        type(communication_table), intent(in) :: comm_tbl
!        integer(kind = kint), intent(in) :: NP
!        real(kind = kreal), intent(inout) :: X(6*NP)
!
      module solver_SR_6_type
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
      subroutine SOLVER_SEND_RECV_6_type(NP, comm_tbl, X)
!
      use t_comm_table
      use solver_SR_6
!
      type(communication_table), intent(in) :: comm_tbl
      integer(kind = kint), intent(in) :: NP
!
      real(kind = kreal), intent(inout) :: X(6*NP)
!
!
      if (comm_tbl%num_neib .gt. 0) then
        call SOLVER_SEND_RECV_6                                         &
     &      (NP, comm_tbl%num_neib, comm_tbl%id_neib,                   &
     &      comm_tbl%istack_import, comm_tbl%item_import,               &
     &      comm_tbl%istack_export, comm_tbl%item_export, X(1) )
      end if
!
      end subroutine SOLVER_SEND_RECV_6_type
!
! ----------------------------------------------------------------------
!
      end module solver_SR_6_type
