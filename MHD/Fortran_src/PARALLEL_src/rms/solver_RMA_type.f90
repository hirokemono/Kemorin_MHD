!
!      module SOLVER_RMA_type
!
!        programmed by H.Matsui on Feb. 2003
!        modified by H. Matsui on June. 2006
!
!!      subroutine init_RMA_SendRecv(nod_comm)
!!
!!      subroutine SOLVER_RMA_1_type(NP, comm_tbl, RMA_r, X)
!!      subroutine SOLVER_RMA_3_type(NP, comm_tbl, RMA_r, X)
!!      subroutine SOLVER_RMA_6_type(NP, comm_tbl, RMA_r, X)
!!      subroutine SOLVER_RMA_N_type(NP, NB, comm_tbl, RMA_r, X)
!!
!!      subroutine SOLVER_RMA_int_type(NP, comm_tbl, RMA_i, iX)
!
      module SOLVER_RMA_type
!
      use m_precision
!
      use t_comm_table
      use t_solver_RMA
      use t_solver_RMA_int
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
!
      type(communication_table), intent(in) :: nod_comm
      integer(kind = kint), parameter :: isix = 6
!
!
      call init_window_4_SR                                             &
     &   (isix, nod_comm%num_neib, nod_comm%istack_import, RMA_r)
      call init_window_4_RMA_int                                        &
     &   (nod_comm%num_neib, nod_comm%istack_import, RMA_i)
      call init_work_4_RMA                                              &
     &   (nod_comm%num_neib, nod_comm%id_neib, nod_comm%istack_import,  &
     &    RMA_r%import_a)
!
!$omp parallel workshare
      RMA_i%import_a(1:nod_comm%num_neib)                               &
     &      = RMA_r%import_a(1:nod_comm%num_neib)
!$omp end parallel workshare
!
      end subroutine init_RMA_SendRecv
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine SOLVER_RMA_1_type(NP, comm_tbl, RMA_r, X)
!
      use solver_RMA
!
      type(communication_table), intent(in) :: comm_tbl
      integer(kind = kint), intent(in) :: NP
!
      real(kind = kreal), intent(inout) :: X(NP)
      type(RMA_real_buffer), intent(inout) :: RMA_r
!
!
      if(iflag_FSR_time) call start_elapsed_time(ist_elapsed_FSR+1)
      call SOLVER_REMOTE_ACCESS                                         &
     &   (NP, comm_tbl%num_neib, comm_tbl%id_neib,                      &
     &    comm_tbl%istack_import, comm_tbl%item_import,                 &
     &    comm_tbl%istack_export, comm_tbl%item_export, RMA_r, X(1) )
      if(iflag_FSR_time) call end_elapsed_time(ist_elapsed_FSR+1)
!
      end subroutine SOLVER_RMA_1_type
!
! ----------------------------------------------------------------------
!
      subroutine SOLVER_RMA_3_type(NP, comm_tbl, RMA_r, X)
!
      use solver_RMA_3
!
      type(communication_table), intent(in) :: comm_tbl
      integer(kind = kint), intent(in) :: NP
!
      real(kind = kreal), intent(inout) :: X(3*NP)
      type(RMA_real_buffer), intent(inout) :: RMA_r
!
!
      if(iflag_FSR_time) call start_elapsed_time(ist_elapsed_FSR+1)
      call SOLVER_REMOTE_ACCESS_3                                       &
     &   (NP, comm_tbl%num_neib, comm_tbl%id_neib,                      &
     &    comm_tbl%istack_import, comm_tbl%item_import,                 &
     &    comm_tbl%istack_export, comm_tbl%item_export, RMA_r, X(1))
      if(iflag_FSR_time) call end_elapsed_time(ist_elapsed_FSR+1)
!
      end subroutine SOLVER_RMA_3_type
!
! ----------------------------------------------------------------------
!
      subroutine SOLVER_RMA_6_type(NP, comm_tbl, RMA_r, X)
!
      use solver_RMA_6
!
      type(communication_table), intent(in) :: comm_tbl
      integer(kind = kint), intent(in) :: NP
!
      real(kind = kreal), intent(inout) :: X(6*NP)
      type(RMA_real_buffer), intent(inout) :: RMA_r
!
!
      if(iflag_FSR_time) call start_elapsed_time(ist_elapsed_FSR+1)
      call SOLVER_REMOTE_ACCESS_6                                       &
     &   (NP, comm_tbl%num_neib, comm_tbl%id_neib,                      &
     &   comm_tbl%istack_import, comm_tbl%item_import,                  &
     &   comm_tbl%istack_export, comm_tbl%item_export, RMA_r, X(1))
      if(iflag_FSR_time) call end_elapsed_time(ist_elapsed_FSR+1)
!
      end subroutine SOLVER_RMA_6_type
!
! ----------------------------------------------------------------------
!
      subroutine SOLVER_RMA_N_type(NP, NB, comm_tbl, RMA_r, X)
!
      use solver_RMA_N
!
      type(communication_table), intent(in) :: comm_tbl
      integer(kind = kint), intent(in) :: NP, NB
!
      real(kind = kreal), intent(inout) :: X(NB*NP)
      type(RMA_real_buffer), intent(inout) :: RMA_r
!
!
      if(iflag_FSR_time) call start_elapsed_time(ist_elapsed_FSR+1)
      call SOLVER_REMOTE_ACCESS_N                                       &
     &   (NP, NB, comm_tbl%num_neib, comm_tbl%id_neib,                  &
     &   comm_tbl%istack_import, comm_tbl%item_import,                  &
     &   comm_tbl%istack_export, comm_tbl%item_export, RMA_r, X(1))
      if(iflag_FSR_time) call end_elapsed_time(ist_elapsed_FSR+1)
!
      end subroutine SOLVER_RMA_N_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine SOLVER_RMA_int_type(NP, comm_tbl, RMA_i, iX)
!
      type(communication_table), intent(in) :: comm_tbl
      integer(kind = kint), intent(in) :: NP
!
      integer(kind = kint), intent(inout) :: iX(NP)
      type(RMA_int_buffer), intent(inout) :: RMA_i
!
!
      if(iflag_FSR_time) call start_elapsed_time(ist_elapsed_FSR+1)
      call solver_remote_access_i                                       &
     &   (NP, comm_tbl%num_neib, comm_tbl%id_neib,                      &
     &    comm_tbl%istack_import, comm_tbl%item_import,                 &
     &    comm_tbl%istack_export, comm_tbl%item_export, RMA_i, iX(1))
      if(iflag_FSR_time) call end_elapsed_time(ist_elapsed_FSR+1)
!
      end subroutine SOLVER_RMA_int_type
!
! ----------------------------------------------------------------------
!
      end module SOLVER_RMA_type
