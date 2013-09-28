!m_type_AMG_data.f90
!      module m_type_AMG_data
!
!     Written by H. Matsui on Dec., 2008
!
      module m_type_AMG_data
!
      use m_precision
!
      use t_comm_table
      use t_vector_for_solver
      use t_solver_djds
!
      implicit  none
!
!
      integer(kind = kint), parameter :: max_MG_level = 12
!   Maximum Levels of multigrid (level 0 is original mesh)
      integer(kind = kint) :: num_MG_level = 1
!   Levels of multigrid (level 0 is original mesh)
!
!
      type(vectors_4_solver), target ::    MG_vector(0:max_MG_level)
      type(communication_table), target :: MG_comm(0:max_MG_level)
      type(DJDS_ordering_table), target                                 &
     &                      :: MG_djds_tbl(0:max_MG_level)
!
      type(DJDS_MATRIX) :: MG_mat(0:max_MG_level)
!
      end module m_type_AMG_data
