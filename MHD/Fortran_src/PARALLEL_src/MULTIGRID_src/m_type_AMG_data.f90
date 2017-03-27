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
      use t_MGCG_data
!
      implicit  none
!
!>      Structure for MGCG solver
      type(MGCG_data), save :: MGCG_WK1
!MGCG_WK1%num_MG_level
!
!>      Structure for MGCG solver
      type(mesh_4_MGCG), save :: MGCG_mesh1
!MGCG_mesh1%num_MG_level
!
!   Maximum Levels of multigrid (level 0 is original mesh)
      integer(kind = kint), parameter :: max_MG_level = 12
!   Levels of multigrid (level 0 is original mesh)
!      integer(kind = kint) :: num_MG_level = 1
!
!
!>     structure of communicator for MGCG
      type(mpi_4_solver), target, save :: MG_mpi(0:max_MG_level)
!>     structure of vectors in MGCG
      type(vectors_4_solver), target, save :: MG_vector(0:max_MG_level)
!
      type(DJDS_MATRIX) :: MG_mat(0:max_MG_level)
!
      end module m_type_AMG_data
