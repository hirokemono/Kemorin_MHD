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
!MGCG_WK1%MG_vector
!
!>      Structure for MGCG solver
      type(mesh_4_MGCG), save :: MGCG_FEM1
!MGCG_FEM1%MG_jacobians
!
!   Maximum Levels of multigrid (level 0 is original mesh)
      integer(kind = kint), parameter :: max_MG_level = 12
!
      end module m_type_AMG_data
