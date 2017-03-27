!m_type_AMG_data.f90
!      module m_type_AMG_data
!
!     Written by H. Matsui on Dec., 2008
!
      module m_type_AMG_data
!
      use m_precision
!
      use t_MGCG_data
!
      implicit  none
!
!>      Structure for MGCG solver
      type(MGCG_data), save :: MGCG_WK1
!
!>      Structure for MGCG solver
      type(mesh_4_MGCG), save :: MGCG_FEM1
!
      end module m_type_AMG_data
