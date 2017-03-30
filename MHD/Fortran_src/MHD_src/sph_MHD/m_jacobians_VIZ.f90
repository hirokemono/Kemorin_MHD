!
!     module m_jacobians_VIZ
!
!> @brief Shape function and jacobians for 3D elements
!      Written by H. Matsui on Dec., 2003
!
      module m_jacobians_VIZ
!
      use m_constants
      use m_precision
      use t_jacobians
      use t_next_node_ele_4_node
!
      implicit  none
!
!
!>   Structure of neighbouring node and element list for each node
      type(next_nod_ele_table), save :: next_tbl_VIZ1
!
!>     Stracture for Jacobians for FEM grid
      type(jacobians_type), save :: jacobians_VIZ1
!
      end module m_jacobians_VIZ
