!>@file   m_element_id_4_node.f90
!!@brief  module m_element_id_4_node
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2006
!
!> @brief included element list for each node
!!
      module m_element_id_4_node
!
      use m_precision
      use m_constants
!
      use t_next_node_ele_4_node
      use t_table_FEM_const
!
      use t_crs_connect
      use t_crs_matrix
!
      implicit none
!
!>   Structure of neighbouring node and element list for each node
      type(next_nod_ele_table), save :: next_tbl1
!
      end module m_element_id_4_node
