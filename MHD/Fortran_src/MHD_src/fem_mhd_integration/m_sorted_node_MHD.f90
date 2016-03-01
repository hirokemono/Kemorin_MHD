!
!     module   m_sorted_node_MHD
!.......................................................................
!
!     Written by H. Matsui
!
      module   m_sorted_node_MHD
!
      use m_precision
      use t_sorted_node_MHD
!
      implicit  none
!
!>  Structures for FEM marix table
      type(tables_MHD_mat_const), save :: MHD1_mat_tbls
!
      end module m_sorted_node_MHD
