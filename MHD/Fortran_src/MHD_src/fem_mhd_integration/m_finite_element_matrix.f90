!
!     module m_finite_element_matrix
!.......................................................................
!
!     Written by H. Matsui and H. Okuda on Jul. 2000
!     Modified by H. Matsui on Oct., 2006
!
!      subroutine deallocate_finite_elem_mt
!
      module m_finite_element_matrix
!
      use m_precision
      use t_MHD_finite_element_mat
      use t_work_FEM_integration
      use t_sorted_node_MHD
      use t_MHD_mass_matricxes
!
      implicit  none
!
!
!>      Stracture for FEM assembling
      type(finite_element_integration), save :: fem_int1
!
!>      Structure of mass matrices for FEM_MHD
      type(lumped_mass_mat_layerd), save :: mk_MHD1
!
!>  Structures for FEM marix table
      type(tables_MHD_mat_const), save :: MHD1_mat_tbls
!
      end module m_finite_element_matrix
