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
      use t_finite_element_mat
      use t_MHD_finite_element_mat
      use t_int_surface_data
      use t_work_FEM_integration
!
      implicit  none
!
!
!>      Stracture for FEM assembling
      type(finite_element_integration), save :: fem_int1
!
!>      Stracture for FEM assembling
      type(arrays_finite_element_mat), save :: rhs_mat1
!
!>      Work array for FEM assemble in MHD model
      type(work_MHD_fe_mat), save :: mhd_fem1_wk
! 
!>      Work area for FEM assemble
      type(work_finite_element_mat), save :: fem1_wk
!
!>      Work area for surface FEM assemble
      type(work_surface_element_mat), save :: surf1_wk
!
      type(finite_ele_mat_node), save :: f1_l
!
      type(finite_ele_mat_node), save :: f1_nl
!
      end module m_finite_element_matrix
