!
!     module m_finite_element_matrix
!.......................................................................
!
!     Written by H. Matsui and H. Okuda on Jul. 2000
!     Modified by H. Matsui on Oct., 2006
!
!      subroutine allocate_finite_elem_mt
!      subroutine deallocate_finite_elem_mt
!
      module m_finite_element_matrix
!
      use m_precision
      use t_finite_element_mat
!
      implicit  none
!
!>      Work area for FEM assemble
      type(work_finite_element_mat), save :: fem1_wk
!
      type(finite_ele_mat_node), save :: f1_l
!
      type(finite_ele_mat_node), save :: f1_nl
!
      type(lumped_mass_matrices), save :: m1_lump
!
!   ---------------------------------------------------------------------
!
      contains
!
!   ---------------------------------------------------------------------
!
      subroutine allocate_finite_elem_mt
!
      use m_geometry_data
      use m_machine_parameter
      use m_phys_constants
!
!
      call alloc_type_fem_matrices(n_vector, node1, f1_l)
      call alloc_type_fem_matrices(n_vector, node1, f1_nl)
!
      call alloc_type_fem_lumped_mass(node1%numnod, m1_lump)
      call alloc_type_fem_mat_work(ele1, fem1_wk)
!
      end subroutine allocate_finite_elem_mt
!
!   ---------------------------------------------------------------------
!
      subroutine deallocate_finite_elem_mt
!
      call dealloc_type_fem_mat_work(fem1_wk)
      call dealloc_type_fem_lumped_mass(m1_lump)
      call dealloc_type_fem_matrices(f1_nl)
      call dealloc_type_fem_matrices(f1_l)
!
      end subroutine deallocate_finite_elem_mt
!
!   ---------------------------------------------------------------------
!
      end module m_finite_element_matrix
