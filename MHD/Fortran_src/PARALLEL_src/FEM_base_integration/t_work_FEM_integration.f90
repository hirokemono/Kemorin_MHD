!t_work_FEM_integration.f90
!     module t_work_FEM_integration
!.......................................................................
!
!     Written by H. Matsui on Mar. 2009
!
!>    Structure for lumped mass matrix and RHS vector assembler
!
!      subroutine alloc_fem_mat_base_type(node, ele, rhs_mat)
!        type(node_data), intent(in) :: node
!        type(element_data), intent(in) :: ele
!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!      subroutine dealloc_finite_elem_mat(rhs_mat)
!
      module t_work_FEM_integration
!
      use m_precision
      use t_finite_element_mat
      use t_int_surface_data
!
      implicit  none
!
!
      type finite_ele_matrices
      end type finite_ele_matrices
!
      type arrays_finite_element_mat
!>     lumped mass matrix
        type(lumped_mass_matrices) ::    m_lump
!
!>     Structure of work area  for RHS linear vector assemble
        type(finite_ele_mat_node) :: f_l
!>     Structure of work area  for RHS nonlinear vector assemble
        type(finite_ele_mat_node) :: f_nl
!
!>     Structure of work area  for volume integration
        type(work_finite_element_mat) :: fem_wk
!
!>     Structure of work area  for surface integration
        type(work_surface_element_mat) :: surf_wk
      end type arrays_finite_element_mat
!
!   ---------------------------------------------------------------------
!
      contains
!
!   ---------------------------------------------------------------------
!
      subroutine alloc_fem_mat_base_type(node, ele, rhs_mat)
!
      use t_geometry_data
      use m_phys_constants
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!
!
      call alloc_finite_elem_mat(node, ele, rhs_mat%m_lump,             &
     &    rhs_mat%fem_wk, rhs_mat%f_l, rhs_mat%f_nl)
!
      end subroutine alloc_fem_mat_base_type
!
!   ---------------------------------------------------------------------
!   ---------------------------------------------------------------------
!
      subroutine dealloc_finite_elem_mat(rhs_mat)
!
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!
!
      call dealloc_int_surf_data(rhs_mat%surf_wk)
!
      call dealloc_type_fem_mat_work(rhs_mat%fem_wk)
      call dealloc_type_fem_lumped_mass(rhs_mat%m_lump)
!
      call dealloc_type_fem_matrices(rhs_mat%f_l)
      call dealloc_type_fem_matrices(rhs_mat%f_nl)
!
      end subroutine dealloc_finite_elem_mat
!
!   ---------------------------------------------------------------------
!
      end module t_work_FEM_integration
