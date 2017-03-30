!t_work_FEM_integration.f90
!     module t_work_FEM_integration
!.......................................................................
!
!     Written by H. Matsui on Mar. 2009
!
!>    Structure for lumped mass matrix and RHS vector assembler
!
!!@verbatim
!!      subroutine alloc_fem_mat_base_type                              &
!!     &         (node, ele, surf, surf_grp, rhs_mat)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!      subroutine dealloc_fem_mat_base_type(rhs_mat)
!!@endverbatim
!
      module t_work_FEM_integration
!
      use m_precision
      use t_finite_element_mat
      use t_int_surface_data
      use t_jacobians
      use t_table_FEM_const
!
      implicit  none
!
!
      type arrays_finite_element_mat
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
!
!>      Stracture for FEM assembling
      type finite_element_integration
!>        Stracture for Jacobians for FEM grid
        type(jacobians_type) :: jacobians
!>        Structure for FEM construction table
        type(tables_4_FEM_assembles) :: rhs_tbl
!
!>        lumped mass matrix
        type(lumped_mass_matrices) ::    m_lump
      end type finite_element_integration
!
!   ---------------------------------------------------------------------
!
      contains
!
!   ---------------------------------------------------------------------
!
      subroutine alloc_fem_mat_base_type(mesh, surf, group, rhs_mat)
!
      use t_mesh_data
      use t_surface_data
      use t_group_data
      use m_phys_constants
!
      type(mesh_geometry), intent(in) :: mesh
      type(surface_data), intent(in) :: surf
      type(mesh_groups), intent(in) ::   group
!
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!
!
      call alloc_finite_elem_mat(mesh%node, mesh%ele,                   &
     &    rhs_mat%fem_wk, rhs_mat%f_l, rhs_mat%f_nl)
!
      call alloc_int_surf_data                                          &
     &   (group%surf_grp%num_item, surf%nnod_4_surf, rhs_mat%surf_wk)
!
      end subroutine alloc_fem_mat_base_type
!
!   ---------------------------------------------------------------------
!
      subroutine alloc_fem_int_base_type(mesh, fem_int)
!
      use t_mesh_data
!
      type(mesh_geometry), intent(in) :: mesh
!
      type(finite_element_integration), intent(inout) :: fem_int
!
!
      call alloc_type_fem_lumped_mass(mesh%node%numnod, fem_int%m_lump)
!
      end subroutine alloc_fem_int_base_type
!
!   ---------------------------------------------------------------------
!   ---------------------------------------------------------------------
!
      subroutine dealloc_fem_mat_base_type(rhs_mat)
!
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!
!
      call dealloc_int_surf_data(rhs_mat%surf_wk)
!
      call dealloc_finite_elem_mat                                      &
     &   (rhs_mat%fem_wk, rhs_mat%f_l, rhs_mat%f_nl)
!
      end subroutine dealloc_fem_mat_base_type
!
!   ---------------------------------------------------------------------
!
      subroutine dealloc_fem_int_base_type(fem_int)
!
      type(finite_element_integration), intent(inout) :: fem_int
!
!
      call dealloc_type_fem_lumped_mass(fem_int%m_lump)
!
      end subroutine dealloc_fem_int_base_type
!
!   ---------------------------------------------------------------------
!
      end module t_work_FEM_integration
