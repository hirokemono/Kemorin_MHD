!
!      module int_region_ele_field_2_node
!
!     Written by H. Matsui on Oct., 2006
!
!!      subroutine int_ele_scalar_2_node(iele_fsmp_stack, m_lump,       &
!!     &          node, ele, jac_3d, rhs_tbl, scalar_ele,               &
!!     &          scalar_nod, fem_wk, f_l)
!!      subroutine int_ele_vector_2_node(iele_fsmp_stack, m_lump,       &
!!     &          node, ele, jac_3d, rhs_tbl, vector_ele,               &
!!     &          vector_nod, fem_wk, f_l)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l
!
      module int_region_ele_field_2_node
!
      use m_precision
      use m_constants
      use m_phys_constants
!
      use t_geometry_data
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_ele_scalar_2_node(iele_fsmp_stack, m_lump,         &
     &          node, ele, jac_3d, rhs_tbl, scalar_ele,                 &
     &          scalar_nod, fem_wk, f_l)
!
      use int_element_field_2_node
      use cal_ff_smp_to_ffs
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind = kreal), intent(in) :: scalar_ele(ele%numele)
      real(kind = kreal), intent(inout) :: scalar_nod(node%numnod)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      call int_area_ele_scalar_2_node(node, ele, jac_3d, rhs_tbl,       &
     &    iele_fsmp_stack, scalar_ele, fem_wk, f_l)
      call cal_ff_smp_2_scalar(node, rhs_tbl,                           &
     &    f_l%ff_smp, m_lump%ml, n_scalar, ione, scalar_nod)
!
      end subroutine int_ele_scalar_2_node
!
!-----------------------------------------------------------------------
!
      subroutine int_ele_vector_2_node(iele_fsmp_stack, m_lump,         &
     &          node, ele, jac_3d, rhs_tbl, vector_ele,                 &
     &          vector_nod, fem_wk, f_l)
!
      use int_element_field_2_node
      use cal_ff_smp_to_ffs
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
      integer (kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      real(kind = kreal), intent(in)                                    &
     &                   :: vector_ele(ele%numele,n_vector)
      real(kind = kreal), intent(inout)                                 &
     &                   :: vector_nod(node%numnod,n_vector)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      call int_area_ele_vector_2_node(node, ele, jac_3d, rhs_tbl,       &
     &    iele_fsmp_stack, vector_ele, fem_wk, f_l)
      call cal_ff_smp_2_vector(node, rhs_tbl,                           &
     &    f_l%ff_smp, m_lump%ml, n_vector, ione, vector_nod)
!
      end subroutine int_ele_vector_2_node
!
!-----------------------------------------------------------------------
!
      end module int_region_ele_field_2_node
