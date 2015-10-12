!int_vol_vect_diff_type.f90
!      module int_vol_vect_diff_type
!
!     programmed by H.Matsui on July 2005
!     Modified by H. Matsui on Oct., 2006
!
!      subroutine int_vol_grad_type(mesh, jac_3d, rhs_tbl, nod_fld,     &
!     &          iele_fsmp_stack, num_int, i_field, fem_wk, f_nl)
!      subroutine int_vol_div_type(mesh, jac_3d, rhs_tbl, nod_fld,      &
!     &          iele_fsmp_stack, num_int, i_field, fem_wk, f_nl)
!      subroutine int_vol_rot_type(mesh, jac_3d, rhs_tbl, nod_fld,      &
!     &          iele_fsmp_stack, num_int, i_field, fem_wk, f_nl)
!
!      subroutine int_vol_div_tsr_type(mesh, jac_3d, rhs_tbl, nod_fld,  &
!     &          iele_fsmp_stack, num_int, i_field, fem_wk, f_nl)
!      subroutine int_vol_div_as_tsr_type(mesh, jac_3d, rhs_tbl,        &
!     &          nod_fld, iele_fsmp_stack, num_int, i_field,            &
!     &          fem_wk, f_nl)
!
      module int_vol_vect_diff_type
!
      use m_precision
!
      use m_phys_constants
      use t_mesh_data
      use t_phys_data
      use t_table_FEM_const
      use t_finite_element_mat
!
      use nodal_fld_2_each_element
      use cal_skv_to_ff_smp
      use fem_skv_vector_diff_type
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_grad_type(mesh, jac_3d, rhs_tbl, nod_fld,      &
     &          iele_fsmp_stack, num_int, i_field, fem_wk, f_nl)
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: i_field, num_int
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, mesh%ele, fem_wk%sk6)
!
! -------- loop for shape function for the field values
!
      do k2 = 1, mesh%ele%nnod_4_ele
        call scalar_phys_2_each_element(mesh%node, mesh%ele, nod_fld,   &
     &          k2, i_field, fem_wk%scalar_1)
        call fem_skv_gradient(iele_fsmp_stack, num_int, k2,             &
     &      mesh%ele, jac_3d, fem_wk%scalar_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(mesh%node, mesh%ele, rhs_tbl,           &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_grad_type
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_div_type(mesh, jac_3d, rhs_tbl, nod_fld,       &
     &          iele_fsmp_stack, num_int, i_field, fem_wk, f_nl)
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: i_field, num_int
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_scalar, mesh%ele, fem_wk%sk6)
!
! -------- loop for shape function for the field values
!
      do k2 = 1, mesh%ele%nnod_4_ele
        call vector_phys_2_each_element(mesh%node, mesh%ele, nod_fld,   &
     &          k2, i_field, fem_wk%vector_1)
        call fem_skv_divergence(iele_fsmp_stack, num_int, k2,           &
     &      mesh%ele, jac_3d, fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp(mesh%node, mesh%ele, rhs_tbl,           &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_div_type
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_rot_type(mesh, jac_3d, rhs_tbl, nod_fld,       &
     &          iele_fsmp_stack, num_int, i_field, fem_wk, f_nl)
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: i_field, num_int
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, mesh%ele, fem_wk%sk6)
!
! -------- loop for shape function for the field values
!
      do k2 = 1, mesh%ele%nnod_4_ele
        call vector_phys_2_each_element(mesh%node, mesh%ele, nod_fld,   &
     &          k2, i_field, fem_wk%vector_1)
        call fem_skv_rotation(iele_fsmp_stack, num_int, k2,             &
     &      mesh%ele, jac_3d, fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(mesh%node, mesh%ele, rhs_tbl,           &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_rot_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_div_tsr_type(mesh, jac_3d, rhs_tbl, nod_fld,   &
     &          iele_fsmp_stack, num_int, i_field, fem_wk, f_nl)
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: i_field, num_int
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, mesh%ele, fem_wk%sk6)
!
! -------- loop for shape function for the field values
!
      do k2 = 1, mesh%ele%nnod_4_ele
        call tensor_phys_2_each_element(mesh%node, mesh%ele, nod_fld,   &
     &      k2, i_field, fem_wk%tensor_1)
        call fem_skv_div_tensor(iele_fsmp_stack, num_int, k2,           &
     &      mesh%ele, jac_3d, fem_wk%tensor_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(mesh%node, mesh%ele, rhs_tbl,           &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_div_tsr_type
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_div_as_tsr_type(mesh, jac_3d, rhs_tbl,         &
     &          nod_fld, iele_fsmp_stack, num_int, i_field,             &
     &          fem_wk, f_nl)
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: i_field, num_int
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, mesh%ele, fem_wk%sk6)
!
! -------- loop for shape function for the field values
!
      do k2 = 1, mesh%ele%nnod_4_ele
        call vector_phys_2_each_element(mesh%node, mesh%ele, nod_fld,   &
     &          k2, i_field, fem_wk%vector_1)
        call fem_skv_div_asym_tsr(iele_fsmp_stack, num_int, k2,         &
     &      mesh%ele, jac_3d, fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(mesh%node, mesh%ele, rhs_tbl,           &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_div_as_tsr_type
!
!-----------------------------------------------------------------------
!
      end module int_vol_vect_diff_type
