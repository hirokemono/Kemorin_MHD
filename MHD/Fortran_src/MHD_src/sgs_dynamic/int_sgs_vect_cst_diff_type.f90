!int_sgs_vect_cst_diff_type.f90
!      module int_sgs_vect_cst_diff_type
!
!     programmed by H.Matsui on July 2005
!     Modified by H. Matsui on Oct., 2006
!
!      subroutine int_sgs_grad_w_const_type(mesh, jac_3d, FEM_elens,    &
!     &          rhs_tbl, nod_fld, iele_fsmp_stack, num_int,            &
!     &          i_filter, ak_diff, i_field, coef, fem_wk, f_nl)
!      subroutine int_sgs_div_w_const_type(mesh, jac_3d, FEM_elens,     &
!     &          rhs_tbl, nod_fld, iele_fsmp_stack, num_int,            &
!     &          i_filter, ak_diff, i_field, coef, fem_wk, f_nl)
!      subroutine int_sgs_rot_w_const_type(mesh, jac_3d, FEM_elens,     &
!     &          rhs_tbl, nod_fld, iele_fsmp_stack, num_int,            &
!     &          i_filter, ak_diff, i_field, coef, fem_wk, f_nl)
!
!      subroutine int_sgs_div_tsr_w_const_type(mesh, jac_3d, FEM_elens, &
!     &          rhs_tbl, nod_fld, iele_fsmp_stack, num_int,            &
!     &          i_filter, ak_diff, i_field, coef, fem_wk, f_nl)
!      subroutine int_sgs_div_as_tsr_w_cst_type(mesh, jac_3d, FEM_elens,&
!     &          rhs_tbl, nod_fld, iele_fsmp_stack, num_int,            &
!     &          i_filter, ak_diff, i_field, coef, fem_wk, f_nl)
!
      module int_sgs_vect_cst_diff_type
!
      use m_precision
!
      use m_phys_constants
      use t_mesh_data
      use t_phys_data
      use t_table_FEM_const
      use t_filter_elength
      use t_finite_element_mat
!
      use nodal_fld_cst_to_ele_type
      use cal_skv_to_ff_smp
      use fem_skv_diffs_sgs_type
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_sgs_grad_w_const_type(mesh, jac_3d, FEM_elens,     &
     &          rhs_tbl, nod_fld, iele_fsmp_stack, num_int,             &
     &          i_filter, ak_diff, i_field, coef, fem_wk, f_nl)
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field, i_filter
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind=kreal), intent(in) :: ak_diff(mesh%ele%numele)
      real(kind = kreal), intent(in) :: coef
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
        call scalar_cst_phys_each_ele_type(mesh, nod_fld,               &
     &          k2, i_field, coef, fem_wk%scalar_1)
        call fem_skv_grad_sgs_galerkin(iele_fsmp_stack, num_int, k2,    &
     &      i_filter, ak_diff, mesh%ele, jac_3d, FEM_elens,             &
     &      fem_wk%scalar_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(mesh%node, mesh%ele, rhs_tbl,           &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_sgs_grad_w_const_type
!
!-----------------------------------------------------------------------
!
      subroutine int_sgs_div_w_const_type(mesh, jac_3d, FEM_elens,      &
     &          rhs_tbl, nod_fld, iele_fsmp_stack, num_int,             &
     &          i_filter, ak_diff, i_field, coef, fem_wk, f_nl)
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field, i_filter
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind=kreal), intent(in) :: ak_diff(mesh%ele%numele)
      real(kind = kreal), intent(in) :: coef
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
        call vector_cst_phys_each_ele_type(mesh, nod_fld,               &
     &          k2, i_field, coef, fem_wk%vector_1)
        call fem_skv_div_sgs_galerkin(iele_fsmp_stack, num_int, k2,     &
     &      i_filter, ak_diff, mesh%ele, jac_3d, FEM_elens,             &
     &      fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp(mesh%node, mesh%ele, rhs_tbl,           &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_sgs_div_w_const_type
!
!-----------------------------------------------------------------------
!
      subroutine int_sgs_rot_w_const_type(mesh, jac_3d, FEM_elens,      &
     &          rhs_tbl, nod_fld, iele_fsmp_stack, num_int,             &
     &          i_filter, ak_diff, i_field, coef, fem_wk, f_nl)
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field, i_filter
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind=kreal), intent(in) :: ak_diff(mesh%ele%numele)
      real(kind = kreal), intent(in) :: coef
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
        call vector_cst_phys_each_ele_type(mesh, nod_fld,               &
     &          k2, i_field, coef, fem_wk%vector_1)
        call fem_skv_rot_sgs_galerkin(iele_fsmp_stack, num_int, k2,     &
     &      i_filter, ak_diff, mesh%ele, jac_3d, FEM_elens,             &
     &      fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(mesh%node, mesh%ele, rhs_tbl,           &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_sgs_rot_w_const_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_sgs_div_tsr_w_const_type(mesh, jac_3d, FEM_elens,  &
     &          rhs_tbl, nod_fld, iele_fsmp_stack, num_int,             &
     &          i_filter, ak_diff, i_field, coef, fem_wk, f_nl)
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field, i_filter
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind=kreal), intent(in) :: ak_diff(mesh%ele%numele)
      real(kind = kreal), intent(in) :: coef
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
        call tensor_cst_phys_each_ele_type(mesh, nod_fld,               &
     &          k2, i_field, coef, fem_wk%tensor_1)
        call fem_skv_div_tensor_sgs_galerkin(iele_fsmp_stack,           &
     &      num_int, k2,  i_filter, ak_diff,                            &
     &      mesh%ele, jac_3d, FEM_elens, fem_wk%tensor_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(mesh%node, mesh%ele, rhs_tbl,           &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_sgs_div_tsr_w_const_type
!
!-----------------------------------------------------------------------
!
      subroutine int_sgs_div_as_tsr_w_cst_type(mesh, jac_3d, FEM_elens, &
     &          rhs_tbl, nod_fld, iele_fsmp_stack, num_int,             &
     &          i_filter, ak_diff, i_field, coef, fem_wk, f_nl)
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field, i_filter
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind=kreal), intent(in) :: ak_diff(mesh%ele%numele)
      real(kind = kreal), intent(in) :: coef
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
        call vector_cst_phys_each_ele_type(mesh, nod_fld,               &
     &          k2, i_field, coef, fem_wk%vector_1)
        call fem_skv_div_as_tsr_sgs_galerkin(iele_fsmp_stack,           &
     &      num_int, k2, i_filter, ak_diff, mesh%ele, jac_3d,           &
     &      FEM_elens, fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(mesh%node, mesh%ele, rhs_tbl,           &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_sgs_div_as_tsr_w_cst_type
!
!-----------------------------------------------------------------------
!
      end module int_sgs_vect_cst_diff_type
