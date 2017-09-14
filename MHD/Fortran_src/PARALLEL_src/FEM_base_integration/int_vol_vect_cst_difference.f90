!int_vol_vect_cst_difference.f90
!      module int_vol_vect_cst_difference
!
!     programmed by H.Matsui on July 2005
!     Modified by H. Matsui on Oct., 2006
!
!!      subroutine int_vol_grad_w_const                                 &
!!     &         (node, ele, jac_3d, rhs_tbl, nod_fld, iele_fsmp_stack, &
!!     &          num_int, i_field, coef, fem_wk, f_nl)
!!      subroutine int_vol_div_w_const                                  &
!!     &         (node, ele, jac_3d, rhs_tbl, nod_fld, iele_fsmp_stack, &
!!     &          num_int, i_field, coef, fem_wk, f_nl)
!!      subroutine int_vol_rot_w_const                                  &
!!     &         (node, ele, jac_3d, rhs_tbl, nod_fld, iele_fsmp_stack, &
!!     &          num_int, i_field, coef, fem_wk, f_nl)
!!
!!      subroutine int_vol_div_tsr_w_const                              &
!!     &         (node, ele, jac_3d, rhs_tbl, nod_fld, iele_fsmp_stack, &
!!     &          num_int, i_field, coef, fem_wk, f_nl)
!!      subroutine int_vol_div_as_tsr_w_const                           &
!!     &         (node, ele, jac_3d, rhs_tbl, nod_fld, iele_fsmp_stack, &
!!     &          num_int, i_field, coef, fem_wk, f_nl)
!!        type(node_data), intent(in) ::    node
!!        type(element_data), intent(in) :: ele
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(phys_data),intent(in) ::     nod_fld
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!
      module int_vol_vect_cst_difference
!
      use m_precision
!
      use m_phys_constants
!
      use t_geometry_data
      use t_phys_data
      use m_fem_gauss_int_coefs
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
!
      use nodal_fld_cst_to_element
      use cal_skv_to_ff_smp
      use fem_skv_vector_diff_type
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_grad_w_const                                   &
     &         (node, ele, jac_3d, rhs_tbl, nod_fld, iele_fsmp_stack,   &
     &          num_int, i_field, coef, fem_wk, f_nl)
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: coef
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),intent(in) ::     nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! -------- loop for shape function for the field values
!
      do k2 = 1, ele%nnod_4_ele
        call scalar_cst_phys_2_each_ele(node, ele, nod_fld,             &
     &      k2, i_field, coef, fem_wk%scalar_1)
        call fem_skv_gradient(iele_fsmp_stack, num_int, k2,             &
     &      ele, g_FEM1, jac_3d, fem_wk%scalar_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_grad_w_const
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_div_w_const                                    &
     &         (node, ele, jac_3d, rhs_tbl, nod_fld, iele_fsmp_stack,   &
     &          num_int, i_field, coef, fem_wk, f_nl)
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: coef
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),intent(in) ::     nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
! -------- loop for shape function for the field values
!
      do k2 = 1, ele%nnod_4_ele
        call vector_cst_phys_2_each_ele(node, ele, nod_fld,             &
     &      k2, i_field, coef, fem_wk%vector_1)
        call fem_skv_divergence(iele_fsmp_stack, num_int, k2,           &
     &      ele, g_FEM1, jac_3d, fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_div_w_const
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_rot_w_const                                    &
     &         (node, ele, jac_3d, rhs_tbl, nod_fld, iele_fsmp_stack,   &
     &          num_int, i_field, coef, fem_wk, f_nl)
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: coef
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),intent(in) ::     nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! -------- loop for shape function for the field values
!
      do k2 = 1, ele%nnod_4_ele
        call vector_cst_phys_2_each_ele(node, ele, nod_fld,             &
     &      k2, i_field, coef, fem_wk%vector_1)
        call fem_skv_rotation(iele_fsmp_stack, num_int, k2,             &
     &      ele, g_FEM1, jac_3d, fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_rot_w_const
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_div_tsr_w_const                                &
     &         (node, ele, jac_3d, rhs_tbl, nod_fld, iele_fsmp_stack,   &
     &          num_int, i_field, coef, fem_wk, f_nl)
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: coef
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),intent(in) ::     nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! -------- loop for shape function for the field values
!
      do k2 = 1, ele%nnod_4_ele
        call tensor_cst_phys_2_each_ele(node, ele, nod_fld,             &
     &      k2, i_field, coef, fem_wk%tensor_1)
        call fem_skv_div_tensor(iele_fsmp_stack, num_int, k2,           &
     &      ele, g_FEM1, jac_3d, fem_wk%tensor_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_div_tsr_w_const
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_div_as_tsr_w_const                             &
     &         (node, ele, jac_3d, rhs_tbl, nod_fld, iele_fsmp_stack,   &
     &          num_int, i_field, coef, fem_wk, f_nl)
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: coef
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),intent(in) ::     nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! -------- loop for shape function for the field values
!
      do k2 = 1, ele%nnod_4_ele
        call vector_cst_phys_2_each_ele(node, ele, nod_fld,             &
     &      k2, i_field, coef, fem_wk%vector_1)
        call fem_skv_div_asym_tsr(iele_fsmp_stack, num_int, k2,         &
     &      ele, g_FEM1, jac_3d, fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_div_as_tsr_w_const
!
!-----------------------------------------------------------------------
!
      end module int_vol_vect_cst_difference
