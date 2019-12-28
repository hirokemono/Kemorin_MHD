!>@file   int_vol_vect_cst_difference.f90
!!@brief  module int_vol_vect_cst_difference
!!
!!@author H. Matsui 
!!@date Programmed by H. Matsui in July, 2005
!!        modified by H. Matsui in Oct., 2006
!!
!>@brief  Finite elememt integration for fifferences wirh constant
!!
!!@verbatim
!!      subroutine int_vol_grad_w_const                                 &
!!     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,           &
!!     &          iele_fsmp_stack, num_int, i_field, coef, fem_wk, f_nl)
!!      subroutine int_vol_div_w_const                                  &
!!     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,           &
!!     &          iele_fsmp_stack, num_int, i_field, coef, fem_wk, f_nl)
!!      subroutine int_vol_rot_w_const                                  &
!!     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,           &
!!     &          iele_fsmp_stack, num_int, i_field, coef, fem_wk, f_nl)
!!
!!      subroutine int_vol_div_tsr_w_const                              &
!!     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,           &
!!     &          iele_fsmp_stack,  num_int, i_field, coef, fem_wk, f_nl)
!!      subroutine int_vol_div_as_tsr_w_const                           &
!!     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,           &
!!     &          iele_fsmp_stack, num_int, i_field, coef, fem_wk, f_nl)
!!        type(node_data), intent(in) ::    node
!!        type(element_data), intent(in) :: ele
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(phys_data),intent(in) ::     nod_fld
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!!@endverbatim
!
      module int_vol_vect_cst_difference
!
      use m_precision
      use m_machine_parameter
      use m_geometry_constants
      use m_phys_constants
!
      use t_geometry_data
      use t_phys_data
      use t_fem_gauss_int_coefs
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
!
      use nodal_fld_cst_to_element
      use cal_skv_to_ff_smp
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
     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,             &
     &          iele_fsmp_stack, num_int, i_field, coef, fem_wk, f_nl)
!
      use fem_skv_grad
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: coef
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
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
        call fem_skv_all_grad                                           &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &      np_smp, iele_fsmp_stack, g_FEM%max_int_point,               &
     &      g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,         &
     &      num_int, k2, jac_3d%ntot_int, jac_3d%xjac,                  &
     &      jac_3d%an, jac_3d%dnx, fem_wk%scalar_1, fem_wk%sk6)
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
     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,             &
     &          iele_fsmp_stack, num_int, i_field, coef, fem_wk, f_nl)
!
      use fem_skv_div
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: coef
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
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
        call fem_skv_all_div                                            &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &      np_smp, iele_fsmp_stack, g_FEM%max_int_point,               &
     &      g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,         &
     &      num_int, k2, jac_3d%ntot_int, jac_3d%xjac,                  &
     &      jac_3d%an, jac_3d%dnx, fem_wk%vector_1, fem_wk%sk6)
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
     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,             &
     &          iele_fsmp_stack, num_int, i_field, coef, fem_wk, f_nl)
!
      use fem_skv_rot
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: coef
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
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
        call fem_all_skv_rot                                            &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &      np_smp, iele_fsmp_stack, g_FEM%max_int_point,               &
     &      g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,         &
     &      num_int, k2, jac_3d%ntot_int, jac_3d%xjac,                  &
     &      jac_3d%an, jac_3d%dnx, fem_wk%vector_1, fem_wk%sk6)
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
     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,             &
     &          iele_fsmp_stack,  num_int, i_field, coef, fem_wk, f_nl)
!
      use fem_skv_div_flux
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: coef
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
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
        call fem_skv_all_div_flux                                       &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &      np_smp, iele_fsmp_stack, g_FEM%max_int_point,               &
     &      g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,         &
     &      num_int, k2, jac_3d%ntot_int, jac_3d%xjac,                  &
     &      jac_3d%an, jac_3d%dnx, fem_wk%tensor_1, fem_wk%sk6)
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
     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,             &
     &          iele_fsmp_stack, num_int, i_field, coef, fem_wk, f_nl)
!
      use fem_skv_div_asym_t
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: coef
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
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
        call fem_skv_all_div_asym_t                                     &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &      np_smp, iele_fsmp_stack, g_FEM%max_int_point,               &
     &      g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,         &
     &      num_int, k2, jac_3d%ntot_int, jac_3d%xjac,                  &
     &      jac_3d%an, jac_3d%dnx, fem_wk%vector_1, fem_wk%sk6)
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
