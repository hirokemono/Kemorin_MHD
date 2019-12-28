!>@file   int_vol_vect_diff_upw.f90
!!@brief  module int_vol_vect_diff_upw
!!
!!@author H. Matsui 
!!@date Programmed by H. Matsui in July, 2005
!!        modified by H. Matsui in Oct., 2006
!!
!>@brief  Finite elememt integration for magnetic induction equation
!!
!!@verbatim
!!      subroutine int_vol_gradient_upw                                 &
!!     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,           &
!!     &          iele_fsmp_stack, num_int, dt, i_field,                &
!!     &          ncomp_ele, iv_up, d_ele, fem_wk, f_nl)
!!      subroutine int_vol_divergence_upw                               &
!!     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,           &
!!     &          iele_fsmp_stack, num_int, dt, i_field,                &
!!     &          ncomp_ele, iv_up, d_ele, fem_wk, f_nl)
!!      subroutine int_vol_rotation_upw                                 &
!!     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,           &
!!     &          iele_fsmp_stack, num_int, dt, i_field,                &
!!     &          ncomp_ele, iv_up, d_ele, fem_wk, f_nl)
!!
!!      subroutine int_vol_div_tsr_upw                                  &
!!     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,           &
!!     &          iele_fsmp_stack, num_int, dt, i_field,                &
!!     &          ncomp_ele, iv_up, d_ele, fem_wk, f_nl)
!!      subroutine int_vol_div_as_tsr_upw                               &
!!     &         (node, ele, g_FEM, jac_3d, rhs_tbl,  nod_fld,          &
!!     &          iele_fsmp_stack, num_int, dt, i_field,                &
!!     &          ncomp_ele, iv_up, d_ele, fem_wk, f_nl)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(phys_data),    intent(in) :: nod_fld
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!!@endverbatim
!
      module int_vol_vect_diff_upw
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
      use nodal_fld_2_each_element
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
      subroutine int_vol_gradient_upw                                   &
     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,             &
     &          iele_fsmp_stack, num_int, dt, i_field,                  &
     &          ncomp_ele, iv_up, d_ele, fem_wk, f_nl)
!
      use fem_skv_grad_upw
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_ele, iv_up
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      real(kind = kreal), intent(in) :: dt
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
        call scalar_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, i_field, fem_wk%scalar_1)
        call fem_skv_all_grad_upw                                       &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &      np_smp, iele_fsmp_stack, g_FEM%max_int_point,               &
     &      g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,         &
     &      num_int, k2, dt, jac_3d%ntot_int, jac_3d%xjac,              &
     &      jac_3d%an, jac_3d%dnx, jac_3d%dnx, d_ele(1,iv_up),          &
     &      fem_wk%scalar_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_gradient_upw
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_divergence_upw                                 &
     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,             &
     &          iele_fsmp_stack, num_int, dt, i_field,                  &
     &          ncomp_ele, iv_up, d_ele, fem_wk, f_nl)
!
      use fem_skv_div_upw
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_ele, iv_up
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      real(kind = kreal), intent(in) :: dt
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
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, i_field, fem_wk%vector_1)
        call fem_skv_all_div_upw                                        &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &      np_smp, iele_fsmp_stack, g_FEM%max_int_point,               &
     &      g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,         &
     &      num_int, k2, dt, jac_3d%ntot_int, jac_3d%xjac,              &
     &      jac_3d%an, jac_3d%dnx, jac_3d%dnx, d_ele(1,iv_up),          &
     &      fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_divergence_upw
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_rotation_upw                                   &
     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,             &
     &          iele_fsmp_stack, num_int, dt, i_field,                  &
     &          ncomp_ele, iv_up, d_ele, fem_wk, f_nl)
!
      use fem_skv_rot_upw
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_ele, iv_up
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      real(kind = kreal), intent(in) :: dt
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
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, i_field, fem_wk%vector_1)
        call fem_all_skv_rot_upw                                        &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &      np_smp, iele_fsmp_stack, g_FEM%max_int_point,               &
     &      g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,         &
     &      num_int, k2, dt, jac_3d%ntot_int, jac_3d%xjac,              &
     &      jac_3d%an, jac_3d%dnx, jac_3d%dnx, d_ele(1,iv_up),          &
     &      fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_rotation_upw
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_div_tsr_upw                                    &
     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,             &
     &          iele_fsmp_stack, num_int, dt, i_field,                  &
     &          ncomp_ele, iv_up, d_ele, fem_wk, f_nl)
!
      use fem_skv_div_flux_upw
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_ele, iv_up
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      real(kind = kreal), intent(in) :: dt
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
        call tensor_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, i_field, fem_wk%tensor_1)
        call fem_skv_all_div_flux_upw                                   &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &      np_smp, iele_fsmp_stack, g_FEM%max_int_point,               &
     &      g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,         &
     &      num_int, k2, dt, jac_3d%ntot_int, jac_3d%xjac,              &
     &      jac_3d%an, jac_3d%dnx, jac_3d%dnx, d_ele(1,iv_up),          &
     &      fem_wk%tensor_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_div_tsr_upw
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_div_as_tsr_upw                                 &
     &         (node, ele, g_FEM, jac_3d, rhs_tbl,  nod_fld,            &
     &          iele_fsmp_stack, num_int, dt, i_field,                  &
     &          ncomp_ele, iv_up, d_ele, fem_wk, f_nl)
!
      use fem_skv_div_asym_t_upw
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: num_int
      integer(kind=kint), intent(in) :: i_field
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: ncomp_ele, iv_up
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      real(kind = kreal), intent(in) :: dt
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
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, i_field, fem_wk%vector_1)
        call fem_skv_all_div_asym_t_upw                                 &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &      np_smp, iele_fsmp_stack, g_FEM%max_int_point,               &
     &      g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,         &
     &      num_int, k2, dt, jac_3d%ntot_int, jac_3d%xjac,              &
     &      jac_3d%an, jac_3d%dnx, jac_3d%dnx, d_ele(1,iv_up),          &
     &      fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_div_as_tsr_upw
!
!-----------------------------------------------------------------------
!
      end module int_vol_vect_diff_upw
