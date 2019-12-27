!>@file   int_vol_fractional.f90
!!@brief  module int_vol_fractional
!!
!!@author H. Matsui
!!@date Programmed in June. 2005
!
!>@brief  Integration for fractional step scheme
!!
!!@verbatim
!!      subroutine int_vol_div_vect_linear                              &
!!     &         (node, ele, g_FEM, jac_3d, jac_3d_l, rhs_tbl, nod_fld, &
!!     &          iele_fsmp_stack, n_int, i_vector, fem_wk, f_l)
!!      subroutine int_vol_solenoidal_co                                &
!!     &         (node, ele, g_FEM, jac_3d, jac_3d_l, rhs_tbl, nod_fld, &
!!     &          iele_fsmp_stack, n_int, i_scalar, fem_wk, f_nl)
!!
!!      subroutine int_vol_scalar_diffuse                               &
!!     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,           &
!!     &          iele_fsmp_stack, n_int, coef_crank, ak_d, i_scalar,   &
!!     &          fem_wk, f_l)
!!      subroutine int_vol_vector_diffuse                               &
!!     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,           &
!!     &          iele_fsmp_stack, n_int, coef_crank, ak_d, i_vector,   &
!!     &          fem_wk, f_l)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(phys_data),    intent(in) :: nod_fld
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l
!!@endverbatim
!
      module int_vol_fractional
!
      use m_precision
      use m_machine_parameter
      use m_geometry_constants
      use m_phys_constants
!
      use t_geometry_data
      use t_phys_data
      use t_table_FEM_const
      use t_jacobians
      use t_finite_element_mat
      use t_fem_gauss_int_coefs
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_div_vect_linear                                &
     &         (node, ele, g_FEM, jac_3d, jac_3d_l, rhs_tbl, nod_fld,   &
     &          iele_fsmp_stack, n_int, i_vector, fem_wk, f_l)
!
      use cal_skv_to_ff_smp
      use nodal_fld_2_each_element
      use fem_skv_vector_diff_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_3d), intent(in) :: jac_3d_l
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: n_int, i_vector
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele%nnod_4_ele
        call vector_phys_2_each_element(node, ele, nod_fld,   &
     &      k2, i_vector, fem_wk%vector_1)
        call fem_skv_div_to_linear(iele_fsmp_stack, n_int, k2,          &
     &      ele, g_FEM, jac_3d, jac_3d_l, fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp(node, ele, rhs_tbl,           &
     &    fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_vol_div_vect_linear
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_solenoidal_co                                  &
     &          (node, ele, g_FEM, jac_3d, jac_3d_l, rhs_tbl, nod_fld,  &
     &           iele_fsmp_stack, n_int, i_scalar, fem_wk, f_nl)
!
      use cal_skv_to_ff_smp
      use nodal_fld_2_each_element
      use fem_skv_vector_diff_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_3d), intent(in) :: jac_3d_l
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: n_int, i_scalar
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2=1, num_t_linear
        call scalar_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, i_scalar, fem_wk%scalar_1)
        call fem_skv_linear_gradient(iele_fsmp_stack, n_int, k2,        &
     &      ele, g_FEM, jac_3d, jac_3d_l, fem_wk%scalar_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_solenoidal_co
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_scalar_diffuse                                 &
     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,             &
     &          iele_fsmp_stack, n_int, coef_crank, ak_d, i_scalar,     &
     &          fem_wk, f_l)
!
      use cal_skv_to_ff_smp
      use nodal_fld_2_each_element
      use fem_skv_diffusion
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int, i_scalar
      real (kind=kreal), intent(in) :: coef_crank
      real(kind=kreal), intent(in) :: ak_d(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele%nnod_4_ele
        call scalar_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, i_scalar, fem_wk%scalar_1)
        call fem_skv_scalar_diffuse                                     &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &      np_smp, iele_fsmp_stack, g_FEM%max_int_point,               &
     &      g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d, n_int,  &
     &      k2, jac_3d%ntot_int, jac_3d%xjac, jac_3d%dnx, jac_3d%dnx,   &
     &      ak_d, fem_wk%scalar_1, fem_wk%sk6)
      end do
!
      call add1_skv_coef_to_ff_v_smp                                    &
     &   (node, ele, rhs_tbl, coef_crank, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_vol_scalar_diffuse
!
!  ---------------------------------------------------------------------
!
      subroutine int_vol_vector_diffuse                                 &
     &         (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,             &
     &          iele_fsmp_stack, n_int, coef_crank, ak_d, i_vector,     &
     &          fem_wk, f_l)
!
      use cal_skv_to_ff_smp
      use nodal_fld_2_each_element
      use fem_skv_diffusion
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind=kint), intent(in) :: n_int, i_vector
      real (kind=kreal), intent(in) :: coef_crank
      real(kind=kreal), intent(in) :: ak_d(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele%nnod_4_ele
        call vector_phys_2_each_element                                 &
     &     (node, ele, nod_fld, k2, i_vector, fem_wk%vector_1)
        call fem_skv_vector_diffuse                                     &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &      np_smp, iele_fsmp_stack, g_FEM%max_int_point,               &
     &      g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d, n_int,  &
     &      k2, jac_3d%ntot_int, jac_3d%xjac, jac_3d%dnx, jac_3d%dnx,   &
     &      ak_d, fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add3_skv_coef_to_ff_v_smp                                    &
     &   (node, ele, rhs_tbl, coef_crank, fem_wk%sk6, f_l%ff_smp)
!
      end subroutine int_vol_vector_diffuse
!
!  ---------------------------------------------------------------------
!
      end module int_vol_fractional
