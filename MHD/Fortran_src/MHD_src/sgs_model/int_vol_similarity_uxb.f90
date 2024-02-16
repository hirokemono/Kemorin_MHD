!>@file   int_vol_similarity_uxb.f90
!!@brief  module int_vol_similarity_uxb
!!
!!@author H. Matsui
!!@date Programmed in 2004
!!      Modified in July, 2007
!
!>@brief  Integration of SGS induction term by scale similarity model
!!
!!@verbatim
!!      subroutine int_simi_vp_induct(num_int, node, ele, conduct,      &
!!     &          iphys_SGS_wk, nod_fld, g_FEM, jac_3d, rhs_tbl,        &
!!     &          Csim_SGS_uxb, fem_wk, f_nl)
!!      subroutine int_simi_vp_induct_upm                               &
!!     &         (num_int, dt, node, ele, conduct, iphys_SGS_wk,        &
!!     &          nod_fld, g_FEM, jac_3d, rhs_tbl, Csim_SGS_uxb,        &
!!     &          ncomp_ele, iele_magne, d_ele, fem_wk, f_nl)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
!!        type(phys_data), intent(in) :: nod_fld
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(SGS_model_coefficient), intent(in) :: Csim_SGS_uxb
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!!@endverbatim
!
      module int_vol_similarity_uxb
!
      use m_precision
      use m_machine_parameter
      use m_phys_constants
!
      use t_FEM_control_parameter
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_SGS_model_coef_labels
      use t_fem_gauss_int_coefs
      use t_jacobians
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_material_property
      use t_SGS_model_coefs
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_simi_vp_induct(num_int, node, ele, conduct,        &
     &          iphys_SGS_wk, nod_fld, g_FEM, jac_3d, rhs_tbl,          &
     &          Csim_SGS_uxb, fem_wk, f_nl)
!
      use nodal_fld_2_each_element
      use fem_skv_nodal_field
      use cal_skv_to_ff_smp
      use cal_product_to_skv
!
      integer (kind=kint), intent(in) :: num_int
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
      type(phys_data), intent(in) :: nod_fld
      type(field_geometry_data), intent(in) :: conduct
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(SGS_model_coefficient), intent(in) :: Csim_SGS_uxb
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
      do k2 = 1, ele%nnod_4_ele
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, iphys_SGS_wk%i_simi, fem_wk%vector_1)
        call fem_skv_vector_field                                       &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &      conduct%istack_ele_fld_smp,  &
     &      g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3, &
     &      g_FEM%owe3d, jac_3d%ntot_int, num_int, k2, jac_3d%xjac,     &
     &      jac_3d%an, jac_3d%an, fem_wk%vector_1, fem_wk%sk6)
        call scalar_prod_to_skv_tensor                                  &
     &     (ele%numele, conduct%istack_ele_fld_smp, ele%nnod_4_ele,     &
     &      Csim_SGS_uxb%coef(1,1), fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_simi_vp_induct
!
!-----------------------------------------------------------------------
!
      subroutine int_simi_vp_induct_upm                                 &
     &         (num_int, dt, node, ele, conduct, iphys_SGS_wk,          &
     &          nod_fld, g_FEM, jac_3d, rhs_tbl, Csim_SGS_uxb,          &
     &          ncomp_ele, iele_magne, d_ele, fem_wk, f_nl)
!
      use nodal_fld_2_each_element
      use fem_skv_nodal_fld_upwind
      use cal_skv_to_ff_smp
      use cal_product_to_skv
!
      integer (kind=kint), intent(in) :: num_int
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
      type(phys_data), intent(in) :: nod_fld
      type(field_geometry_data), intent(in) :: conduct
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(SGS_model_coefficient), intent(in) :: Csim_SGS_uxb
!
      integer(kind = kint), intent(in) :: ncomp_ele, iele_magne
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      real(kind = kreal), intent(in) :: dt
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind = kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
      do k2 = 1, ele%nnod_4_ele
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, iphys_SGS_wk%i_simi, fem_wk%vector_1)
!
        call fem_skv_vector_field_upwind(conduct%istack_ele_fld_smp,    &
     &      num_int, k2, dt, d_ele(1,iele_magne), ele, g_FEM, jac_3d,   &
     &      fem_wk%vector_1, fem_wk%sk6)
!
        call scalar_prod_to_skv_tensor                                  &
     &     (ele%numele, conduct%istack_ele_fld_smp, ele%nnod_4_ele,     &
     &      Csim_SGS_uxb%coef(1,1), fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_simi_vp_induct_upm
!
!-----------------------------------------------------------------------
!
      end module int_vol_similarity_uxb
