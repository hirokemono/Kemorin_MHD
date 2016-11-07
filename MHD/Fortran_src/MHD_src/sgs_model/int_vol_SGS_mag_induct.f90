 !int_vol_SGS_mag_induct.f90
!     module int_vol_SGS_mag_induct
!
!     numerical integration for finite elememt equations of induction
!
!        programmed by H.Matsui and H.Okuda
!                              on July 2000 (ver 1.1)
!        modified by H. Matsui on Oct., 2005
!        modified by H. Matsui on Aug., 2007
!
!!      subroutine int_vol_div_SGS_idct_mod_pg(node, ele,               &
!!     &         nod_fld, iphys, jac_3d, rhs_tbl, FEM_elens, diff_coefs,&
!!     &         iele_fsmp_stack, n_int, i_filter, iak_diff_uxb,        &
!!     &         coef_induct, fem_wk, mhd_fem_wk, f_nl)
!!      subroutine int_vol_div_SGS_idct_mod_upm(node, ele,              &
!!     &         nod_fld, iphys, jac_3d, rhs_tbl, FEM_elens, diff_coefs,&
!!     &         iele_fsmp_stack, n_int, i_filter, iak_diff_uxb,        &
!!     &         coef_induct, ncomp_ele, i_magne, d_ele,                &
!!     &         fem_wk, mhd_fem_wk, f_nl)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(phys_data),    intent(in) :: nod_fld
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!
      module int_vol_SGS_mag_induct
!
      use m_precision
      use m_machine_parameter
!
      use m_phys_constants
      use m_fem_gauss_int_coefs
      use t_geometry_data
      use t_phys_address
      use t_phys_data
      use t_table_FEM_const
      use t_filter_elength
      use t_finite_element_mat
      use t_MHD_finite_element_mat
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
      subroutine int_vol_div_SGS_idct_mod_pg(node, ele,                 &
     &         nod_fld, iphys, jac_3d, rhs_tbl, FEM_elens, diff_coefs,  &
     &         iele_fsmp_stack, n_int, i_filter, iak_diff_uxb,          &
     &         coef_induct, fem_wk, mhd_fem_wk, f_nl)
!
      use sgs_terms_to_each_ele
      use cal_skv_to_ff_smp
      use fem_skv_div_sgs_flux_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int, i_filter, iak_diff_uxb
      real(kind=kreal), intent(in) :: coef_induct
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele%nnod_4_ele
        call SGS_const_induct_each_ele(node, ele, nod_fld,              &
     &      k2, iphys%i_magne, iphys%i_velo, iphys%i_SGS_induct_t,      &
     &      coef_induct, mhd_fem_wk%sgs_v1, fem_wk%vector_1)
        call fem_skv_div_sgs_asym_tsr                                   &
     &     (iele_fsmp_stack, n_int, k2, i_filter,                       &
     &      diff_coefs%num_field, iak_diff_uxb, diff_coefs%ak,          &
     &      ele, jac_3d, FEM_elens, mhd_fem_wk%sgs_v1,                  &
     &      fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_div_SGS_idct_mod_pg
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_div_SGS_idct_mod_upm(node, ele,                &
     &         nod_fld, iphys, jac_3d, rhs_tbl, FEM_elens, diff_coefs,  &
     &         iele_fsmp_stack, n_int, i_filter, iak_diff_uxb,          &
     &         coef_induct, ncomp_ele, i_magne, d_ele,                  &
     &         fem_wk, mhd_fem_wk, f_nl)
!
      use sgs_terms_to_each_ele
      use cal_skv_to_ff_smp
      use fem_skv_div_sgs_flux_upw
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(phys_data),    intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: diff_coefs
!
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: n_int, i_filter, iak_diff_uxb
!
      integer(kind = kint), intent(in) :: ncomp_ele, i_magne
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      real(kind=kreal), intent(in) :: coef_induct
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      integer(kind=kint) :: k2
!
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
      do k2 = 1, ele%nnod_4_ele
        call SGS_const_induct_each_ele(node, ele, nod_fld,              &
     &      k2, iphys%i_magne, iphys%i_velo, iphys%i_SGS_induct_t,      &
     &      coef_induct, mhd_fem_wk%sgs_v1, fem_wk%vector_1)
        call fem_skv_div_sgs_asym_t_upwind                              &
     &     (iele_fsmp_stack, n_int, k2, i_filter,                       &
     &      diff_coefs%num_field, iak_diff_uxb, diff_coefs%ak,          &
     &      ele, jac_3d, FEM_elens, d_ele(1,i_magne),                   &
     &      mhd_fem_wk%sgs_v1, fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_div_SGS_idct_mod_upm
!
!-----------------------------------------------------------------------
!
      end module int_vol_SGS_mag_induct
