!>@file   int_vol_magne_pre.f90
!!@brief  module int_vol_magne_pre
!!
!!@author H. Matsui 
!!@date Programmed by H. Matsui in 2002
!!        modified by H. Matsui in Oct., 2006
!!        modified by H. Matsui in Aug., 2007
!!
!>@brief  Finite elememt integration for magnetic induction equation
!!
!!@verbatim
!!      subroutine int_vol_magne_pre_ele(num_int,                       &
!!     &          SGS_param, cmt_param, node, ele, conduct, cd_prop,    &
!!     &          iphys_base, iphys_SGS, nod_fld,                       &
!!     &          ncomp_ele, d_ele, iphys_ele_base, g_FEM, jac_3d,      &
!!     &          rhs_tbl, FEM_elens, Cdiff_SGS_uxb,                    &
!!     &          mhd_fem_wk, fem_wk, f_nl)
!!      subroutine int_vol_magne_pre_ele_upm(num_int, dt,               &
!!     &          SGS_param, cmt_param, node, ele, conduct, cd_prop,    &
!!     &          iphys_base, iphys_SGS, nod_fld, ncomp_ele, d_ele,     &
!!     &          iphys_ele_base, g_FEM, jac_3d, rhs_tbl, FEM_elens,    &
!!     &          ak_diff, mhd_fem_wk, fem_wk, f_nl)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(base_field_address), intent(in) :: iphys_base
!!        type(SGS_term_address), intent(in) :: iphys_SGS
!!        type(phys_data), intent(in) :: nod_fld
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_model_coefficient), intent(in) :: Cdiff_SGS_uxb
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!@endverbatim
!
      module int_vol_magne_pre
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
      use m_phys_constants
!
      use t_SGS_control_parameter
      use t_physical_property
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_base_field_labels
      use t_SGS_term_labels
      use t_fem_gauss_int_coefs
      use t_jacobians
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_phys_address
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_material_property
      use t_FEM_SGS_model_coefs
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_magne_pre_ele(num_int,                         &
     &          SGS_param, cmt_param, node, ele, conduct, cd_prop,      &
     &          iphys_base, iphys_SGS, nod_fld,                         &
     &          ncomp_ele, d_ele, iphys_ele_base, g_FEM, jac_3d,        &
     &          rhs_tbl, FEM_elens, Cdiff_SGS_uxb,                      &
     &          mhd_fem_wk, fem_wk, f_nl)
!
      use cal_add_smp
      use nodal_fld_2_each_element
      use nodal_fld_cst_to_element
      use sgs_terms_to_each_ele
      use cal_skv_to_ff_smp
      use fem_skv_div_asym_t
      use fem_skv_induction
      use fem_skv_div_sgs_flux_type
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(base_field_address), intent(in) :: iphys_base
      type(SGS_term_address), intent(in) :: iphys_SGS
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: nod_fld
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_model_coefficient), intent(in) :: Cdiff_SGS_uxb
!
      integer(kind = kint), intent(in) :: num_int
      integer(kind = kint), intent(in) :: ncomp_ele
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      integer(kind=kint) :: k2
!
!
!  ---------  set number of integral points
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
!
! -------- loop for shape function for the phsical values
      do k2=1, ele%nnod_4_ele
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, iphys_base%i_velo, mhd_fem_wk%velo_1)
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, iphys_base%i_magne, mhd_fem_wk%magne_1)
!
!$omp parallel
        call add_const_to_vector_smp                                    &
     &     (ele%numele, d_ele(1,iphys_ele_base%i_magne),                &
     &      cd_prop%ex_magne, fem_wk%vector_1)
!$omp end parallel
!
        call fem_skv_induction_pg                                       &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &      np_smp, conduct%istack_ele_fld_smp, g_FEM%max_int_point,    &
     &      g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,         &
     &      jac_3d%ntot_int, num_int, k2,                               &
     &      jac_3d%xjac, jac_3d%an, jac_3d%dnx, mhd_fem_wk%velo_1,      &
     &      mhd_fem_wk%magne_1, d_ele(1,iphys_ele_base%i_velo),         &
     &      fem_wk%vector_1, cd_prop%coef_induct, fem_wk%sk6)
!
        if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none                    &
     &    .and. cmt_param%iflag_c_uxb .eq. id_SGS_commute_ON) then
           call SGS_const_induct_each_ele(node, ele, nod_fld, k2,       &
     &         iphys_base%i_magne, iphys_base%i_velo,                   &
     &         iphys_SGS%i_SGS_induct_t, cd_prop%coef_induct,           &
     &         mhd_fem_wk%sgs_v1, fem_wk%vector_1)
           call fem_skv_div_sgs_asym_tsr                                &
     &        (conduct%istack_ele_fld_smp, num_int, k2,                 &
     &         SGS_param%ifilter_final, ele, g_FEM, jac_3d,             &
     &         FEM_elens, Cdiff_SGS_uxb, mhd_fem_wk%sgs_v1,             &
     &         fem_wk%vector_1, fem_wk%sk6)
        else if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
          call vector_cst_phys_2_each_ele(node, ele, nod_fld, k2,       &
     &        iphys_SGS%i_SGS_induct_t, cd_prop%coef_induct,            &
     &        mhd_fem_wk%sgs_v1)
          call fem_skv_all_div_asym_t                                   &
     &       (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,               &
     &        np_smp, conduct%istack_ele_fld_smp, g_FEM%max_int_point,  &
     &        g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,       &
     &        num_int, k2, jac_3d%ntot_int, jac_3d%xjac,                &
     &        jac_3d%an, jac_3d%dnx, mhd_fem_wk%sgs_v1, fem_wk%sk6)
        end if
      end do
!
      call add3_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_magne_pre_ele
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_magne_pre_ele_upm(num_int, dt,                 &
     &          SGS_param, cmt_param, node, ele, conduct, cd_prop,      &
     &          iphys_base, iphys_SGS, nod_fld, ncomp_ele, d_ele,       &
     &          iphys_ele_base, g_FEM, jac_3d, rhs_tbl, FEM_elens,      &
     &          ak_diff, mhd_fem_wk, fem_wk, f_nl)
!
      use cal_add_smp
      use nodal_fld_2_each_element
      use nodal_fld_cst_to_element
      use sgs_terms_to_each_ele
      use cal_skv_to_ff_smp
      use fem_skv_induction
      use fem_skv_div_sgs_flux_upw
      use fem_skv_div_asym_t_upw
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(base_field_address), intent(in) :: iphys_base
      type(SGS_term_address), intent(in) :: iphys_SGS
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: nod_fld
      type(field_geometry_data), intent(in) :: conduct
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(conductive_property), intent(in) :: cd_prop
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: num_int
      integer(kind = kint), intent(in) :: ncomp_ele
      real(kind = kreal), intent(in) :: d_ele(ele%numele,ncomp_ele)
      real(kind = kreal), intent(in) :: dt
      real(kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      integer(kind=kint) :: k2
!
!  ---------  set number of integral points
!
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
!
! -------- loop for shape function for the phsical values
      do k2=1, ele%nnod_4_ele
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, iphys_base%i_velo, mhd_fem_wk%velo_1)
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, iphys_base%i_magne, mhd_fem_wk%magne_1)
!
!$omp parallel
        call add_const_to_vector_smp                                    &
     &     (ele%numele, d_ele(1,iphys_ele_base%i_magne),                &
     &      cd_prop%ex_magne, fem_wk%vector_1)
!$omp end parallel
!
        call fem_skv_induction_upm                                      &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &      np_smp, conduct%istack_ele_fld_smp, g_FEM%max_int_point,    &
     &      g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,         &
     &      jac_3d%ntot_int, num_int, k2, dt, jac_3d%xjac,              &
     &      jac_3d%an, jac_3d%dnx, jac_3d%dnx,                          &
     &      mhd_fem_wk%velo_1, mhd_fem_wk%magne_1,                      &
     &      d_ele(1,iphys_ele_base%i_velo), fem_wk%vector_1,            &
     &      d_ele(1,iphys_ele_base%i_magne), cd_prop%coef_induct,       &
     &      fem_wk%sk6)
!
        if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none                    &
     &    .and. cmt_param%iflag_c_uxb .eq. id_SGS_commute_ON) then
          call SGS_const_induct_each_ele(node, ele, nod_fld, k2,        &
     &        iphys_base%i_magne, iphys_base%i_velo,                    &
     &        iphys_SGS%i_SGS_induct_t, cd_prop%coef_induct,            &
     &        mhd_fem_wk%sgs_v1, fem_wk%vector_1)
          call fem_skv_div_sgs_asym_t_upwind                            &
     &       (conduct%istack_ele_fld_smp, num_int, k2,                  &
     &        SGS_param%ifilter_final, dt, ak_diff,                     &
     &        ele, g_FEM, jac_3d, FEM_elens,                            &
     &        d_ele(1,iphys_ele_base%i_magne), mhd_fem_wk%sgs_v1,       &
     &        fem_wk%vector_1, fem_wk%sk6)
        else if (SGS_param%iflag_SGS_uxb .ne. id_SGS_none) then
          call vector_cst_phys_2_each_ele(node, ele, nod_fld, k2,       &
     &        iphys_SGS%i_SGS_induct_t, cd_prop%coef_induct,            &
     &        mhd_fem_wk%sgs_v1)
          call fem_skv_all_div_asym_t_upw                               &
     &       (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,               &
     &        np_smp, conduct%istack_ele_fld_smp, g_FEM%max_int_point,  &
     &        g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,       &
     &        num_int, k2, dt, jac_3d%ntot_int, jac_3d%xjac,            &
     &        jac_3d%an, jac_3d%dnx, jac_3d%dnx,                        &
     &        d_ele(1,iphys_ele_base%i_magne), mhd_fem_wk%sgs_v1,       &
     &        fem_wk%sk6)
        end if
!
      end do
!
      call add3_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, f_nl%ff_smp)
!
      end subroutine int_vol_magne_pre_ele_upm
!
!-----------------------------------------------------------------------
!
      end module int_vol_magne_pre
