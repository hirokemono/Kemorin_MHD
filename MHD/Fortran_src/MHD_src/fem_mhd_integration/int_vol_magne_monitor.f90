!
!     module int_vol_magne_monitor
!
!     numerical integration for finite elememt equations of induction
!
!        programmed by H.Matsui and H.Okuda
!                              on July 2000 (ver 1.1)
!        modified by H. Matsui on Oct., 2005
!        modified by H. Matsui on Aug., 2007
!
!!      subroutine int_vol_magne_monitor_pg(i_field, iak_diff_uxb,      &
!!     &         node, ele, conduct, iphys, nod_fld, iphys_ele, ele_fld,&
!!     &         jac_3d, rhs_tbl, FEM_elen, diff_coefs,                 &
!!     &         mhd_fem_wk, fem_wk, f_nl)
!!      subroutine int_vol_magne_monitor_upm(i_field, iak_diff_uxb,     &
!!     &         node, ele, conduct, iphys, nod_fld, iphys_ele, ele_fld,&
!!     &         jac_3d, rhs_tbl, FEM_elen, diff_coefs,                 &
!!     &         mhd_fem_wk, fem_wk, f_nl)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elen
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      module int_vol_magne_monitor
!
      use m_precision
!
      use m_control_parameter
      use m_machine_parameter
      use m_phys_constants
      use m_physical_property
      use m_fem_gauss_int_coefs
!
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_phys_address
      use t_MHD_finite_element_mat
      use t_filter_elength
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
      subroutine int_vol_magne_monitor_pg(i_field, iak_diff_uxb,        &
     &         node, ele, conduct, iphys, nod_fld, iphys_ele, ele_fld,  &
     &         jac_3d, rhs_tbl, FEM_elen, diff_coefs,                   &
     &         mhd_fem_wk, fem_wk, f_nl)
!
      use int_vol_vect_differences
      use int_vol_vect_cst_difference
      use int_vol_mag_induction
      use int_vol_SGS_mag_induct
!
      integer(kind=kint), intent(in) :: i_field, iak_diff_uxb
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: conduct
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(SGS_coefficients_type), intent(in) :: diff_coefs
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!
      if (i_field .eq. iphys%i_induction) then
        call int_vol_mag_induct_pg(node, ele,                          &
     &      jac_3d, rhs_tbl, nod_fld, iphys, iphys_ele,                &
     &      conduct%istack_ele_fld_smp, intg_point_t_evo,              &
     &      ele_fld%ntot_phys, ele_fld%d_fld,                          &
     &      fem_wk, mhd_fem_wk, f_nl)
!
      else if (i_field .eq. iphys%i_induct_div) then
        call int_vol_div_asym_tsr                                       &
     &     (node, ele, jac_3d, rhs_tbl, nod_fld,                        &
     &      conduct%istack_ele_fld_smp, intg_point_t_evo,               &
     &      iphys%i_induct_t, fem_wk, f_nl)
!
      else if (i_field .eq. iphys%i_SGS_induction) then
        if(iflag_commute_induction .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_idct_mod_pg(node, ele,                   &
     &        nod_fld, iphys, jac_3d, rhs_tbl, FEM_elen, diff_coefs,    &
     &        conduct%istack_ele_fld_smp, intg_point_t_evo,             &
     &        ifilter_final, iak_diff_uxb, coef_induct,                 &
     &        fem_wk, mhd_fem_wk, f_nl)
        else
          call int_vol_div_as_tsr_w_const                               &
     &       (node, ele, jac_3d, rhs_tbl, nod_fld,                      &
     &        conduct%istack_ele_fld_smp, intg_point_t_evo,             &
     &        iphys%i_SGS_induct_t, coef_induct, fem_wk, f_nl)
        end if
      end if
!
      end subroutine int_vol_magne_monitor_pg
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_magne_monitor_upm(i_field, iak_diff_uxb,       &
     &         node, ele, conduct, iphys, nod_fld, iphys_ele, ele_fld,  &
     &         jac_3d, rhs_tbl, FEM_elen, diff_coefs,                   &
     &         mhd_fem_wk, fem_wk, f_nl)
!
      use int_vol_vect_diff_upw
      use int_vol_vect_cst_diff_upw
      use int_vol_mag_induction
      use int_vol_SGS_mag_induct
!
      integer(kind=kint), intent(in) :: i_field, iak_diff_uxb
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: conduct
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(SGS_coefficients_type), intent(in) :: diff_coefs
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!
      if (i_field .eq. iphys%i_induction) then
        call int_vol_mag_induct_upm(node, ele,                          &
     &      jac_3d, rhs_tbl, nod_fld, iphys, iphys_ele,                 &
     &      conduct%istack_ele_fld_smp, intg_point_t_evo,               &
     &      ele_fld%ntot_phys, ele_fld%d_fld,                           &
     &      fem_wk, mhd_fem_wk, f_nl)
!
      else if (i_field .eq. iphys%i_induct_div) then
        call int_vol_div_as_tsr_upw                                     &
     &     (node, ele, jac_3d, rhs_tbl, nod_fld,                        &
     &      conduct%istack_ele_fld_smp, intg_point_t_evo,               &
     &      iphys%i_induct_t, ele_fld%ntot_phys, iphys_ele%i_magne,     &
     &      ele_fld%d_fld, fem_wk, f_nl)
!
      else if (i_field .eq. iphys%i_SGS_induction) then
        if(iflag_commute_induction .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_idct_mod_upm(node, ele,                  &
     &        nod_fld, iphys, jac_3d, rhs_tbl, FEM_elen, diff_coefs,    &
     &        conduct%istack_ele_fld_smp, intg_point_t_evo,             &
     &        ifilter_final, iak_diff_uxb, coef_induct,                 &
     &        ele_fld%ntot_phys, iphys_ele%i_magne, ele_fld%d_fld,      &
     &        fem_wk, mhd_fem_wk, f_nl)
        else
          call int_vol_div_as_tsr_cst_upw                               &
     &       (node, ele, jac_3d, rhs_tbl, nod_fld,                      &
     &        conduct%istack_ele_fld_smp, intg_point_t_evo,             &
     &        iphys%i_SGS_induct_t, ele_fld%ntot_phys,                  &
     &        iphys_ele%i_magne, ele_fld%d_fld, coef_induct,            &
     &        fem_wk, f_nl)
        end if
      end if
!
      end subroutine int_vol_magne_monitor_upm
!
!-----------------------------------------------------------------------
!
      end module int_vol_magne_monitor
