!
!     module int_vol_temp_monitor
!
!     numerical integration for finite elememt equations of heat
!
!     programmed by H.Matsui and H.Okuda
!                                    on July 2002
!     modified by H. Matsui on Aug., 2005
!     modified by H. Matsui on Aug., 2007
!
!!      subroutine int_vol_ene_monitor(i_field, SGS_param, cmt_param,   &
!!     &          node, ele, fluid, property, iphys, nod_fld,           &
!!     &          iphys_ele, ele_fld, jac_3d, rhs_tbl, FEM_elens,       &
!!     &          diff_coefs, mhd_fem_wk, fem_wk, f_nl)
!!      subroutine int_vol_ene_monitor_upw                              &
!!     &         (i_field, SGS_param, cmt_param,                        &
!!     &          node, ele, fluid, property, iphys, nod_fld,           &
!!     &          iphys_ele, ele_fld, jac_3d, rhs_tbl, FEM_elens,       &
!!     &          diff_coefs,  mhd_fem_wk, fem_wk, f_nl)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(scalar_property), intent(in) :: property
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_terms_address), intent(in) :: ifld_diff
!!        type(SGS_coefficients_type), intent(in) :: diff_coefs
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      module int_vol_temp_monitor
!
      use m_precision
!
      use t_SGS_control_parameter
      use t_physical_property
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
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
      subroutine int_vol_ene_monitor(i_field, SGS_param, cmt_param,     &
     &          node, ele, fluid, property, iphys, nod_fld,             &
     &          iphys_ele, ele_fld, jac_3d, rhs_tbl, FEM_elens,         &
     &          ifld_diff, diff_coefs, mhd_fem_wk, fem_wk, f_nl)
!
      use int_vol_inertia
      use int_vol_vect_cst_difference
      use int_vol_SGS_div_flux
!
      integer (kind=kint), intent(in) :: i_field
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid
      type(scalar_property), intent(in) :: property
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(SGS_coefficients_type), intent(in) :: diff_coefs
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!
      if (i_field .eq. iphys%i_SGS_div_h_flux) then
        if(cmt_param%iflag_c_hf .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_vec_flux(node, ele, nod_fld,             &
     &        jac_3d, rhs_tbl, FEM_elens, diff_coefs,                   &
     &        fluid%istack_ele_fld_smp, intg_point_t_evo,               &
     &        iphys%i_velo, iphys%i_temp, iphys%i_SGS_h_flux,           &
     &        SGS_param%ifilter_final, ifld_diff%i_heat_flux,           &
     &        property%coef_nega_adv, fem_wk, mhd_fem_wk, f_nl)
        else
          call int_vol_div_w_const                                      &
     &       (node, ele, jac_3d, rhs_tbl, nod_fld,                      &
     &        fluid%istack_ele_fld_smp, intg_point_t_evo,               &
     &        iphys%i_SGS_h_flux, property%coef_nega_adv, fem_wk, f_nl)
        end if
      end if
!
      end subroutine int_vol_ene_monitor
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_ene_monitor_upw                                &
     &         (i_field, SGS_param, cmt_param,                          &
     &          node, ele, fluid, property, iphys, nod_fld,             &
     &          iphys_ele, ele_fld, jac_3d, rhs_tbl, FEM_elens,         &
     &          ifld_diff, diff_coefs, mhd_fem_wk, fem_wk, f_nl)
!
      use int_vol_inertia
      use int_vol_vect_cst_diff_upw
      use int_vol_SGS_div_flux
!
      integer (kind = kint), intent(in) :: i_field
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid
      type(scalar_property), intent(in) :: property
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(SGS_coefficients_type), intent(in) :: diff_coefs
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!
      if (i_field .eq. iphys%i_SGS_div_h_flux) then
        if(cmt_param%iflag_c_hf .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_vec_flux_upw(node, ele, nod_fld,         &
     &        jac_3d, rhs_tbl, FEM_elens, diff_coefs,                   &
     &        fluid%istack_ele_fld_smp, intg_point_t_evo,               &
     &        iphys%i_velo, iphys%i_temp, iphys%i_SGS_h_flux,           &
     &        SGS_param%ifilter_final, ifld_diff%i_heat_flux,           &
     &        ele_fld%ntot_phys, iphys_ele%i_velo, ele_fld%d_fld,       &
     &        property%coef_nega_adv, fem_wk, mhd_fem_wk, f_nl)
        else
          call int_vol_div_w_const_upw                                  &
     &       (node, ele, jac_3d, rhs_tbl, nod_fld,                      &
              fluid%istack_ele_fld_smp, intg_point_t_evo,               &
     &        iphys%i_SGS_h_flux, ele_fld%ntot_phys, iphys_ele%i_velo,  &
     &        ele_fld%d_fld, property%coef_nega_adv, fem_wk, f_nl)
        end if
      end if
!
      end subroutine int_vol_ene_monitor_upw
!
!-----------------------------------------------------------------------
!
      end module int_vol_temp_monitor
