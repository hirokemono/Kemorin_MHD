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
!!      subroutine int_vol_magne_monitor_pg(i_field, num_int,           &
!!     &          SGS_param, cmt_param, node, ele, conduct, cd_prop,    &
!!     &          iphys_base, iphys_frc, iphys_div_frc, iphys_SGS,      &
!!     &          nod_fld, iphys_ele_base, ele_fld, g_FEM, jac_3d,      &
!!     &          rhs_tbl, FEM_elen, Cdiff_SGS_uxb,                     &
!!     &          mhd_fem_wk, fem_wk, f_nl)
!!      subroutine int_vol_magne_monitor_upm(i_field, num_int, dt,      &
!!     &          SGS_param, cmt_param, node, ele, conduct, cd_prop,    &
!!     &          iphys_base, iphys_frc, iphys_div_frc, iphys_SGS,      &
!!     &          nod_fld, iphys_ele_base, ele_fld, g_FEM, jac_3d,      &
!!     &          rhs_tbl, FEM_elen, ak_diff, mhd_fem_wk, fem_wk,f_nl)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(base_field_address), intent(in) :: iphys_base
!!        type(base_force_address), intent(in) :: iphys_frc
!!        type(base_force_address), intent(in) :: iphys_div_frc
!!        type(SGS_term_address), intent(in) :: iphys_SGS
!!        type(phys_data), intent(in) :: nod_fld
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elen
!!        type(SGS_model_coefficient), intent(in) :: Cdiff_SGS_uxb
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      module int_vol_magne_monitor
!
      use m_precision
      use m_machine_parameter
      use m_phys_constants
!
      use t_SGS_control_parameter
      use t_physical_property
      use t_geometry_data_MHD
      use t_geometry_data
      use t_phys_data
      use t_base_field_labels
      use t_base_force_labels
      use t_SGS_term_labels
      use t_fem_gauss_int_coefs
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
      subroutine int_vol_magne_monitor_pg(i_field, num_int,             &
     &          SGS_param, cmt_param, node, ele, conduct, cd_prop,      &
     &          iphys_base, iphys_frc, iphys_div_frc, iphys_SGS,        &
     &          nod_fld, iphys_ele_base, ele_fld, g_FEM, jac_3d,        &
     &          rhs_tbl, FEM_elen, Cdiff_SGS_uxb,                       &
     &          mhd_fem_wk, fem_wk, f_nl)
!
      use int_vol_vect_differences
      use int_vol_vect_cst_difference
      use int_vol_mag_induction
      use int_vol_SGS_mag_induct
!
      integer(kind=kint), intent(in) :: i_field
      integer(kind=kint), intent(in) :: num_int
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      type(base_field_address), intent(in) :: iphys_base
      type(base_force_address), intent(in) :: iphys_frc
      type(base_force_address), intent(in) :: iphys_div_frc
      type(SGS_term_address), intent(in) :: iphys_SGS
      type(phys_data), intent(in) :: nod_fld
!
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elen
      type(SGS_model_coefficient), intent(in) :: Cdiff_SGS_uxb
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!
      if (i_field .eq. iphys_frc%i_induction) then
        call int_vol_mag_induct_pg(node, ele, cd_prop, g_FEM, jac_3d,   &
     &      rhs_tbl, nod_fld, iphys_base, iphys_ele_base,               &
     &      conduct%istack_ele_fld_smp, num_int,                        &
     &      ele_fld%ntot_phys, ele_fld%d_fld, fem_wk, mhd_fem_wk, f_nl)
!
      else if (i_field .eq. iphys_div_frc%i_induct_t) then
        call int_vol_div_asym_tsr                                       &
     &     (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,                 &
     &      conduct%istack_ele_fld_smp, num_int,                        &
     &      iphys_frc%i_induct_t, fem_wk, f_nl)
!
      else if (i_field .eq. iphys_SGS%i_SGS_induction) then
        if(cmt_param%iflag_c_uxb .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_idct_mod_pg                              &
     &       (node, ele, nod_fld, iphys_base, iphys_SGS,                &
     &        g_FEM, jac_3d, rhs_tbl, FEM_elen, Cdiff_SGS_uxb,          &
     &        conduct%istack_ele_fld_smp, num_int,                      &
     &        SGS_param%ifilter_final, cd_prop%coef_induct,             &
     &        fem_wk, mhd_fem_wk, f_nl)
        else
          call int_vol_div_as_tsr_w_const                               &
     &       (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,               &
     &        conduct%istack_ele_fld_smp, num_int,                      &
     &        iphys_SGS%i_SGS_induct_t, cd_prop%coef_induct,            &
     &        fem_wk, f_nl)
        end if
      end if
!
      end subroutine int_vol_magne_monitor_pg
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_magne_monitor_upm(i_field, num_int, dt,        &
     &          SGS_param, cmt_param, node, ele, conduct, cd_prop,      &
     &          iphys_base, iphys_frc, iphys_div_frc, iphys_SGS,        &
     &          nod_fld, iphys_ele_base, ele_fld, g_FEM, jac_3d,        &
     &          rhs_tbl, FEM_elen, ak_diff, mhd_fem_wk, fem_wk,f_nl)
!
      use int_vol_vect_diff_upw
      use int_vol_vect_cst_diff_upw
      use int_vol_mag_induction
      use int_vol_SGS_mag_induct
!
      integer(kind=kint), intent(in) :: i_field
      integer(kind=kint), intent(in) :: num_int
      real(kind = kreal), intent(in) :: dt
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      type(base_field_address), intent(in) :: iphys_base
      type(base_force_address), intent(in) :: iphys_frc
      type(base_force_address), intent(in) :: iphys_div_frc
      type(SGS_term_address), intent(in) :: iphys_SGS
      type(phys_data), intent(in) :: nod_fld
!
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elen
      real(kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
!
      if (i_field .eq. iphys_frc%i_induction) then
        call int_vol_mag_induct_upm(node, ele, cd_prop, g_FEM, jac_3d,  &
     &      rhs_tbl, nod_fld, iphys_base, iphys_ele_base,               &
     &      conduct%istack_ele_fld_smp, num_int, dt,                    &
     &      ele_fld%ntot_phys, ele_fld%d_fld, fem_wk, mhd_fem_wk, f_nl)
!
      else if (i_field .eq. iphys_div_frc%i_induct_t) then
        call int_vol_div_as_tsr_upw                                     &
     &     (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,                 &
     &      conduct%istack_ele_fld_smp, num_int, dt,                    &
     &      iphys_frc%i_induct_t, ele_fld%ntot_phys,                    &
     &      iphys_ele_base%i_magne, ele_fld%d_fld, fem_wk, f_nl)
!
      else if (i_field .eq. iphys_SGS%i_SGS_induction) then
        if(cmt_param%iflag_c_uxb .eq. id_SGS_commute_ON) then
          call int_vol_div_SGS_idct_mod_upm                             &
     &       (node, ele, nod_fld, iphys_base, iphys_SGS,                &
     &        g_FEM, jac_3d, rhs_tbl, FEM_elen,                         &
     &        conduct%istack_ele_fld_smp, num_int, dt,                  &
     &        SGS_param%ifilter_final, ak_diff,                         &
     &        cd_prop%coef_induct, ele_fld%ntot_phys,                   &
     &        iphys_ele_base%i_magne, ele_fld%d_fld, fem_wk,            &
     &        mhd_fem_wk, f_nl)
        else
          call int_vol_div_as_tsr_cst_upw                               &
     &       (node, ele, g_FEM, jac_3d, rhs_tbl, nod_fld,               &
     &        conduct%istack_ele_fld_smp, num_int, dt,                  &
     &        iphys_SGS%i_SGS_induct_t, ele_fld%ntot_phys,              &
     &        iphys_ele_base%i_magne, ele_fld%d_fld,                    &
     &        cd_prop%coef_induct, fem_wk, f_nl)
        end if
      end if
!
      end subroutine int_vol_magne_monitor_upm
!
!-----------------------------------------------------------------------
!
      end module int_vol_magne_monitor
