!
!     module cal_diff_coef_sgs_hf
!
!     Written by H. Matsui
!
!!      subroutine s_cal_diff_coef_sgs_hf                               &
!!     &         (iak_diff_hf, icomp_sgs_hf, icomp_diff_hf, ie_dfvx,    &
!!     &          nod_comm, node, ele, surf, sf_grp, nod_bcs, sf_bcs,   &
!!     &          iphys, iphys_ele, ele_fld, fluid, layer_tbl,          &
!!     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl,            &
!!     &          FEM_elens, filtering, sgs_coefs, wk_filter,           &
!!     &          wk_cor, wk_lsq, wk_diff, mhd_fem_wk, fem_wk, surf_wk, &
!!     &          f_l, f_nl, nod_fld, diff_coefs)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(nodal_bcs_4_scalar_type), intent(in) :: nod_bcs
!!        type(scaler_surf_bc_type), intent(in) :: sf_bcs
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(jacobians_2d), intent(in) :: jac_sf_grp_q
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(dynamis_correlation_data), intent(inout) :: wk_cor
!!        type(dynamis_least_suare_data), intent(inout) :: wk_lsq
!!        type(dynamic_model_data), intent(inout) :: wk_diff
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
      module cal_diff_coef_sgs_hf
!
      use m_precision
!
      use t_comm_table
      use t_geometry_data_MHD
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_2d
      use t_jacobian_3d
      use t_table_FEM_const
      use t_layering_ele_list
      use t_MHD_finite_element_mat
      use t_finite_element_mat
      use t_int_surface_data
      use t_filter_elength
      use t_filtering_data
      use t_ele_info_4_dynamic
      use t_work_4_dynamic_model
      use t_work_layer_correlate
      use t_bc_data_temp
      use t_surface_bc_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_diff_coef_sgs_hf                                 &
     &         (iak_diff_hf, icomp_sgs_hf, icomp_diff_hf, ie_dfvx,      &
     &          nod_comm, node, ele, surf, sf_grp, nod_bcs, sf_bcs,     &
     &          iphys, iphys_ele, ele_fld, fluid, layer_tbl,            &
     &          jac_3d_q, jac_3d_l, jac_sf_grp_q, rhs_tbl,              &
     &          FEM_elens, filtering, sgs_coefs, wk_filter,             &
     &          wk_cor, wk_lsq, wk_diff, mhd_fem_wk, fem_wk, surf_wk,   &
     &          f_l, f_nl, nod_fld, diff_coefs)
!
      use m_machine_parameter
      use m_control_parameter
      use m_phys_constants
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_filtering_scalars
      use cal_sgs_fluxes_simi
      use commute_error_h_flux
      use cal_div_sgs_flux_simi
      use cal_sgs_heat_fluxes_grad
      use cal_model_diff_coefs
      use set_boundary_scalars
      use nod_phys_send_recv
!
      integer(kind = kint), intent(in) :: iak_diff_hf
      integer(kind = kint), intent(in) :: icomp_sgs_hf, icomp_diff_hf
      integer(kind = kint), intent(in) :: ie_dfvx
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid
      type(nodal_bcs_4_scalar_type), intent(in) :: nod_bcs
      type(scaler_surf_bc_type), intent(in) :: sf_bcs
      type(layering_tbl), intent(in) :: layer_tbl
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(dynamis_correlation_data), intent(inout) :: wk_cor
      type(dynamis_least_suare_data), intent(inout) :: wk_lsq
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
!    reset model coefficients
!
      call reset_diff_model_coefs(ele%numele, ele%istack_ele_smp,       &
     &    diff_coefs%num_field, iak_diff_hf, diff_coefs%ak)
      call clear_work_4_dynamic_model(iphys, nod_fld)
!
!   gradient model by filtered field (to iphys%i_sgs_grad_f)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_filter_hf_grad'
      call cal_sgs_s_flux_grad_w_coef                                   &
     &   (iflag_temp_supg, SGS_param1%itype_Csym_h_flux,                &
     &    ifilter_4delta, icomp_sgs_hf,                                 &
     &    iphys%i_sgs_grad_f, iphys%i_filter_temp, ie_dfvx,             &
     &    nod_comm, node, ele, fluid, iphys_ele, ele_fld, jac_3d_q,     &
     &    rhs_tbl, FEM_elens, sgs_coefs, mhd_fem_wk, fem_wk,            &
     &    f_l, nod_fld)
!
!   take divergence of filtered heat flux (to iphys%i_sgs_simi)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_div_sgs_filter_hf_simi'
      call cal_div_sgs_hf_simi(iphys%i_sgs_simi, iphys%i_sgs_grad_f,    &
     &    iphys%i_filter_velo, iphys%i_filter_temp,                     &
     &    nod_comm, node, ele, fluid, iphys_ele, ele_fld,               &
     &    jac_3d_q, rhs_tbl, fem_wk, mhd_fem_wk, f_l, f_nl, nod_fld)
!
!   take divergence of heat flux (to iphys%i_sgs_grad)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_div_sgs_h_flux_simi'
      call cal_div_sgs_hf_simi(iphys%i_sgs_grad, iphys%i_SGS_h_flux,    &
     &    iphys%i_velo, iphys%i_sgs_temp,                               &
     &    nod_comm, node, ele, fluid, iphys_ele, ele_fld,               &
     &    jac_3d_q, rhs_tbl, fem_wk, mhd_fem_wk, f_l, f_nl, nod_fld)
!
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_scalar_whole(nod_comm, node, filtering,         &
     &    iphys%i_sgs_grad, iphys%i_sgs_grad, wk_filter, nod_fld)
!
!    take difference (to iphys%i_sgs_simi)
!
      call subtract_2_nod_scalars(nod_fld,                              &
     &    iphys%i_sgs_grad, iphys%i_sgs_simi, iphys%i_sgs_simi)
      call delete_field_by_fixed_s_bc                                   &
     &   (nod_bcs%nod_bc_s, iphys%i_sgs_simi, nod_fld)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_scalar, iphys%i_sgs_simi)
!
!    obtain modeled commutative error  ( to iphys%i_sgs_grad_f)
!
      call cal_commute_error_4_hf                                       &
     &   (fluid%istack_ele_fld_smp, mhd_fem_wk%mlump_fl,                &
     &    node, ele, surf, sf_grp, jac_3d_q, jac_sf_grp_q,              &
     &    rhs_tbl, FEM_elens, sf_bcs%sgs, ifilter_4delta,               &
     &    iphys%i_sgs_grad_f, iphys%i_sgs_grad_f, iphys%i_filter_velo,  &
     &    iphys%i_filter_temp, fem_wk, surf_wk, f_l, f_nl, nod_fld)
      call delete_field_by_fixed_s_bc                                   &
     &   (nod_bcs%nod_bc_s, iphys%i_sgs_grad_f, nod_fld)
!
      call scalar_send_recv(iphys%i_sgs_grad_f, nod_comm, nod_fld)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_scalar, iphys%i_sgs_grad_f)
!
!    obtain modeled commutative error  ( to iphys%i_sgs_grad)
!
      call cal_commute_error_4_hf                                       &
     &   (fluid%istack_ele_fld_smp, mhd_fem_wk%mlump_fl,                &
     &    node, ele, surf, sf_grp, jac_3d_q, jac_sf_grp_q,              &
     &    rhs_tbl, FEM_elens, sf_bcs%sgs, ifilter_2delta,               &
     &    iphys%i_sgs_grad, iphys%i_SGS_h_flux, iphys%i_velo,           &
     &    iphys%i_sgs_temp, fem_wk, surf_wk, f_l, f_nl, nod_fld)
!
      call scalar_send_recv(iphys%i_sgs_grad, nod_comm, nod_fld)
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_scalar_whole(nod_comm, node, filtering,         &
     &    iphys%i_sgs_grad, iphys%i_sgs_grad, wk_filter, nod_fld)
      call delete_field_by_fixed_s_bc                                   &
     &   (nod_bcs%nod_bc_s, iphys%i_sgs_grad, nod_fld)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_scalar, iphys%i_sgs_grad)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &   'cal_diff_coef_fluid', n_scalar, iak_diff_hf, icomp_diff_hf
      call cal_diff_coef_fluid(layer_tbl,                               &
     &    node, ele, fluid, iphys, nod_fld, jac_3d_q, jac_3d_l,         &
     &    n_scalar, iak_diff_hf, icomp_diff_hf, intg_point_t_evo,       &
     &    wk_cor, wk_lsq, wk_diff, diff_coefs)
!
      end subroutine s_cal_diff_coef_sgs_hf
!
!-----------------------------------------------------------------------
!
      end module cal_diff_coef_sgs_hf
