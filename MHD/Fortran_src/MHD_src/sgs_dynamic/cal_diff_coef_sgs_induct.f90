!
!     module cal_diff_coef_sgs_induct
!
!     Written by H. Matsui
!
!!      subroutine s_cal_diff_coef_sgs_induct(iak_diff_uxb,             &
!!     &         icomp_sgs_uxb, icomp_diff_uxb, ie_dfvx, ie_dfbx,       &
!!     &         FEM_prm, SGS_par, nod_comm, node, ele, surf,           &
!!     &         fluid, conduct, cd_prop, layer_tbl, sf_grp, Bsf_bcs,   &
!!     &         iphys, iphys_ele, ele_fld, jac_3d_q, jac_3d_l,         &
!!     &         jac_sf_grp_q, rhs_tbl, FEM_elens, sgs_coefs, filtering,&
!!     &         wk_filter, wk_cor, wk_lsq, wk_diff, mhd_fem_wk, fem_wk,&
!!     &         surf_wk, f_l, f_nl, nod_fld, diff_coefs)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: fluid, conduct
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!!        type(jacobians_2d), intent(in) :: jac_sf_grp_q
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(dynamis_correlation_data), intent(inout) :: wk_cor
!!        type(dynamis_least_suare_data), intent(inout) :: wk_lsq
!!        type(dynamic_model_data), intent(inout) :: wk_diff
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!
      module cal_diff_coef_sgs_induct
!
      use m_precision
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
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
      use t_material_property
      use t_SGS_model_coefs
      use t_filtering_data
      use t_ele_info_4_dynamic
      use t_work_4_dynamic_model
      use t_work_layer_correlate
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
      subroutine s_cal_diff_coef_sgs_induct(iak_diff_uxb,               &
     &         icomp_sgs_uxb, icomp_diff_uxb, ie_dfvx, ie_dfbx,         &
     &         FEM_prm, SGS_par, nod_comm, node, ele, surf,             &
     &         fluid, conduct, cd_prop, layer_tbl, sf_grp, Bsf_bcs,     &
     &         iphys, iphys_ele, ele_fld, jac_3d_q, jac_3d_l,           &
     &         jac_sf_grp_q, rhs_tbl, FEM_elens, sgs_coefs, filtering,  &
     &         wk_filter, wk_cor, wk_lsq, wk_diff, mhd_fem_wk, fem_wk,  &
     &         surf_wk, f_l, f_nl, nod_fld, diff_coefs)
!
      use m_machine_parameter
      use m_phys_constants
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_filtering_scalars
      use cal_sgs_fluxes_simi
      use commute_error_h_flux
      use cal_div_sgs_flux_simi
      use cal_sgs_inductions_grad
      use cal_model_diff_coefs
      use nod_phys_send_recv
!
      integer(kind = kint), intent(in) :: iak_diff_uxb
      integer(kind = kint), intent(in) :: icomp_sgs_uxb, icomp_diff_uxb
      integer(kind = kint), intent(in) :: ie_dfvx, ie_dfbx
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(field_geometry_data), intent(in) :: fluid
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(surface_group_data), intent(in) :: sf_grp
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(layering_tbl), intent(in) :: layer_tbl
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
      type(jacobians_2d), intent(in) :: jac_sf_grp_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(filtering_data_type), intent(in) :: filtering
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(dynamis_correlation_data), intent(inout) :: wk_cor
      type(dynamis_least_suare_data), intent(inout) :: wk_lsq
      type(dynamic_model_data), intent(inout) :: wk_diff
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_l, f_nl
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
      type(phys_data), intent(inout) :: nod_fld
!
!    reset model coefficients
!
      call reset_diff_model_coefs(ele%numele, ele%istack_ele_smp,       &
     &    diff_coefs%num_field, iak_diff_uxb, diff_coefs%ak)
      call clear_work_4_dynamic_model(iphys, nod_fld)
!
!   gradient model by filtered field (to iphys%i_sgs_grad_f)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_filter_induct_grad'
      call cal_sgs_induct_t_grad_w_coef                                 &
     &   (ifilter_4delta, icomp_sgs_uxb, iphys%i_sgs_grad_f,            &
     &    iphys%i_filter_velo, iphys%i_filter_magne, ie_dfvx, ie_dfbx,  &
     &    FEM_prm, SGS_par%model_p, nod_comm, node, ele, conduct,       &
     &    cd_prop, iphys_ele, ele_fld, jac_3d_q, rhs_tbl, FEM_elens,    &
     &    sgs_coefs, fem_wk, mhd_fem_wk, f_l, nod_fld)
!
!   take divergence of filtered heat flux (to iphys%i_sgs_simi)
!
      if (iflag_debug.gt.0) write(*,*) 'cal_div_sgs_filter_idct_simi'
      call cal_div_sgs_idct_simi(iphys%i_sgs_simi, iphys%i_sgs_grad_f,  &
     &    iphys%i_filter_velo, iphys%i_filter_magne,                    &
     &    FEM_prm, nod_comm, node, ele, conduct, iphys_ele, ele_fld,    &
     &    jac_3d_q, rhs_tbl, fem_wk, mhd_fem_wk, f_l, f_nl, nod_fld)
!
!   take divergence of heat flux (to iphys%i_sgs_grad)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_div_sgs_induct_simi'
      call cal_div_sgs_idct_simi(iphys%i_sgs_grad,                      &
     &    iphys%i_SGS_induct_t, iphys%i_velo, iphys%i_magne,            &
     &    FEM_prm, nod_comm, node, ele, conduct, iphys_ele, ele_fld,    &
     &    jac_3d_q, rhs_tbl, fem_wk, mhd_fem_wk, f_l, f_nl, nod_fld)
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_vector_whole                                    &
     &   (SGS_par%filter_p, nod_comm, node, filtering,                  &
     &    iphys%i_sgs_grad, iphys%i_sgs_grad, wk_filter, nod_fld)
!
!    take difference (to iphys%i_sgs_simi)
!
      call subtract_2_nod_vectors(nod_fld,                              &
     &    iphys%i_sgs_grad, iphys%i_sgs_simi, iphys%i_sgs_simi)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_vector, iphys%i_sgs_simi)
!
!    obtain modeled commutative error  ( to iphys%i_sgs_grad_f)
!
      call cal_commute_error_4_idct(FEM_prm%npoint_t_evo_int,           &
     &    conduct%istack_ele_fld_smp, mhd_fem_wk%mlump_cd,              &
     &    node, ele, surf, sf_grp, Bsf_bcs, jac_3d_q, jac_sf_grp_q,     &
     &    rhs_tbl, FEM_elens, ifilter_4delta,                           &
     &    iphys%i_sgs_grad_f, iphys%i_sgs_grad_f, iphys%i_filter_velo,  &
     &    iphys%i_filter_magne, fem_wk, surf_wk, f_l, f_nl, nod_fld)
!
      call vector_send_recv                                             &
     &   (iphys%i_sgs_grad_f, nod_comm, nod_fld)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_vector, iphys%i_sgs_grad_f)
!
!    obtain modeled commutative error  ( to iphys%i_sgs_grad)
!
      call cal_commute_error_4_idct(FEM_prm%npoint_t_evo_int,           &
     &    conduct%istack_ele_fld_smp, mhd_fem_wk%mlump_cd,              &
     &    node, ele, surf, sf_grp, Bsf_bcs, jac_3d_q, jac_sf_grp_q,     &
     &    rhs_tbl, FEM_elens, ifilter_2delta,                           &
     &    iphys%i_sgs_grad, iphys%i_SGS_induct_t, iphys%i_velo,         &
     &    iphys%i_magne, fem_wk, surf_wk, f_l, f_nl, nod_fld)
!
      call vector_send_recv                                             &
     &   (iphys%i_sgs_grad, nod_comm, nod_fld)
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_vector_whole                                    &
     &   (SGS_par%filter_p, nod_comm, node, filtering,                  &
     &    iphys%i_sgs_grad, iphys%i_sgs_grad, wk_filter, nod_fld)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_vector, iphys%i_sgs_grad)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &   'cal_diff_coef_fluid', n_vector, iak_diff_uxb, icomp_diff_uxb
      call cal_diff_coef_fluid(SGS_par, layer_tbl,  node, ele,          &
     &   fluid, iphys, nod_fld, jac_3d_q, jac_3d_l, n_vector,           &
     &    iak_diff_uxb, icomp_diff_uxb, FEM_prm%npoint_t_evo_int,       &
     &    wk_cor, wk_lsq, wk_diff, diff_coefs)
!
      end subroutine s_cal_diff_coef_sgs_induct
!
!-----------------------------------------------------------------------
!
      end module cal_diff_coef_sgs_induct
