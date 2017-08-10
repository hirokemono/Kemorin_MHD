!
!     module cal_diff_coef_sgs_hf
!
!     Written by H. Matsui
!
!!      subroutine s_cal_diff_coef_sgs_sf                               &
!!     &         (itype_Csym_flux, iflag_supg, num_int, dt,             &
!!     &          ifield, ifield_f, ivelo, ivelo_f, i_sgs,              &
!!     &          iak_diff_flux, icomp_sgs_flux, icomp_diff_sf, ie_dfvx,&
!!     &          SGS_par, mesh, group, surf, Snod_bcs, sf_bcs,         &
!!     &          iphys, iphys_ele, ele_fld, fluid, layer_tbl,          &
!!     &          jacobians, rhs_tbl, FEM_elens, filtering, sgs_coefs,  &
!!     &          mlump_fl, wk_filter, wk_cor, wk_lsq, wk_diff,         &
!!     &          mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl,               &
!!     &          nod_fld, diff_coefs)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(surface_data), intent(in) :: surf
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(nodal_bcs_4_scalar_type), intent(in) :: Snod_bcs
!!        type(scaler_surf_bc_type), intent(in) :: sf_bcs
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(jacobians_type), intent(in) :: jacobians
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs
!!        type(lumped_mass_matrices), intent(in) :: mlump_fl
!!        type(filtering_work_type), intent(inout) :: wk_filter
!!        type(dynamic_correlation_data), intent(inout) :: wk_cor
!!        type(dynamic_least_suare_data), intent(inout) :: wk_lsq
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
      use t_SGS_control_parameter
      use t_mesh_data
      use t_geometry_data_MHD
      use t_surface_data
      use t_phys_data
      use t_phys_address
      use t_jacobians
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
      subroutine s_cal_diff_coef_sgs_sf                                 &
     &         (itype_Csym_flux, iflag_supg, num_int, dt,               &
     &          ifield, ifield_f, ivelo, ivelo_f, i_sgs,                &
     &          iak_diff_flux, icomp_sgs_flux, icomp_diff_sf, ie_dfvx,  &
     &          SGS_par, mesh, group, surf, Snod_bcs, sf_bcs,           &
     &          iphys, iphys_ele, ele_fld, fluid, layer_tbl,            &
     &          jacobians, rhs_tbl, FEM_elens, filtering, sgs_coefs,    &
     &          mlump_fl, wk_filter, wk_cor, wk_lsq, wk_diff,           &
     &          mhd_fem_wk, fem_wk, surf_wk, f_l, f_nl,                 &
     &          nod_fld, diff_coefs)
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
      use cal_sgs_heat_fluxes_grad
      use cal_model_diff_coefs
      use set_boundary_scalars
      use nod_phys_send_recv
!
      integer(kind = kint), intent(in) :: itype_Csym_flux
      integer(kind = kint), intent(in) :: iflag_supg, num_int
      integer (kind=kint), intent(in) :: i_sgs, ifield, ifield_f
      integer (kind=kint), intent(in) :: ivelo, ivelo_f
!
      integer(kind = kint), intent(in) :: iak_diff_flux
      integer(kind = kint), intent(in) :: icomp_sgs_flux, icomp_diff_sf
      integer(kind = kint), intent(in) :: ie_dfvx
      real(kind = kreal), intent(in) :: dt
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(surface_data), intent(in) :: surf
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid
      type(nodal_bcs_4_scalar_type), intent(in) :: Snod_bcs
      type(scaler_surf_bc_type), intent(in) :: sf_bcs
      type(layering_tbl), intent(in) :: layer_tbl
      type(jacobians_type), intent(in) :: jacobians
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(lumped_mass_matrices), intent(in) :: mlump_fl
!
      type(filtering_work_type), intent(inout) :: wk_filter
      type(dynamic_correlation_data), intent(inout) :: wk_cor
      type(dynamic_least_suare_data), intent(inout) :: wk_lsq
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
      call reset_diff_model_coefs                                       &
     &   (mesh%ele%numele, mesh%ele%istack_ele_smp,                     &
     &    diff_coefs%num_field, iak_diff_flux, diff_coefs%ak)
      call clear_work_4_dynamic_model(iphys, nod_fld)
!
!   gradient model by filtered field (to iphys%i_sgs_grad_f)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_filter_hf_grad'
      call cal_sgs_s_flux_grad_w_coef(iflag_supg, num_int, dt,          &
     &    itype_Csym_flux, SGS_par%model_p%icoord_Csim, ifilter_4delta, &
     &    icomp_sgs_flux, iphys%i_sgs_grad_f, ifield_f, ie_dfvx,        &
     &    mesh%nod_comm, mesh%node, mesh%ele, fluid, iphys_ele,         &
     &    ele_fld, jacobians%jac_3d, rhs_tbl, FEM_elens, sgs_coefs,     &
     &    mlump_fl, mhd_fem_wk, fem_wk, f_l, nod_fld)
!
!   take divergence of filtered heat flux (to iphys%i_sgs_simi)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_div_sgs_filter_hf_simi'
      call cal_div_sgs_sf_simi(iphys%i_sgs_simi, iphys%i_sgs_grad_f,    &
     &    ivelo_f, ifield_f, iflag_supg, num_int, dt,                   &
     &    mesh%nod_comm, mesh%node, mesh%ele, fluid, iphys_ele,         &
     &    ele_fld, jacobians%jac_3d, rhs_tbl, fem_wk, mlump_fl,         &
     &    f_l, f_nl, nod_fld)
!
!   take divergence of heat flux (to iphys%i_sgs_grad)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_div_sgs_h_flux_simi'
      call cal_div_sgs_sf_simi(iphys%i_sgs_grad,                        &
     &    i_sgs, ivelo, ifield, iflag_supg, num_int, dt,                &
     &    mesh%nod_comm, mesh%node, mesh%ele, fluid, iphys_ele,         &
     &    ele_fld, jacobians%jac_3d, rhs_tbl, fem_wk, mlump_fl,         &
     &    f_l, f_nl, nod_fld)
!
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_scalar_whole                                    &
     &   (SGS_par%filter_p, mesh%nod_comm, mesh%node, filtering,        &
     &    iphys%i_sgs_grad, iphys%i_sgs_grad, wk_filter, nod_fld)
!
!    take difference (to iphys%i_sgs_simi)
!
      call subtract_2_nod_scalars(nod_fld,                              &
     &    iphys%i_sgs_grad, iphys%i_sgs_simi, iphys%i_sgs_simi)
      call delete_field_by_fixed_s_bc                                   &
     &   (Snod_bcs%nod_bc_s, iphys%i_sgs_simi, nod_fld)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_scalar, iphys%i_sgs_simi)
!
!    obtain modeled commutative error  ( to iphys%i_sgs_grad_f)
!
      call cal_commute_error_4_sf(num_int, fluid%istack_ele_fld_smp,    &
     &    mlump_fl, mesh%node, mesh%ele, surf, group%surf_grp,          &
     &    jacobians%jac_3d, jacobians%jac_sf_grp,                       &
     &    rhs_tbl, FEM_elens, sf_bcs%sgs, ifilter_4delta,               &
     &    iphys%i_sgs_grad_f, iphys%i_sgs_grad_f, ivelo_f, ifield_f,    &
     &    fem_wk, surf_wk, f_l, f_nl, nod_fld)
      call delete_field_by_fixed_s_bc                                   &
     &   (Snod_bcs%nod_bc_s, iphys%i_sgs_grad_f, nod_fld)
!
      call scalar_send_recv(iphys%i_sgs_grad_f, mesh%nod_comm, nod_fld)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_scalar, iphys%i_sgs_grad_f)
!
!    obtain modeled commutative error  ( to iphys%i_sgs_grad)
!
      call cal_commute_error_4_sf(num_int, fluid%istack_ele_fld_smp,    &
     &    mlump_fl, mesh%node, mesh%ele, surf, group%surf_grp,          &
     &    jacobians%jac_3d, jacobians%jac_sf_grp,                       &
     &    rhs_tbl, FEM_elens, sf_bcs%sgs, ifilter_2delta,               &
     &    iphys%i_sgs_grad, i_sgs, ivelo, ifield,                       &
     &    fem_wk, surf_wk, f_l, f_nl, nod_fld)
!
      call scalar_send_recv(iphys%i_sgs_grad, mesh%nod_comm, nod_fld)
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_scalar_whole                                    &
     &   (SGS_par%filter_p, mesh%nod_comm, mesh%node, filtering,        &
     &    iphys%i_sgs_grad, iphys%i_sgs_grad, wk_filter, nod_fld)
      call delete_field_by_fixed_s_bc                                   &
     &   (Snod_bcs%nod_bc_s, iphys%i_sgs_grad, nod_fld)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_scalar, iphys%i_sgs_grad)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &   'cal_diff_coef_fluid', n_scalar, iak_diff_flux, icomp_diff_sf
      call cal_diff_coef_fluid(SGS_par, layer_tbl,                      &
     &    mesh%node, mesh%ele, fluid, iphys, nod_fld,                   &
     &    jacobians%jac_3d, jacobians%jac_3d_l,                         &
     &    n_scalar, iak_diff_flux, icomp_diff_sf, num_int,              &
     &    wk_cor, wk_lsq, wk_diff, diff_coefs)
!
      end subroutine s_cal_diff_coef_sgs_sf
!
!-----------------------------------------------------------------------
!
      end module cal_diff_coef_sgs_hf
