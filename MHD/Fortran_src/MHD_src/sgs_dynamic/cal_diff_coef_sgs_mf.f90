!
!     module cal_diff_coef_sgs_mf
!
!     Written by H. Matsui
!
!!      subroutine s_cal_diff_coef_sgs_mf                               &
!!     &         (iak_diff_mf, icomp_sgs_mf, icomp_diff_mf, ie_dfvx, dt,&
!!     &          FEM_prm, SGS_par, mesh, group, surf,                  &
!!     &          Vnod_bcs, Vsf_bcs, iphys, iphys_ele, ele_fld,         &
!!     &          fluid, layer_tbl, jacobians, rhs_tbl,                 &
!!     &          FEM_elens, filtering, sgs_coefs, mlump_fl, wk_filter, &
!!     &          wk_cor, wk_lsq, wk_diff, mhd_fem_wk, fem_wk, surf_wk, &
!!     &          f_l, f_nl, nod_fld, diff_coefs)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(surface_data), intent(in) :: surf
!!        type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
!!        type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(jacobians_type), intent(in) :: jacobians
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(filtering_data_type), intent(in) :: filtering
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs
!!        type(lumped_mass_matrices), intent(in) :: mlump_fl
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
      module cal_diff_coef_sgs_mf
!
      use m_precision
!
      use t_FEM_control_parameter
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
      use t_bc_data_velo
      use t_surface_bc_data
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
      subroutine s_cal_diff_coef_sgs_mf                                 &
     &         (iak_diff_mf, icomp_sgs_mf, icomp_diff_mf, ie_dfvx, dt,  &
     &          FEM_prm, SGS_par, mesh, group, surf,                    &
     &          Vnod_bcs, Vsf_bcs, iphys, iphys_ele, ele_fld,           &
     &          fluid, layer_tbl, jacobians, rhs_tbl,                   &
     &          FEM_elens, filtering, sgs_coefs, mlump_fl, wk_filter,   &
     &          wk_cor, wk_lsq, wk_diff, mhd_fem_wk, fem_wk, surf_wk,   &
     &          f_l, f_nl, nod_fld, diff_coefs)
!
      use m_machine_parameter
      use m_phys_constants
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_filtering_scalars
      use cal_sgs_fluxes_simi
      use cal_div_sgs_flux_simi
      use commute_error_h_flux
      use cal_sgs_mom_fluxes_grad
      use cal_model_diff_coefs
      use set_nodal_bc_id_data
      use nod_phys_send_recv
!
      integer(kind = kint), intent(in) :: iak_diff_mf
      integer(kind = kint), intent(in) :: icomp_sgs_mf, icomp_diff_mf
      integer(kind = kint), intent(in) :: ie_dfvx
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(surface_data), intent(in) :: surf
      type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
      type(velocity_surf_bc_type), intent(in)  :: Vsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid
      type(layering_tbl), intent(in) :: layer_tbl
      type(jacobians_type), intent(in) :: jacobians
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(lumped_mass_matrices), intent(in) :: mlump_fl
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
!
!    reset model coefficients
!
      call reset_diff_model_coefs                                       &
     &   (mesh%ele%numele, mesh%ele%istack_ele_smp,                     &
     &    diff_coefs%num_field, iak_diff_mf, diff_coefs%ak)
      call clear_work_4_dynamic_model(iphys, nod_fld)
!
!   gradient model by filtered field (to iphys%i_sgs_grad_f)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_filter_m_flux_grad'
      call cal_sgs_m_flux_grad_w_coef                                   &
     &   (ifilter_4delta, icomp_sgs_mf, iphys%i_sgs_grad_f,             &
     &    iphys%i_filter_velo, ie_dfvx, dt, FEM_prm, SGS_par%model_p,   &
     &    mesh%nod_comm, mesh%node, mesh%ele, fluid,                    &
     &    iphys_ele, ele_fld, jacobians%jac_3d, FEM_elens, sgs_coefs,   &
     &    rhs_tbl, mlump_fl, fem_wk, mhd_fem_wk, nod_fld)
!
!   take divergence of filtered heat flux (to iphys%i_sgs_simi)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_div_sgs_filter_mf_simi'
      call cal_div_sgs_mf_simi(iphys%i_sgs_simi,                        &
     &    iphys%i_sgs_grad_f, iphys%i_filter_velo, dt,                  &
     &    FEM_prm, mesh%nod_comm, mesh%node, mesh%ele, fluid,           &
     &    iphys_ele, ele_fld, jacobians%jac_3d, rhs_tbl, fem_wk,        &
     &    mlump_fl, f_l, f_nl, nod_fld)
!
!   take divergence of heat flux (to iphys%i_sgs_grad)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_div_sgs_m_flux_simi'
      call cal_div_sgs_mf_simi                                          &
     &   (iphys%i_sgs_grad, iphys%i_SGS_m_flux, iphys%i_velo, dt,       &
     &    FEM_prm, mesh%nod_comm, mesh%node, mesh%ele, fluid,           &
     &    iphys_ele, ele_fld, jacobians%jac_3d, rhs_tbl, fem_wk,        &
     &    mlump_fl, f_l, f_nl, nod_fld)
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_vector_whole                                    &
     &   (SGS_par%filter_p, mesh%nod_comm, mesh%node, filtering,        &
     &    iphys%i_sgs_grad, iphys%i_sgs_grad, wk_filter, nod_fld)
!
!    take difference (to iphys%i_sgs_simi)
!
      call subtract_2_nod_vectors(nod_fld,                              &
     &    iphys%i_sgs_grad, iphys%i_sgs_simi, iphys%i_sgs_simi)
      call delete_field_by_fixed_v_bc                                   &
     &   (Vnod_bcs, iphys%i_sgs_simi, nod_fld)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_vector, iphys%i_sgs_simi)
!
!    obtain modeled commutative error  ( to iphys%i_sgs_grad_f)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_commute_error_4_filter_mf'
      call cal_commute_error_4_mf                                       &
     &   (FEM_prm%npoint_t_evo_int, fluid%istack_ele_fld_smp,           &
     &    mlump_fl, mesh%node, mesh%ele, surf, group%surf_grp,          &
     &    jacobians%jac_3d, jacobians%jac_sf_grp,                       &
     &    rhs_tbl, FEM_elens, Vsf_bcs%sgs, ifilter_4delta,              &
     &    iphys%i_sgs_grad_f, iphys%i_sgs_grad_f, iphys%i_filter_velo,  &
     &    fem_wk, surf_wk, f_l, f_nl, nod_fld)
!
      if (iflag_debug.gt.0) write(*,*) 'delete_field_by_fixed_v_bc',    &
     &                     iphys%i_sgs_grad_f
      call delete_field_by_fixed_v_bc                                   &
     &   (Vnod_bcs, iphys%i_sgs_grad_f, nod_fld)
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &      'vector_send_recv', iphys%i_sgs_grad_f
      call vector_send_recv                                             &
     &   (iphys%i_sgs_grad_f, mesh%nod_comm, nod_fld)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_vector, iphys%i_sgs_grad_f)
!
!    obtain modeled commutative error  ( to iphys%i_sgs_grad)
!
      if (iflag_debug.gt.0)   write(*,*) 'cal_commute_error_4_m_flux'
      call cal_commute_error_4_mf                                       &
     &   (FEM_prm%npoint_t_evo_int, fluid%istack_ele_fld_smp,           &
     &    mlump_fl, mesh%node, mesh%ele, surf, group%surf_grp,          &
     &    jacobians%jac_3d, jacobians%jac_sf_grp,                       &
     &    rhs_tbl, FEM_elens, Vsf_bcs%sgs, ifilter_2delta,              &
     &    iphys%i_sgs_grad, iphys%i_SGS_m_flux, iphys%i_velo,           &
     &    fem_wk, surf_wk, f_l, f_nl, nod_fld)
!
      call vector_send_recv                                             &
     &   (iphys%i_sgs_grad, mesh%nod_comm, nod_fld)
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_vector_whole                                    &
     &   (SGS_par%filter_p, mesh%nod_comm, mesh%node, filtering,        &
     &    iphys%i_sgs_grad, iphys%i_sgs_grad, wk_filter, nod_fld)
      call delete_field_by_fixed_v_bc                                   &
     &   (Vnod_bcs, iphys%i_sgs_grad, nod_fld)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_vector, iphys%i_sgs_grad)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &   'cal_diff_coef_fluid', n_vector, iak_diff_mf, icomp_diff_mf
      call cal_diff_coef_fluid(SGS_par, layer_tbl,                      &
     &    mesh%node, mesh%ele, fluid, iphys, nod_fld,                   &
     &    jacobians%jac_3d, jacobians%jac_3d_l, n_vector,               &
     &    iak_diff_mf, icomp_diff_mf, FEM_prm%npoint_t_evo_int,         &
     &    wk_cor, wk_lsq, wk_diff, diff_coefs)
!
      end subroutine s_cal_diff_coef_sgs_mf
!
!-----------------------------------------------------------------------
!
      end module cal_diff_coef_sgs_mf
