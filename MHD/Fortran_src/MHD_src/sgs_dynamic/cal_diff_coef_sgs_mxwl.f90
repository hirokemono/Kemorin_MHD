!
!     module cal_diff_coef_sgs_mxwl
!
!     Written by H. Matsui
!
!!      subroutine s_cal_diff_coef_sgs_mxwl(dt, FEM_prm, SGS_par,       &
!!     &          mesh, group, fluid, Vnod_bcs, Bsf_bcs,                &
!!     &          iphys_base, iphys_fil, iphys_SGS, iphys_SGS_wk,       &
!!     &          iphys_ele_base, ele_fld, fem_int, FEM_filters,        &
!!     &          iak_diff_sgs, icomp_diff_sgs, icomp_sgs_term,         &
!!     &          iphys_elediff_fil, sgs_coefs, mk_MHD, FEM_SGS_wk,     &
!!     &          mhd_fem_wk, rhs_mat, nod_fld, diff_coefs, v_sol)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
!!        type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!!        type(base_field_address), intent(in) :: iphys_base
!!        type(base_field_address), intent(in) :: iphys_fil
!!        type(SGS_term_address), intent(in) :: iphys_SGS
!!        type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(filters_on_FEM), intent(in) :: FEM_filters
!!        type(SGS_term_address), intent(in) :: iak_diff_sgs
!!        type(SGS_term_address), intent(in) :: icomp_diff_sgs
!!        type(SGS_term_address), intent(in) :: icomp_sgs_term
!!        type(base_field_address), intent(in) :: iphys_elediff_fil
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs
!!        type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!!        type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(SGS_coefficients_type), intent(inout) :: diff_coefs
!!        type(vectors_4_solver), intent(inout) :: v_sol
!
      module cal_diff_coef_sgs_mxwl
!
      use m_precision
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_mesh_data
      use t_geometry_data_MHD
      use t_surface_data
      use t_phys_data
      use t_base_field_labels
      use t_SGS_term_labels
      use t_base_field_labels
      use t_SGS_term_labels
      use t_table_FEM_const
      use t_jacobians
      use t_MHD_finite_element_mat
      use t_MHD_mass_matrices
      use t_FEM_MHD_filter_data
      use t_bc_data_velo
      use t_surface_bc_vector
      use t_material_property
      use t_SGS_model_coefs
      use t_work_FEM_integration
      use t_work_FEM_dynamic_SGS
      use t_vector_for_solver
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_diff_coef_sgs_mxwl(dt, FEM_prm, SGS_par,         &
     &          mesh, group, fluid, Vnod_bcs, Bsf_bcs,                  &
     &          iphys_base, iphys_fil, iphys_SGS, iphys_SGS_wk,         &
     &          iphys_ele_base, ele_fld, fem_int, FEM_filters,          &
     &          iak_diff_sgs, icomp_diff_sgs, icomp_sgs_term,           &
     &          iphys_elediff_fil, sgs_coefs, mk_MHD, FEM_SGS_wk,       &
     &          mhd_fem_wk, rhs_mat, nod_fld, diff_coefs, v_sol)
!
      use m_machine_parameter
      use m_phys_constants
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_filtering_scalars
      use cal_sgs_fluxes_simi
      use commute_error_h_flux
      use cal_sgs_mom_fluxes_grad
      use cal_div_sgs_flux_simi
      use cal_model_diff_coefs
      use set_nodal_bc_id_data
      use nod_phys_send_recv
!
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(nodal_bcs_4_momentum_type), intent(in) :: Vnod_bcs
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(base_field_address), intent(in) :: iphys_base
      type(base_field_address), intent(in) :: iphys_fil
      type(SGS_term_address), intent(in) :: iphys_SGS
      type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid
      type(finite_element_integration), intent(in) :: fem_int
      type(filters_on_FEM), intent(in) :: FEM_filters
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(SGS_term_address), intent(in) :: iak_diff_sgs
      type(SGS_term_address), intent(in) :: icomp_diff_sgs
      type(SGS_term_address), intent(in) :: icomp_sgs_term
      type(base_field_address), intent(in) :: iphys_elediff_fil
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
      type(vectors_4_solver), intent(inout) :: v_sol
!
!    reset model coefficients
!
      call reset_diff_model_coefs                                       &
     &   (mesh%ele%numele, mesh%ele%istack_ele_smp,                     &
     &    diff_coefs%num_field, iak_diff_sgs%i_SGS_Lorentz,             &
     &    diff_coefs%ak)
      call clear_work_4_dynamic_model(iphys_SGS_wk, nod_fld)
!
!   gradient model by filtered field (to iphys_SGS_wk%i_wd_nlg)
!
      if (iflag_debug.gt.0) write(*,*) 'cal_sgs_filter_maxwell_grad'
      call cal_sgs_m_flux_grad_w_coef(ifilter_4delta,                   &
     &    icomp_sgs_term%i_SGS_Lorentz, iphys_SGS_wk%i_wd_nlg,          &
     &    iphys_fil%i_magne, iphys_elediff_fil%i_magne, dt,             &
     &    FEM_prm, SGS_par%model_p, mesh%nod_comm, mesh%node, mesh%ele, &
     &    fluid, iphys_ele_base, ele_fld, fem_int%jcs,                  &
     &    FEM_filters%FEM_elens, sgs_coefs, fem_int%rhs_tbl,            &
     &    mk_MHD%mlump_fl, rhs_mat%fem_wk, mhd_fem_wk, nod_fld, v_sol)
!
!   take divergence of filtered heat flux (to iphys_SGS_wk%i_simi)
!
      if (iflag_debug.gt.0) write(*,*) 'cal_div_sgs_filter_mxwl_simi'
      call cal_div_sgs_mf_simi(iphys_SGS_wk%i_simi,                     &
     &    iphys_SGS_wk%i_wd_nlg, iphys_fil%i_magne, dt,                 &
     &    FEM_prm, mesh%nod_comm, mesh%node, mesh%ele, fluid,           &
     &    iphys_ele_base, ele_fld, fem_int%jcs, fem_int%rhs_tbl,        &
     &    rhs_mat%fem_wk, mk_MHD%mlump_fl, rhs_mat%f_l, rhs_mat%f_nl,   &
     &    nod_fld, v_sol)
!
!   take divergence of heat flux (to iphys_SGS_wk%i_nlg)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_div_sgs_maxwell_simi'
      call cal_div_sgs_mf_simi(iphys_SGS_wk%i_nlg,                      &
     &    iphys_SGS%i_SGS_maxwell, iphys_base%i_magne, dt,              &
     &    FEM_prm, mesh%nod_comm, mesh%node, mesh%ele, fluid,           &
     &    iphys_ele_base, ele_fld, fem_int%jcs, fem_int%rhs_tbl,        &
     &    rhs_mat%fem_wk, mk_MHD%mlump_fl, rhs_mat%f_l, rhs_mat%f_nl,   &
     &    nod_fld, v_sol)
!
!    filtering (to iphys_SGS_wk%i_nlg)
!
      call cal_filtered_vector_whole(SGS_par%filter_p,                  &
     &   mesh%nod_comm, mesh%node, FEM_filters%filtering,               &
     &    iphys_SGS_wk%i_nlg, iphys_SGS_wk%i_nlg, FEM_SGS_wk%wk_filter, &
     &    nod_fld, v_sol)
!
!    take difference (to iphys_SGS_wk%i_simi)
!
      call subtract_2_nod_vectors(nod_fld,                              &
     &    iphys_SGS_wk%i_nlg, iphys_SGS_wk%i_simi, iphys_SGS_wk%i_simi)
      call delete_field_by_fixed_v_bc                                   &
     &   (Vnod_bcs, iphys_SGS_wk%i_simi, nod_fld)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_vector, iphys_SGS_wk%i_simi)
!
!    obtain modeled commutative error  ( to iphys_SGS_wk%i_wd_nlg)
!
      call cal_commute_error_4_mf                                       &
     &   (FEM_prm%npoint_t_evo_int, fluid%istack_ele_fld_smp,           &
     &    mk_MHD%mlump_fl, mesh%node, mesh%ele, mesh%surf,              &
     &    group%surf_grp, fem_int%jcs, fem_int%rhs_tbl,                 &
     &    FEM_filters%FEM_elens, Bsf_bcs%sgs, ifilter_4delta,           &
     &    iphys_SGS_wk%i_wd_nlg, iphys_SGS_wk%i_wd_nlg,                 &
     &    iphys_fil%i_magne, rhs_mat%fem_wk, rhs_mat%surf_wk,           &
     &    rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
!
      call vector_send_recv                                             &
     &   (iphys_SGS_wk%i_wd_nlg, mesh%nod_comm, nod_fld, v_sol)
      call delete_field_by_fixed_v_bc                                   &
     &   (Vnod_bcs, iphys_SGS_wk%i_wd_nlg, nod_fld)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_vector, iphys_SGS_wk%i_wd_nlg)
!
!    obtain modeled commutative error  ( to iphys_SGS_wk%i_nlg)
!
      call cal_commute_error_4_mf                                       &
     &   (FEM_prm%npoint_t_evo_int, fluid%istack_ele_fld_smp,           &
     &    mk_MHD%mlump_fl, mesh%node, mesh%ele, mesh%surf,              &
     &    group%surf_grp, fem_int%jcs, fem_int%rhs_tbl,                 &
     &    FEM_filters%FEM_elens, Bsf_bcs%sgs, ifilter_2delta,           &
     &    iphys_SGS_wk%i_nlg, iphys_SGS%i_SGS_maxwell,                  &
     &    iphys_base%i_magne, rhs_mat%fem_wk, rhs_mat%surf_wk,          &
     &    rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
!
      call vector_send_recv                                             &
     &   (iphys_SGS_wk%i_nlg, mesh%nod_comm, nod_fld, v_sol)
!
!    filtering (to iphys_SGS_wk%i_nlg)
!
      call cal_filtered_vector_whole(SGS_par%filter_p,                  &
     &     mesh%nod_comm, mesh%node, FEM_filters%filtering,             &
     &    iphys_SGS_wk%i_nlg, iphys_SGS_wk%i_nlg, FEM_SGS_wk%wk_filter, &
     &    nod_fld, v_sol)
      call delete_field_by_fixed_v_bc                                   &
     &   (Vnod_bcs, iphys_SGS_wk%i_nlg, nod_fld)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_vector, iphys_SGS_wk%i_nlg)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &                     'cal_diff_coef_fluid', n_vector,             &
     &                     iak_diff_sgs%i_SGS_Lorentz,                  &
     &                     icomp_diff_sgs%i_SGS_Lorentz
      call cal_diff_coef_fluid(SGS_par, FEM_filters%layer_tbl,          &
     &    mesh%node, mesh%ele, fluid, iphys_SGS_wk, nod_fld,            &
     &    fem_int%jcs, n_vector, iak_diff_sgs%i_SGS_Lorentz,            &
     &    icomp_diff_sgs%i_SGS_Lorentz, FEM_prm%npoint_t_evo_int,       &
     &    FEM_SGS_wk%wk_cor, FEM_SGS_wk%wk_lsq, FEM_SGS_wk%wk_diff,     &
     &    diff_coefs)
!
      end subroutine s_cal_diff_coef_sgs_mxwl
!
!-----------------------------------------------------------------------
!
      end module cal_diff_coef_sgs_mxwl
