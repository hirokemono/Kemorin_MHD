!
!     module cal_diff_coef_sgs_induct
!
!     Written by H. Matsui
!
!!      subroutine s_cal_diff_coef_sgs_induct(iak_diff_uxb,             &
!!     &         icomp_sgs_uxb, icomp_diff_uxb, ie_dfvx, ie_dfbx, dt,   &
!!     &         FEM_prm, SGS_par, mesh, group, surf, fluid, conduct,   &
!!     &         cd_prop, Bsf_bcs, iphys, iphys_ele, ele_fld, fem_int,  &
!!     &         sgs_coefs, FEM_filters, mk_MHD, FEM_SGS_wk,            &
!!     &         mhd_fem_wk, rhs_mat, nod_fld, diff_coefs)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(surface_data), intent(in) :: surf
!!        type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: fluid, conduct
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(filters_on_FEM), intent(in) :: FEM_filters
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs
!!        type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!
      module cal_diff_coef_sgs_induct
!
      use m_precision
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_geometry_data_MHD
      use t_mesh_data
      use t_surface_data
      use t_phys_data
      use t_phys_address
      use t_jacobians
      use t_table_FEM_const
      use t_mhd_mass_matricxes
      use t_MHD_finite_element_mat
      use t_material_property
      use t_SGS_model_coefs
      use t_FEM_MHD_filter_data
      use t_surface_bc_data
      use t_work_FEM_integration
      use t_work_FEM_dynamic_SGS
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
     &         icomp_sgs_uxb, icomp_diff_uxb, ie_dfvx, ie_dfbx, dt,     &
     &         FEM_prm, SGS_par, mesh, group, surf, fluid, conduct,     &
     &         cd_prop, Bsf_bcs, iphys, iphys_ele, ele_fld, fem_int,    &
     &         sgs_coefs, FEM_filters, mk_MHD, FEM_SGS_wk,              &
     &         mhd_fem_wk, rhs_mat, nod_fld, diff_coefs)
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
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(surface_data), intent(in) :: surf
      type(field_geometry_data), intent(in) :: fluid
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(phys_address), intent(in) :: iphys
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(finite_element_integration), intent(in) :: fem_int
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(filters_on_FEM), intent(in) :: FEM_filters
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
      type(phys_data), intent(inout) :: nod_fld
!
!    reset model coefficients
!
      call reset_diff_model_coefs                                       &
     &   (mesh%ele%numele, mesh%ele%istack_ele_smp,                     &
     &    diff_coefs%num_field, iak_diff_uxb, diff_coefs%ak)
      call clear_work_4_dynamic_model(iphys, nod_fld)
!
!   gradient model by filtered field (to iphys%i_sgs_grad_f)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_filter_induct_grad'
      call cal_sgs_induct_t_grad_w_coef                                 &
     &   (ifilter_4delta, icomp_sgs_uxb, iphys%i_sgs_grad_f,            &
     &    iphys%i_filter_velo, iphys%i_filter_magne,                    &
     &    ie_dfvx, ie_dfbx, dt, FEM_prm, SGS_par%model_p,               &
     &    mesh%nod_comm, mesh%node, mesh%ele, conduct, cd_prop,         &
     &    iphys_ele, ele_fld, fem_int%jcs, fem_int%rhs_tbl,             &
     &    FEM_filters%FEM_elens, sgs_coefs, mk_MHD%mlump_cd,            &
     &    rhs_mat%fem_wk, mhd_fem_wk, rhs_mat%f_l, nod_fld)
!
!   take divergence of filtered heat flux (to iphys%i_sgs_simi)
!
      if (iflag_debug.gt.0) write(*,*) 'cal_div_sgs_filter_idct_simi'
      call cal_div_sgs_idct_simi(iphys%i_sgs_simi, iphys%i_sgs_grad_f,  &
     &    iphys%i_filter_velo, iphys%i_filter_magne, dt, FEM_prm,       &
     &    mesh%nod_comm, mesh%node, mesh%ele, conduct, iphys_ele,       &
     &    ele_fld, fem_int%jcs, fem_int%rhs_tbl, rhs_mat%fem_wk,        &
     &    mk_MHD%mlump_cd, rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
!
!   take divergence of heat flux (to iphys%i_sgs_grad)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_div_sgs_induct_simi'
      call cal_div_sgs_idct_simi(iphys%i_sgs_grad,                      &
     &    iphys%i_SGS_induct_t, iphys%i_velo, iphys%i_magne, dt,        &
     &    FEM_prm, mesh%nod_comm, mesh%node, mesh%ele, conduct,         &
     &    iphys_ele, ele_fld, fem_int%jcs, fem_int%rhs_tbl,             &
     &    rhs_mat%fem_wk, mk_MHD%mlump_cd, rhs_mat%f_l, rhs_mat%f_nl,   &
     &    nod_fld)
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_vector_whole(SGS_par%filter_p,                  &
     &    mesh%nod_comm, mesh%node, FEM_filters%filtering,              &
     &    iphys%i_sgs_grad, iphys%i_sgs_grad, FEM_SGS_wk%wk_filter,     &
     &    nod_fld)
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
      call cal_commute_error_4_idct                                     &
     &   (FEM_prm%npoint_t_evo_int, conduct%istack_ele_fld_smp,         &
     &    mk_MHD%mlump_cd, mesh%node, mesh%ele, surf, group%surf_grp,   &
     &    Bsf_bcs, fem_int%jcs, fem_int%rhs_tbl, FEM_filters%FEM_elens, &
     &    ifilter_4delta, iphys%i_sgs_grad_f, iphys%i_sgs_grad_f,       &
     &    iphys%i_filter_velo, iphys%i_filter_magne, rhs_mat%fem_wk,    &
     &    rhs_mat%surf_wk, rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
!
      call vector_send_recv                                             &
     &   (iphys%i_sgs_grad_f, mesh%nod_comm, nod_fld)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_vector, iphys%i_sgs_grad_f)
!
!    obtain modeled commutative error  ( to iphys%i_sgs_grad)
!
      call cal_commute_error_4_idct                                     &
     &   (FEM_prm%npoint_t_evo_int, conduct%istack_ele_fld_smp,         &
     &    mk_MHD%mlump_cd, mesh%node, mesh%ele, surf, group%surf_grp,   &
     &    Bsf_bcs, fem_int%jcs, fem_int%rhs_tbl, FEM_filters%FEM_elens, &
     &    ifilter_2delta, iphys%i_sgs_grad, iphys%i_SGS_induct_t,       &
     &    iphys%i_velo, iphys%i_magne, rhs_mat%fem_wk, rhs_mat%surf_wk, &
     &    rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
!
      call vector_send_recv                                             &
     &   (iphys%i_sgs_grad, mesh%nod_comm, nod_fld)
!
!    filtering (to iphys%i_sgs_grad)
!
      call cal_filtered_vector_whole(SGS_par%filter_p,                  &
     &    mesh%nod_comm, mesh%node, FEM_filters%filtering,              &
     &    iphys%i_sgs_grad, iphys%i_sgs_grad, FEM_SGS_wk%wk_filter,     &
     &    nod_fld)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_vector, iphys%i_sgs_grad)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &   'cal_diff_coef_fluid', n_vector, iak_diff_uxb, icomp_diff_uxb
      call cal_diff_coef_fluid(SGS_par, FEM_filters%layer_tbl,          &
     &    mesh%node, mesh%ele,  fluid, iphys, nod_fld,                  &
     &    fem_int%jcs, n_vector, iak_diff_uxb, icomp_diff_uxb,          &
     &    FEM_prm%npoint_t_evo_int, FEM_SGS_wk%wk_cor,                  &
     &    FEM_SGS_wk%wk_lsq, FEM_SGS_wk%wk_diff, diff_coefs)
!
      end subroutine s_cal_diff_coef_sgs_induct
!
!-----------------------------------------------------------------------
!
      end module cal_diff_coef_sgs_induct
