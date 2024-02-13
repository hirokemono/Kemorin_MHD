!
!     module cal_diff_coef_sgs_induct
!
!     Written by H. Matsui
!
!!      subroutine s_cal_diff_coef_sgs_induct                           &
!!     &         (iak_diff_SGS_induction, icomp_SGS_induction,          &
!!     &          icomp_diff_sgs_induction, iphys_elediff_fil, dt,      &
!!     &          FEM_prm, SGS_par, mesh, group,                        &
!!     &          fluid, conduct, cd_prop, Bsf_bcs,                     &
!!     &          iphys_base, iphys_fil, iphys_SGS, iphys_SGS_wk,       &
!!     &          iphys_ele_base, ele_fld, fem_int, sgs_coefs,          &
!!     &          FEM_filters, mk_MHD, FEM_SGS_wk, mhd_fem_wk, rhs_mat, &
!!     &          nod_fld, diff_coefs, v_sol, SR_sig, SR_r)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(vector_surf_bc_type), intent(in) :: Bsf_bcs
!!        type(base_field_address), intent(in) :: iphys_base
!!        type(base_field_address), intent(in) :: iphys_fil
!!        type(SGS_term_address), intent(in) :: iphys_SGS
!!        type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: fluid, conduct
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(filters_on_FEM), intent(in) :: FEM_filters
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs
!!        type(base_field_address), intent(in) :: iphys_elediff_fil
!!        type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
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
      use t_base_field_labels
      use t_SGS_term_labels
      use t_SGS_model_coef_labels
      use t_jacobians
      use t_table_FEM_const
      use t_MHD_mass_matrices
      use t_MHD_finite_element_mat
      use t_material_property
      use t_SGS_model_coefs
      use t_FEM_MHD_filter_data
      use t_surface_bc_vector
      use t_work_FEM_integration
      use t_work_FEM_dynamic_SGS
      use t_vector_for_solver
      use t_solver_SR
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_diff_coef_sgs_induct                             &
     &         (iak_diff_SGS_induction, icomp_SGS_induction,            &
     &          icomp_diff_sgs_induction, iphys_elediff_fil, dt,        &
     &          FEM_prm, SGS_par, mesh, group,                          &
     &          fluid, conduct, cd_prop, Bsf_bcs,                       &
     &          iphys_base, iphys_fil, iphys_SGS, iphys_SGS_wk,         &
     &          iphys_ele_base, ele_fld, fem_int, sgs_coefs,            &
     &          FEM_filters, mk_MHD, FEM_SGS_wk, mhd_fem_wk, rhs_mat,   &
     &          nod_fld, diff_coefs, v_sol, SR_sig, SR_r)
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
      real(kind = kreal), intent(in) :: dt
      integer(kind = kint), intent(in) :: iak_diff_SGS_induction
      integer(kind = kint), intent(in) :: icomp_SGS_induction
      integer(kind = kint), intent(in) :: icomp_diff_sgs_induction
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(field_geometry_data), intent(in) :: fluid
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(vector_surf_bc_type), intent(in) :: Bsf_bcs
      type(base_field_address), intent(in) :: iphys_base
      type(base_field_address), intent(in) :: iphys_fil
      type(SGS_term_address), intent(in) :: iphys_SGS
      type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(finite_element_integration), intent(in) :: fem_int
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(base_field_address), intent(in) :: iphys_elediff_fil
      type(filters_on_FEM), intent(in) :: FEM_filters
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!    reset model coefficients
!
      call reset_diff_model_coefs                                       &
     &   (mesh%ele%numele, mesh%ele%istack_ele_smp,                     &
     &    diff_coefs%num_field, iak_diff_SGS_induction, diff_coefs%ak)
      call clear_work_4_dynamic_model(iphys_SGS_wk, nod_fld)
!
!   gradient model by filtered field (to iphys_SGS_wk%i_wd_nlg)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_filter_induct_grad'
      call cal_sgs_induct_t_grad_w_coef(ifilter_4delta,                 &
     &    icomp_SGS_induction, iphys_SGS_wk%i_wd_nlg,                   &
     &    dt, FEM_prm, SGS_par%model_p,                                 &
     &    mesh%nod_comm, mesh%node, mesh%ele, conduct, cd_prop,         &
     &    iphys_fil, iphys_ele_base, iphys_elediff_fil,                 &
     &    ele_fld, fem_int%jcs, fem_int%rhs_tbl,                        &
     &    FEM_filters%FEM_elens, sgs_coefs, mk_MHD%mlump_cd,            &
     &    rhs_mat%fem_wk, mhd_fem_wk, rhs_mat%f_l, nod_fld,             &
     &    v_sol, SR_sig, SR_r)
!
!   take divergence of filtered heat flux (to iphys_SGS_wk%i_simi)
!
      if (iflag_debug.gt.0) write(*,*) 'cal_div_sgs_filter_idct_simi'
      call cal_div_sgs_idct_simi                                        &
     &   (iphys_SGS_wk%i_simi, iphys_SGS_wk%i_wd_nlg,                   &
     &    iphys_fil%i_velo, iphys_fil%i_magne,                          &
     &    dt, FEM_prm, mesh%nod_comm, mesh%node, mesh%ele, conduct,     &
     &    iphys_ele_base, ele_fld, fem_int%jcs, fem_int%rhs_tbl,        &
     &    rhs_mat%fem_wk, mk_MHD%mlump_cd, rhs_mat%f_l, rhs_mat%f_nl,   &
     &    nod_fld, v_sol, SR_sig, SR_r)
!
!   take divergence of heat flux (to iphys_SGS_wk%i_nlg)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_div_sgs_induct_simi'
      call cal_div_sgs_idct_simi                                        &
     &   (iphys_SGS_wk%i_nlg, iphys_SGS%i_SGS_induct_t,                 &
     &    iphys_base%i_velo, iphys_base%i_magne,                        &
     &    dt, FEM_prm, mesh%nod_comm, mesh%node, mesh%ele, conduct,     &
     &    iphys_ele_base, ele_fld, fem_int%jcs, fem_int%rhs_tbl,        &
     &    rhs_mat%fem_wk, mk_MHD%mlump_cd, rhs_mat%f_l, rhs_mat%f_nl,   &
     &    nod_fld, v_sol, SR_sig, SR_r)
!
!    filtering (to iphys_SGS_wk%i_nlg)
!
      call cal_filtered_vector_whole(SGS_par%filter_p,                  &
     &    mesh%nod_comm, mesh%node, FEM_filters%filtering,              &
     &    iphys_SGS_wk%i_nlg, iphys_SGS_wk%i_nlg, FEM_SGS_wk%wk_filter, &
     &    nod_fld, v_sol, SR_sig, SR_r)
!
!    take difference (to iphys_SGS_wk%i_simi)
!
      call subtract_2_nod_vectors(nod_fld,                              &
     &    iphys_SGS_wk%i_nlg, iphys_SGS_wk%i_simi, iphys_SGS_wk%i_simi)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_vector, iphys_SGS_wk%i_simi)
!
!    obtain modeled commutative error  ( to iphys_SGS_wk%i_wd_nlg)
!
      call cal_commute_error_4_idct(FEM_prm%npoint_t_evo_int,           &
     &    conduct%istack_ele_fld_smp, mk_MHD%mlump_cd,                  &
     &    mesh%node, mesh%ele, mesh%surf, group%surf_grp,               &
     &    Bsf_bcs, fem_int%jcs, fem_int%rhs_tbl, FEM_filters%FEM_elens, &
     &    ifilter_4delta, iphys_SGS_wk%i_wd_nlg, iphys_SGS_wk%i_wd_nlg, &
     &    iphys_fil%i_velo, iphys_fil%i_magne,                          &
     &    rhs_mat%fem_wk, rhs_mat%surf_wk, rhs_mat%f_l, rhs_mat%f_nl,   &
     &    nod_fld)
!
      call vector_send_recv(iphys_SGS_wk%i_wd_nlg, mesh%nod_comm,       &
     &                      nod_fld, v_sol, SR_sig, SR_r)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_vector, iphys_SGS_wk%i_wd_nlg)
!
!    obtain modeled commutative error  ( to iphys_SGS_wk%i_nlg)
!
      call cal_commute_error_4_idct(FEM_prm%npoint_t_evo_int,           &
     &    conduct%istack_ele_fld_smp, mk_MHD%mlump_cd,                  &
     &    mesh%node, mesh%ele, mesh%surf, group%surf_grp,               &
     &    Bsf_bcs, fem_int%jcs, fem_int%rhs_tbl,                        &
     &    FEM_filters%FEM_elens, ifilter_2delta,                        &
     &    iphys_SGS_wk%i_nlg, iphys_SGS%i_SGS_induct_t,                 &
     &    iphys_base%i_velo, iphys_base%i_magne,                        &
     &    rhs_mat%fem_wk, rhs_mat%surf_wk, rhs_mat%f_l, rhs_mat%f_nl,   &
     &    nod_fld)
!
      call vector_send_recv(iphys_SGS_wk%i_nlg, mesh%nod_comm,          &
     &                      nod_fld, v_sol, SR_sig, SR_r)
!
!    filtering (to iphys_SGS_wk%i_nlg)
!
      call cal_filtered_vector_whole(SGS_par%filter_p,                  &
     &    mesh%nod_comm, mesh%node, FEM_filters%filtering,              &
     &    iphys_SGS_wk%i_nlg, iphys_SGS_wk%i_nlg, FEM_SGS_wk%wk_filter, &
     &    nod_fld, v_sol, SR_sig, SR_r)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_vector, iphys_SGS_wk%i_nlg)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &                     'cal_diff_coef_fluid', n_vector,             &
     &                     iak_diff_SGS_induction,                      &
     &                     icomp_diff_sgs_induction
      call cal_diff_coef_fluid(SGS_par%iflag_SGS_initial,               &
     &    SGS_par%model_p, SGS_par%commute_p,                           &
     &    FEM_filters%layer_tbl, mesh%node, mesh%ele,                   &
     &    fluid, iphys_SGS_wk, nod_fld, fem_int%jcs, n_vector,          &
     &    iak_diff_SGS_induction, icomp_diff_sgs_induction,             &
     &    FEM_prm%npoint_t_evo_int, FEM_SGS_wk%wk_cor,                  &
     &    FEM_SGS_wk%wk_lsq, FEM_SGS_wk%wk_diff, diff_coefs)
!
      end subroutine s_cal_diff_coef_sgs_induct
!
!-----------------------------------------------------------------------
!
      end module cal_diff_coef_sgs_induct
