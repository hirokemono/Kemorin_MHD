!
!     module cal_diff_coef_sgs_hf
!
!     Written by H. Matsui
!
!!      subroutine s_cal_diff_coef_sgs_sf                               &
!!     &        (itype_Csym_flux, iflag_supg, num_int, dt,              &
!!     &         ifield, ifield_f, ivelo, ivelo_f, i_sgs,               &
!!     &         icomp_sgs_flux, icomp_diff_sf, iphys_elediff_fil_v,    &
!!     &         SGS_par, mesh, group, Snod_bcs, sf_bcs,                &
!!     &         iphys_SGS_wk, iphys_ele_base, ele_fld, fluid, fem_int, &
!!     &         FEM_filters, sgs_coefs, mk_MHD, FEM_SGS_wk, mhd_fem_wk,&
!!     &         rhs_mat, nod_fld, Cdiff_SGS_flux, v_sol, SR_sig, SR_r)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(nodal_bcs_4_scalar_type), intent(in) :: Snod_bcs
!!        type(scaler_surf_bc_type), intent(in) :: sf_bcs
!!        type(jacobians_type), intent(in) :: jacs
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(filters_on_FEM), intent(in) :: FEM_filters
!!        type(SGS_coefficients_type), intent(in) :: sgs_coefs
!!        type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!!        type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(SGS_model_coefficient), intent(inout) :: Cdiff_SGS_flux
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
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
      use t_base_field_labels
      use t_SGS_model_coef_labels
      use t_jacobians
      use t_table_FEM_const
      use t_FEM_MHD_filter_data
      use t_MHD_finite_element_mat
      use t_MHD_mass_matrices
      use t_bc_data_temp
      use t_surface_bc_data
      use t_surface_bc_scalar
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
      subroutine s_cal_diff_coef_sgs_sf                                 &
     &        (itype_Csym_flux, iflag_supg, num_int, dt,                &
     &         ifield, ifield_f, ivelo, ivelo_f, i_sgs,                 &
     &         icomp_sgs_flux, icomp_diff_sf, iphys_elediff_fil_v,      &
     &         SGS_par, mesh, group, Snod_bcs, sf_bcs,                  &
     &         iphys_SGS_wk, iphys_ele_base, ele_fld, fluid, fem_int,   &
     &         FEM_filters, sgs_coefs, mk_MHD, FEM_SGS_wk, mhd_fem_wk,  &
     &         rhs_mat, nod_fld, Cdiff_SGS_flux, v_sol, SR_sig, SR_r)
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
      integer(kind = kint), intent(in) :: icomp_sgs_flux, icomp_diff_sf
      integer(kind = kint), intent(in) :: iphys_elediff_fil_v
      real(kind = kreal), intent(in) :: dt
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: fluid
      type(nodal_bcs_4_scalar_type), intent(in) :: Snod_bcs
      type(scaler_surf_bc_type), intent(in) :: sf_bcs
      type(finite_element_integration), intent(in) :: fem_int
      type(filters_on_FEM), intent(in) :: FEM_filters
      type(SGS_coefficients_type), intent(in) :: sgs_coefs
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_model_coefficient), intent(inout) :: Cdiff_SGS_flux
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!    reset model coefficients
!
      call reset_diff_model_coefs                                       &
     &   (mesh%ele%numele, mesh%ele%istack_ele_smp,                     &
     &    Cdiff_SGS_flux%coef(1,1))
      call clear_work_4_dynamic_model(iphys_SGS_wk, nod_fld)
!
!   gradient model by filtered field (to iphys_SGS_wk%i_wd_nlg)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_filter_hf_grad'
      call cal_sgs_s_flux_grad_w_coef                                   &
     &   (iflag_supg, num_int, dt, itype_Csym_flux,                     &
     &    SGS_par%model_p%icoord_Csim, ifilter_4delta, icomp_sgs_flux,  &
     &    iphys_SGS_wk%i_wd_nlg, ifield_f, iphys_elediff_fil_v,         &
     &    mesh%nod_comm, mesh%node, mesh%ele, fluid,                    &
     &    iphys_ele_base, ele_fld, fem_int%jcs, fem_int%rhs_tbl,        &
     &    FEM_filters%FEM_elens, sgs_coefs, mk_MHD%mlump_fl,            &
     &    mhd_fem_wk, rhs_mat%fem_wk, rhs_mat%f_l, nod_fld,             &
     &    v_sol, SR_sig, SR_r)
!
!   take divergence of filtered heat flux (to iphys_SGS_wk%i_simi)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_div_sgs_filter_hf_simi'
      call cal_div_sgs_sf_simi                                          &
     &   (iphys_SGS_wk%i_simi, iphys_SGS_wk%i_wd_nlg,                   &
     &    ivelo_f, ifield_f, iflag_supg, num_int, dt,                   &
     &    mesh%nod_comm, mesh%node, mesh%ele, fluid, iphys_ele_base,    &
     &    ele_fld, fem_int%jcs, fem_int%rhs_tbl, rhs_mat%fem_wk,        &
     &    mk_MHD%mlump_fl, rhs_mat%f_l, rhs_mat%f_nl, nod_fld,          &
     &    v_sol, SR_sig, SR_r)
!
!   take divergence of heat flux (to iphys_SGS_wk%i_nlg)
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_div_sgs_h_flux_simi'
      call cal_div_sgs_sf_simi(iphys_SGS_wk%i_nlg,                      &
     &    i_sgs, ivelo, ifield, iflag_supg, num_int, dt,                &
     &    mesh%nod_comm, mesh%node, mesh%ele, fluid, iphys_ele_base,    &
     &    ele_fld, fem_int%jcs, fem_int%rhs_tbl, rhs_mat%fem_wk,        &
     &    mk_MHD%mlump_fl, rhs_mat%f_l, rhs_mat%f_nl, nod_fld,          &
     &    v_sol, SR_sig, SR_r)
!
!
!    filtering (to iphys_SGS_wk%i_nlg)
!
      call cal_filtered_scalar_whole(SGS_par%filter_p,                  &
     &    mesh%nod_comm, mesh%node, FEM_filters%filtering,              &
     &    iphys_SGS_wk%i_nlg, iphys_SGS_wk%i_nlg, FEM_SGS_wk%wk_filter, &
     &    nod_fld, v_sol, SR_sig, SR_r)
!
!    take difference (to iphys_SGS_wk%i_simi)
!
      call subtract_2_nod_scalars(nod_fld,                              &
     &    iphys_SGS_wk%i_nlg, iphys_SGS_wk%i_simi, iphys_SGS_wk%i_simi)
      call delete_field_by_fixed_s_bc                                   &
     &   (Snod_bcs%nod_bc_s, iphys_SGS_wk%i_simi, nod_fld)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_scalar, iphys_SGS_wk%i_simi)
!
!    obtain modeled commutative error  ( to iphys_SGS_wk%i_wd_nlg)
!
      call cal_commute_error_4_sf(num_int, fluid%istack_ele_fld_smp,    &
     &    mk_MHD%mlump_fl, mesh%node, mesh%ele, mesh%surf,              &
     &    group%surf_grp, fem_int%jcs, fem_int%rhs_tbl,                 &
     &    FEM_filters%FEM_elens, sf_bcs%sgs, ifilter_4delta,            &
     &    iphys_SGS_wk%i_wd_nlg, iphys_SGS_wk%i_wd_nlg,                 &
     &    ivelo_f, ifield_f, rhs_mat%fem_wk, rhs_mat%surf_wk,           &
     &    rhs_mat%f_l, rhs_mat%f_nl, nod_fld)
      call delete_field_by_fixed_s_bc                                   &
     &   (Snod_bcs%nod_bc_s, iphys_SGS_wk%i_wd_nlg, nod_fld)
!
      call scalar_send_recv(iphys_SGS_wk%i_wd_nlg, mesh%nod_comm,       &
     &                      nod_fld, v_sol, SR_sig, SR_r)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_scalar, iphys_SGS_wk%i_wd_nlg)
!
!    obtain modeled commutative error  ( to iphys_SGS_wk%i_nlg)
!
      call cal_commute_error_4_sf(num_int, fluid%istack_ele_fld_smp,    &
     &    mk_MHD%mlump_fl, mesh%node, mesh%ele, mesh%surf,              &
     &    group%surf_grp, fem_int%jcs, fem_int%rhs_tbl,                 &
     &    FEM_filters%FEM_elens, sf_bcs%sgs, ifilter_2delta,            &
     &    iphys_SGS_wk%i_nlg, i_sgs, ivelo, ifield,                     &
     &    rhs_mat%fem_wk, rhs_mat%surf_wk, rhs_mat%f_l, rhs_mat%f_nl,   &
     &    nod_fld)
!
      call scalar_send_recv(iphys_SGS_wk%i_nlg, mesh%nod_comm,          &
     &                      nod_fld, v_sol, SR_sig, SR_r)
!
!    filtering (to iphys_SGS_wk%i_nlg)
!
      call cal_filtered_scalar_whole(SGS_par%filter_p,                  &
     &    mesh%nod_comm, mesh%node, FEM_filters%filtering,              &
     &    iphys_SGS_wk%i_nlg, iphys_SGS_wk%i_nlg, FEM_SGS_wk%wk_filter, &
     &    nod_fld, v_sol, SR_sig, SR_r)
      call delete_field_by_fixed_s_bc                                   &
     &   (Snod_bcs%nod_bc_s, iphys_SGS_wk%i_nlg, nod_fld)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_scalar, iphys_SGS_wk%i_nlg)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)  'cal_diff_coef_fluid',         &
     &           n_scalar, Cdiff_SGS_flux%iak_Csim, icomp_diff_sf
      call cal_diff_coef_fluid(SGS_par%iflag_SGS_initial,               &
     &    SGS_par%model_p, SGS_par%commute_p,                           &
     &    FEM_filters%layer_tbl, mesh%node, mesh%ele, fluid,            &
     &    iphys_SGS_wk, nod_fld, fem_int%jcs, n_scalar,                 &
     &    icomp_diff_sf, num_int, FEM_SGS_wk%wk_cor,                    &
     &    FEM_SGS_wk%wk_lsq, FEM_SGS_wk%wk_diff, Cdiff_SGS_flux)
!
      Cdiff_SGS_flux%flag_set = .TRUE.
!
      end subroutine s_cal_diff_coef_sgs_sf
!
!-----------------------------------------------------------------------
!
      end module cal_diff_coef_sgs_hf
