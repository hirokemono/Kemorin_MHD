!cal_sgs_uxb_dynamic_simi.f90
!      module cal_sgs_uxb_dynamic_simi
!
!     Written by H. Matsui on Oct. 2005
!     Modified by H. Matsui on Aug., 2007
!
!!      subroutine s_cal_sgs_uxb_dynamic_simi                           &
!!     &         (FEM_prm, SGS_par, mesh, iphys_base, iphys_fil,        &
!!     &          iphys_wfl, iphys_SGS_wk, fem_int, FEM_filters,        &
!!     &          iak_SGS_induction, icomp_SGS_induction,               &
!!     &          FEM_SGS_wk, nod_fld, Csim_SGS_uxb, v_sol, SR_sig, SR_r)
!!      subroutine cal_sgs_induct_t_dynamic_simi(FEM_prm, SGS_par, mesh,&
!!     &          iphys_base, iphys_fil, iphys_wfl, iphys_SGS,          &
!!     &          iphys_SGS_wk, fem_int, FEM_filters,                   &
!!     &          iak_SGS_induction, icomp_SGS_induction, FEM_SGS_wk,   &
!!     &          rhs_mat, nod_fld, Csim_SGS_uxb, v_sol, SR_sig, SR_r)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(base_field_address), intent(in) :: iphys_base
!!        type(base_field_address), intent(in) :: iphys_fil
!!        type(base_field_address), intent(in) :: iphys_wfl
!!        type(SGS_term_address), intent(in) :: iphys_SGS
!!        type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(filters_on_FEM), intent(in) :: FEM_filters
!!        type(SGS_model_coefficient), intent(inout) :: Csim_SGS_uxb
!!        type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!
      module cal_sgs_uxb_dynamic_simi
!
      use m_precision
!
      use m_machine_parameter
      use m_phys_constants
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_mesh_data
      use t_phys_data
      use t_base_field_labels
      use t_SGS_term_labels
      use t_SGS_model_coef_labels
      use t_table_FEM_const
      use t_jacobians
      use t_MHD_finite_element_mat
      use t_FEM_MHD_filter_data
      use t_material_property
      use t_FEM_SGS_model_coefs
      use t_work_FEM_integration
      use t_work_FEM_dynamic_SGS
      use t_vector_for_solver
      use t_solver_SR
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_cal_sgs_uxb_dynamic_simi                             &
     &         (FEM_prm, SGS_par, mesh, iphys_base, iphys_fil,          &
     &          iphys_wfl, iphys_SGS_wk, fem_int, FEM_filters,          &
     &          FEM_SGS_wk, nod_fld, Csim_SGS_uxb, v_sol, SR_sig, SR_r)
!
      use reset_dynamic_model_coefs
      use cal_filtering_scalars
      use cal_sgs_fluxes_simi
      use cal_model_diff_coefs
      use cvt_dynamic_scheme_coord
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(base_field_address), intent(in) :: iphys_base
      type(base_field_address), intent(in) :: iphys_fil
      type(base_field_address), intent(in) :: iphys_wfl
      type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
      type(finite_element_integration), intent(in) :: fem_int
      type(filters_on_FEM), intent(in) :: FEM_filters
!
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(SGS_model_coefficient), intent(inout) :: Csim_SGS_uxb
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!    reset model coefficients
!
      call reset_vector_sgs_model_coefs                                 &
     &   (mesh%ele, FEM_filters%layer_tbl, Csim_SGS_uxb)
      call clear_work_4_dynamic_model(iphys_SGS_wk, nod_fld)
!
!   similarity model with wider filter
!
      if (iflag_debug.gt.0)                                             &
     &     write(*,*) 'cal_sgs_uxb_simi_wide wide_filter_fld%i_velo'
      call cal_sgs_uxb_simi(iphys_SGS_wk%i_wd_nlg,                      &
     &    iphys_fil%i_velo, iphys_fil%i_magne,                          &
     &    iphys_wfl%i_velo, iphys_wfl%i_magne,                          &
     &    SGS_par%filter_p, mesh%nod_comm, mesh%node,                   &
     &    FEM_filters%wide_filtering, FEM_SGS_wk%wk_filter,             &
     &    nod_fld, v_sol, SR_sig, SR_r)
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_vector, iphys_SGS_wk%i_simi)
!
!    SGS term by similarity model (to iphys_SGS_wk%i_simi)
!
      if (iflag_debug.gt.0)                                             &
     &     write(*,*) 'cal_sgs_uxb_simi'
      call cal_sgs_uxb_simi(iphys_SGS_wk%i_simi,                        &
     &    iphys_base%i_velo, iphys_base%i_magne,                        &
     &    iphys_fil%i_velo, iphys_fil%i_magne,                          &
     &    SGS_par%filter_p, mesh%nod_comm, mesh%node,                   &
     &    FEM_filters%filtering, FEM_SGS_wk%wk_filter,                  &
     &    nod_fld, v_sol, SR_sig, SR_r)
!
!      filtering
!
      call cal_filtered_vector_whole(SGS_par%filter_p,                  &
     &    mesh%nod_comm, mesh%node, FEM_filters%filtering,              &
     &    iphys_SGS_wk%i_nlg, iphys_SGS_wk%i_simi,                      &
     &    FEM_SGS_wk%wk_filter, nod_fld, v_sol, SR_sig, SR_r)
!
!   Change coordinate
!
      call cvt_vector_dynamic_scheme_coord                              &
     &   (SGS_par%model_p, mesh%node, iphys_SGS_wk, nod_fld)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_model_coefs', n_vector,    &
     &                  Csim_SGS_uxb%iak_Csim, Csim_SGS_uxb%icomp_Csim
      call cal_model_coefs(SGS_par, FEM_filters%layer_tbl,              &
     &    mesh%node, mesh%ele, iphys_SGS_wk, nod_fld, fem_int%jcs,      &
     &    SGS_par%model_p%itype_Csym_uxb,                               &
     &    FEM_prm%npoint_t_evo_int, FEM_SGS_wk%wk_cor,                  &
     &    FEM_SGS_wk%wk_lsq, FEM_SGS_wk%wk_sgs, Csim_SGS_uxb)
!
      end subroutine s_cal_sgs_uxb_dynamic_simi
!
!  ---------------------------------------------------------------------
!
      subroutine cal_sgs_induct_t_dynamic_simi(FEM_prm, SGS_par, mesh,  &
     &          iphys_base, iphys_fil, iphys_wfl, iphys_SGS,            &
     &          iphys_SGS_wk, fem_int, FEM_filters, FEM_SGS_wk,         &
     &          rhs_mat, nod_fld, Csim_SGS_uxb, v_sol, SR_sig, SR_r)
!
      use reset_dynamic_model_coefs
      use cal_filtering_scalars
      use cal_sgs_fluxes_simi
      use cal_model_diff_coefs
      use copy_nodal_fields
      use int_element_field_2_node
      use cal_similarity_terms
      use cvt_dynamic_scheme_coord
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(base_field_address), intent(in) :: iphys_base
      type(base_field_address), intent(in) :: iphys_fil
      type(base_field_address), intent(in) :: iphys_wfl
      type(SGS_term_address), intent(in) :: iphys_SGS
      type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
      type(finite_element_integration), intent(in) :: fem_int
      type(filters_on_FEM), intent(in) :: FEM_filters
!
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(SGS_model_coefficient), intent(inout) :: Csim_SGS_uxb
      type(phys_data), intent(inout) :: nod_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!    reset model coefficients
!
      call reset_vector_sgs_model_coefs                                 &
     &   (mesh%ele, FEM_filters%layer_tbl, Csim_SGS_uxb)
      call reset_vector_sgs_nod_m_coefs(Csim_SGS_uxb)
      call clear_work_4_dynamic_model(iphys_SGS_wk, nod_fld)
!
!   similarity model with wider filter
!
      if (iflag_debug.gt.0)                                             &
     &   write(*,*) 'cal_sgs_induct_t_simi_wide wide_filter_fld%i_velo'
      call cal_sgs_induct_t_simi(iphys_SGS_wk%i_wd_nlg,                 &
     &    iphys_fil%i_velo, iphys_fil%i_magne,                          &
     &    iphys_wfl%i_velo, iphys_wfl%i_magne, SGS_par%filter_p,        &
     &    mesh%nod_comm, mesh%node, FEM_filters%wide_filtering,         &
     &    Csim_SGS_uxb, FEM_SGS_wk%wk_filter, nod_fld,                  &
     &    v_sol, SR_sig, SR_r)
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_vector, iphys_SGS_wk%i_wd_nlg)
!
!    SGS term by similarity model
!
      if (iflag_debug.gt.0)                                             &
     &     write(*,*) 'cal_sgs_induct_t_simi'
      call cal_sgs_induct_t_simi(iphys_SGS%i_SGS_induct_t,              &
     &    iphys_base%i_velo, iphys_base%i_magne,                        &
     &    iphys_fil%i_velo, iphys_fil%i_magne, SGS_par%filter_p,        &
     &    mesh%nod_comm, mesh%node, FEM_filters%filtering,              &
     &    Csim_SGS_uxb, FEM_SGS_wk%wk_filter, nod_fld,                  &
     &    v_sol, SR_sig, SR_r)
!
!    copy to work array
!
       call copy_vector_component(nod_fld,                              &
     &     iphys_SGS%i_SGS_induct_t, iphys_SGS_wk%i_simi)
!
!      filtering
!
      call cal_filtered_vector_whole(SGS_par%filter_p,                  &
     &    mesh%nod_comm, mesh%node, FEM_filters%filtering,              &
     &    iphys_SGS_wk%i_nlg, iphys_SGS%i_SGS_induct_t,                 &
     &    FEM_SGS_wk%wk_filter, nod_fld, v_sol, SR_sig, SR_r)
!
!   Change coordinate
!
      call cvt_vector_dynamic_scheme_coord                              &
     &   (SGS_par%model_p, mesh%node, iphys_SGS_wk, nod_fld)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &                    'cal_model_coefs', n_asym_tensor,             &
     &                  Csim_SGS_uxb%iak_Csim, Csim_SGS_uxb%icomp_Csim
      call cal_model_coefs(SGS_par, FEM_filters%layer_tbl,              &
     &    mesh%node, mesh%ele, iphys_SGS_wk, nod_fld, fem_int%jcs,      &
     &    SGS_par%model_p%itype_Csym_uxb,                               &
     &    FEM_prm%npoint_t_evo_int, FEM_SGS_wk%wk_cor,                  &
     &    FEM_SGS_wk%wk_lsq, FEM_SGS_wk%wk_sgs, Csim_SGS_uxb)
!
      call cal_ele_vector_2_node(mesh%node, mesh%ele,                   &
     &    fem_int%jcs, fem_int%rhs_tbl, fem_int%m_lump,                 &
     &    Csim_SGS_uxb%coef(1,1), Csim_SGS_uxb%coef_nod(1,1),           &
     &    rhs_mat%fem_wk, rhs_mat%f_l)
!
      end subroutine cal_sgs_induct_t_dynamic_simi
!
!  ---------------------------------------------------------------------
!
      end module cal_sgs_uxb_dynamic_simi
