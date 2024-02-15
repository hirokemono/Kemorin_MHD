!cal_sgs_m_flux_dynamic_simi.f90
!      module cal_sgs_m_flux_dynamic_simi
!
!     Written by H. Matsui on Oct. 2005
!     Modified by H. Matsui on Aug., 2007
!
!!      subroutine s_cal_sgs_m_flux_dynamic_simi(FEM_prm, SGS_par, mesh,&
!!     &          iphys_base, iphys_fil, iphys_wfl, iphys_SGS,          &
!!     &          iphys_SGS_wk, fem_int, FEM_filters,                   &
!!     &          iak_sgs_term, icomp_sgs_term, FEM_SGS_wk,             &
!!     &          rhs_mat, nod_fld, sgs_coefs, sgs_coefs_nod,           &
!!     &          v_sol, SR_sig, SR_r)
!!      subroutine cal_sgs_maxwell_dynamic_simi(FEM_prm, SGS_par, mesh, &
!!     &          iphys_base, iphys_fil, iphys_wfl, iphys_SGS,          &
!!     &          iphys_SGS_wk, fem_int, FEM_filters,                   &
!!     &          iak_sgs_term, icomp_sgs_term, FEM_SGS_wk,             &
!!     &          rhs_mat, nod_fld, sgs_coefs, sgs_coefs_nod,           &
!!     &          v_sol, SR_sig, SR_r)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(base_field_address), intent(in) :: iphys_base
!!        type(base_field_address), intent(in) :: iphys_fil
!!        type(base_field_address), intent(in) :: iphys_wfl
!!        type(SGS_term_address), intent(in) :: iphys_SGS
!!        type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(filters_on_FEM), intent(in) :: FEM_filters
!!        type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs_nod
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!
      module cal_sgs_m_flux_dynamic_simi
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
      use t_FEM_MHD_filter_data
      use t_material_property
      use t_SGS_model_coefs
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
      subroutine s_cal_sgs_m_flux_dynamic_simi(FEM_prm, SGS_par, mesh,  &
     &          iphys_base, iphys_fil, iphys_wfl, iphys_SGS,            &
     &          iphys_SGS_wk, fem_int, FEM_filters,                     &
     &          iak_sgs_term, icomp_sgs_term, FEM_SGS_wk,               &
     &          rhs_mat, nod_fld, sgs_coefs, sgs_coefs_nod,             &
     &          v_sol, SR_sig, SR_r)
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_sgs_fluxes_simi
      use cal_filtering_scalars
      use cal_model_diff_coefs
      use int_element_field_2_node
      use cal_similarity_terms
!
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
      type(SGS_term_address), intent(in) :: iak_sgs_term
      type(SGS_term_address), intent(in) :: icomp_sgs_term
!
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs_nod
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!    reset model coefficients
!
      call reset_tensor_sgs_model_coefs                                 &
     &   (mesh%ele, FEM_filters%layer_tbl,                              &
     &    icomp_sgs_term%i_SGS_m_flux, sgs_coefs)
      call reset_tensor_sgs_nod_m_coefs                                 &
     &   (mesh%node%numnod, mesh%node%istack_nod_smp,                   &
     &    sgs_coefs_nod%ak(1,icomp_sgs_term%i_SGS_m_flux))
      call clear_work_4_dynamic_model(iphys_SGS_wk, nod_fld)
!
!   similarity model with wider filter
!
      if (iflag_debug.gt.0)                                             &
     &     write(*,*) 'cal_sgs_mf_simi_wide wide_filter_fld%i_velo'
      call cal_sgs_mf_simi(iphys_SGS_wk%i_wd_nlg,                       &
     &    iphys_fil%i_velo, iphys_wfl%i_velo,                           &
     &    icomp_sgs_term%i_SGS_m_flux, SGS_par%filter_p,                &
     &    mesh%nod_comm, mesh%node, FEM_filters%wide_filtering,         &
     &    sgs_coefs_nod, FEM_SGS_wk%wk_filter, nod_fld,                 &
     &    v_sol, SR_sig, SR_r)
!
!    SGS term by similarity model
!
      if (iflag_debug.gt.0)                                             &
     &     write(*,*) 'cal_sgs_mf_simi iphys_SGS%i_SGS_m_flux'
      call cal_sgs_mf_simi                                              &
     &   (iphys_SGS%i_SGS_m_flux, iphys_base%i_velo,                    &
     &    iphys_fil%i_velo, icomp_sgs_term%i_SGS_m_flux,                &
     &    SGS_par%filter_p, mesh%nod_comm, mesh%node,                   &
     &    FEM_filters%filtering, sgs_coefs_nod, FEM_SGS_wk%wk_filter,   &
     &    nod_fld, v_sol, SR_sig, SR_r)
!
!    copy to work array
!
       call copy_tensor_component(nod_fld,                              &
     &     iphys_SGS%i_SGS_m_flux, iphys_SGS_wk%i_simi)
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_sym_tensor, iphys_SGS_wk%i_simi)
!
!      filtering
!
      call cal_filtered_sym_tensor_whole(SGS_par%filter_p,              &
     &    mesh%nod_comm, mesh%node, FEM_filters%filtering,              &
     &    iphys_SGS_wk%i_nlg, iphys_SGS%i_SGS_m_flux,                   &
     &    FEM_SGS_wk%wk_filter, nod_fld, v_sol, SR_sig, SR_r)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_sym_tensor, iphys_SGS_wk%i_nlg)
!
!   Change coordinate
!
      call cvt_tensor_dynamic_scheme_coord                              &
     &   (SGS_par%model_p, mesh%node, iphys_SGS_wk, nod_fld)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &                     'cal_model_coefs', n_sym_tensor,             &
     &                     iak_sgs_term%i_SGS_m_flux,                   &
     &                     icomp_sgs_term%i_SGS_m_flux
      call cal_model_coefs(SGS_par, FEM_filters%layer_tbl,              &
     &    mesh%node, mesh%ele, iphys_SGS_wk, nod_fld, fem_int%jcs,      &
     &    SGS_par%model_p%SGS_momentum%itype_Csym_flux, n_sym_tensor,   &
     &    iak_sgs_term%i_SGS_m_flux, icomp_sgs_term%i_SGS_m_flux,       &
     &    FEM_prm%npoint_t_evo_int, FEM_SGS_wk%wk_cor,                  &
     &    FEM_SGS_wk%wk_lsq, FEM_SGS_wk%wk_sgs, sgs_coefs)
!
      call cal_ele_sym_tensor_2_node(mesh%node, mesh%ele,               &
     &    fem_int%jcs, fem_int%rhs_tbl, fem_int%m_lump,                 &
     &    sgs_coefs%ntot_comp, icomp_sgs_term%i_SGS_m_flux,             &
     &    sgs_coefs%ak, sgs_coefs_nod%ntot_comp,                        &
     &    icomp_sgs_term%i_SGS_m_flux, sgs_coefs_nod%ak,                &
     &    rhs_mat%fem_wk, rhs_mat%f_l)
!
      end subroutine s_cal_sgs_m_flux_dynamic_simi
!
!  ---------------------------------------------------------------------
!
      subroutine cal_sgs_maxwell_dynamic_simi(FEM_prm, SGS_par, mesh,   &
     &          iphys_base, iphys_fil, iphys_wfl, iphys_SGS,            &
     &          iphys_SGS_wk, fem_int, FEM_filters,                     &
     &          iak_sgs_term, icomp_sgs_term, FEM_SGS_wk,               &
     &          rhs_mat, nod_fld, sgs_coefs, sgs_coefs_nod,             &
     &          v_sol, SR_sig, SR_r)
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_sgs_fluxes_simi
      use cal_filtering_scalars
      use cal_model_diff_coefs
      use int_element_field_2_node
      use cal_similarity_terms
!
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
      type(SGS_term_address), intent(in) :: iak_sgs_term
      type(SGS_term_address), intent(in) :: icomp_sgs_term
!
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs_nod
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!    reset model coefficients
!
      call reset_tensor_sgs_model_coefs                                 &
     &   (mesh%ele, FEM_filters%layer_tbl,                              &
     &    icomp_sgs_term%i_SGS_Lorentz, sgs_coefs)
      call reset_tensor_sgs_nod_m_coefs                                 &
     &   (mesh%node%numnod, mesh%node%istack_nod_smp,                   &
     &    sgs_coefs_nod%ak(1,icomp_sgs_term%i_SGS_Lorentz))
      call clear_work_4_dynamic_model(iphys_SGS_wk, nod_fld)
!
!   similarity model with wider filter
!
      if (iflag_debug.gt.0)                                             &
     &     write(*,*) 'cal_sgs_mf_simi_wide wide_filter_fld%i_magne'
      call cal_sgs_mf_simi(iphys_SGS_wk%i_wd_nlg,                       &
     &    iphys_fil%i_magne, iphys_wfl%i_magne,                         &
     &    icomp_sgs_term%i_SGS_Lorentz, SGS_par%filter_p,               &
     &    mesh%nod_comm, mesh%node, FEM_filters%wide_filtering,         &
     &    sgs_coefs_nod, FEM_SGS_wk%wk_filter, nod_fld,                 &
     &    v_sol, SR_sig, SR_r)
!
!      call check_nodal_data                                            &
!     &   ((50+my_rank), nod_fld, n_sym_tensor, iphys_SGS_wk%i_wd_nlg)
!
!    SGS term by similarity model
!
      if (iflag_debug.gt.0)                                             &
     &     write(*,*) 'cal_sgs_mf_simi iphys_SGS%i_SGS_maxwell'
      call cal_sgs_mf_simi                                              &
     &   (iphys_SGS%i_SGS_maxwell, iphys_base%i_magne,                  &
     &    iphys_fil%i_magne, icomp_sgs_term%i_SGS_Lorentz,              &
     &    SGS_par%filter_p, mesh%nod_comm, mesh%node,                   &
     &    FEM_filters%filtering, sgs_coefs_nod, FEM_SGS_wk%wk_filter,   &
     &    nod_fld, v_sol, SR_sig, SR_r)
!
!    copy to work array
!
       call copy_tensor_component(nod_fld,                              &
     &     iphys_SGS%i_SGS_maxwell, iphys_SGS_wk%i_simi)
!
!    filtering
!
      call cal_filtered_sym_tensor_whole(SGS_par%filter_p,              &
     &     mesh%nod_comm, mesh%node, FEM_filters%filtering,             &
     &    iphys_SGS_wk%i_nlg, iphys_SGS%i_SGS_maxwell,                  &
     &    FEM_SGS_wk%wk_filter, nod_fld, v_sol, SR_sig, SR_r)
!
!   Change coordinate
!
      call cvt_tensor_dynamic_scheme_coord                              &
     &   (SGS_par%model_p, mesh%node, iphys_SGS_wk, nod_fld)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*)                                 &
     &                     'cal_model_coefs', n_sym_tensor,             &
     &                     iak_sgs_term%i_SGS_Lorentz,                  &
     &                     icomp_sgs_term%i_SGS_Lorentz
      call cal_model_coefs(SGS_par, FEM_filters%layer_tbl,              &
     &    mesh%node, mesh%ele, iphys_SGS_wk, nod_fld, fem_int%jcs,      &
     &    SGS_par%model_p%itype_Csym_maxwell, n_sym_tensor,             &
     &    iak_sgs_term%i_SGS_Lorentz, icomp_sgs_term%i_SGS_Lorentz,     &
     &    FEM_prm%npoint_t_evo_int, FEM_SGS_wk%wk_cor,                  &
     &    FEM_SGS_wk%wk_lsq, FEM_SGS_wk%wk_sgs, sgs_coefs)
!
      call cal_ele_sym_tensor_2_node(mesh%node, mesh%ele,               &
     &    fem_int%jcs, fem_int%rhs_tbl, fem_int%m_lump,                 &
     &    sgs_coefs%ntot_comp, icomp_sgs_term%i_SGS_Lorentz,            &
     &    sgs_coefs%ak, sgs_coefs_nod%ntot_comp,                        &
     &    icomp_sgs_term%i_SGS_Lorentz, sgs_coefs_nod%ak,               &
     &    rhs_mat%fem_wk, rhs_mat%f_l)
!
      end subroutine cal_sgs_maxwell_dynamic_simi
!
!  ---------------------------------------------------------------------
!
      end module cal_sgs_m_flux_dynamic_simi
