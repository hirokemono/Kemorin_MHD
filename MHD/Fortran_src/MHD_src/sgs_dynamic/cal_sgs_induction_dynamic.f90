!cal_sgs_induction_dynamic.f90
!      module cal_sgs_induction_dynamic
!
!     Written by H. Matsui on Oct. 2005
!     Modified by H. Matsui on Aug., 2007
!
!!      subroutine cal_sgs_uxb_dynamic(dt, FEM_prm, SGS_par, mesh,      &
!!     &          iphys_base, iphys_fil, iphys_SGS, iphys_SGS_wk,       &
!!     &          iphys_ele_base, ele_fld, conduct, cd_prop, fem_int,   &
!!     &          FEM_filters, iak_sgs_term, icomp_sgs_term,            &
!!     &          iphys_elediff_vec, iphys_elediff_fil, mk_MHD,         &
!!     &          FEM_SGS_wk, mhd_fem_wk, rhs_mat,                      &
!!     &          nod_fld, sgs_coefs, v_sol)
!!      subroutine cal_sgs_induct_t_dynamic(dt, FEM_prm, SGS_par, mesh, &
!!     &          iphys_base, iphys_fil, iphys_SGS, iphys_SGS_wk,       &
!!     &          iphys_ele_base, ele_fld, conduct, cd_prop, fem_int,   &
!!     &          FEM_filters, iak_sgs_term, icomp_sgs_term,            &
!!     &          iphys_elediff_vec, iphys_elediff_fil, sgs_coefs_nod,  &
!!     &          mk_MHD, FEM_SGS_wk, mhd_fem_wk, rhs_mat,              &
!!     &          nod_fld, sgs_coefs, v_sol)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(base_field_address), intent(in) :: iphys_base
!!        type(base_field_address), intent(in) :: iphys_fil
!!        type(SGS_term_address), intent(in) :: iphys_SGS
!!        type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
!!        type(base_field_address), intent(in) :: iphys_ele_base
!!        type(phys_data), intent(in) :: ele_fld
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(finite_element_integration), intent(in) :: fem_int
!!        type(filters_on_FEM), intent(in) :: FEM_filters
!!        type(SGS_term_address), intent(in) :: iak_sgs_term
!!        type(SGS_term_address), intent(in) :: icomp_sgs_term
!!        type(base_field_address), intent(in) :: iphys_elediff_vec
!!        type(base_field_address), intent(in) :: iphys_elediff_fil
!!        type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!!        type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
!!        type(arrays_finite_element_mat), intent(inout) :: rhs_mat
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!!        type(vectors_4_solver), intent(inout) :: v_sol
!
      module cal_sgs_induction_dynamic
!
      use m_precision
!
      use calypso_mpi
!
      use m_phys_constants
      use m_machine_parameter
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_physical_property
      use t_mesh_data
      use t_geometry_data_MHD
      use t_phys_data
      use t_base_field_labels
      use t_SGS_term_labels
      use t_SGS_model_coef_labels
      use t_table_FEM_const
      use t_jacobians
      use t_MHD_finite_element_mat
      use t_FEM_MHD_filter_data
      use t_ele_info_4_dynamic
      use t_MHD_mass_matrices
      use t_material_property
      use t_SGS_model_coefs
      use t_work_FEM_integration
      use t_work_FEM_dynamic_SGS
      use t_vector_for_solver
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_sgs_uxb_dynamic(dt, FEM_prm, SGS_par, mesh,        &
     &          iphys_base, iphys_fil, iphys_SGS, iphys_SGS_wk,         &
     &          iphys_ele_base, ele_fld, conduct, cd_prop, fem_int,     &
     &          FEM_filters, iak_sgs_term, icomp_sgs_term,              &
     &          iphys_elediff_vec, iphys_elediff_fil, mk_MHD,           &
     &          FEM_SGS_wk, mhd_fem_wk, rhs_mat,                        &
     &          nod_fld, sgs_coefs, v_sol)
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_filtering_scalars
      use cal_sgs_fluxes_simi
      use cal_sgs_uxb_grad
      use cal_model_diff_coefs
      use cvt_dynamic_scheme_coord
!
      real(kind = kreal), intent(in) :: dt
!
      type(SGS_term_address), intent(in) :: iak_sgs_term
      type(SGS_term_address), intent(in) :: icomp_sgs_term
      type(base_field_address), intent(in) :: iphys_elediff_vec
      type(base_field_address), intent(in) :: iphys_elediff_fil
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(base_field_address), intent(in) :: iphys_base
      type(base_field_address), intent(in) :: iphys_fil
      type(SGS_term_address), intent(in) :: iphys_SGS
      type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(finite_element_integration), intent(in) :: fem_int
      type(filters_on_FEM), intent(in) :: FEM_filters
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(vectors_4_solver), intent(inout) :: v_sol
!
!    reset model coefficients
!
      call reset_vector_sgs_model_coefs                                 &
     &   (mesh%ele, FEM_filters%layer_tbl,                              &
     &    icomp_sgs_term%i_SGS_induction, sgs_coefs)
      call clear_work_4_dynamic_model(iphys_SGS_wk, nod_fld)
!
!    SGS term by similarity model (to iphys_SGS_wk%i_simi)
!
      if (iflag_debug.gt.0) write(*,*) 'cal_sgs_uxb_simi'
      call cal_sgs_uxb_simi(iphys_SGS_wk%i_simi,                        &
     &    iphys_base%i_velo, iphys_base%i_magne,                        &
     &    iphys_fil%i_velo, iphys_fil%i_magne,                          &
     &    SGS_par%filter_p, mesh%nod_comm, mesh%node,                   &
     &    FEM_filters%filtering,FEM_SGS_wk%wk_filter, nod_fld, v_sol)
!
!   gradient model by filtered field
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_filter_uxb_grad_4_dyn'
      call cal_sgs_vp_induct_grad_no_coef(ifilter_4delta,               &
     &    iphys_SGS_wk%i_wd_nlg, iphys_fil%i_magne,                     &
     &    iphys_elediff_fil%i_velo, dt, FEM_prm,                        &
     &    mesh%nod_comm, mesh%node, mesh%ele, conduct, cd_prop,         &
     &    iphys_ele_base, ele_fld, fem_int%jcs, fem_int%rhs_tbl,        &
     &    FEM_filters%FEM_elens, mk_MHD%mlump_cd, mhd_fem_wk,           &
     &    rhs_mat%fem_wk, rhs_mat%f_l, nod_fld, v_sol)
!
!   gradient model by original field
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_uxb_grad_4_dyn'
      call cal_sgs_vp_induct_grad_no_coef(ifilter_2delta,               &
     &    iphys_SGS%i_SGS_vp_induct, iphys_base%i_magne,                &
     &    iphys_elediff_vec%i_velo, dt, FEM_prm,                        &
     &    mesh%nod_comm, mesh%node, mesh%ele,                           &
     &    conduct, cd_prop, iphys_ele_base, ele_fld,                    &
     &    fem_int%jcs, fem_int%rhs_tbl, FEM_filters%FEM_elens,          &
     &    mk_MHD%mlump_cd, mhd_fem_wk, rhs_mat%fem_wk,                  &
     &    rhs_mat%f_l, nod_fld, v_sol)
!
!      filtering
!
      call cal_filtered_vector_whole(SGS_par%filter_p,                  &
     &    mesh%nod_comm, mesh%node, FEM_filters%filtering,              &
     &    iphys_SGS_wk%i_nlg, iphys_SGS%i_SGS_vp_induct,                &
     &    FEM_SGS_wk%wk_filter, nod_fld, v_sol)
!
!   Change coordinate
!
      call cvt_vector_dynamic_scheme_coord                              &
     &   (SGS_par%model_p, mesh%node, iphys_SGS_wk, nod_fld)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_model_coefs', n_vector,    &
     &                     iak_sgs_term%i_SGS_induction,                &
     &                     icomp_sgs_term%i_SGS_induction
      call cal_model_coefs(SGS_par, FEM_filters%layer_tbl,              &
     &    mesh%node, mesh%ele, iphys_SGS_wk, nod_fld, fem_int%jcs,      &
     &    SGS_par%model_p%itype_Csym_uxb, n_vector,                     &
     &    iak_sgs_term%i_SGS_induction, icomp_sgs_term%i_SGS_induction, &
     &    FEM_prm%npoint_t_evo_int,         &
     &    FEM_SGS_wk%wk_cor, FEM_SGS_wk%wk_lsq, FEM_SGS_wk%wk_sgs,      &
     &    sgs_coefs)
!
      end subroutine cal_sgs_uxb_dynamic
!
!  ---------------------------------------------------------------------
!
      subroutine cal_sgs_induct_t_dynamic(dt, FEM_prm, SGS_par, mesh,   &
     &          iphys_base, iphys_fil, iphys_SGS, iphys_SGS_wk,         &
     &          iphys_ele_base, ele_fld, conduct, cd_prop, fem_int,     &
     &          FEM_filters, iak_sgs_term, icomp_sgs_term,              &
     &          iphys_elediff_vec, iphys_elediff_fil, sgs_coefs_nod,    &
     &          mk_MHD, FEM_SGS_wk, mhd_fem_wk, rhs_mat,                &
     &          nod_fld, sgs_coefs, v_sol)
!
      use reset_dynamic_model_coefs
      use copy_nodal_fields
      use cal_filtering_scalars
      use cal_sgs_fluxes_simi
      use cal_sgs_inductions_grad
      use cal_model_diff_coefs
      use cvt_dynamic_scheme_coord
      use reduce_model_coefs
!
      type(SGS_term_address), intent(in) :: iak_sgs_term
      type(SGS_term_address), intent(in) :: icomp_sgs_term
      type(base_field_address), intent(in) :: iphys_elediff_vec
      type(base_field_address), intent(in) :: iphys_elediff_fil
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_geometry), intent(in) :: mesh
      type(base_field_address), intent(in) :: iphys_base
      type(base_field_address), intent(in) :: iphys_fil
      type(SGS_term_address), intent(in) :: iphys_SGS
      type(dynamic_SGS_work_address), intent(in) :: iphys_SGS_wk
      type(base_field_address), intent(in) :: iphys_ele_base
      type(phys_data), intent(in) :: ele_fld
      type(field_geometry_data), intent(in) :: conduct
      type(conductive_property), intent(in) :: cd_prop
      type(finite_element_integration), intent(in) :: fem_int
      type(filters_on_FEM), intent(in) :: FEM_filters
      type(SGS_coefficients_type), intent(in) :: sgs_coefs_nod
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(phys_data), intent(inout) :: nod_fld
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(vectors_4_solver), intent(inout) :: v_sol
!
!    reset model coefficients
!
      call reset_vector_sgs_model_coefs                                 &
     &   (mesh%ele, FEM_filters%layer_tbl,                              &
     &    icomp_sgs_term%i_SGS_induction, sgs_coefs)
      call clear_work_4_dynamic_model(iphys_SGS_wk, nod_fld)
!
!    SGS term by similarity model
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_induct_t_simi'
      call cal_sgs_induct_t_simi(iphys_SGS%i_SGS_induct_t,              &
     &    iphys_base%i_velo, iphys_base%i_magne,                        &
     &    iphys_fil%i_velo, iphys_fil%i_magne,                          &
     &    icomp_sgs_term%i_SGS_induction, SGS_par%filter_p,             &
     &    mesh%nod_comm, mesh%node,  FEM_filters%filtering,             &
     &    sgs_coefs_nod, FEM_SGS_wk%wk_filter, nod_fld, v_sol)
!
!    copy to work array
!
       call copy_vector_component(nod_fld,                              &
      &    iphys_SGS%i_SGS_induct_t, iphys_SGS_wk%i_simi)
!
!   gradient model by filtered field
!
      if (iflag_debug.gt.0) write(*,*) 'cal_sgs_filter_idt_grad_4_dyn'
      call cal_sgs_induct_t_grad_no_coef                                &
     &   (ifilter_4delta, iphys_SGS_wk%i_wd_nlg,                        &
     &    iphys_fil%i_velo, iphys_fil%i_magne,                          &
     &    iphys_elediff_fil%i_velo, iphys_elediff_fil%i_magne,          &
     &    dt, FEM_prm, mesh%nod_comm, mesh%node, mesh%ele, conduct,     &
     &    cd_prop, iphys_ele_base, ele_fld, fem_int%jcs,                &
     &    fem_int%rhs_tbl, FEM_filters%FEM_elens, mk_MHD%mlump_cd,      &
     &    rhs_mat%fem_wk, mhd_fem_wk, rhs_mat%f_l, nod_fld, v_sol)
!
!   gradient model by original field
!
      if (iflag_debug.gt.0)  write(*,*) 'cal_sgs_induct_t_grad_4_dyn'
      call cal_sgs_induct_t_grad_no_coef                                &
     &   (ifilter_2delta,  iphys_SGS%i_SGS_induct_t,                    &
     &    iphys_base%i_velo, iphys_base%i_magne,                        &
     &    iphys_elediff_vec%i_velo, iphys_elediff_vec%i_magne,          &
     &    dt, FEM_prm, mesh%nod_comm, mesh%node, mesh%ele, conduct,     &
     &    cd_prop, iphys_ele_base, ele_fld, fem_int%jcs,                &
     &    fem_int%rhs_tbl, FEM_filters%FEM_elens, mk_MHD%mlump_cd,      &
     &    rhs_mat%fem_wk, mhd_fem_wk, rhs_mat%f_l, nod_fld, v_sol)
!
!      filtering
!
      call cal_filtered_vector_whole(SGS_par%filter_p,                  &
     &    mesh%nod_comm, mesh%node, FEM_filters%filtering,              &
     &    iphys_SGS_wk%i_nlg, iphys_SGS%i_SGS_induct_t,                 &
     &    FEM_SGS_wk%wk_filter, nod_fld, v_sol)
!
!   Change coordinate
!
      call cvt_vector_dynamic_scheme_coord                              &
     &   (SGS_par%model_p, mesh%node, iphys_SGS_wk, nod_fld)
!
!     obtain model coefficient
!
      if (iflag_debug.gt.0 )  write(*,*)                                &
     &         'cal_model_coefs', n_asym_tensor,                        &
     &          iak_sgs_term%i_SGS_induction,                           &
     &          icomp_sgs_term%i_SGS_induction
      call cal_model_coefs(SGS_par, FEM_filters%layer_tbl,              &
     &    mesh%node, mesh%ele, iphys_SGS_wk, nod_fld, fem_int%jcs,      &
     &    SGS_par%model_p%itype_Csym_uxb, n_asym_tensor,                &
     &    iak_sgs_term%i_SGS_induction, icomp_sgs_term%i_SGS_induction, &
     &    FEM_prm%npoint_t_evo_int,         &
     &    FEM_SGS_wk%wk_cor, FEM_SGS_wk%wk_lsq, FEM_SGS_wk%wk_sgs,      &
     &    sgs_coefs)
!
      call reduce_model_coefs_layer(SGS_par%model_p%SGS_uxb_factor,     &
     &    iak_sgs_term%i_SGS_induction, FEM_SGS_wk%wk_sgs)
      call reduce_ele_vect_model_coefs                                  &
     &   (mesh%ele, SGS_par%model_p%SGS_uxb_factor,                     &
     &    sgs_coefs%ntot_comp, icomp_sgs_term%i_SGS_induction,          &
     &    sgs_coefs%ak)
!
      end subroutine cal_sgs_induct_t_dynamic
!
!  ---------------------------------------------------------------------
!
      end module cal_sgs_induction_dynamic
