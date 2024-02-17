!>@file   lead_physical_values.f90
!!        module lead_physical_values
!!
!! @author H. Matsui
!! @date ...when???
!!
!> @brief Evaluate many kind of field data
!!
!!@verbatim
!!      subroutine lead_fields_by_FEM(istep, MHD_step, FEM_model,       &
!!     &          SGS_par, iphys_LES, ak_MHD, FEM_filters, FEM_MHD,     &
!!     &          SGS_MHD_wk, Csims_FEM_MHD, m_SR)
!!        type(MHD_step_param), intent(in) :: MHD_step
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_data), intent(in) ::   geofem
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(FEM_MHD_BC_data), intent(in) :: FEM_MHD_BCs
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(filters_on_FEM), intent(in) :: FEM_filters
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(FEM_mesh_field_data), intent(inout) :: FEM_MHD
!!        type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(mesh_SR), intent(inout) :: m_SR
!!@endverbatim
!
      module lead_physical_values
!
      use m_precision
      use m_machine_parameter
!
      use t_FEM_MHD_model_data
      use t_SGS_control_parameter
      use t_MHD_step_parameter
      use t_reference_scalar_param
      use t_time_data
      use t_mesh_data
      use t_surface_data
      use t_edge_data
      use t_phys_data
      use t_phys_address
      use t_base_field_labels
      use t_SGS_model_addresses
      use t_table_FEM_const
      use t_MHD_mass_matrices
      use t_FEM_MHD_filter_data
      use t_material_property
      use t_SGS_model_coefs
      use t_FEM_SGS_model_coefs
      use t_work_FEM_SGS_MHD
      use t_mesh_SR
!
      implicit none
!
      private :: cal_energy_fluxes
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine lead_fields_by_FEM(istep, MHD_step, FEM_model,         &
     &          SGS_par, iphys_LES, ak_MHD, FEM_filters, FEM_MHD,       &
     &          SGS_MHD_wk, Csims_FEM_MHD, m_SR)
!
      use update_after_evolution
      use itp_potential_on_edge
      use MHD_field_by_rotation
      use cal_helicities
      use output_viz_file_control
!
      integer(kind = kint), intent(in) :: istep
      type(MHD_step_param), intent(in) :: MHD_step
      type(FEM_MHD_model_data), intent(in) :: FEM_model
      type(SGS_paremeters), intent(in) :: SGS_par
      type(SGS_model_addresses), intent(in) :: iphys_LES
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(filters_on_FEM), intent(in) :: FEM_filters
!
      type(FEM_mesh_field_data), intent(inout) :: FEM_MHD
      type(work_FEM_SGS_MHD), intent(inout) :: SGS_MHD_wk
      type(SGS_coefficients_data), intent(inout) :: Csims_FEM_MHD
      type(mesh_SR), intent(inout) :: m_SR
!
!
      if(lead_field_data_flag(istep, MHD_step) .eqv. .FALSE.) return
!
      if (iflag_debug.gt.0) write(*,*) 'cal_potential_on_edge'
      call cal_potential_on_edge                                        &
     &   (FEM_MHD%geofem%mesh%node, FEM_MHD%geofem%mesh%ele,            &
     &    FEM_MHD%geofem%mesh%edge, FEM_MHD%iphys, FEM_MHD%field)
!
      if (iflag_debug.gt.0) write(*,*) 'update_FEM_fields'
      call update_FEM_fields                                            &
     &   (MHD_step%time_d, FEM_model%FEM_prm, SGS_par, FEM_MHD%geofem,  &
     &    FEM_model%MHD_mesh, FEM_model%FEM_MHD_BCs,                    &
     &    FEM_MHD%iphys, iphys_LES,  &
     &    FEM_filters, SGS_MHD_wk, FEM_MHD%field, Csims_FEM_MHD, m_SR)
!
      call cal_field_by_rotation(MHD_step%time_d%dt, FEM_model%FEM_prm, &
     &   SGS_par%model_p, SGS_par%commute_p,                            &
     &   FEM_MHD%geofem%mesh, FEM_MHD%geofem%group,                     &
     &   FEM_model%MHD_mesh%fluid, FEM_model%MHD_mesh%conduct,          &
     &   FEM_model%MHD_prop%cd_prop,                                    &
     &   FEM_model%FEM_MHD_BCs%nod_bcs, FEM_model%FEM_MHD_BCs%surf_bcs, &
     &   FEM_MHD%iphys%base, SGS_MHD_wk%iphys_ele_base,                 &
     &   SGS_MHD_wk%ele_fld, SGS_MHD_wk%fem_int,                        &
     &   FEM_filters%FEM_elens, Csims_FEM_MHD%diff_coefs,               &
     &   SGS_MHD_wk%mk_MHD, SGS_MHD_wk%mhd_fem_wk, SGS_MHD_wk%rhs_mat,  &
     &   FEM_MHD%field, m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
!
      if (iflag_debug.gt.0) write(*,*) 'cal_helicity'
      call cal_helicity(FEM_MHD%iphys, FEM_MHD%field)
!
      if (iflag_debug.gt.0) write(*,*) 'cal_energy_fluxes'
      call cal_energy_fluxes                                            &
     &   (MHD_step%time_d%dt, FEM_model%FEM_prm, SGS_par,               &
     &    FEM_MHD%geofem, FEM_model%MHD_mesh, FEM_model%MHD_prop,       &
     &    FEM_model%FEM_MHD_BCs, FEM_MHD%iphys, iphys_LES,              &
     &    SGS_MHD_wk%iphys_ele_base, ak_MHD, SGS_MHD_wk%fem_int,        &
     &    FEM_filters%FEM_elens, Csims_FEM_MHD, FEM_filters%filtering,  &
     &    SGS_MHD_wk%mk_MHD, SGS_MHD_wk%FEM_SGS_wk,                     &
     &    SGS_MHD_wk%mhd_fem_wk, SGS_MHD_wk%rhs_mat, FEM_MHD%field,     &
     &    SGS_MHD_wk%ele_fld, m_SR%v_sol, m_SR%SR_sig, m_SR%SR_r)
!
      end subroutine lead_fields_by_FEM
!
! ----------------------------------------------------------------------
!
      subroutine cal_energy_fluxes                                      &
     &        (dt, FEM_prm, SGS_par, geofem, MHD_mesh, MHD_prop,        &
     &         FEM_MHD_BCs, iphys, iphys_LES, iphys_ele_base,           &
     &         ak_MHD, fem_int, FEM_elens, Csims_FEM_MHD, filtering,    &
     &         mk_MHD, FEM_SGS_wk, mhd_fem_wk, rhs_mat,                 &
     &         nod_fld, ele_fld, v_sol, SR_sig, SR_r)
!
      use cal_MHD_forces_4_monitor
      use cal_sgs_4_monitor
      use cal_true_sgs_terms
      use vector_gradients_4_monitor
!
      real(kind = kreal), intent(in) :: dt
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_paremeters), intent(in) :: SGS_par
      type(mesh_data), intent(in) :: geofem
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(FEM_MHD_BC_data), intent(in) :: FEM_MHD_BCs
      type(phys_address), intent(in) :: iphys
      type(SGS_model_addresses), intent(in) :: iphys_LES
      type(base_field_address), intent(in) :: iphys_ele_base
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(finite_element_integration), intent(in) :: fem_int
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(filtering_data_type), intent(in) :: filtering
      type(SGS_coefficients_data), intent(in) :: Csims_FEM_MHD
      type(lumped_mass_mat_layerd), intent(in) :: mk_MHD
!
      type(work_FEM_dynamic_SGS), intent(inout) :: FEM_SGS_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(arrays_finite_element_mat), intent(inout) :: rhs_mat
      type(phys_data), intent(inout) :: nod_fld
      type(phys_data), intent(inout) :: ele_fld
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call cal_true_sgs_terms_pre(dt, FEM_prm, SGS_par, geofem%mesh,    &
     &    geofem%group%surf_grp, MHD_mesh%fluid, MHD_mesh%conduct,      &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    FEM_MHD_BCs%nod_bcs, FEM_MHD_BCs%surf_bcs,                    &
     &    iphys, iphys_LES, iphys_ele_base, ak_MHD, fem_int, FEM_elens, &
     &    Csims_FEM_MHD%diff_coefs, mk_MHD, mhd_fem_wk, rhs_mat,        &
     &    nod_fld, ele_fld, v_sol, SR_sig, SR_r)
!
      call cal_sgs_terms_4_monitor                                      &
     &   (dt, FEM_prm, SGS_par%model_p, SGS_par%filter_p, geofem%mesh,  &
     &    MHD_mesh%fluid, MHD_mesh%conduct, MHD_prop%cd_prop,           &
     &    iphys, iphys_LES, iphys_ele_base, ele_fld,                    &
     &    fem_int%jcs, fem_int%rhs_tbl, FEM_elens,                      &
     &    Csims_FEM_MHD%iphys_elediff_vec, Csims_FEM_MHD%sgs_coefs,     &
     &    filtering, mk_MHD, FEM_SGS_wk%wk_filter, mhd_fem_wk,          &
     &    rhs_mat%fem_wk, rhs_mat%f_l, rhs_mat%f_nl, nod_fld,           &
     &    v_sol, SR_sig, SR_r)
!
      call cal_fluxes_4_monitor(geofem%mesh%node,                       &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop, iphys, nod_fld)
!
      call vect_gradients_4_monitor(dt, FEM_prm, geofem%mesh,           &
     &    MHD_mesh%fluid, iphys, iphys_ele_base, fem_int, mk_MHD,       &
     &    rhs_mat, nod_fld, ele_fld, v_sol, SR_sig, SR_r)
      call cal_forces_4_monitor(dt, FEM_prm, SGS_par, geofem%mesh,      &
     &    MHD_mesh%fluid, MHD_mesh%conduct, geofem%group%surf_grp,      &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    FEM_MHD_BCs%nod_bcs, FEM_MHD_BCs%surf_bcs,                    &
     &    iphys, iphys_LES, iphys_ele_base, ak_MHD, fem_int, FEM_elens, &
     &    Csims_FEM_MHD%diff_coefs, mk_MHD, mhd_fem_wk, rhs_mat,        &
     &    nod_fld, ele_fld, v_sol, SR_sig, SR_r)
      call cal_diff_of_sgs_terms                                        &
     &   (dt, FEM_prm, SGS_par%model_p, SGS_par%commute_p, geofem%mesh, &
     &    geofem%group%surf_grp, MHD_mesh%fluid, MHD_mesh%conduct,      &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    FEM_MHD_BCs%nod_bcs, FEM_MHD_BCs%surf_bcs,                    &
     &    iphys, iphys_LES, iphys_ele_base, ak_MHD, fem_int, FEM_elens, &
     &    Csims_FEM_MHD%diff_coefs, mk_MHD, mhd_fem_wk, rhs_mat,        &
     &    nod_fld, ele_fld, v_sol, SR_sig, SR_r)
!
      call cal_true_sgs_terms_post                                      &
     &   (SGS_par%filter_p, geofem%mesh, iphys%div_forces,              &
     &    iphys_LES%true_SGS, iphys_LES%true_div_SGS, iphys_LES%SGS_wk, &
     &    filtering, FEM_SGS_wk%wk_filter, nod_fld,                     &
     &    v_sol, SR_sig, SR_r)
!
      call cal_work_4_forces(FEM_prm, geofem%mesh,                      &
     &   MHD_prop%fl_prop, MHD_prop%cd_prop, iphys, iphys_LES,          &
     &   fem_int, mk_MHD, mhd_fem_wk, rhs_mat, nod_fld,                 &
     &   v_sol, SR_sig, SR_r)
!
      call cal_work_4_sgs_terms(FEM_prm, geofem%mesh, MHD_mesh%conduct, &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop, iphys, iphys_LES,         &
     &    fem_int%jcs, fem_int%rhs_tbl, mk_MHD, mhd_fem_wk,             &
     &    rhs_mat%fem_wk, rhs_mat%f_nl, nod_fld, v_sol, SR_sig, SR_r)
! 
      end subroutine cal_energy_fluxes
!
!  ---------------------------------------------------------------------
!
      end module lead_physical_values
