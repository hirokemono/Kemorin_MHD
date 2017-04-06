!
!     module set_aiccg_matrices_type
!
!        programmed H.Matsui on Dec., 2008
!
!
!!      subroutine s_set_aiccg_matrices                                 &
!!     &        (iflag_scheme, dt, FEM_prm, SGS_param, cmt_param,       &
!!     &         mesh, group, ele_mesh, MHD_mesh, nod_bcs, surf_bcs,    &
!!     &         fl_prop, cd_prop, ht_prop, cp_prop, ak_MHD, jacobians, &
!!     &         FEM_elens, ifld_diff, diff_coefs, rhs_tbl,             &
!!     &         djds_tbl, djds_tbl_fl, djds_tbl_l, djds_tbl_fl_l,      &
!!     &         MG_mat_q, MG_mat_fl_q, MG_mat_full_cd_q, MG_mat_linear,&
!!     &         MG_mat_fl_l, mlump_fl, mlump_cd, surf_wk, fem_wk,      &
!!     &         mat_velo, mat_magne, mat_temp, mat_light,              &
!!     &         mat_press, mat_magp)
!!        type(FEM_MHD_paremeters), intent(in) :: FEM_prm
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(element_geometry), intent(in) ::      ele_mesh
!!        type(mesh_data_MHD), intent(in) ::          MHD_mesh
!!        type(nodal_boundarty_conditions), intent(in) ::   nod_bcs
!!        type(surface_boundarty_conditions), intent(in) :: surf_bcs
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(jacobians_type), intent(in) :: jacobians
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(tables_4_FEM_assembles), intent(in) ::   rhs_tbl
!!        type(table_mat_const), intent(in) :: MG_mat_q
!!        type(table_mat_const), intent(in) :: MG_mat_fl_q
!!        type(table_mat_const), intent(in) :: MG_mat_full_cd_q
!!        type(DJDS_ordering_table),  intent(in) :: djds_tbl
!!        type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl
!!        type(lumped_mass_matrices), intent(in) :: mlump_fl, mlump_cd
!!        type(work_surface_element_mat), intent(in) :: surf_wk
!!
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(DJDS_MATRIX),  intent(inout) :: mat_velo
!!        type(DJDS_MATRIX),  intent(inout) :: mat_magne
!!        type(DJDS_MATRIX),  intent(inout) :: mat_temp
!!        type(DJDS_MATRIX),  intent(inout) :: mat_light
!!        type(DJDS_MATRIX),  intent(inout) :: mat_press
!!        type(DJDS_MATRIX),  intent(inout) :: mat_magp
!
      module set_aiccg_matrices_type
!
      use m_precision
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_aiccg_matrices                                   &
     &        (iflag_scheme, dt, FEM_prm, SGS_param, cmt_param,         &
     &         mesh, group, ele_mesh, MHD_mesh, nod_bcs, surf_bcs,      &
     &         fl_prop, cd_prop, ht_prop, cp_prop, ak_MHD, jacobians,   &
     &         FEM_elens, ifld_diff, diff_coefs, rhs_tbl,               &
     &         djds_tbl, djds_tbl_fl, djds_tbl_l, djds_tbl_fl_l,        &
     &         MG_mat_q, MG_mat_fl_q, MG_mat_full_cd_q, MG_mat_linear,  &
     &         MG_mat_fl_l, mlump_fl, mlump_cd, surf_wk, fem_wk,        &
     &         mat_velo, mat_magne, mat_temp, mat_light,                &
     &         mat_press, mat_magp)
!
      use t_FEM_control_parameter
      use t_SGS_control_parameter
      use t_physical_property
      use t_mesh_data
      use t_geometry_data_MHD
      use t_surface_group_geometry
      use t_nodal_bc_data
      use t_MHD_boundary_data
      use t_coefs_element_4_MHD
      use t_jacobians
      use t_finite_element_mat_MHD
      use t_work_FEM_integration
      use t_finite_element_mat
      use t_filter_elength
      use t_solver_djds
      use t_material_property
      use t_SGS_model_coefs
!
      use init_iccg_matrices
      use int_vol_poisson_matrix
      use int_vol_lumped_mat_crank
      use set_aiccg_bc_vectors
      use int_vol_consist_evo_mat
!
      integer(kind=kint), intent(in) :: iflag_scheme
      real(kind = kreal), intent(in) :: dt
!
      type(FEM_MHD_paremeters), intent(in) :: FEM_prm
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(element_geometry), intent(in) :: ele_mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(nodal_boundarty_conditions), intent(in) ::   nod_bcs
      type(surface_boundarty_conditions), intent(in) :: surf_bcs
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(jacobians_type), intent(in) :: jacobians
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(SGS_coefficients_type), intent(in) :: diff_coefs
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: MG_mat_q
      type(table_mat_const), intent(in) :: MG_mat_fl_q
      type(table_mat_const), intent(in) :: MG_mat_full_cd_q
      type(table_mat_const), intent(in) :: MG_mat_linear
      type(table_mat_const), intent(in) :: MG_mat_fl_l
      type(DJDS_ordering_table),  intent(in) :: djds_tbl
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_l
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl_l
      type(lumped_mass_matrices), intent(in) :: mlump_fl, mlump_cd
      type(work_surface_element_mat), intent(in) :: surf_wk
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX),  intent(inout) :: mat_velo
      type(DJDS_MATRIX),  intent(inout) :: mat_magne
      type(DJDS_MATRIX),  intent(inout) :: mat_temp
      type(DJDS_MATRIX),  intent(inout) :: mat_light
      type(DJDS_MATRIX),  intent(inout) :: mat_press
      type(DJDS_MATRIX),  intent(inout) :: mat_magp
!
!
      call reset_MHD_aiccg_matrices                                     &
     &   (mesh%node, mesh%ele, MHD_mesh%fluid,                          &
     &    fl_prop, cd_prop, ht_prop, cp_prop,                           &
     &    djds_tbl, djds_tbl_fl, djds_tbl_l, djds_tbl_fl_l,             &
     &    mat_velo, mat_magne, mat_temp, mat_light,                     &
     &    mat_press, mat_magp)
!
      if (iflag_debug.eq.1) write(*,*) 'matrix assemble'
!
      call int_MHD_poisson_matrices(FEM_prm%npoint_poisson_int,         &
     &    SGS_param%ifilter_final, cmt_param%iflag_c_magne,             &
     &    mesh, fl_prop, cd_prop, jacobians%jac_3d_l,                   &
     &    rhs_tbl, MG_mat_linear, MG_mat_fl_l,                          &
     &    FEM_elens, ifld_diff, diff_coefs, fem_wk,                     &
     &    mat_press, mat_magp)
!
      if (iflag_scheme .eq. id_Crank_nicolson) then
        call int_vol_crank_mat_lump                                     &
     &     (mesh, MHD_mesh%fluid, MHD_mesh%conduct,                     &
     &      fl_prop, cd_prop, ht_prop, cp_prop,                         &
     &      djds_tbl, djds_tbl_fl, mlump_fl, mlump_cd,                  &
     &      mat_velo, mat_magne, mat_temp, mat_light)
!
        call int_MHD_crank_matrices                                     &
     &     (FEM_prm%npoint_t_evo_int, dt, SGS_param%ifilter_final,      &
     &      mesh, fl_prop, cd_prop, ht_prop, cp_prop, ak_MHD,           &
     &      jacobians%jac_3d, rhs_tbl,                                  &
     &      MG_mat_q, MG_mat_fl_q, MG_mat_full_cd_q,                    &
     &      FEM_elens, ifld_diff, diff_coefs, fem_wk,                   &
     &      mat_velo, mat_magne, mat_temp, mat_light)
!
      else if (iflag_scheme .eq. id_Crank_nicolson_cmass) then
        call int_vol_crank_mat_consist(FEM_prm%npoint_t_evo_int,        &
     &      mesh, fl_prop, cd_prop, ht_prop, cp_prop,                   &
     &      jacobians%jac_3d, rhs_tbl, MG_mat_fl_q, MG_mat_full_cd_q,   &
     &      fem_wk, mat_velo, mat_magne, mat_temp, mat_light)
        call int_MHD_crank_matrices                                     &
     &     (FEM_prm%npoint_t_evo_int, dt, SGS_param%ifilter_final,      &
     &      mesh, fl_prop, cd_prop, ht_prop, cp_prop, ak_MHD,           &
     &      jacobians%jac_3d, rhs_tbl, MG_mat_q, MG_mat_fl_q,           &
     &      MG_mat_full_cd_q, FEM_elens, ifld_diff, diff_coefs,         &
     &      fem_wk, mat_velo, mat_magne, mat_temp, mat_light)
      end if
!
!     set boundary conditions
!
      call set_aiccg_bc_phys(FEM_prm%npoint_t_evo_int, dt,              &
     &    mesh%ele, ele_mesh%surf, group%surf_grp,                      &
     &    fl_prop, cd_prop, ht_prop, cp_prop,                           &
     &    jacobians%jac_sf_grp, rhs_tbl, MG_mat_fl_q, nod_bcs,          &
     &    surf_bcs, djds_tbl, djds_tbl_fl, djds_tbl_l, djds_tbl_fl_l,   &
     &    ak_MHD%ak_d_velo, surf_wk, fem_wk, mat_velo, mat_magne,       &
     &    mat_temp, mat_light, mat_press, mat_magp)
!
      end subroutine s_set_aiccg_matrices
!
! ---------------------------------------------------------------------
!
      end module set_aiccg_matrices_type
