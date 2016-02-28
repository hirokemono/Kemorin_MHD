!
!     module set_aiccg_matrices_type
!
!        programmed H.Matsui on Dec., 2008
!
!
!      subroutine s_set_aiccg_matrices_type(femmesh, ele_mesh,          &
!     &          MHD_mesh, nodal_bc, surface_bc, djds_tbl, djds_tbl_fl, &
!     &          djds_tbl_cd, djds_tbl_l, djds_tbl_fl_l,                &
!     &          jacobians, ak_AMG, rhs_tbl, djds_const,                &
!     &          djds_const_fl, djds_const_cd, filter_MHD,              &
!     &          mk_MHD, MG_FEM_mat, mat_velo, mat_magne,               &
!     &          mat_temp, mat_d_scalar ,mat_press, mat_magp)
!        type(mesh_data), intent(in) ::             femmesh
!        type(element_geometry), intent(in) ::      ele_mesh
!        type(mesh_data_MHD), intent(in) ::          MHD_mesh
!        type(nodal_boundarty_conditions), intent(in) ::   nodal_bc
!        type(surface_boundarty_conditions), intent(in) :: surface_bc
!        type(jacobians_type), intent(in) ::         jacobians
!        type(coefs_4_MHD_AMG), intent(in) ::        ak_AMG
!        type(tables_4_FEM_assembles), intent(in) ::   rhs_tbl
!        type(table_mat_const), intent(in) :: djds_const
!        type(table_mat_const), intent(in) :: djds_const_fl
!        type(table_mat_const), intent(in) :: djds_const_cd
!        type(gradient_model_data_type), intent(in) :: filter_MHD
!        type(lumped_mass_mat_layerd), intent(in) ::    mk_MHD
!        type(DJDS_ordering_table),  intent(in) :: djds_tbl
!        type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl
!        type(DJDS_ordering_table),  intent(in) :: djds_tbl_cd
!
!        type(arrays_finite_element_mat), intent(inout) :: MG_FEM_mat
!        type(DJDS_MATRIX),  intent(inout) :: mat_velo
!        type(DJDS_MATRIX),  intent(inout) :: mat_magne
!        type(DJDS_MATRIX),  intent(inout) :: mat_temp
!        type(DJDS_MATRIX),  intent(inout) :: mat_d_scalar
!        type(DJDS_MATRIX),  intent(inout) :: mat_press
!        type(DJDS_MATRIX),  intent(inout) :: mat_magp
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
      subroutine s_set_aiccg_matrices_type(femmesh, ele_mesh,           &
     &          MHD_mesh, nodal_bc, surface_bc, djds_tbl, djds_tbl_fl,  &
     &          djds_tbl_cd, djds_tbl_l, djds_tbl_fl_l,                 &
     &          jacobians, ak_AMG, rhs_tbl, djds_const,                 &
     &          djds_const_fl, djds_const_cd, djds_const_l,             &
     &          djds_const_fl_l, filter_MHD,                            &
     &          mk_MHD, MG_FEM_mat, mat_velo, mat_magne,                &
     &          mat_temp, mat_d_scalar ,mat_press, mat_magp)
!
      use m_control_parameter
      use t_mesh_data
      use t_geometry_data_MHD
      use t_surface_group_geometry
      use t_nodal_bc_data
      use t_surface_bc_data
      use t_coefs_element_4_MHD
      use t_jacobians
      use t_finite_element_mat_MHD
      use t_finite_element_mat
      use t_filter_elength
      use t_solver_djds
!
      use reset_MHD_aiccg_mat_type
      use int_MHD_poisson_mat_type
      use int_vol_lump_crank_type
      use set_aiccg_bc_phys_type
      use int_crank_mat_consist_type
!
      type(mesh_data), intent(in) ::             femmesh
      type(element_geometry), intent(in) ::      ele_mesh
      type(mesh_data_MHD), intent(in) ::          MHD_mesh
      type(nodal_boundarty_conditions), intent(in) ::   nodal_bc
      type(surface_boundarty_conditions), intent(in) :: surface_bc
      type(jacobians_type), intent(in) ::         jacobians
      type(coefs_4_MHD_AMG), intent(in) ::        ak_AMG
      type(tables_4_FEM_assembles), intent(in) ::   rhs_tbl
      type(table_mat_const), intent(in) :: djds_const
      type(table_mat_const), intent(in) :: djds_const_fl
      type(table_mat_const), intent(in) :: djds_const_cd
      type(table_mat_const), intent(in) :: djds_const_l
      type(table_mat_const), intent(in) :: djds_const_fl_l
      type(lumped_mass_mat_layerd), intent(in) ::    mk_MHD
      type(gradient_model_data_type), intent(in) :: filter_MHD
      type(DJDS_ordering_table),  intent(in) :: djds_tbl
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_cd
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_l
      type(DJDS_ordering_table),  intent(in) :: djds_tbl_fl_l
!
      type(arrays_finite_element_mat), intent(inout) :: MG_FEM_mat
      type(DJDS_MATRIX),  intent(inout) :: mat_velo
      type(DJDS_MATRIX),  intent(inout) :: mat_magne
      type(DJDS_MATRIX),  intent(inout) :: mat_temp
      type(DJDS_MATRIX),  intent(inout) :: mat_d_scalar
      type(DJDS_MATRIX),  intent(inout) :: mat_press
      type(DJDS_MATRIX),  intent(inout) :: mat_magp
!
!
      call s_reset_MHD_aiccg_mat_type(femmesh, MHD_mesh,                &
     &    djds_tbl_fl, djds_tbl_cd, djds_tbl_l, djds_tbl_fl_l,          &
     &    mat_velo, mat_magne, mat_temp, mat_d_scalar,                  &
     &    mat_press, mat_magp)
!
      call int_MHD_poisson_matrices_type(femmesh%mesh,                  &
     &    jacobians%jac_3d_l, filter_MHD, ak_AMG, rhs_tbl,              &
     &    djds_const_l, djds_const_fl_l, MG_FEM_mat%fem_wk,             &
     &    mat_press, mat_magp)
!
      if (iflag_scheme .eq. id_Crank_nicolson) then
        call int_vol_crank_mat_lump_type(femmesh%mesh, MHD_mesh,        &
     &      mk_MHD, djds_tbl, djds_tbl_fl,                              &
     &      mat_velo, mat_magne,  mat_temp, mat_d_scalar)
!
        call int_MHD_crank_matrices_type(femmesh%mesh,                  &
     &      jacobians%jac_3d, rhs_tbl, djds_const, djds_const_fl,       &
     &      djds_const_cd, filter_MHD, ak_AMG, MG_FEM_mat%fem_wk,       &
     &      mat_velo, mat_magne, mat_temp, mat_d_scalar)
!
      else if (iflag_scheme .eq. id_Crank_nicolson_cmass) then
        call s_int_crank_mat_consist_type(femmesh%mesh,                 &
     &      jacobians%jac_3d, rhs_tbl, djds_const_fl, djds_const_cd,    &
     &      MG_FEM_mat%fem_wk, mat_velo, mat_magne,                     &
     &      mat_temp, mat_d_scalar)
        call int_MHD_crank_matrices_type(femmesh%mesh,                  &
     &      jacobians%jac_3d, rhs_tbl, djds_const, djds_const_fl,       &
     &      djds_const_cd, filter_MHD, ak_AMG, MG_FEM_mat%fem_wk,       &
     &      mat_velo, mat_magne, mat_temp, mat_d_scalar)
      end if
!
!     set boundary conditions
!
      call s_set_aiccg_bc_phys_type(femmesh, ele_mesh%surf,             &
     &     nodal_bc, surface_bc, jacobians%jac_sf_grp,                  &
     &     ak_AMG, djds_tbl, djds_tbl_fl, djds_tbl_l, djds_tbl_fl_l,    &
     &     MG_FEM_mat%fem_wk, mat_velo, mat_magne, mat_temp,            &
     &     mat_d_scalar, mat_press,  mat_magp)
!
      end subroutine s_set_aiccg_matrices_type
!
! ---------------------------------------------------------------------
!
      end module set_aiccg_matrices_type
