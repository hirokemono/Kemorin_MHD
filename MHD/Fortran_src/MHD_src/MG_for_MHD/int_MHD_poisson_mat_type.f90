!int_MHD_poisson_mat_type.f90
!      module int_MHD_poisson_mat_type
!
! numerical integration for finite elememt equations(Poisson's equation)
!        programmed by H.Matsui on May, 2009
!
!      subroutine int_MHD_poisson_matrices_type(mesh, jac_3d_l,         &
!     &          filter_MHD, ak_AMG, rhs_tbl, djds_const_l,             &
!     &          djds_const_fl_l, fem_wk, mat_press, mat_magp)
!        type(mesh_geometry), intent(in) :: mesh
!        type(jacobians_3d), intent(in) :: jac_3d_l
!        type(coefs_4_MHD_AMG), intent(in) :: ak_AMG
!        type(gradient_model_data_type), intent(in) :: filter_MHD
!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!        type(table_mat_const), intent(in) :: djds_const_l
!        type(table_mat_const), intent(in) :: djds_const_fl_l
!        type(work_finite_element_mat), intent(inout) :: fem_wk
!        type(DJDS_MATRIX),  intent(inout) :: mat_press
!        type(DJDS_MATRIX),  intent(inout) :: mat_magp
!
!      subroutine int_MHD_crank_matrices_type(mesh, jac_3d, rhs_tbl,    &
!     &          djds_const, djds_const_fl, djds_const_full_cd,         &
!     &          filter_MHD, ak_AMG, fem_wk, mat_velo, mat_magne,       &
!     &          mat_temp, mat_d_scalar)
!        type(mesh_geometry), intent(in) :: mesh
!        type(jacobians_3d), intent(in) :: jac_3d
!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!        type(table_mat_const), intent(in) :: djds_const
!        type(table_mat_const), intent(in) :: djds_const_fl
!        type(table_mat_const), intent(in) :: djds_const_full_cd
!        type(gradient_model_data_type), intent(in) :: filter_MHD
!        type(coefs_4_MHD_AMG), intent(in) :: ak_AMG
!        type(work_finite_element_mat), intent(inout) :: fem_wk
!        type(DJDS_MATRIX),  intent(inout) :: mat_velo
!        type(DJDS_MATRIX),  intent(inout) :: mat_magne
!        type(DJDS_MATRIX),  intent(inout) :: mat_temp
!        type(DJDS_MATRIX),  intent(inout) :: mat_d_scalar
!
      module int_MHD_poisson_mat_type
!
      use m_precision
!
      use m_control_parameter
      use m_geometry_constants
      use m_t_int_parameter
!
      use t_mesh_data
      use t_geometry_data_MHD
      use t_jacobians
      use t_filter_elength
      use t_coefs_element_4_MHD
      use t_table_FEM_const
      use t_finite_element_mat
      use t_solver_djds
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_MHD_poisson_matrices_type(mesh, jac_3d_l,          &
     &          filter_MHD, ak_AMG, rhs_tbl, djds_const_l,              &
     &          djds_const_fl_l, fem_wk, mat_press, mat_magp)
!
      use sel_int_poisson_sgs_mat_t
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d_l
      type(coefs_4_MHD_AMG), intent(in) :: ak_AMG
      type(gradient_model_data_type), intent(in) :: filter_MHD
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: djds_const_l
      type(table_mat_const), intent(in) :: djds_const_fl_l
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX),  intent(inout) :: mat_press
      type(DJDS_MATRIX),  intent(inout) :: mat_magp
!
!
      if ( iflag_t_evo_4_velo .gt. id_no_evolution) then
        call sel_int_poisson_sgs_mat_type(mesh%ele, jac_3d_l,           &
     &      filter_MHD, rhs_tbl, djds_const_fl_l, fem_wk,               &
     &      intg_point_poisson, iflag_commute_velo, ifilter_final,      &
     &      ak_AMG%ak_diff_v, mat_press)
      end if
!
      if ( iflag_t_evo_4_magne .gt. id_no_evolution                     &
     &    .or.  iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call sel_int_poisson_sgs_mat_type(mesh%ele, jac_3d_l,           &
     &      filter_MHD, rhs_tbl, djds_const_l, fem_wk,                  &
     &      intg_point_poisson, iflag_commute_magne, ifilter_final,     &
     &      ak_AMG%ak_diff_b, mat_magp)
      end if
!
      end subroutine int_MHD_poisson_matrices_type
!
! ----------------------------------------------------------------------
!
      subroutine int_MHD_crank_matrices_type(mesh, jac_3d, rhs_tbl,     &
     &          djds_const, djds_const_fl, djds_const_full_cd,          &
     &          filter_MHD, ak_AMG, fem_wk, mat_velo, mat_magne,        &
     &          mat_temp, mat_d_scalar)
!
      use sel_int_poisson_sgs_mat_t
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: djds_const
      type(table_mat_const), intent(in) :: djds_const_fl
      type(table_mat_const), intent(in) :: djds_const_full_cd
      type(gradient_model_data_type), intent(in) :: filter_MHD
      type(coefs_4_MHD_AMG), intent(in) :: ak_AMG
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX),  intent(inout) :: mat_velo
      type(DJDS_MATRIX),  intent(inout) :: mat_magne
      type(DJDS_MATRIX),  intent(inout) :: mat_temp
      type(DJDS_MATRIX),  intent(inout) :: mat_d_scalar
!
!
      if (iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
        call sel_int_diffuse_sgs_mat33_type(mesh%ele, jac_3d,           &
     &      filter_MHD, rhs_tbl, djds_const_fl, fem_wk, coef_imp_v,     &
     &      intg_point_t_evo, iflag_commute_velo, ifilter_final,        &
     &      ak_AMG%ak_diff_v, ak_AMG%ak_d_velo, mat_velo)
      end if
!
      if (iflag_t_evo_4_magne .ge. id_Crank_nicolson) then
        call sel_int_diffuse_sgs_mat33_type(mesh%ele, jac_3d,           &
     &      filter_MHD, rhs_tbl, djds_const_full_cd, fem_wk,            &
     &      coef_imp_b, intg_point_t_evo, iflag_commute_magne,          &
     &      ifilter_final, ak_AMG%ak_diff_b, ak_AMG%ak_d_magne,         &
     &      mat_magne)
      end if
!
      if (iflag_t_evo_4_vect_p .ge. id_Crank_nicolson) then
        call sel_int_diffuse_sgs_mat33_type(mesh%ele, jac_3d,           &
     &      filter_MHD, rhs_tbl, djds_const, fem_wk, coef_imp_b,        &
     &      intg_point_t_evo, iflag_commute_magne, ifilter_final,       &
     &      ak_AMG%ak_diff_b, ak_AMG%ak_d_magne, mat_magne)
      end if
!
      if (iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
        call sel_int_diffuse_sgs_mat11_type(mesh%ele, jac_3d,           &
     &      filter_MHD, rhs_tbl, djds_const_fl, fem_wk, coef_imp_t,     &
     &      intg_point_t_evo, iflag_commute_temp, ifilter_final,        &
     &      ak_AMG%ak_diff_t, ak_AMG%ak_d_temp, mat_temp)
      end if
!
      if (iflag_t_evo_4_composit .ge. id_Crank_nicolson) then
        call sel_int_diffuse_sgs_mat11_type(mesh%ele, jac_3d,           &
     &      filter_MHD, rhs_tbl, djds_const_fl, fem_wk, coef_imp_c,     &
     &      intg_point_t_evo, iflag_commute_composit, ifilter_final,    &
     &      ak_AMG%ak_diff_d, ak_AMG%ak_d_composit, mat_d_scalar)
      end if
!
      end subroutine int_MHD_crank_matrices_type
!
! ----------------------------------------------------------------------
!
      end module int_MHD_poisson_mat_type
