!
!     module int_vol_poisson_matrix
!
! numerical integration for finite elememt equations(Poisson's equation)
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2003 (ver 1.1)
!        modifired by H. Matsui on June, 2005
!        modifired by H. Matsui on Nov., 2007
!
!!      subroutine int_MHD_poisson_matrices(mesh, jac_3d_l, rhs_tbl,    &
!!     &          MG_mat_linear, MG_mat_fl_l, FEM_elens,                &
!!     &          ifld_diff, diff_coefs, fem_wk, mat_press, mat_magp)
!!      subroutine int_MHD_crank_matrices(mesh, ak_MHD, jac_3d, rhs_tbl,&
!!     &          MG_mat_q, MG_mat_fl_q, MG_mat_full_cd_q,              &
!!     &          FEM_elens, ifld_diff, diff_coefs, fem_wk,             &
!!     &          mat_velo, mat_magne, mat_temp, mat_light)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(coefs_4_MHD_type), intent(in) :: ak_MHD
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(table_mat_const), intent(in) :: MG_mat_linear
!!        type(table_mat_const), intent(in) :: MG_mat_fl_l
!!        type(table_mat_const), intent(in) :: MG_mat_q
!!        type(table_mat_const), intent(in) :: MG_mat_fl_q
!!        type(table_mat_const), intent(in) :: MG_mat_full_cd_q
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(DJDS_MATRIX),  intent(inout) :: mat_press
!!        type(DJDS_MATRIX),  intent(inout) :: mat_magp
!!        type(DJDS_MATRIX),  intent(inout) :: mat_velo
!!        type(DJDS_MATRIX),  intent(inout) :: mat_magne
!!        type(DJDS_MATRIX),  intent(inout) :: mat_temp
!!        type(DJDS_MATRIX),  intent(inout) :: mat_light
!
      module int_vol_poisson_matrix
!
      use m_precision
!
      use m_control_parameter
      use m_phys_constants
!
      use t_mesh_data
      use t_geometry_data
      use t_jacobians
      use t_table_FEM_const
      use t_filter_elength
      use t_finite_element_mat
      use t_table_FEM_const
      use t_material_property
      use t_SGS_model_coefs
      use t_solver_djds
!
      implicit none
!
      private :: sel_int_poisson_mat
      private :: sel_int_diffuse3_crank_mat
      private :: choose_int_diffuse1_crank_mat
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_MHD_poisson_matrices(mesh, jac_3d_l, rhs_tbl,      &
     &          MG_mat_linear, MG_mat_fl_l, FEM_elens,                  &
     &          ifld_diff, diff_coefs, fem_wk, mat_press, mat_magp)
!
      type(mesh_geometry), intent(in) :: mesh
      type(jacobians_3d), intent(in) :: jac_3d_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const),  intent(in) :: MG_mat_linear
      type(table_mat_const),  intent(in) :: MG_mat_fl_l
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(SGS_coefficients_type), intent(in) :: diff_coefs
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX),  intent(inout) :: mat_press
      type(DJDS_MATRIX),  intent(inout) :: mat_magp
!
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        call sel_int_poisson_mat(mesh%ele, jac_3d_l,                    &
     &      rhs_tbl, MG_mat_fl_l, FEM_elens, intg_point_poisson,        &
     &      diff_coefs%num_field, ifld_diff%i_velo, diff_coefs%ak,      &
     &      ifilter_final, fem_wk, mat_press)
      end if
!
      if (     iflag_t_evo_4_magne .gt.  id_no_evolution                &
     &    .or. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call sel_int_poisson_mat(mesh%ele, jac_3d_l,                    &
     &      rhs_tbl, MG_mat_linear, FEM_elens, intg_point_poisson,      &
     &      diff_coefs%num_field, ifld_diff%i_magne, diff_coefs%ak,     &
     &      ifilter_final, fem_wk, mat_magp)
      end if
!
      end subroutine int_MHD_poisson_matrices
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_MHD_crank_matrices(mesh, ak_MHD, jac_3d, rhs_tbl,  &
     &          MG_mat_q, MG_mat_fl_q, MG_mat_full_cd_q,                &
     &          FEM_elens, ifld_diff, diff_coefs, fem_wk,               &
     &          mat_velo, mat_magne, mat_temp, mat_light)
!
      use m_t_int_parameter
!
      type(mesh_geometry), intent(in) :: mesh
      type(coefs_4_MHD_type), intent(in) :: ak_MHD
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: MG_mat_q
      type(table_mat_const), intent(in) :: MG_mat_fl_q
      type(table_mat_const), intent(in) :: MG_mat_full_cd_q
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(SGS_terms_address), intent(in) :: ifld_diff
      type(SGS_coefficients_type), intent(in) :: diff_coefs
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX),  intent(inout) :: mat_velo
      type(DJDS_MATRIX),  intent(inout) :: mat_magne
      type(DJDS_MATRIX),  intent(inout) :: mat_temp
      type(DJDS_MATRIX),  intent(inout) :: mat_light
!
!
      if (iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
        call sel_int_diffuse3_crank_mat(mesh%ele, jac_3d,               &
     &      rhs_tbl, MG_mat_fl_q, FEM_elens, intg_point_t_evo,          &
     &      diff_coefs%num_field, ifld_diff%i_velo, diff_coefs%ak,      &
     &      coef_imp_v, ak_MHD%ak_d_velo, ifilter_final, fem_wk,        &
     &      mat_velo)
      end if
!
      if (iflag_t_evo_4_magne .ge. id_Crank_nicolson) then
        call sel_int_diffuse3_crank_mat(mesh%ele, jac_3d,               &
     &      rhs_tbl, MG_mat_full_cd_q, FEM_elens, intg_point_t_evo,     &
     &      diff_coefs%num_field, ifld_diff%i_magne, diff_coefs%ak,     &
     &      coef_imp_b, ak_MHD%ak_d_magne, ifilter_final, fem_wk,       &
     &      mat_magne)
      end if
!
      if (iflag_t_evo_4_vect_p .ge. id_Crank_nicolson) then
        call sel_int_diffuse3_crank_mat(mesh%ele, jac_3d,               &
     &      rhs_tbl, MG_mat_q, FEM_elens, intg_point_t_evo,             &
     &      diff_coefs%num_field, ifld_diff%i_magne, diff_coefs%ak,     &
     &      coef_imp_b, ak_MHD%ak_d_magne, ifilter_final, fem_wk,       &
     &      mat_magne)
      end if
!
      if (iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
        call choose_int_diffuse1_crank_mat(mesh%ele, jac_3d,            &
     &      rhs_tbl, MG_mat_fl_q, FEM_elens, intg_point_t_evo,          &
     &      diff_coefs%num_field, ifld_diff%i_temp, diff_coefs%ak,      &
     &      coef_imp_t, ak_MHD%ak_d_temp, ifilter_final, fem_wk,        &
     &      mat_temp)
      end if
!
      if (iflag_t_evo_4_composit .ge. id_Crank_nicolson) then
        call choose_int_diffuse1_crank_mat(mesh%ele, jac_3d,            &
     &      rhs_tbl, MG_mat_fl_q, FEM_elens, intg_point_t_evo,          &
     &      diff_coefs%num_field, ifld_diff%i_light, diff_coefs%ak,     &
     &      coef_imp_c, ak_MHD%ak_d_composit, ifilter_final, fem_wk,    &
     &      mat_light)
      end if
!
      end subroutine int_MHD_crank_matrices
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sel_int_poisson_mat                                    &
     &         (ele, jac_3d_l, rhs_tbl, MG_mat_tbl, FEM_elens, n_int,   &
     &          num_diff_kinds, iak_diff, ak_diff,                      &
     &          i_filter, fem_wk, mat11_DJDS)
!
      use int_vol_poisson_mat
      use int_vol_poisson_sgs_matrix
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d_l
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: MG_mat_tbl
!
      integer(kind = kint), intent(in) :: n_int, i_filter
      integer(kind = kint), intent(in) :: num_diff_kinds, iak_diff
      real(kind = kreal), intent(in)                                    &
     &                    :: ak_diff(ele%numele,num_diff_kinds)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX), intent(inout) :: mat11_DJDS
!
!
      if (iflag_commute_magne .eq. id_SGS_commute_ON) then
        call int_vol_poisson_sgs_mat11                                  &
     &     (ele, jac_3d_l, rhs_tbl, MG_mat_tbl, FEM_elens, n_int,       &
     &      i_filter, ak_diff(1,iak_diff), fem_wk, mat11_DJDS)
      else
        call int_vol_poisson_mat11(ele, jac_3d_l, rhs_tbl, MG_mat_tbl,  &
     &      n_int, fem_wk, mat11_DJDS)
      end if
!
      end subroutine sel_int_poisson_mat
!
! ----------------------------------------------------------------------
!
      subroutine sel_int_diffuse3_crank_mat                             &
     &         (ele, jac_3d, rhs_tbl, MG_mat_tbl, FEM_elens,            &
     &          n_int, num_diff_kinds, iak_diff, ak_diff, coef_imp,     &
     &          ak_d, i_filter, fem_wk, mat33_DJDS)
!
      use int_vol_poisson_mat
      use int_vol_poisson_sgs_matrix
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: MG_mat_tbl
!
      integer(kind = kint), intent(in) :: n_int, i_filter
      integer(kind = kint), intent(in) :: num_diff_kinds, iak_diff
      real(kind = kreal), intent(in)                                    &
     &                    :: ak_diff(ele%numele,num_diff_kinds)
      real(kind = kreal), intent(in) :: coef_imp
      real(kind = kreal), intent(in) :: ak_d(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX), intent(inout) :: mat33_DJDS
!
!
      if(iak_diff .gt. 0) then
        call int_vol_diffuse_sgs_mat33                                  &
     &     (ele, jac_3d, rhs_tbl, MG_mat_tbl, FEM_elens,                &
     &      n_int, coef_imp, i_filter, ak_diff(1,iak_diff),             &
     &      ak_d, fem_wk, mat33_DJDS)
      else
        call int_vol_diffuse_mat33(ele, jac_3d, rhs_tbl, MG_mat_tbl,    &
     &      n_int, coef_imp, ak_d, fem_wk, mat33_DJDS)
      end if
!
      end subroutine sel_int_diffuse3_crank_mat
!
! ----------------------------------------------------------------------
!
      subroutine choose_int_diffuse1_crank_mat                          &
     &         (ele, jac_3d, rhs_tbl, MG_mat_tbl, FEM_elens,            &
     &          n_int, num_diff_kinds, iak_diff, ak_diff, coef_imp,     &
     &          ak_d, i_filter, fem_wk, mat11_DJDS)
!
      use int_vol_poisson_mat
      use int_vol_poisson_sgs_matrix
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: MG_mat_tbl
!
      integer(kind = kint), intent(in) :: n_int, i_filter
      integer(kind = kint), intent(in) :: num_diff_kinds, iak_diff
      real(kind = kreal), intent(in)                                    &
     &                    :: ak_diff(ele%numele,num_diff_kinds)
      real(kind = kreal), intent(in) :: coef_imp
      real(kind = kreal), intent(in) :: ak_d(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX), intent(inout) :: mat11_DJDS
!
!
      if(iak_diff .gt. 0) then
        call int_vol_diffuse_sgs_mat11                                  &
     &     (ele, jac_3d, rhs_tbl, MG_mat_tbl, FEM_elens,                &
     &      n_int, coef_imp, i_filter, ak_diff(1,iak_diff),             &
     &      ak_d, fem_wk, mat11_DJDS)
      else
        call int_vol_diffuse_mat11(ele, jac_3d, rhs_tbl, MG_mat_tbl,    &
     &      n_int, coef_imp, ak_d, fem_wk, mat11_DJDS)
      end if
!
      end subroutine choose_int_diffuse1_crank_mat
!
! ----------------------------------------------------------------------
!
      end module int_vol_poisson_matrix
