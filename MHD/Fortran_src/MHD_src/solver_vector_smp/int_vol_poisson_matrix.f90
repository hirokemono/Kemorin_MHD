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
!!      subroutine int_vol_poisson_matrices(ele, jac_3d_l, rhs_tbl,    &
!!     &          mat_tbl_l, mat_tbl_fl_l, FEM_elens, fem_wk)
!!      subroutine int_vol_crank_matrices(ele, jac_3d, rhs_tbl,        &
!!     &          mat_tbl_q, mat_tbl_fl_q, mat_tbl_full_cd_q,          &
!!     &          FEM_elens, fem_wk)
!
      module int_vol_poisson_matrix
!
      use m_precision
!
      use m_control_parameter
      use m_phys_constants
!
      use t_geometry_data
      use t_jacobian_3d
      use t_table_FEM_const
      use t_filter_elength
      use t_finite_element_mat
      use t_table_FEM_const
!
      implicit none
!
      private :: choose_int_poisson_mat
      private :: choose_int_diffuse3_crank_mat
      private :: choose_int_diffuse1_crank_mat
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_poisson_matrices(ele, jac_3d_l, rhs_tbl,       &
     &          mat_tbl_l, mat_tbl_fl_l, FEM_elens, fem_wk)
!
      use m_phys_constants
      use m_solver_djds_MHD
      use m_SGS_model_coefs
      use m_SGS_address
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const),  intent(in) :: mat_tbl_l
      type(table_mat_const),  intent(in) :: mat_tbl_fl_l
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        call choose_int_poisson_mat(ele, jac_3d_l,                      &
     &      rhs_tbl, mat_tbl_fl_l, FEM_elens, intg_point_poisson,       &
     &      num_diff_kinds, iak_diff_v, ak_diff, fem_wk, Pmat_DJDS)
      end if
!
      if (     iflag_t_evo_4_magne .gt.  id_no_evolution                &
     &    .or. iflag_t_evo_4_vect_p .gt. id_no_evolution) then
        call choose_int_poisson_mat(ele, jac_3d_l,                      &
     &      rhs_tbl, mat_tbl_l, FEM_elens, intg_point_poisson,          &
     &      num_diff_kinds, iak_diff_b, ak_diff, fem_wk, Fmat_DJDS)
      end if
!
      end subroutine int_vol_poisson_matrices
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_crank_matrices(ele, jac_3d, rhs_tbl,           &
     &          mat_tbl_q, mat_tbl_fl_q, mat_tbl_full_cd_q,             &
     &          FEM_elens, fem_wk)
!
      use m_t_int_parameter
      use m_phys_constants
      use m_ele_material_property
      use m_SGS_model_coefs
      use m_SGS_address
      use m_solver_djds_MHD
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const),  intent(in) :: mat_tbl_q
      type(table_mat_const),  intent(in) :: mat_tbl_fl_q
      type(table_mat_const),  intent(in) :: mat_tbl_full_cd_q
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
!
      if (iflag_t_evo_4_velo .ge. id_Crank_nicolson) then
        call choose_int_diffuse3_crank_mat(ele, jac_3d,                 &
     &      rhs_tbl, mat_tbl_fl_q, FEM_elens, intg_point_t_evo,         &
     &      num_diff_kinds, iak_diff_v, ak_diff, coef_imp_v,            &
     &      ak_d_velo, fem_wk, MHD1_matrices%Vmat_MG_DJDS(0))
      end if
!
      if (iflag_t_evo_4_magne .ge. id_Crank_nicolson) then
        call choose_int_diffuse3_crank_mat(ele, jac_3d,                 &
     &      rhs_tbl, mat_tbl_full_cd_q, FEM_elens, intg_point_t_evo,    &
     &      num_diff_kinds, iak_diff_b, ak_diff, coef_imp_b,            &
     &      ak_d_magne, fem_wk, MHD1_matrices%Bmat_MG_DJDS(0))
      end if
!
      if (iflag_t_evo_4_vect_p .ge. id_Crank_nicolson) then
        call choose_int_diffuse3_crank_mat(ele, jac_3d,                 &
     &      rhs_tbl, mat_tbl_q, FEM_elens, intg_point_t_evo,            &
     &      num_diff_kinds, iak_diff_b, ak_diff, coef_imp_b,            &
     &      ak_d_magne, fem_wk, MHD1_matrices%Bmat_MG_DJDS(0))
      end if
!
      if (iflag_t_evo_4_temp .ge. id_Crank_nicolson) then
        call choose_int_diffuse1_crank_mat(ele, jac_3d,                 &
     &      rhs_tbl, mat_tbl_fl_q, FEM_elens, intg_point_t_evo,         &
     &      num_diff_kinds, iak_diff_t, ak_diff, coef_imp_t,            &
     &      ak_d_temp, fem_wk, Tmat_DJDS)
      end if
!
      if (iflag_t_evo_4_composit .ge. id_Crank_nicolson) then
        call choose_int_diffuse1_crank_mat(ele, jac_3d,                 &
     &      rhs_tbl, mat_tbl_fl_q, FEM_elens, intg_point_t_evo,         &
     &      num_diff_kinds, iak_diff_c, ak_diff, coef_imp_c,            &
     &      ak_d_composit, fem_wk, Cmat_DJDS)
      end if
!
      end subroutine int_vol_crank_matrices
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine choose_int_poisson_mat                                 &
     &         (ele, jac_3d_l, rhs_tbl, mat_tbl, FEM_elens, n_int,      &
     &          num_diff_kinds, iak_diff, ak_diff, fem_wk, mat11_DJDS)
!
      use int_vol_poisson_mat
      use int_vol_poisson_sgs_matrix
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: mat_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: n_int
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
     &     (ele, jac_3d_l, rhs_tbl, mat_tbl, FEM_elens, n_int,          &
     &      ifilter_final, ak_diff(1,iak_diff), fem_wk, mat11_DJDS)
      else
        call int_vol_poisson_mat11                                      &
     &     (ele, jac_3d_l, rhs_tbl, mat_tbl, n_int, fem_wk, mat11_DJDS)
      end if
!
      end subroutine choose_int_poisson_mat
!
! ----------------------------------------------------------------------
!
      subroutine choose_int_diffuse3_crank_mat                          &
     &         (ele, jac_3d, rhs_tbl, mat_tbl, FEM_elens,               &
     &          n_int, num_diff_kinds, iak_diff, ak_diff, coef_imp,     &
     &          ak_d, fem_wk, mat33_DJDS)
!
      use int_vol_poisson_mat
      use int_vol_poisson_sgs_matrix
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: mat_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: n_int
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
     &     (ele, jac_3d, rhs_tbl, mat_tbl, FEM_elens,                   &
     &      n_int, coef_imp, ifilter_final, ak_diff(1,iak_diff),        &
     &      ak_d, fem_wk, mat33_DJDS)
      else
        call int_vol_diffuse_mat33(ele, jac_3d, rhs_tbl, mat_tbl,       &
     &      n_int, coef_imp, ak_d, fem_wk, mat33_DJDS)
      end if
!
      end subroutine choose_int_diffuse3_crank_mat
!
! ----------------------------------------------------------------------
!
      subroutine choose_int_diffuse1_crank_mat                          &
     &         (ele, jac_3d, rhs_tbl, mat_tbl, FEM_elens,               &
     &          n_int, num_diff_kinds, iak_diff, ak_diff, coef_imp,     &
     &          ak_d, fem_wk, mat11_DJDS)
!
      use int_vol_poisson_mat
      use int_vol_poisson_sgs_matrix
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: mat_tbl
      type(gradient_model_data_type), intent(in) :: FEM_elens
!
      integer(kind = kint), intent(in) :: n_int
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
     &     (ele, jac_3d, rhs_tbl, mat_tbl, FEM_elens,                   &
     &      n_int, coef_imp, ifilter_final, ak_diff(1,iak_diff),        &
     &      ak_d, fem_wk, mat11_DJDS)
      else
        call int_vol_diffuse_mat11(ele, jac_3d, rhs_tbl, mat_tbl,       &
     &      n_int, coef_imp, ak_d, fem_wk, mat11_DJDS)
      end if
!
      end subroutine choose_int_diffuse1_crank_mat
!
! ----------------------------------------------------------------------
!
      end module int_vol_poisson_matrix
