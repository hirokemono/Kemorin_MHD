!sel_int_poisson_sgs_mat_t.f90
!      module sel_int_poisson_sgs_mat_t
!
!     Written by H. Matsui on Oct. 2005
!
!      subroutine sel_int_poisson_sgs_mat_type(ele, jac_3d_l, FEM_elens,&
!     &          rhs_tbl, djds_const, fem_wk, n_int, iflag_commute,     &
!     &          i_filter, ak_diff, mat11)
!
!      subroutine sel_int_diffuse_sgs_mat11_type(ele, jac_3d,           &
!     &          FEM_elens, rhs_tbl, djds_const, fem_wk, coef_imp,      &
!     &          n_int, iflag_commute, i_filter, ak_diff, ak_d, mat11)
!      subroutine sel_int_diffuse_sgs_mat33_type(ele, jac_3d,           &
!     &          FEM_elens, rhs_tbl, djds_const, fem_wk, coef_imp,      &
!     &          n_int, iflag_commute, i_filter, ak_diff, ak_d, mat33)
!
      module sel_int_poisson_sgs_mat_t
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
!
      use t_mesh_data
      use t_jacobians
      use t_finite_element_mat
      use t_table_FEM_const
      use t_filter_elength
      use t_solver_djds
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_int_poisson_sgs_mat_type(ele, jac_3d_l, FEM_elens, &
     &          rhs_tbl, djds_const, fem_wk, n_int, iflag_commute,      &
     &          i_filter, ak_diff, mat11)
!
      use int_vol_poisson_mat
      use int_vol_poisson_sgs_matrix
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d_l
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: djds_const
!
      integer(kind=kint), intent(in) :: n_int, i_filter, iflag_commute
      real (kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX),  intent(inout) :: mat11
!
!
      if(iflag_commute .eq. id_SGS_commute_ON) then
        call int_vol_poisson_sgs_mat11                                  &
     &     (ele, jac_3d_l, rhs_tbl, djds_const, FEM_elens,              &
     &      n_int, i_filter, ak_diff, fem_wk, mat11)
      else
        call int_vol_poisson_mat11(ele, jac_3d_l, rhs_tbl, djds_const,  &
     &      n_int, fem_wk, mat11)
      end if
!
      end subroutine sel_int_poisson_sgs_mat_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sel_int_diffuse_sgs_mat11_type(ele, jac_3d,            &
     &          FEM_elens, rhs_tbl, djds_const, fem_wk, coef_imp,       &
     &          n_int, iflag_commute, i_filter, ak_diff, ak_d, mat11)
!
      use int_vol_poisson_mat
      use int_vol_poisson_sgs_matrix
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: djds_const
!
      integer(kind=kint), intent(in) :: n_int, i_filter, iflag_commute
!
      real(kind=kreal), intent(in) :: coef_imp
      real(kind=kreal), intent(in) :: ak_d(ele%numele)
      real (kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX),  intent(inout) :: mat11
!
!
      if(iflag_commute .eq. id_SGS_commute_ON) then
        call int_vol_diffuse_sgs_mat11                                  &
     &     (ele, jac_3d, rhs_tbl, djds_const, FEM_elens,                &
     &      n_int, coef_imp, i_filter, ak_diff, ak_d, fem_wk, mat11)
      else
        call int_vol_diffuse_mat11(ele, jac_3d, rhs_tbl, djds_const,    &
     &      n_int, coef_imp, ak_d, fem_wk, mat11)
      end if
!
      end subroutine sel_int_diffuse_sgs_mat11_type
!
! ----------------------------------------------------------------------
!
      subroutine sel_int_diffuse_sgs_mat33_type(ele, jac_3d,            &
     &          FEM_elens, rhs_tbl, djds_const, fem_wk, coef_imp,       &
     &          n_int, iflag_commute, i_filter, ak_diff, ak_d, mat33)
!
      use int_vol_poisson_mat
      use int_vol_poisson_sgs_matrix
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: djds_const
!
      integer(kind=kint), intent(in) :: n_int, i_filter, iflag_commute
!
      real(kind=kreal), intent(in) :: coef_imp
      real(kind=kreal), intent(in) :: ak_d(ele%numele)
      real (kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX),  intent(inout) :: mat33
!
!
      if(iflag_commute .eq. id_SGS_commute_ON) then
        call int_vol_diffuse_sgs_mat33                                  &
     &     (ele, jac_3d, rhs_tbl, djds_const, FEM_elens,                &
     &      n_int, coef_imp, i_filter, ak_diff, ak_d, fem_wk, mat33)
      else
        call int_vol_diffuse_mat33(ele, jac_3d, rhs_tbl, djds_const,    &
     &      n_int, coef_imp, ak_d, fem_wk, mat33)
      end if
!
      end subroutine sel_int_diffuse_sgs_mat33_type
!
! ----------------------------------------------------------------------
!
      end module sel_int_poisson_sgs_mat_t
