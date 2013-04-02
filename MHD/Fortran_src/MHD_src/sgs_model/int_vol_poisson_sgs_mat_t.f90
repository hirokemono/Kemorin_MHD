!int_vol_poisson_sgs_mat_t.f90
!      module int_vol_poisson_sgs_mat_t
!
!     Written by H. Matsui on Oct. 2005
!
!      subroutine int_vol_poisson_sgs_mat_type(ele, jac_3d_l, FEM_elens,&
!     &          rhs_tbl, djds_const, fem_wk, n_int, i_filter, ak_diff, &
!     &          mat11)
!
!      subroutine int_vol_diffuse_sgs_mat11_type(ele, jac_3d,           &
!     &          FEM_elens, rhs_tbl, djds_const, fem_wk, coef_imp,      &
!     &          n_int, i_filter, ak_diff, ak_d, mat11)
!      subroutine int_vol_diffuse_sgs_mat33_type(ele, jac_3d,           &
!     &          FEM_elens, rhs_tbl, djds_const, fem_wk, coef_imp,      &
!     &          n_int, i_filter, ak_diff, ak_d, mat33)
!
      module int_vol_poisson_sgs_mat_t
!
      use m_precision
!
      use m_machine_parameter
!
      use t_mesh_data
      use t_jacobians
      use t_finite_element_mat
      use t_table_FEM_const
      use t_solver_djds
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_poisson_sgs_mat_type(ele, jac_3d_l, FEM_elens, &
     &          rhs_tbl, djds_const, fem_wk, n_int, i_filter, ak_diff,  &
     &          mat11)
!
      use fem_skv_diffusion_sgs_type
      use add_skv1_2_matrix_type
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d_l
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: djds_const
!
      integer(kind=kint), intent(in) :: n_int, i_filter
      real (kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX),  intent(inout) :: mat11
!
      integer(kind = kint) :: k2
!
!
      do  k2 = 1, num_t_linear
        call reset_sk6_type(n_scalar, ele%numele, num_t_linear, fem_wk)
        call fem_skv_poisson_linear_sgs_type(ele%istack_ele_smp, n_int, &
     &      k2, i_filter, ak_diff, ele, jac_3d_l, FEM_elens, fem_wk)
        call add_skv1_2_matrix11_type(ele, rhs_tbl, djds_const,         &
     &      fem_wk, k2, mat11)
      end do
!
      end subroutine int_vol_poisson_sgs_mat_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_diffuse_sgs_mat11_type(ele, jac_3d,            &
     &          FEM_elens, rhs_tbl, djds_const, fem_wk, coef_imp,       &
     &          n_int, i_filter, ak_diff, ak_d, mat11)
!
      use fem_skv_diffusion_sgs_type
      use cal_poisson_matrices_type
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: djds_const
!
      integer(kind=kint), intent(in) :: n_int, i_filter
!
      real(kind=kreal), intent(in) :: coef_imp
      real(kind=kreal), intent(in) :: ak_d(ele%numele)
      real (kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX),  intent(inout) :: mat11
!
      integer(kind = kint) :: k2
!
!
      do  k2 = 1, ele%nnod_4_ele
        call reset_sk6_type(n_scalar, ele%numele, ele%nnod_4_ele,       &
     &      fem_wk)
        call fem_skv_poisson_sgs_type(ele%istack_ele_smp, n_int, k2,    &
     &      i_filter, ak_diff, ele, jac_3d, FEM_elens, fem_wk)
        call cal_scalar_diffuse_mat_type(ele, rhs_tbl, djds_const,      &
     &      fem_wk, k2, coef_imp, ak_d, mat11)
      end do
!
      end subroutine int_vol_diffuse_sgs_mat11_type
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_diffuse_sgs_mat33_type(ele, jac_3d,            &
     &          FEM_elens, rhs_tbl, djds_const, fem_wk, coef_imp,       &
     &          n_int, i_filter, ak_diff, ak_d, mat33)
!
      use fem_skv_diffusion_sgs_type
      use cal_poisson_matrices_type
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: djds_const
!
      integer(kind=kint), intent(in) :: n_int, i_filter
!
      real(kind=kreal), intent(in) :: coef_imp
      real(kind=kreal), intent(in) :: ak_d(ele%numele)
      real (kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX),  intent(inout) :: mat33
!
      integer(kind = kint) :: k2
!
!
      do  k2 = 1, ele%nnod_4_ele
        call reset_sk6_type(n_scalar, ele%numele, ele%nnod_4_ele,       &
     &      fem_wk)
        call fem_skv_poisson_sgs_type(ele%istack_ele_smp, n_int, k2,    &
     &      i_filter, ak_diff, ele, jac_3d, FEM_elens, fem_wk)
        call cal_vect_diffuse_mat_type(ele, rhs_tbl, djds_const,        &
     &      fem_wk, k2, coef_imp, ak_d, mat33)
      end do
!
      end subroutine int_vol_diffuse_sgs_mat33_type
!
! ----------------------------------------------------------------------
!
      end module int_vol_poisson_sgs_mat_t
