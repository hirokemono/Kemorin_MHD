!int_vol_poisson_sgs_matrix.f90
!      module int_vol_poisson_sgs_matrix
!
!     Written by H. Matsui on Oct. 2005
!
!!      subroutine int_vol_poisson_sgs_mat11                            &
!!     &         (ele, jac_3d_l, rhs_tbl, MG_mat_tbl, FEM_elens, n_int, &
!!     &          i_filter, ak_diff, fem_wk, mat11)
!!
!!      subroutine int_vol_diffuse_sgs_mat11                            &
!!     &         (ele, jac_3d, rhs_tbl, MG_mat_tbl, FEM_elens, n_int,   &
!!     &          dt, coef_imp, i_filter, ak_diff, ak_d, fem_wk, mat11)
!!      subroutine int_vol_diffuse_sgs_mat33                            &
!!     &         (ele, jac_3d, rhs_tbl, MG_mat_tbl, FEM_elens, n_int,   &
!!     &          dt, coef_imp, i_filter, ak_diff, ak_d, fem_wk, mat33)
!
      module int_vol_poisson_sgs_matrix
!
      use m_precision
!
      use m_machine_parameter
      use m_phys_constants
      use t_geometry_data
      use t_table_FEM_const
      use t_filter_elength
      use t_solver_djds
      use t_filter_elength
      use t_finite_element_mat
!
      use fem_skv_diffusion_sgs_type
      use cal_skv_to_ff_smp
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_poisson_sgs_mat11                              &
     &         (ele, jac_3d_l, rhs_tbl, MG_mat_tbl, FEM_elens, n_int,   &
     &          i_filter, ak_diff, fem_wk, mat11)
!
      use add_skv1_to_crs_matrix
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d_l
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: MG_mat_tbl
!
      integer(kind = kint), intent(in) :: n_int, i_filter
      real(kind=kreal), intent(in) :: ak_diff(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX),  intent(inout) :: mat11
!
      integer(kind = kint) :: k2
!
!
      do  k2 = 1, ele%nnod_4_ele
        call reset_sk6(n_scalar, ele, fem_wk%sk6)
        call fem_skv_poisson_linear_sgs_type(ele%istack_ele_smp,        &
     &      n_int, k2, i_filter, ak_diff, ele, jac_3d_l, FEM_elens,     &
     &      fem_wk%sk6)
        call add_skv1_to_crs_matrix11(ele, rhs_tbl, MG_mat_tbl,         &
     &      k2, fem_wk%sk6, mat11%num_non0, mat11%aiccg)
      end do
!
      end subroutine int_vol_poisson_sgs_mat11
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_diffuse_sgs_mat11                              &
     &         (ele, jac_3d, rhs_tbl, MG_mat_tbl, FEM_elens, n_int,     &
     &          dt, coef_imp, i_filter, ak_diff, ak_d, fem_wk, mat11)
!
      use cal_poisson_matrices
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: MG_mat_tbl
!
      real(kind = kreal), intent(in) :: dt
      real(kind = kreal), intent(in) :: coef_imp
      real(kind = kreal), intent(in) :: ak_d(ele%numele)
      real(kind = kreal), intent(in) :: ak_diff(ele%numele)
!
      integer(kind = kint), intent(in) :: n_int, i_filter
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX),  intent(inout) :: mat11
!
      integer(kind = kint) :: k2
!
!
      do  k2 = 1, ele%nnod_4_ele
        call reset_sk6(n_scalar, ele, fem_wk%sk6)
        call fem_skv_poisson_sgs_type(ele%istack_ele_smp, n_int, k2,    &
     &      i_filter, ak_diff, ele, jac_3d, FEM_elens, fem_wk%sk6)
        call cal_scalar_diffuse_mat(ele, rhs_tbl, MG_mat_tbl, fem_wk,   &
     &      k2, dt, coef_imp, ak_d, mat11)
      end do
!
      end subroutine int_vol_diffuse_sgs_mat11
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_diffuse_sgs_mat33                              &
     &         (ele, jac_3d, rhs_tbl, MG_mat_tbl, FEM_elens, n_int,     &
     &          dt, coef_imp, i_filter, ak_diff, ak_d, fem_wk, mat33)
!
      use cal_poisson_matrices
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: MG_mat_tbl
!
      real(kind = kreal), intent(in) :: dt
      real(kind = kreal), intent(in) :: coef_imp
      real(kind = kreal), intent(in) :: ak_d(ele%numele)
      real(kind = kreal), intent(in) :: ak_diff(ele%numele)
!
      integer(kind = kint), intent(in) :: n_int, i_filter
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX),  intent(inout) :: mat33
!
      integer(kind = kint) :: k2
!
!
      do  k2 = 1, ele%nnod_4_ele
        call reset_sk6(n_scalar, ele, fem_wk%sk6)
        call fem_skv_poisson_sgs_type(ele%istack_ele_smp, n_int, k2,    &
     &      i_filter, ak_diff, ele, jac_3d, FEM_elens, fem_wk%sk6)
        call cal_vect_diffuse_mat(ele, rhs_tbl, MG_mat_tbl, fem_wk,     &
     &      k2, dt, coef_imp, ak_d, mat33)
      end do
!
      end subroutine int_vol_diffuse_sgs_mat33
!
!-----------------------------------------------------------------------
!
      end module int_vol_poisson_sgs_matrix
