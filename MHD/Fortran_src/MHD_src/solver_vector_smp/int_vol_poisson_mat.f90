!>@file   int_vol_poisson_mat.f90
!!@brief  module int_vol_poisson_mat
!!
!!@author H. Matsui
!!@date Programmed in Oct. 2005
!
!>@brief  Integration of poisson matrix and diffusion matrix
!!
!!@verbatim
!!      subroutine int_vol_poisson_mat11                                &
!!     &         (ele, g_FEM, jac_3d_l, rhs_tbl, MG_mat_tbl,            &
!!     &          n_int, fem_wk, mat11)
!!
!!      subroutine int_vol_diffuse_mat11                                &
!!     &         (ele, g_FEM, jac_3d, rhs_tbl, MG_mat_tbl,              &
!!     &          n_int, dt, coef_imp, ak_d, fem_wk, mat11)
!!      subroutine int_vol_diffuse_mat33                                &
!!     &         (ele, g_FEM, jac_3d, rhs_tbl, MG_mat_tbl,              &
!!     &          n_int, dt, coef_imp, ak_d, fem_wk, mat33)
!!        type(element_data), intent(in) :: ele
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(table_mat_const), intent(in) :: MG_mat_tbl
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(DJDS_MATRIX),  intent(inout) :: mat33
!!@endverbatim
!
      module int_vol_poisson_mat
!
      use m_precision
      use m_machine_parameter
      use m_geometry_constants
      use m_phys_constants
!
      use t_geometry_data
      use t_table_FEM_const
      use t_jacobians
      use t_finite_element_mat
      use t_fem_gauss_int_coefs
      use t_solver_djds
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_poisson_mat11                                  &
     &         (ele, g_FEM, jac_3d_l, rhs_tbl, MG_mat_tbl,              &
     &          n_int, fem_wk, mat11)
!
      use fem_skv_diffusion
      use cal_skv_to_ff_smp
      use add_skv1_to_crs_matrix
!
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: MG_mat_tbl
!
      integer(kind = kint), intent(in) :: n_int
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX),  intent(inout) :: mat11
!
      integer(kind = kint) :: k2
!
!
      do  k2 = 1, ele%nnod_4_ele
        call reset_sk6(n_scalar, ele, fem_wk%sk6)
        call fem_skv_poisson                                            &
     &    (ele%numele, num_t_linear, num_t_linear,                      &
     &     np_smp, ele%istack_ele_smp, g_FEM%max_int_point,             &
     &     g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,          &
     &     n_int, k2, jac_3d_l%ntot_int, jac_3d_l%xjac, jac_3d_l%dnx,   &
     &     jac_3d_l%dnx, fem_wk%sk6)
!
        call add_skv1_to_crs_matrix11(ele, rhs_tbl, MG_mat_tbl,         &
     &      k2, fem_wk%sk6, mat11%num_non0, mat11%aiccg)
      end do
!
      end subroutine int_vol_poisson_mat11
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_vol_diffuse_mat11                                  &
     &         (ele, g_FEM, jac_3d, rhs_tbl, MG_mat_tbl,                &
     &          n_int, dt, coef_imp, ak_d, fem_wk, mat11)
!
      use fem_skv_diffusion
      use cal_skv_to_ff_smp
      use cal_poisson_matrices
!
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: MG_mat_tbl
!
      real(kind = kreal), intent(in) :: dt
      real(kind=kreal), intent(in) :: coef_imp
      real(kind=kreal), intent(in) :: ak_d(ele%numele)
!
      integer(kind = kint), intent(in) :: n_int
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX),  intent(inout) :: mat11
!
      integer(kind = kint) :: k2
!
!
      do  k2 = 1, ele%nnod_4_ele
        call reset_sk6(n_scalar, ele, fem_wk%sk6)
        call fem_skv_poisson                                            &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &      np_smp, ele%istack_ele_smp, g_FEM%max_int_point,            &
     &      g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,         &
     &      n_int, k2, jac_3d%ntot_int, jac_3d%xjac,                    &
     &      jac_3d%dnx, jac_3d%dnx, fem_wk%sk6)
!
        call cal_scalar_diffuse_mat(ele, rhs_tbl, MG_mat_tbl, fem_wk,   &
     &      k2, dt, coef_imp, ak_d, mat11)
      end do
!
      end subroutine int_vol_diffuse_mat11
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_diffuse_mat33                                  &
     &         (ele, g_FEM, jac_3d, rhs_tbl, MG_mat_tbl,                &
     &          n_int, dt, coef_imp, ak_d, fem_wk, mat33)
!
      use fem_skv_diffusion
      use cal_skv_to_ff_smp
      use cal_poisson_matrices
!
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: MG_mat_tbl
!
      real(kind = kreal), intent(in) :: dt
      real(kind=kreal), intent(in) :: coef_imp
      real(kind=kreal), intent(in) :: ak_d(ele%numele)
!
      integer(kind = kint), intent(in) :: n_int
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX),  intent(inout) :: mat33
!
      integer(kind = kint) :: k2
!
!
      do  k2 = 1, ele%nnod_4_ele
        call reset_sk6(n_scalar, ele, fem_wk%sk6)
        call fem_skv_poisson                                            &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &      np_smp, ele%istack_ele_smp, g_FEM%max_int_point,            &
     &      g_FEM%maxtot_int_3d, g_FEM%int_start3, g_FEM%owe3d,         &
     &      n_int, k2, jac_3d%ntot_int, jac_3d%xjac,                    &
     &      jac_3d%dnx, jac_3d%dnx, fem_wk%sk6)
!
        call cal_vect_diffuse_mat(ele, rhs_tbl, MG_mat_tbl, fem_wk,     &
     &      k2, dt, coef_imp, ak_d, mat33)
      end do
!
      end subroutine int_vol_diffuse_mat33
!
!-----------------------------------------------------------------------
!
      end module int_vol_poisson_mat
