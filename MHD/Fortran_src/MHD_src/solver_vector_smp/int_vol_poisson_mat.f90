!int_vol_poisson_mat.f90
!      module int_vol_poisson_mat
!
!     Written by H. Matsui on Oct. 2005
!
!!      subroutine int_vol_poisson_mat11                                &
!!     &         (ele, jac_3d_l, rhs_tbl, MG_mat_tbl,                   &
!!     &          n_int, fem_wk, mat11)
!!
!!      subroutine int_vol_diffuse_mat11                                &
!!     &         (ele, jac_3d, rhs_tbl, MG_mat_tbl,                     &
!!     &          n_int, dt, coef_imp, ak_d, fem_wk, mat11)
!!      subroutine int_vol_diffuse_mat33                                &
!!     &         (ele, jac_3d, rhs_tbl, MG_mat_tbl,                     &
!!     &          n_int, dt, coef_imp, ak_d, fem_wk, mat33)
!
      module int_vol_poisson_mat
!
      use m_precision
!
      use m_machine_parameter
      use m_phys_constants
!
      use t_geometry_data
      use t_table_FEM_const
      use t_jacobians
      use t_finite_element_mat
      use t_solver_djds
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_poisson_mat11                                  &
     &         (ele, jac_3d_l, rhs_tbl, MG_mat_tbl,                     &
     &          n_int, fem_wk, mat11)
!
      use fem_skv_diffusion_type
      use cal_skv_to_ff_smp
      use add_skv1_to_crs_matrix
!
      type(element_data), intent(in) :: ele
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
        call fem_skv_poisson_linear_type(ele%istack_ele_smp,            &
     &      n_int, k2, ele, jac_3d_l, fem_wk%sk6)
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
     &         (ele, jac_3d, rhs_tbl, MG_mat_tbl,                       &
     &          n_int, dt, coef_imp, ak_d, fem_wk, mat11)
!
      use fem_skv_diffusion_type
      use cal_skv_to_ff_smp
      use cal_poisson_matrices
!
      type(element_data), intent(in) :: ele
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
        call fem_skv_poisson_type(ele%istack_ele_smp, n_int, k2,        &
     &      ele, jac_3d, fem_wk%sk6)
        call cal_scalar_diffuse_mat(ele, rhs_tbl, MG_mat_tbl, fem_wk,   &
     &      k2, dt, coef_imp, ak_d, mat11)
      end do
!
      end subroutine int_vol_diffuse_mat11
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_diffuse_mat33                                  &
     &         (ele, jac_3d, rhs_tbl, MG_mat_tbl,                       &
     &          n_int, dt, coef_imp, ak_d, fem_wk, mat33)
!
      use fem_skv_diffusion_type
      use cal_skv_to_ff_smp
      use cal_poisson_matrices
!
      type(element_data), intent(in) :: ele
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
        call fem_skv_poisson_type(ele%istack_ele_smp, n_int, k2,       &
     &      ele, jac_3d, fem_wk%sk6)
        call cal_vect_diffuse_mat(ele, rhs_tbl, MG_mat_tbl, fem_wk,    &
     &      k2, dt, coef_imp, ak_d, mat33)
      end do
!
      end subroutine int_vol_diffuse_mat33
!
!-----------------------------------------------------------------------
!
      end module int_vol_poisson_mat
