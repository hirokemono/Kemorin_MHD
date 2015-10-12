!>@file   int_vol_poisson_mat_type.f90
!!@brief  module int_vol_poisson_mat_type
!!
!!@author H. Matsui
!!@date        Written by H. Matsui on Jan., 2009
!!@n      modified by H. Matsui on Nov., 2013
!
!>@brief   Matrix assemble for structure
!!
!!@verbatim
!!      subroutine int_vol_poisson_mat11_type(ele, jac_3d_l, rhs_tbl,   &
!!     &          djds_const, fem_wk, n_int, mat11)
!!
!!      subroutine int_vol_diffuse_mat11_type(ele, jac_3d, rhs_tbl,     &
!!     &          djds_const, fem_wk, coef_imp, n_int, ak_d, mat11)
!!      subroutine int_vol_diffuse_mat33_type(ele, jac_3d, rhs_tbl,     &
!!     &          djds_const, fem_wk, coef_imp, n_int, ak_d, mat33)
!!@endverbatim
!
      module int_vol_poisson_mat_type
!
      use m_precision
!
      use m_machine_parameter
      use m_phys_constants
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
      subroutine int_vol_poisson_mat11_type(ele, jac_3d_l, rhs_tbl,     &
     &          djds_const, fem_wk, n_int, mat11)
!
      use fem_skv_diffusion_type
      use cal_skv_to_ff_smp
      use add_skv1_2_matrix_type
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d_l
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: djds_const
!
      integer(kind=kint), intent(in) :: n_int
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX),  intent(inout) :: mat11
!
      integer(kind = kint) :: k2
!
!
      do  k2 = 1, num_t_linear
        call reset_sk6(n_scalar, ele, fem_wk%sk6)
        call fem_skv_poisson_linear_type(ele%istack_ele_smp, n_int, k2, &
     &      ele, jac_3d_l, fem_wk%sk6)
        call add_skv1_2_matrix11_type(ele, rhs_tbl,                     &
     &      djds_const%idx_4_mat, fem_wk%sk6, k2, mat11)
      end do
!
      end subroutine int_vol_poisson_mat11_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine int_vol_diffuse_mat11_type(ele, jac_3d, rhs_tbl,       &
     &          djds_const, fem_wk, coef_imp, n_int, ak_d, mat11)
!
      use fem_skv_diffusion_type
      use cal_skv_to_ff_smp
      use cal_poisson_matrices_type
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: djds_const
!
      integer(kind=kint), intent(in) :: n_int
!
      real(kind=kreal), intent(in) :: coef_imp
      real(kind=kreal), intent(in) :: ak_d(ele%numele)
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
        call cal_scalar_diffuse_mat_type(ele, rhs_tbl, djds_const,      &
     &      fem_wk, k2, coef_imp, ak_d, mat11)
      end do
!
      end subroutine int_vol_diffuse_mat11_type
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_diffuse_mat33_type(ele, jac_3d, rhs_tbl,       &
     &          djds_const, fem_wk, coef_imp, n_int, ak_d, mat33)
!
      use fem_skv_diffusion_type
      use cal_skv_to_ff_smp
      use cal_poisson_matrices_type
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: djds_const
!
      integer(kind=kint), intent(in) :: n_int
!
      real(kind=kreal), intent(in) :: coef_imp
      real(kind=kreal), intent(in) :: ak_d(ele%numele)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX),  intent(inout) :: mat33
!
      integer(kind = kint) :: k2
!
!
      do  k2 = 1, ele%nnod_4_ele
        call reset_sk6(n_scalar, ele, fem_wk%sk6)
        call fem_skv_poisson_type(ele%istack_ele_smp, n_int, k2,        &
     &      ele, jac_3d, fem_wk%sk6)
        call cal_vect_diffuse_mat_type(ele, rhs_tbl, djds_const,        &
     &      fem_wk, k2, coef_imp, ak_d, mat33)
      end do
!
      end subroutine int_vol_diffuse_mat33_type
!
! ----------------------------------------------------------------------
!
      end module int_vol_poisson_mat_type
