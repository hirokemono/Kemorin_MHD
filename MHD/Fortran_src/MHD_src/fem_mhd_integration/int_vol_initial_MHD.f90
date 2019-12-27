!>@file   int_vol_initial_MHD.f90
!!@brief  module int_vol_initial_MHD
!!
!!@author H. Matsui and H.Okuda 
!!@date Programmed in July 2000 (ver 1.1)
!!      Modified in Oct. 2005
!
!>@brief  Integration of initial fields
!!
!!@verbatim
!!      subroutine int_vol_initial_scalar                               &
!!     &         (num_int, iele_fsmp_stack, i_fld, coef,                &
!!     &          node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl,           &
!!     &          fem_wk, mhd_fem_wk)
!!      subroutine int_vol_initial_vector                               &
!!     &         (num_int, iele_fsmp_stack, i_fld, coef,                &
!!     &          node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl,           &
!!     &          fem_wk, mhd_fem_wk)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_data), intent(in) :: nod_fld
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!@endverbatim
!
      module int_vol_initial_MHD
!
      use m_precision
      use m_machine_parameter
      use m_phys_constants
!
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_fem_gauss_int_coefs
      use t_jacobians
      use t_jacobian_3d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_MHD_finite_element_mat
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_initial_scalar                                 &
     &         (num_int, iele_fsmp_stack, i_fld, coef,                  &
     &          node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl,             &
     &          fem_wk, mhd_fem_wk)
!
      use nodal_fld_2_each_element
      use cal_skv_to_ff_smp
      use fem_skv_nodal_field
!
      integer(kind = kint), intent(in) :: i_fld, num_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: coef
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      integer(kind = kint) :: k2
!
!
      if (coef .eq. 0.0d0) return
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
! -------- loop for shape function for phsical values
      do k2 = 1, ele%nnod_4_ele
        call scalar_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, i_fld, fem_wk%scalar_1)
        call fem_skv_scalar_field                                       &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &      iele_fsmp_stack, g_FEM%max_int_point, g_FEM%maxtot_int_3d,  &
     &      g_FEM%int_start3, g_FEM%owe3d, jac_3d%ntot_int, num_int,    &
     &      k2, jac_3d%xjac, jac_3d%an, jac_3d%an,                      &
     &      fem_wk%scalar_1, fem_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, mhd_fem_wk%ff_m_smp)
!
      end subroutine int_vol_initial_scalar
!
!-----------------------------------------------------------------------
!
      subroutine int_vol_initial_vector                                 &
     &         (num_int, iele_fsmp_stack, i_fld, coef,                  &
     &          node, ele, nod_fld, g_FEM, jac_3d, rhs_tbl,             &
     &          fem_wk, mhd_fem_wk)
!
      use nodal_fld_2_each_element
      use cal_skv_to_ff_smp
      use fem_skv_nodal_field
!
      integer(kind = kint), intent(in) :: i_fld, num_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: coef
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
      integer(kind = kint) :: k2
!
!
      if (coef .eq. 0.0d0) return
      call reset_sk6(n_vector, ele, fem_wk%sk6)
!
! -------- loop for shape function for phsical values
      do k2 = 1, ele%nnod_4_ele
        call vector_phys_2_each_element(node, ele, nod_fld,             &
     &      k2, i_fld, fem_wk%vector_1)
        call fem_skv_vector_field                                       &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &      iele_fsmp_stack, g_FEM%max_int_point, g_FEM%maxtot_int_3d,  &
     &      g_FEM%int_start3, g_FEM%owe3d, jac_3d%ntot_int, num_int,    &
     &      k2, jac_3d%xjac, jac_3d%an, jac_3d%an,                      &
     &      fem_wk%vector_1, fem_wk%sk6)
      end do
!
      call add3_skv_to_ff_v_smp(node, ele, rhs_tbl,                     &
     &    fem_wk%sk6, mhd_fem_wk%ff_m_smp)
!
      end subroutine int_vol_initial_vector
!
!-----------------------------------------------------------------------
!
      end module int_vol_initial_MHD
