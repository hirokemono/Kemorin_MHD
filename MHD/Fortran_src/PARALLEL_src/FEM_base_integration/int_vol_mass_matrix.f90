!>@file   int_vol_mass_matrix.f90
!!@brief  module int_vol_mass_matrix
!!
!!@author H. Matsui and H.Okuda 
!!@date Programmed in July 2000 (ver 1.1)
!!      Modified in Oct. 2005
!
!>@brief  Integration of mass matrix over domain
!!
!!@verbatim
!!      subroutine int_lumped_mass_matrix                               &
!!     &         (node, ele, g_FEM, jac_3d, rhs_tbl,                    &
!!     &          num_int, fem_wk, rhs_l, m_lump)
!!
!!      subroutine int_lump_mass_matrix_linear                          &
!!     &         (node, ele, g_FEM, jac_3d, rhs_tbl, num_int,           &
!!     &          fem_wk, rhs_l, m_lump)
!!      subroutine int_lump_mass_matrix_quad                            &
!!     &         (node, ele, g_FEM, jac_3d_q, rhs_tbl, num_int,         &
!!     &          fem_wk, rhs_l, m_lump)
!!
!!      subroutine int_consist_mass_matrix                              &
!!     &         (ele, g_FEM, jac_3d, rhs_tbl, mat_tbl,                 &
!!     &          iele_fsmp_stack, num_int, fem_wk, nmat_size, aiccg)
!!
!!      subroutine int_mass_matrix(node, ele, g_FEM, jac_3d, rhs_tbl,   &
!!     &          iele_fsmp_stack, num_int, fem_wk, rhs_l, m_lump)
!!      subroutine int_mass_matrix_diag                                 &
!!     &         (node, ele, g_FEM, jac_3d, rhs_tbl,                    &
!!     &          iele_fsmp_stack, num_int, fem_wk, rhs_l, m_lump)
!!      subroutine int_mass_matrix_HRZ                                  &
!!     &         (node, ele, g_FEM, jac_3d_q, rhs_tbl,                  &
!!     &          iele_fsmp_stack, num_int, fem_wk, rhs_l, m_lump)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d_q
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(table_mat_const), intent(in) :: mat_tbl
!!
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(finite_ele_mat_node), intent(inout) :: rhs_l
!!        type(lumped_mass_matrices), intent(inout) :: m_lump
!!@endverbatim
!
      module int_vol_mass_matrix
!
      use m_precision
      use m_constants
      use m_phys_constants
      use m_machine_parameter
!
      use t_geometry_data
      use t_fem_gauss_int_coefs
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
!
      use cal_ff_smp_to_ffs
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_lumped_mass_matrix                                 &
     &         (node, ele, g_FEM, jac_3d, rhs_tbl,                      &
     &          num_int, fem_wk, rhs_l, m_lump)
!
      use m_geometry_constants
!
      integer (kind=kint), intent(in) :: num_int
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: rhs_l
      type(lumped_mass_matrices), intent(inout) :: m_lump
!
!
      if     (ele%nnod_4_ele.eq.num_t_quad                              &
     &   .or. ele%nnod_4_ele.eq.num_t_lag) then
        call int_lump_mass_matrix_quad                                  &
     &     (node, ele, g_FEM, jac_3d, rhs_tbl,                          &
     &      num_int, fem_wk, rhs_l, m_lump)
      else
        call int_lump_mass_matrix_linear                                &
     &     (node, ele, g_FEM, jac_3d, rhs_tbl,                          &
     &      num_int, fem_wk, rhs_l, m_lump)
      end if
!
      end subroutine int_lumped_mass_matrix
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_lump_mass_matrix_linear                            &
     &         (node, ele, g_FEM, jac_3d, rhs_tbl, num_int,             &
     &          fem_wk, rhs_l, m_lump)
!
      integer (kind=kint), intent(in) :: num_int
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: rhs_l
      type(lumped_mass_matrices), intent(inout) :: m_lump
!
!
      call int_mass_matrix_diag(node, ele, g_FEM, jac_3d, rhs_tbl,      &
     &    ele%istack_ele_smp, num_int, fem_wk, rhs_l, m_lump)
!
      end subroutine int_lump_mass_matrix_linear
!
!-----------------------------------------------------------------------
!
      subroutine int_lump_mass_matrix_quad                              &
     &         (node, ele, g_FEM, jac_3d_q, rhs_tbl, num_int,           &
     &          fem_wk, rhs_l, m_lump)
!
      use fem_skv_mass_mat
      use cal_skv_to_ff_smp
!
      integer (kind=kint), intent(in) :: num_int
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: rhs_l
      type(lumped_mass_matrices), intent(inout) :: m_lump
!
!
      call reset_ff_smp(node%max_nod_smp, rhs_l)
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
      call fem_skv_mass_mat_diag_HRZ                                    &
     &   (ele%numele, ele%nnod_4_ele, np_smp, ele%istack_ele_smp,       &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d_q%ntot_int, num_int, jac_3d_q%xjac,       &
     &    jac_3d_q%an, fem_wk%sk6)
      call sum_skv_diagonal_4_HRZ(ele%numele, ele%nnod_4_ele, np_smp,   &
     &    ele%istack_ele_smp, fem_wk%sk6, fem_wk%me_diag)
!
      call volume_average_skv_HRZ                                       &
     &   (ele%numele, ele%nnod_4_ele, np_smp, ele%istack_ele_smp,       &
     &    ele%volume_ele, fem_wk%sk6, fem_wk%me_diag)
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, rhs_l%ff_smp)
!
      call cal_ff_smp_2_ml                                              &
     &   (node, rhs_tbl, rhs_l%ff_smp, m_lump%ml, m_lump%ml_o)
!
      end subroutine int_lump_mass_matrix_quad
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_consist_mass_matrix                                &
     &         (ele, g_FEM, jac_3d, rhs_tbl, mat_tbl,                   &
     &          iele_fsmp_stack, num_int, fem_wk, nmat_size, aiccg)
!
      use fem_skv_mass_mat
      use cal_skv_to_ff_smp
      use add_skv1_to_crs_matrix
!
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: mat_tbl
!
      integer (kind=kint), intent(in) :: num_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      integer (kind = kint), intent(in) :: nmat_size
      real(kind=kreal), intent(inout) :: aiccg(0:nmat_size)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
      integer (kind = kint) :: k2
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, ele%nnod_4_ele
        call reset_sk6(n_scalar, ele, fem_wk%sk6)
        call fem_skv_mass_matrix                                        &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &      np_smp, iele_fsmp_stack,                                    &
     &      g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3, &
     &      g_FEM%owe3d, jac_3d%ntot_int, num_int, jac_3d%xjac,         &
     &      jac_3d%an, jac_3d%an, k2, fem_wk%sk6)
        call add_skv1_to_crs_matrix11(ele, rhs_tbl, mat_tbl,            &
     &      k2, fem_wk%sk6, nmat_size, aiccg)
      end do
!
      end subroutine int_consist_mass_matrix
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine int_mass_matrix(node, ele, g_FEM, jac_3d, rhs_tbl,     &
     &          iele_fsmp_stack, num_int, fem_wk, rhs_l, m_lump)
!
      use fem_skv_mass_mat
      use cal_skv_to_ff_smp
!
      integer (kind=kint), intent(in) :: num_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: rhs_l
      type(lumped_mass_matrices), intent(inout) :: m_lump
!
      integer (kind = kint) :: k2
!
!  ----------  clear the vector and lumped mass matrix
!
      call reset_ff_smp(node%max_nod_smp, rhs_l)
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
! -------- loop for shape function for the phsical values
!
      do k2 = 1, ele%nnod_4_ele
        call fem_skv_mass_matrix                                        &
     &     (ele%numele, ele%nnod_4_ele, ele%nnod_4_ele,                 &
     &      np_smp, iele_fsmp_stack,                                    &
     &      g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3, &
     &      g_FEM%owe3d, jac_3d%ntot_int, num_int, jac_3d%xjac,         &
     &      jac_3d%an, jac_3d%an, k2, fem_wk%sk6)
      end do
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, rhs_l%ff_smp)
      call cal_ff_smp_2_ml                                              &
     &   (node, rhs_tbl, rhs_l%ff_smp, m_lump%ml, m_lump%ml_o)
!
      end subroutine int_mass_matrix
!
!-----------------------------------------------------------------------
!
      subroutine int_mass_matrix_diag                                   &
     &         (node, ele, g_FEM, jac_3d, rhs_tbl,                      &
     &          iele_fsmp_stack, num_int, fem_wk, rhs_l, m_lump)
!
      use fem_skv_mass_mat
      use cal_skv_to_ff_smp
!
      integer (kind=kint), intent(in) :: num_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: rhs_l
      type(lumped_mass_matrices), intent(inout) :: m_lump
!
!
      call reset_ff_smp(node%max_nod_smp, rhs_l)
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
      call fem_skv_mass_matrix_diag                                     &
     &   (ele%numele, ele%nnod_4_ele, np_smp, iele_fsmp_stack,          &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, num_int, jac_3d%xjac,           &
     &    jac_3d%an, fem_wk%sk6)
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, rhs_l%ff_smp)
      call cal_ff_smp_2_ml                                              &
     &   (node, rhs_tbl, rhs_l%ff_smp, m_lump%ml, m_lump%ml_o)
!
      end subroutine int_mass_matrix_diag
!
!-----------------------------------------------------------------------
!
      subroutine int_mass_matrix_HRZ                                    &
     &         (node, ele, g_FEM, jac_3d_q, rhs_tbl,                    &
     &          iele_fsmp_stack, num_int, fem_wk, rhs_l, m_lump)
!
      use fem_skv_mass_mat
      use cal_skv_to_ff_smp
!
      integer (kind=kint), intent(in) :: num_int
      integer (kind=kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d_q
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: rhs_l
      type(lumped_mass_matrices), intent(inout) :: m_lump
!
!
      call reset_ff_smp(node%max_nod_smp, rhs_l)
      call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
      call fem_skv_mass_mat_diag_HRZ                                    &
     &   (ele%numele, ele%nnod_4_ele, np_smp, iele_fsmp_stack,          &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d_q%ntot_int, num_int, jac_3d_q%xjac,       &
     &    jac_3d_q%an, fem_wk%sk6)
      call volume_average_skv_HRZ(ele%numele, ele%nnod_4_ele, np_smp,   &
     &    iele_fsmp_stack, ele%volume_ele, fem_wk%sk6, fem_wk%me_diag)
!
      call add1_skv_to_ff_v_smp                                         &
     &   (node, ele, rhs_tbl, fem_wk%sk6, rhs_l%ff_smp)
      call cal_ff_smp_2_ml                                              &
     &   (node, rhs_tbl, rhs_l%ff_smp, m_lump%ml, m_lump%ml_o)
!
      end subroutine int_mass_matrix_HRZ
!
!-----------------------------------------------------------------------
!
      end module int_vol_mass_matrix
