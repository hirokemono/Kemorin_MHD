!
!      module int_filter_functions
!
!     Written by H. Matsui on Aug., 2006
!
!!      subroutine int_node_filter_matrix                               &
!!     &         (node, ele, g_FEM, jac_3d, inod, n_int,                &
!!     &          nele_grp, iele_grp, nnod_mat_tbl, inod_mat_tbl,       &
!!     &          nnod_filter_mat)
!!      subroutine int_node_filter_weights                              &
!!     &         (ele, g_FEM, jac_3d, n_int, nele_grp, iele_grp)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!
      module int_filter_functions
!
      use m_precision
!
      use calypso_mpi
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine int_node_filter_matrix                                 &
     &         (node, ele, g_FEM, jac_3d, inod, n_int,                  &
     &          nele_grp, iele_grp, nnod_mat_tbl, inod_mat_tbl,         &
     &          nnod_filter_mat)
!
      use t_geometry_data
      use t_fem_gauss_int_coefs
      use t_jacobians
      use m_matrix_4_filter
      use set_int_point_position
      use fem_const_filter_matrix
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer(kind = kint), intent(in) :: inod, n_int
      integer(kind = kint), intent(in) :: nele_grp
      integer(kind = kint), intent(in) :: iele_grp(nele_grp)
      integer(kind = kint), intent(in) :: nnod_mat_tbl, nnod_filter_mat
      integer(kind = kint), intent(in) :: inod_mat_tbl(nnod_mat_tbl)
!
      integer(kind = kint) :: ii, ix
      integer(kind = kint) :: k_order
!
!
      call set_idx_list_4_filter_mat                                    &
     &   (ele%numele, ele%nnod_4_ele, ele%ie, nele_grp, iele_grp,       &
     &    nnod_mat_tbl, inod_mat_tbl, nnod_filter_mat)
!
      mat_work = 0.0d0
!
      do ii = 1, n_int*n_int*n_int
        ix = int_start3(n_int) + ii
!
        call set_integration_position(node, ele,                        &
     &      nele_grp, iele_grp, jac_3d%an(1:ele%nnod_4_ele,ix),         &
     &      xx_int(1,1), xx_int(1,2), xx_int(1,3) )
!
        do k_order = 1, nnod_filter_mat
!
          call fem_sk_filter_moments                                    &
     &       (node%numnod, ele%numele, ele%nnod_4_ele, node%xx,         &
     &        g_FEM%max_int_point, g_FEM%maxtot_int_3d,                 &
     &        g_FEM%int_start3, g_FEM%owe3d,                            &
     &        jac_3d%ntot_int, jac_3d%xjac, jac_3d%an,                  &
     &        nele_grp, iele_grp, inod, ix, k_order)
!
          call sum_sk_2_filter_mat(ele%nnod_4_ele, nele_grp, k_order)
        end do
      end do
!
      end subroutine int_node_filter_matrix
!
! ----------------------------------------------------------------------
!
      subroutine int_node_filter_weights                                &
     &         (ele, g_FEM, jac_3d, n_int, nele_grp, iele_grp)
!
      use t_geometry_data
      use t_fem_gauss_int_coefs
      use t_jacobians
      use set_int_point_position
      use fem_const_filter_matrix
!
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
!
      integer(kind = kint), intent(in) :: n_int
      integer(kind = kint), intent(in) :: nele_grp
      integer(kind = kint), intent(in) :: iele_grp(nele_grp)
!
!
      call fem_sk_filter_weights(ele%numele, ele%nnod_4_ele,            &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, jac_3d%xjac, jac_3d%an,  &
     &    nele_grp, iele_grp)
!
      call sum_sk_2_filter_weight(ele%nnod_4_ele, nele_grp)
!
      end subroutine int_node_filter_weights
!
! ----------------------------------------------------------------------
!
      end module int_filter_functions
