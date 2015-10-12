!
!      module int_filter_functions
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine int_node_filter_matrix(inod, n_int,                   &
!     &          nele_grp, iele_grp, nnod_mat_tbl, inod_mat_tbl,        &
!     &          nnod_filter_mat)
!      subroutine int_node_filter_weights(n_int, nele_grp, iele_grp)
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
      subroutine int_node_filter_matrix(inod, n_int,                    &
     &          nele_grp, iele_grp, nnod_mat_tbl, inod_mat_tbl,         &
     &          nnod_filter_mat)
!
      use m_geometry_data
      use m_jacobians
      use m_fem_gauss_int_coefs
      use m_matrix_4_filter
      use set_int_point_position
      use fem_const_filter_matrix
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
      call set_idx_list_4_filter_mat(nele_grp, iele_grp,                &
     &      nnod_mat_tbl, inod_mat_tbl, nnod_filter_mat)
!
      mat_work = 0.0d0
!
      do ii = 1, n_int*n_int*n_int
        ix = int_start3(n_int) + ii
!
        call set_integration_position(node1, ele1,                      &
     &      nele_grp, iele_grp, jac1_3d_q%an(1:ele1%nnod_4_ele,ix),     &
     &      xx_int(1,1), xx_int(1,2), xx_int(1,3) )
!
        do k_order = 1, nnod_filter_mat
!
          call fem_sk_filter_moments                                    &
     &       (node1%numnod, ele1%numele, ele1%nnod_4_ele, node1%xx,     &
     &        jac1_3d_q%ntot_int, jac1_3d_q%xjac, jac1_3d_q%an,         &
     &        nele_grp, iele_grp, inod, ix, k_order)
!
          call sum_sk_2_filter_mat(nele_grp, k_order)
        end do
      end do
!
      end subroutine int_node_filter_matrix
!
! ----------------------------------------------------------------------
!
      subroutine int_node_filter_weights(n_int, nele_grp, iele_grp)
!
      use m_geometry_data
      use m_jacobians
      use m_fem_gauss_int_coefs
      use set_int_point_position
      use fem_const_filter_matrix
!
      integer(kind = kint), intent(in) :: n_int
      integer(kind = kint), intent(in) :: nele_grp
      integer(kind = kint), intent(in) :: iele_grp(nele_grp)
!
!
      call fem_sk_filter_weights(ele1%numele, ele1%nnod_4_ele,          &
     &    jac1_3d_q%ntot_int, n_int, jac1_3d_q%xjac, jac1_3d_q%an,      &
     &    nele_grp, iele_grp)
!
      call sum_sk_2_filter_weight(nele_grp)
!
      end subroutine int_node_filter_weights
!
! ----------------------------------------------------------------------
!
      end module int_filter_functions
