!
!      module int_filter_functions
!
!     Written by H. Matsui on Aug., 2006
!
!!      subroutine int_node_filter_matrix(node, ele, g_FEM, jac_3d,     &
!!     &          ref_m, fil_coef, inod, n_int, fil_mat)
!!      subroutine int_node_filter_weights                              &
!!     &         (ele, g_FEM, jac_3d, fil_coef, n_int)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(FEM_gauss_int_coefs), intent(in) :: g_FEM
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(reference_moments), intent(in) :: ref_m
!!        type(each_filter_coef), intent(in) :: fil_coef
!!        type(matrix_4_filter), intent(inout) :: fil_mat
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
      subroutine int_node_filter_matrix(node, ele, g_FEM, jac_3d,       &
     &          ref_m, fil_coef, inod, n_int, fil_mat)
!
      use t_geometry_data
      use t_fem_gauss_int_coefs
      use t_jacobians
      use t_reference_moments
      use t_filter_coefs
      use t_matrix_4_filter
      use set_int_point_position
      use fem_const_filter_matrix
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(reference_moments), intent(in) :: ref_m
      type(each_filter_coef), intent(in) :: fil_coef
!
      integer(kind = kint), intent(in) :: inod, n_int
!
      type(matrix_4_filter), intent(inout) :: fil_mat
!
      integer(kind = kint) :: ii, ix
      integer(kind = kint) :: k_order
!
!
      call set_idx_list_4_filter_mat                                    &
     &   (ele%numele, ele%nnod_4_ele, ele%ie,                           &
     &    fil_coef%nele_4_1nod_w, fil_coef%iele_4_1nod_w,               &
     &    fil_coef%nnod_4_1nod_w, fil_coef%inod_4_1nod_w,               &
     &    fil_coef%nnod_4_1nod_f)
!
      fil_mat%mat_work = 0.0d0
!
      do ii = 1, n_int*n_int*n_int
        ix = g_FEM%int_start3(n_int) + ii
!
        call set_integration_position(node, ele,                        &
     &      fil_coef%nele_4_1nod_w, fil_coef%iele_4_1nod_w,             &
     &      jac_3d%an(1,ix), xx_int(1,1), xx_int(1,2), xx_int(1,3))
!
        do k_order = 1, fil_coef%nnod_4_1nod_f
!
          call fem_sk_filter_moments                                    &
     &       (node%numnod, ele%numele, ele%nnod_4_ele, node%xx,         &
     &        ref_m%num_order_3d, ref_m%iorder_mom_3d,                  &
     &        g_FEM%maxtot_int_3d, g_FEM%owe3d,                         &
     &        jac_3d%ntot_int, jac_3d%xjac, jac_3d%an,                  &
     &        fil_coef%nele_4_1nod_w, fil_coef%iele_4_1nod_w,           &
     &        inod, ix, k_order)
!
          call sum_sk_2_filter_mat                                      &
     &       (ele%nnod_4_ele, fil_coef%nele_4_1nod_w, k_order,          &
     &        fil_mat%max_mat_size, fil_mat%num_work, fil_mat%mat_work)
        end do
      end do
!
      end subroutine int_node_filter_matrix
!
! ----------------------------------------------------------------------
!
      subroutine int_node_filter_weights                                &
     &         (ele, g_FEM, jac_3d, fil_coef, n_int)
!
      use t_geometry_data
      use t_fem_gauss_int_coefs
      use t_jacobians
      use t_filter_coefs
      use set_int_point_position
      use fem_const_filter_matrix
!
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(each_filter_coef), intent(inout) :: fil_coef
!
      integer(kind = kint), intent(in) :: n_int
!
!
      call fem_sk_filter_weights(ele%numele, ele%nnod_4_ele,            &
     &    g_FEM%max_int_point, g_FEM%maxtot_int_3d, g_FEM%int_start3,   &
     &    g_FEM%owe3d, jac_3d%ntot_int, n_int, jac_3d%xjac, jac_3d%an,  &
     &    fil_coef%nele_4_1nod_w, fil_coef%iele_4_1nod_w,               &
     &    fil_coef%nnod, fil_coef%filter_1nod)
!
      call sum_sk_2_filter_weight(ele%nnod_4_ele,                       &
     &    fil_coef%nele_4_1nod_w, fil_coef%nnod, fil_coef%weight_1nod)
!
      end subroutine int_node_filter_weights
!
! ----------------------------------------------------------------------
!
      end module int_filter_functions
