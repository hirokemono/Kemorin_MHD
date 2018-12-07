!cal_filter_moments_again.f90
!      module cal_filter_moments_again
!
!     Written by H. Matsui on Mar., 2008
!
!      subroutine s_cal_filter_moments_again(inod, mom_nod)
!
      module cal_filter_moments_again
!
      use m_precision
!
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_cal_filter_moments_again                             &
     &         (node, ele, g_FEM, jac_3d, FEM_elen, inod,               &
     &          ele_4_nod, neib_nod, mom_nod)
!
      use m_ctl_params_4_gen_filter
      use m_filter_coefs
      use m_matrix_4_filter
      use m_crs_matrix_4_filter
!
      use t_filter_elength
      use t_geometry_data
      use t_fem_gauss_int_coefs
      use t_jacobians
      use t_next_node_ele_4_node
      use t_filter_moments
!
      use expand_filter_area_4_1node
      use cal_3d_filter_4_each_node
      use int_filter_functions
      use fem_const_filter_matrix
      use set_simple_filters
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FEM_gauss_int_coefs), intent(in) :: g_FEM
      type(jacobians_3d), intent(in) :: jac_3d
      type(gradient_model_data_type), intent(in) :: FEM_elen
!
      integer(kind = kint), intent(in) :: inod
!
      type(element_around_node), intent(inout) :: ele_4_nod
      type(next_nod_id_4_nod), intent(inout) :: neib_nod
      type(nod_mom_diffs_type), intent(inout) :: mom_nod
!
      integer(kind = kint) :: num_fixed_point = 0
      integer(kind = kint) :: i
!
!
      call copy_next_nod_ele_4_each                                     &
     &   (inod, node%numnod, ele_4_nod, neib_nod)
      call resize_matrix_size_gen_filter(ele%nnod_4_ele,                &
     &    fil_tbl_crs, fil_mat_crs)
!
      do i = 1, maximum_neighbour
        call s_expand_filter_area_4_1node                               &
     &     (inod, node, ele, ele_4_nod, FEM_elen)

        call resize_matrix_size_gen_filter(ele%nnod_4_ele,              &
     &      fil_tbl_crs, fil_mat_crs)
      end do
      mat_size = nnod_near_1nod_weight
!
!    set nxn matrix
!
      call int_node_filter_matrix                                       &
     &   (node, ele, g_FEM, jac_3d, inod, num_int_points,               &
     &    nele_near_1nod_weight, iele_near_1nod_weight(1),              &
     &    nnod_near_1nod_weight, inod_near_1nod_weight(1),              &
     &    nnod_near_1nod_filter)
!
      call copy_2_filter_matrix(num_fixed_point)
!
!      set filter function without normalization
!
      nnod_near_nod_weight(inod) = nnod_near_1nod_weight
      call cal_filter_moms_each_nod_type(inod, mom_nod)
!
      end subroutine s_cal_filter_moments_again
!
! -----------------------------------------------------------------------
!
      end module cal_filter_moments_again
