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
      subroutine s_cal_filter_moments_again(inod, mom_nod)
!
      use m_geometry_data
      use m_ctl_params_4_gen_filter
      use m_filter_coefs
      use m_matrix_4_filter
      use m_filter_elength
      use t_filter_moments
!
      use expand_filter_area_4_1node
      use cal_3d_filter_4_each_node
      use int_filter_functions
      use fem_const_filter_matrix
      use set_simple_filters
!
      integer(kind = kint), intent(in) :: inod
      type(nod_mom_diffs_type), intent(inout) :: mom_nod
!
      integer(kind = kint) :: num_fixed_point = 0
      integer(kind = kint) :: i
!
!
      call copy_next_nod_ele_4_each(inod, node1%numnod)
      call resize_matrix_size_gen_filter(ele1%nnod_4_ele)
!
      do i = 1, maximum_neighbour
        call s_expand_filter_area_4_1node(node1%numnod, inod, ele1)
        call resize_matrix_size_gen_filter(ele1%nnod_4_ele)
      end do
      mat_size = nnod_near_1nod_weight
!
!    set nxn matrix
!
      call int_node_filter_matrix(inod, num_int_points,                 &
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
