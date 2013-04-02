!cal_filter_moments_again.f90
!      module cal_filter_moments_again
!
!     Written by H. Matsui on Mar., 2008
!
!      subroutine s_cal_filter_moments_again(inod)
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
      subroutine s_cal_filter_moments_again(inod)
!
      use m_geometry_parameter
      use m_ctl_params_4_gen_filter
      use m_filter_coefs
      use m_matrix_4_filter
!
      use expand_filter_area_4_1node
      use cal_3d_filter_4_each_node
      use int_filter_functions
      use fem_const_filter_matrix
      use set_simple_filters
      use m_filter_elength
!
      integer(kind = kint), intent(in) :: inod
      integer(kind = kint) :: num_fixed_point = 0
      integer(kind = kint) :: i
!
!
      call copy_next_nod_ele_4_each(inod, numnod)
      call resize_matrix_size_gen_filter
!
      do i = 1, maximum_neighbour
        call s_expand_filter_area_4_1node(inod)
        call resize_matrix_size_gen_filter
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
        call cal_filter_moments_each_nod(ione, inod)
!
      end subroutine s_cal_filter_moments_again
!
! -----------------------------------------------------------------------
!
      end module cal_filter_moments_again
