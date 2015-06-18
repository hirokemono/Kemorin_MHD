!cal_simple_filter_each_node.f90
!      module cal_simple_filter_each_node
!
!     Written by H. Matsui on Nov., 2008
!
!      subroutine set_simple_filter_nod_by_nod(inod)
!      subroutine set_simple_fl_filter_nod_by_nod(inod)
!
      module cal_simple_filter_each_node
!
      use m_precision
!
      use m_constants
!
      implicit none
!
      integer(kind = kint), parameter :: num_fixed_point = 0
      private :: num_fixed_point
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_simple_filter_nod_by_nod(inod)
!
      use m_geometry_parameter
      use m_ctl_params_4_gen_filter
      use m_filter_coefs
      use m_matrix_4_filter
!
      use expand_filter_area_4_1node
      use set_simple_filters
      use cal_3d_filter_4_each_node
      use int_filter_functions
      use fem_const_filter_matrix
      use delete_small_weighting
      use write_filters_4_each_node
!
      integer(kind = kint), intent(in) :: inod
      integer(kind = kint) :: i
!
!
      call copy_next_nod_ele_4_each(inod, numnod)
      call resize_matrix_size_gen_filter
!
!   set filter area for tophat filter
      if ( abs(iflag_tgt_filter_type) .eq. 2) then
        do i = 2, maximum_neighbour
          call s_expand_filter_area_4_1node(inod)
          nnod_near_1nod_filter = nnod_near_1nod_weight
          nele_near_1nod_filter = nele_near_1nod_weight
          call resize_matrix_size_gen_filter
        end do
!
!   set filter area for other filters
      else
        do i = 1, maximum_neighbour
          call s_expand_filter_area_4_1node(inod)
          call resize_matrix_size_gen_filter
        end do
      end if
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
      if      ( abs(iflag_tgt_filter_type) .eq. 2) then
        call set_tophat_filter_4_each_nod
      else if ( abs(iflag_tgt_filter_type) .eq. 3) then
        call set_linear_filter_4_each_nod
      else if ( abs(iflag_tgt_filter_type) .eq. 4) then
        call set_gaussian_filter_each_nod(inod)
      end if
!
        call cal_filter_and_coefficients
        call normalize_each_filter_weight
!
        call s_delete_small_weighting
        call write_each_filter_stack_coef(inod)
!
        nnod_near_nod_weight(inod) = nnod_near_1nod_weight
        call cal_filter_moments_each_nod(ione, inod)
!
      end subroutine set_simple_filter_nod_by_nod
!
! -----------------------------------------------------------------------
!
      subroutine set_simple_fl_filter_nod_by_nod(inod)
!
      use m_geometry_parameter
      use m_ctl_params_4_gen_filter
      use m_filter_coefs
      use m_filter_moments
      use m_matrix_4_filter
!
      use expand_filter_area_4_1node
      use set_simple_filters
      use cal_3d_filter_4_each_node
      use int_filter_functions
      use fem_const_filter_matrix
      use delete_small_weighting
      use write_filters_4_each_node
!
      integer(kind = kint), intent(in) :: inod
      integer(kind = kint) :: i
!
!
      call copy_next_nod_ele_4_each(inod, numnod)
!
!    no filtering
!
      if (nnod_near_1nod_weight .eq. 0) then
        call write_each_no_filter_coef(inod)
      else
!
!   set filter area for tophat filter
        if ( abs(iflag_tgt_filter_type) .eq. 2) then
          do i = 2, maximum_neighbour
            call s_expand_filter_area_4_1node(inod)
            nnod_near_1nod_filter = nnod_near_1nod_weight
            nele_near_1nod_filter = nele_near_1nod_weight
          end do
!
!   set filter area for other filters
        else
          do i = 1, maximum_neighbour
            call s_expand_filter_area_4_1node(inod)
          end do
        end if
        mat_size = nnod_near_1nod_weight
!
!    use same filter for fluid area
!
        if (nnod_near_1nod_weight .eq. nnod_near_nod_weight(inod))      &
     &     then
          call write_each_same_filter_coef(inod)
          call copy_moments_each_point                                  &
     &       (inod, mom1%mom_nod(ione)%moms,                            &
     &        inod, mom1%mom_nod(itwo)%moms)
!
!    construct filter for fluid area
!
        else
          call int_node_filter_matrix(inod, num_int_points,             &
     &        nele_near_1nod_weight, iele_near_1nod_weight(1),          &
     &        nnod_near_1nod_weight, inod_near_1nod_weight(1),          &
     &        nnod_near_1nod_filter)
!
          call copy_2_filter_matrix(num_fixed_point)
!
!      set filter function without normalization
!
          if      ( abs(iflag_tgt_filter_type) .eq. 2) then
            call set_tophat_filter_4_each_nod
          else if ( abs(iflag_tgt_filter_type) .eq. 3) then
            call set_linear_filter_4_each_nod
          else if ( abs(iflag_tgt_filter_type) .eq. 4) then
            call set_gaussian_filter_each_nod(inod)
          end if
!
          call cal_filter_and_coefficients
          call normalize_each_filter_weight
!
          call s_delete_small_weighting
          call write_each_filter_stack_coef(inod)
        end if
      end if
!
      end subroutine set_simple_fl_filter_nod_by_nod
!
! -----------------------------------------------------------------------
!
      end module cal_simple_filter_each_node
