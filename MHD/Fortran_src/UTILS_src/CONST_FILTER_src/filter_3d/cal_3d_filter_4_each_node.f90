!
!      module cal_3d_filter_4_each_node
!
!        programmed by H.Matsui on Mar., 2008
!
!      subroutine const_filter_mat_each_nod(inod, num_fixed_point, ierr)
!      subroutine cal_filter_and_coefficients
!      subroutine cal_rms_filter_coefs(rms_weight, rms_filter)
!
      module cal_3d_filter_4_each_node
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
      subroutine const_filter_mat_each_nod(inod, num_fixed_point, ierr)
!
      use m_matrix_4_filter
      use set_constant_filter_coefs
      use copy_moments_2_matrix
      use fem_const_filter_matrix
      use modify_matrix_and_rhs
!
      integer(kind = kint), intent(in) :: inod
      integer(kind = kint), intent(in) :: num_fixed_point
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 0
!
      if ( num_fixed_point .gt. 0 ) then
        call s_set_constant_filter_coefs(inod, num_fixed_point)
      end if
!
      call set_filter_moments_on_node(inod, num_fixed_point )
      call copy_2_filter_matrix( num_fixed_point )
!      call substitute_fixed_moments( num_fixed_point )
!
      call swap_matrix_by_diag_size(mat_size, max_mat_size,             &
     &         vec_mat, a_mat)
      call check_diagonal(mat_size, max_mat_size, a_mat, ierr)
      if (ierr .eq. 1) return
!
      call scaling_by_diagonal(mat_size, max_mat_size, vec_mat, a_mat)
!
      end subroutine const_filter_mat_each_nod
!
!-----------------------------------------------------------------------
!
      subroutine cal_filter_and_coefficients
!
      use m_ctl_params_4_gen_filter
      use m_filter_coefs
      use copy_moments_2_matrix
      use int_filter_functions
!
!
      weight_1nod = 0.0d0
      call copy_filter_coefs(nnod_near_1nod_weight)
!
      call int_node_filter_weights(num_int_points,                      &
     &    nele_near_1nod_weight, iele_near_1nod_weight(1) )
!
      end subroutine cal_filter_and_coefficients
!
!-----------------------------------------------------------------------
!
      subroutine cal_rms_filter_coefs(rms_weight, ierr2)
!
      use m_filter_coefs
!
      real(kind = kreal), intent(inout) :: rms_weight
      integer(kind = kint), intent(inout) :: ierr2
      integer(kind = kint) :: i
!
!
      rms_weight = 0.0d0
      do i = 1, nnod_near_1nod_weight
        rms_weight = rms_weight + weight_1nod(i)**2
      end do
!
      if (weight_1nod(1) .lt. 0.0d0) then
        ierr2 = 1000
      else
        ierr2 = 0
      end if
!
      if (filter_1nod(1) .le. 0.0d0) then
        ierr2 = -1
      else
        ierr2 = 0
      end if
!
      end subroutine cal_rms_filter_coefs
!
!-----------------------------------------------------------------------
!
      end module cal_3d_filter_4_each_node
