!
!      module copy_moments_2_matrix
!
      module copy_moments_2_matrix
!
!        programmed by H.Matsui on Aug., 2006
!        Modified by H.Matsui on Mar., 2008
!
      use m_precision
!
      implicit none
!
!      subroutine set_filter_moments_on_node(inod, num_fixed_point)
!      subroutine copy_filter_coefs(num_vect)
!
!      subroutine copy_filter_coefs_to_tmp
!      subroutine copy_filter_coefs_from_tmp
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_filter_moments_on_node(inod, num_fixed_point)
!
      use m_reference_moments
      use m_matrix_4_filter
!
      integer(kind = kint), intent(in) :: num_fixed_point, inod
      integer(kind = kint) :: inum
!
!
      if (num_fixed_point.eq. 0) vec_mat = 0.0d0
      do inum = 1, mat_size-num_fixed_point
        vec_mat(inum+num_fixed_point) = seed_moments_nod(inod,inum)
      end do
!
      end subroutine set_filter_moments_on_node
!
!-----------------------------------------------------------------------
!
      subroutine copy_filter_coefs(num_vect)
!
      use m_filter_coefs
      use m_matrix_4_filter
!
      integer(kind = kint), intent(in) :: num_vect
!
      integer(kind = kint) :: i
!
      do i = mat_size+1, num_vect
        filter_1nod(i) = 0.0d0
      end do
      do i = 1, mat_size
        filter_1nod(i) = x_sol(i)
      end do
!
      end subroutine copy_filter_coefs
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_filter_coefs_to_tmp
!
      use m_filter_coefs
!
      i_exp_level_1nod_tmp = i_exp_level_1nod_weight
      nnod_near_1nod_tmp = nnod_near_1nod_weight
      nele_near_1nod_tmp = nele_near_1nod_weight
!
      inod_near_1nod_tmp(1:nnod_near_1nod_weight)                       &
     &      = inod_near_1nod_weight(1:nnod_near_1nod_weight)
      iele_near_1nod_tmp(1:nele_near_1nod_weight)                       &
     &      = iele_near_1nod_weight(1:nele_near_1nod_weight)
!
      weight_tmp(1:nnod_near_1nod_weight)                               &
     &      = weight_1nod(1:nnod_near_1nod_weight)
      filter_tmp(1:nnod_near_1nod_weight)                               &
     &      = filter_1nod(1:nnod_near_1nod_weight)
!
      end subroutine copy_filter_coefs_to_tmp
!
!-----------------------------------------------------------------------
!
      subroutine copy_filter_coefs_from_tmp
!
      use m_filter_coefs
!
      i_exp_level_1nod_weight = i_exp_level_1nod_tmp
      nnod_near_1nod_weight = nnod_near_1nod_tmp
      nele_near_1nod_weight = nele_near_1nod_tmp
!
      inod_near_1nod_weight(1:nnod_near_1nod_weight)                    &
     &      = inod_near_1nod_tmp(1:nnod_near_1nod_weight)
      iele_near_1nod_weight(1:nele_near_1nod_weight)                    &
     &      = iele_near_1nod_tmp(1:nele_near_1nod_weight)
!
      weight_1nod(1:nnod_near_1nod_weight)                              &
     &      = weight_tmp(1:nnod_near_1nod_weight)
      filter_1nod(1:nnod_near_1nod_weight)                              &
     &      = filter_tmp(1:nnod_near_1nod_weight)
!
      end subroutine copy_filter_coefs_from_tmp
!
!-----------------------------------------------------------------------
!
      end module copy_moments_2_matrix
