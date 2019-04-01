!
!      module copy_moments_2_matrix
!
!        programmed by H.Matsui on Aug., 2006
!        Modified by H.Matsui on Mar., 2008
!
!!      subroutine set_filter_moments_on_node(inod, ref_m,              &
!!     &          num_fixed_point, max_size, mat_size, vec_mat)
!!        type(reference_moments), intent(in) :: ref_m
!!      subroutine copy_filter_coefs(fil_mat, fil_coef)
!!        type(matrix_4_filter), intent(in) :: fil_mat
!!        type(each_filter_coef), intent(inout) :: fil_coef
!!
!!      subroutine copy_each_filter_coefs(org_coef, new_coef)
!!        type(each_filter_coef), intent(in) :: org_coef
!!        type(each_filter_coef), intent(inout) :: new_coef
!!      subroutine set_failed_filter_coefs(maximum_neighbour, fil_coef)
!!        type(each_filter_coef), intent(inout) :: fil_coef
!
      module copy_moments_2_matrix
!
      use m_precision
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_filter_moments_on_node(inod, ref_m,                &
     &          num_fixed_point, max_size, mat_size, vec_mat)
!
      use t_reference_moments
!
      type(reference_moments), intent(in) :: ref_m
      integer(kind = kint), intent(in) :: num_fixed_point, inod
      integer(kind = kint), intent(in) :: max_size, mat_size
!
      real(kind = kreal), intent(inout) :: vec_mat(max_size)
!
      integer(kind = kint) :: inum
!
!
      if (num_fixed_point.eq. 0) vec_mat = 0.0d0
      do inum = 1, mat_size - num_fixed_point
        vec_mat(inum+num_fixed_point)                                   &
     &     = ref_m%seed_moments_nod(inod,inum)
      end do
!
      end subroutine set_filter_moments_on_node
!
!-----------------------------------------------------------------------
!
      subroutine copy_filter_coefs(fil_mat, fil_coef)
!
      use t_filter_coefs
      use t_matrix_4_filter
!
      type(matrix_4_filter), intent(in) :: fil_mat
      type(each_filter_coef), intent(inout) :: fil_coef
!
      integer(kind = kint) :: i
!
      do i = fil_mat%mat_size+1, fil_coef%nnod_4_1nod_w
        fil_coef%filter_1nod(i) = 0.0d0
      end do
      do i = 1, fil_mat%mat_size
        fil_coef%filter_1nod(i) = fil_mat%x_sol(i)
      end do
!
      end subroutine copy_filter_coefs
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_each_filter_coefs(org_coef, new_coef)
!
      use t_filter_coefs
!
      type(each_filter_coef), intent(in) :: org_coef
      type(each_filter_coef), intent(inout) :: new_coef
!
      new_coef%ilevel_exp_1nod_w =  org_coef%ilevel_exp_1nod_w
      new_coef%nnod_4_1nod_w = org_coef%nnod_4_1nod_w
      new_coef%nele_4_1nod_w = org_coef%nele_4_1nod_w
!
      new_coef%inod_4_1nod_w(1:org_coef%nnod_4_1nod_w)                  &
     &      = org_coef%inod_4_1nod_w(1:org_coef%nnod_4_1nod_w)
      new_coef%idist_from_1nod(1:org_coef%nnod_4_1nod_w)                &
     &      = org_coef%idist_from_1nod(1:org_coef%nnod_4_1nod_w)
      new_coef%iweight_for_1nod(1:org_coef%nnod_4_1nod_w)               &
     &      = org_coef%iweight_for_1nod(1:org_coef%nnod_4_1nod_w)
!
      new_coef%iele_4_1nod_w(1:org_coef%nele_4_1nod_w)                  &
     &      = org_coef%iele_4_1nod_w(1:org_coef%nele_4_1nod_w)
!
      new_coef%weight_1nod(1:org_coef%nnod_4_1nod_w)                    &
     &      = org_coef%weight_1nod(1:org_coef%nnod_4_1nod_w)
      new_coef%filter_1nod(1:org_coef%nnod_4_1nod_w)                    &
     &      = org_coef%filter_1nod(1:org_coef%nnod_4_1nod_w)
!
      end subroutine copy_each_filter_coefs
!
!-----------------------------------------------------------------------
!
      subroutine set_failed_filter_coefs(maximum_neighbour, fil_coef)
!
      use t_filter_coefs
!
      integer(kind = kint), intent(in) :: maximum_neighbour
      type(each_filter_coef), intent(inout) :: fil_coef
!
      fil_coef%ilevel_exp_1nod_w = -maximum_neighbour
      fil_coef%filter_1nod(1:fil_coef%nnod_4_1nod_w) = 0.0e0
      fil_coef%weight_1nod(1) = 1.0e00
      fil_coef%weight_1nod(2:fil_coef%nnod_4_1nod_w) = 0.0e0
!
      end subroutine set_failed_filter_coefs
!
!-----------------------------------------------------------------------
!
      end module copy_moments_2_matrix
