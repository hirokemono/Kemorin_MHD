!
!      module set_constant_filter_coefs
!
      module set_constant_filter_coefs
!
!     Written by H. Matsui on Nov., 2006
!
      use m_precision
!
      use calypso_mpi
!
      implicit none
!
!      subroutine s_set_constant_filter_coefs(inod, num_fixed)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_set_constant_filter_coefs(inod, num_fixed)
!
      use m_ctl_params_4_gen_filter
      use m_geometry_data
      use m_filter_elength
      use m_filter_coefs
      use m_matrix_4_filter
      use cal_gaussian_at_node
!
      integer(kind = kint), intent(in) :: inod, num_fixed
!
      real(kind = kreal) :: g
      real(kind = kreal), parameter :: one = 1.0d0, zero = 0.0d0
      integer(kind = kint) :: i, j, jnod
!
!
      vec_mat = 0.0d0
      a_mat = 0.0d0
      do i = 1, num_fixed
        j = i
!        j = mat_size-i+1
        jnod = inod_near_1nod_weight(i)
        call s_cal_gaussian_at_node(ref_filter_width(1),                &
     &      xx(jnod,1), xx(jnod,2), xx(jnod,3),                         &
     &      xx(inod,1), xx(inod,2), xx(inod,3),                         &
     &      elen_dx2_nod(inod), elen_dy2_nod(inod), elen_dz2_nod(inod), &
     &      g )
!
        vec_mat(i) = g
        a_mat(i,j) = one
!
      end do
!
      end subroutine s_set_constant_filter_coefs
!
! ----------------------------------------------------------------------
!
      end module set_constant_filter_coefs
