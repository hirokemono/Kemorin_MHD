!
!      module ordering_by_filtering_size
!
      module ordering_by_filtering_size
!
!      Written by H. Matsui on Oct., 2006
!      Modified by H. Matsui on Mar., 2008
!
      use m_precision
!
      implicit none
!
      real(kind = kreal), allocatable :: dist_ratio(:)
      private :: dist_ratio
      private :: cal_distance_ratio_2_filter
      private :: cal_distance_from_filter
!
!      subroutine s_ordering_by_filtering_size
!      subroutine cal_distance_from_filter
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine filter_ordering_by_distance(inod)
!
      use m_geometry_parameter
      use m_filter_coefs
      use add_nodes_elems_4_each_nod
!
      integer(kind = kint), intent(in) :: inod
!
!
      call cal_distance_from_filter(inod)
!
      call sort_added_nod_4_each_by_real(numnod,                        &
     &    nnod_near_1nod_filter, nnod_near_1nod_weight,                 &
     &    inod_near_1nod_weight, dist_ratio)
!
      end subroutine filter_ordering_by_distance
!
! ----------------------------------------------------------------------
!
      subroutine filter_ordering_by_dist_ratio(inod)
!
      use m_geometry_parameter
      use m_filter_coefs
      use add_nodes_elems_4_each_nod
!
      integer(kind = kint), intent(in) :: inod
!
!
      call cal_distance_ratio_2_filter(inod)
!
      call sort_added_nod_4_each_by_real(numnod,                        &
     &    nnod_near_1nod_filter, nnod_near_1nod_weight,                 &
     &    inod_near_1nod_weight, dist_ratio)
!
      end subroutine filter_ordering_by_dist_ratio
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_dist_ratio
!
      use m_geometry_parameter
!
      allocate( dist_ratio(numnod) )
      dist_ratio = 0.0d0
!
      end subroutine allocate_dist_ratio
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_dist_ratio
!
      deallocate( dist_ratio )
!
      end subroutine deallocate_dist_ratio
!
! ----------------------------------------------------------------------
!
      subroutine cal_distance_ratio_2_filter(inod)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_filter_coefs
      use m_filter_elength
!
      integer(kind = kint), intent(in) :: inod
      integer(kind = kint) :: inum, jnod
!
!
      do inum = 1, nnod_near_1nod_weight
        jnod = inod_near_1nod_weight(inum)
        dist_ratio(inum)                                                &
     &        = ((xx(jnod,1) - xx(inod,1))**2 / elen_dx2_ele(inod))     &
     &        + ((xx(jnod,2) - xx(inod,2))**2 / elen_dy2_ele(inod))     &
     &        + ((xx(jnod,3) - xx(inod,3))**2 / elen_dz2_ele(inod))
      end do
!
      end subroutine cal_distance_ratio_2_filter
!
! ----------------------------------------------------------------------
!
      subroutine cal_distance_from_filter(inod)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_filter_coefs
      use m_filter_elength
!
      integer(kind = kint), intent(in) :: inod
      integer(kind = kint) :: inum, jnod
!
!
      do inum = 1, nnod_near_1nod_weight
        jnod = inod_near_1nod_weight(inum)
        dist_ratio(inum) = ( (xx(jnod,1) - xx(inod,1))  )**2            &
     &                   + ( (xx(jnod,2) - xx(inod,2))  )**2            &
     &                   + ( (xx(jnod,3) - xx(inod,3))  )**2
      end do
!
      end subroutine cal_distance_from_filter
!
! ----------------------------------------------------------------------
!
      end module ordering_by_filtering_size
