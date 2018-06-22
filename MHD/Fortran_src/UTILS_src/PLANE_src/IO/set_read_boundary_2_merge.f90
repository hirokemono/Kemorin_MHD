!
!      module set_read_boundary_2_merge
!
!      Written by H. Matsui on Dec., 2006
!
!      subroutine cvt_group_4_overlap(group, ishift)
!      subroutine cvt_surf_grp_4_overlap(ip)
!
      module set_read_boundary_2_merge
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cvt_group_4_overlap(group, ishift)
!
      use t_group_data
!
      integer(kind = kint), intent(in) :: ishift
      type(group_data), intent(inout) :: group
!
      integer(kind = kint) :: i
!
      do i = 1, group%num_item
        group%item_grp(i) = group%item_grp(i) + ishift
      end do
!
      end subroutine cvt_group_4_overlap
!
!  ---------------------------------------------------------------------
!
      subroutine cvt_surf_grp_4_overlap(group, ishift)
!
      use t_group_data
!
      integer(kind = kint), intent(in) :: ishift
      type(surface_group_data), intent(inout) :: group
!
      integer(kind = kint) :: i
!
!
      do i = 1, group%num_item
        group%item_sf_grp(1,i) = group%item_sf_grp(1,i) + ishift
      end do
!
      end subroutine cvt_surf_grp_4_overlap
!
!  ---------------------------------------------------------------------
!
      end module set_read_boundary_2_merge
