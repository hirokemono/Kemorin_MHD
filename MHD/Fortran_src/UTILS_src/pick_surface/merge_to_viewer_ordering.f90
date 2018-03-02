!
!      module merge_to_viewer_ordering
!
!      Written by Kemorin in Jan., 2007
!
!!      subroutine allocate_nod_cvt_table_viewer(merged)
!!        type(mesh_geometry), intent(in) :: merged
!!      subroutine deallocate_nod_cvt_table_viewer
!!
!!      subroutine set_node_cvt_table_viewer(merged, imark_node)
!!        type(mesh_geometry), intent(in) :: merged
!!      subroutine renumber_surf_connect_4_viewer(nnod_4_surf)
!!      subroutine set_node_position_4_viewer(merged)
!!        type(mesh_geometry), intent(in) :: merged
!!      subroutine set_node_group_item_viewer(merged_grp, nod_nod_grp)
!!        type(mesh_groups), intent(in) :: merged_grp
!!        type(viewer_group_data), intent(inout) :: nod_nod_grp
!
      module merge_to_viewer_ordering
!
      use m_precision
      use t_surface_data
!
      implicit none
!
!
      integer(kind = kint), allocatable :: inod_merge2viewer(:)
      integer(kind = kint), allocatable :: inod_viewer2merge(:)
!
      private :: inod_merge2viewer, inod_viewer2merge
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_nod_cvt_table_viewer(merged)
!
      use t_mesh_data
      use m_surface_mesh_4_merge
!
      type(mesh_geometry), intent(in) :: merged
!
      allocate( inod_merge2viewer(merged%node%numnod) )
      allocate( inod_viewer2merge(nodpetot_viewer) )
      inod_merge2viewer = 0
      inod_viewer2merge = 0
!
      end subroutine allocate_nod_cvt_table_viewer
!
!------------------------------------------------------------------
!
      subroutine deallocate_nod_cvt_table_viewer
!
      deallocate( inod_merge2viewer )
      deallocate( inod_viewer2merge )
!
      end subroutine deallocate_nod_cvt_table_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_node_cvt_table_viewer(merged, imark_node)
!
      use t_mesh_data
!
      type(mesh_geometry), intent(in) :: merged
      integer(kind = kint), intent(in)                                  &
     &                     :: imark_node(merged%node%numnod)
!
      integer(kind = kint) :: inod, inum
!
!
      inum = 0
      do inod = 1, merged%node%numnod
        if ( imark_node(inod) .gt. 0 ) then
          inum = inum + 1
          inod_merge2viewer(inod) = inum
          inod_viewer2merge(inum) = inod
        end if
      end do
!
      end subroutine set_node_cvt_table_viewer
!
!------------------------------------------------------------------
!
      subroutine renumber_surf_connect_4_viewer(nnod_4_surf)
!
      use m_surface_mesh_4_merge
!
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint) :: isurf, k1, inod
!
!
      do isurf = 1, surfpetot_viewer
        do k1 = 1, nnod_4_surf
          inod = ie_sf_viewer(isurf,k1)
          ie_sf_viewer(isurf,k1) = inod_merge2viewer(inod)
        end do
      end do
!
      end subroutine renumber_surf_connect_4_viewer
!
!------------------------------------------------------------------
!
      subroutine set_node_position_4_viewer(merged)
!
      use t_mesh_data
      use m_surface_mesh_4_merge
!
      type(mesh_geometry), intent(in) :: merged
!
      integer(kind = kint) :: inum, inod
!
!
      do inum = 1, nodpetot_viewer
        inod = inod_viewer2merge(inum)
        xx_view(inum,1:3) = merged%node%xx(inod,1:3)
      end do
!
      end subroutine set_node_position_4_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_node_group_item_viewer(merged_grp, nod_nod_grp)
!
      use t_mesh_data
      use t_surface_mesh_4_merge
!
      type(mesh_groups), intent(in) :: merged_grp
      type(viewer_group_data), intent(inout) :: nod_nod_grp
!
      integer(kind = kint) :: inum, inod
!
!
      do inum = 1, nod_nod_grp%num_item
        inod = merged_grp%nod_grp%item_grp(inum)
        nod_nod_grp%item_sf(inum) = inod_merge2viewer(inod)
      end do
!
      end subroutine set_node_group_item_viewer
!
!------------------------------------------------------------------
!
      end module merge_to_viewer_ordering
