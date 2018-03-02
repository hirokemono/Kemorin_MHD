!
!      module pickup_node_4_viewer
!
!      Written by Kemorin in Jan., 2007
!
!!      subroutine allocate_imark_nod_pick_node(merged)
!!      subroutine allocate_nod_cvt_table_viewer(merged, view_mesh)
!!        type(mesh_geometry), intent(in) :: merged
!!      subroutine deallocate_imark_nod_pick_node
!!      subroutine deallocate_nod_cvt_table_viewer
!!
!!      subroutine mark_used_node_4_viewer                              &
!!     &         (nnod_4_surf, merged_grp, view_mesh)
!!        type(mesh_groups), intent(in)  :: merged_grp
!!        type(viewer_mesh_data), intent(in) :: view_mesh
!!      subroutine count_used_node_4_viewer                             &
!!     &         (merge_tbl, num_pe, inod_sf_stack, view_mesh)
!!        type(merged_stacks), intent(in) :: merge_tbl
!!        type(viewer_mesh_data), intent(inout) :: view_mesh
!!
!!      subroutine set_node_cvt_table_viewer(merged, imark_node)
!!        type(mesh_geometry), intent(in) :: merged
!!      subroutine renumber_surf_connect_4_viewer                       &
!!     &         (nnod_4_surf, view_mesh, view_mesh)
!!      subroutine set_node_position_4_viewer(merged, view_mesh)
!!        type(mesh_geometry), intent(in) :: merged
!!        type(viewer_mesh_data), intent(inout) :: view_mesh
!!      subroutine set_node_group_item_viewer(merged_grp, nod_nod_grp)
!!        type(mesh_groups), intent(in) :: merged_grp
!!        type(viewer_group_data), intent(inout) :: nod_nod_grp
!
      module pickup_node_4_viewer
!
      use m_precision
      use t_surface_data
      use t_viewer_mesh
!
      implicit none
!
!
      integer(kind = kint), allocatable :: imark_node(:)
      integer(kind = kint), allocatable :: inod_merge2viewer(:)
      integer(kind = kint), allocatable :: inod_viewer2merge(:)
!
      private :: imark_node, inod_merge2viewer, inod_viewer2merge
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_imark_nod_pick_node(merged)
!
      use t_mesh_data
!
      type(mesh_geometry), intent(in) :: merged
!
      allocate( imark_node(merged%node%numnod) )
      imark_node = 0
!
      end subroutine allocate_imark_nod_pick_node
!
!------------------------------------------------------------------
!
      subroutine allocate_nod_cvt_table_viewer(merged, view_mesh)
!
      use t_mesh_data
!
      type(mesh_geometry), intent(in) :: merged
      type(viewer_mesh_data), intent(in) :: view_mesh
!
      allocate( inod_merge2viewer(merged%node%numnod) )
      allocate( inod_viewer2merge(view_mesh%nodpetot_viewer) )
      inod_merge2viewer = 0
      inod_viewer2merge = 0
!
      end subroutine allocate_nod_cvt_table_viewer
!
!------------------------------------------------------------------
!
      subroutine deallocate_imark_nod_pick_node
!
      deallocate( imark_node )
!
      end subroutine deallocate_imark_nod_pick_node
!
!------------------------------------------------------------------
!
      subroutine deallocate_nod_cvt_table_viewer
!
      deallocate( inod_merge2viewer, inod_viewer2merge )
!
      end subroutine deallocate_nod_cvt_table_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mark_used_node_4_viewer                                &
     &         (nnod_4_surf, merged_grp, view_mesh)
!
      use t_mesh_data
!
      integer(kind = kint), intent(in) :: nnod_4_surf
      type(mesh_groups), intent(in)  :: merged_grp
      type(viewer_mesh_data), intent(in) :: view_mesh
!
      integer(kind = kint) :: inum, isurf, inod, k1
!
!
      do isurf = 1, view_mesh%surfpetot_viewer
        do k1 = 1, nnod_4_surf
          inod = view_mesh%ie_sf_viewer(isurf,k1)
          imark_node(inod) = 1
        end do
      end do
!
      do inum = 1, merged_grp%nod_grp%num_item
        inod = merged_grp%nod_grp%item_grp(inum)
        imark_node(inod) = 1
      end do
!
      end subroutine mark_used_node_4_viewer
!
!------------------------------------------------------------------
!
      subroutine count_used_node_4_viewer                               &
     &         (merge_tbl, num_pe, inod_sf_stack, view_mesh)
!
      use t_merged_geometry_data
!
      type(merged_stacks), intent(in) :: merge_tbl
      integer(kind = kint), intent(in) :: num_pe
!
      integer(kind = kint), intent(inout) :: inod_sf_stack(0:num_pe)
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      integer(kind = kint) :: ip, ist, ied, inod
!
      do ip = 1, num_pe
        ist = merge_tbl%istack_nod(ip-1) + 1
        ied = merge_tbl%istack_nod(ip)
        inod_sf_stack(ip) = inod_sf_stack(ip-1)
        do inod = ist, ied
          inod_sf_stack(ip) = inod_sf_stack(ip) + imark_node(inod)
        end do
      end do
      view_mesh%nodpetot_viewer = inod_sf_stack(num_pe)
!
      end subroutine count_used_node_4_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_node_cvt_table_viewer(merged)
!
      use t_mesh_data
!
      type(mesh_geometry), intent(in) :: merged
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
      subroutine renumber_surf_connect_4_viewer                         &
     &         (nnod_4_surf, view_mesh)
!
      integer(kind = kint), intent(in) :: nnod_4_surf
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      integer(kind = kint) :: isurf, k1, inod
!
!
      do isurf = 1, view_mesh%surfpetot_viewer
        do k1 = 1, nnod_4_surf
          inod = view_mesh%ie_sf_viewer(isurf,k1)
          view_mesh%ie_sf_viewer(isurf,k1) = inod_merge2viewer(inod)
        end do
      end do
!
      end subroutine renumber_surf_connect_4_viewer
!
!------------------------------------------------------------------
!
      subroutine set_node_position_4_viewer(merged, view_mesh)
!
      use t_mesh_data
!
      type(mesh_geometry), intent(in) :: merged
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      integer(kind = kint) :: inum, inod
!
!
      do inum = 1, view_mesh%nodpetot_viewer
        inod = inod_viewer2merge(inum)
        view_mesh%xx_view(inum,1:3) = merged%node%xx(inod,1:3)
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
      end module pickup_node_4_viewer
