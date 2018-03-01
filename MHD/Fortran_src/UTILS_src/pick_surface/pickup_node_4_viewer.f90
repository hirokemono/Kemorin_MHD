!
!      module pickup_node_4_viewer
!
!      Written by Kemorin in Jan., 2007
!
!!      subroutine mark_used_node_4_viewer(nnod_4_surf, merged_grp)
!!        type(mesh_groups), intent(in)  :: merged_grp
!!      subroutine count_used_node_4_viewer(merge_tbl)
!!        type(merged_stacks), intent(in) :: merge_tbl
!!      subroutine set_node_cvt_table_viewer(merged)
!!        type(mesh_geometry), intent(in) :: merged
!!
!!      subroutine renumber_surf_connect_4_viewer(nnod_4_surf)
!!      subroutine set_node_position_4_viewer(merged)
!!        type(mesh_geometry), intent(in) :: merged
!
      module pickup_node_4_viewer
!
      use m_precision
!
      use m_pickup_table_4_viewer
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine mark_used_node_4_viewer(nnod_4_surf, merged_grp)
!
      use t_mesh_data
      use m_surface_mesh_4_merge
!
      integer(kind = kint), intent(in) :: nnod_4_surf
      type(mesh_groups), intent(in)  :: merged_grp
!
      integer(kind = kint) :: inum, isurf, inod, k1
!
!
      do isurf = 1, surfpetot_viewer
        do k1 = 1, nnod_4_surf
          inod = ie_sf_viewer(isurf,k1)
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
      subroutine count_used_node_4_viewer(merge_tbl)
!
      use t_merged_geometry_data
      use m_surface_mesh_4_merge
!
      type(merged_stacks), intent(in) :: merge_tbl
!
      integer(kind = kint) :: ip, ist, ied, inod
!
      do ip = 1, num_pe_sf
        ist = merge_tbl%istack_nod(ip-1) + 1
        ied = merge_tbl%istack_nod(ip)
        inod_sf_stack(ip) = inod_sf_stack(ip-1)
        do inod = ist, ied
          inod_sf_stack(ip) = inod_sf_stack(ip) + imark_node(inod)
        end do
      end do
      nodpetot_viewer = inod_sf_stack(num_pe_sf)
!
      end subroutine count_used_node_4_viewer
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
!
      end module pickup_node_4_viewer
