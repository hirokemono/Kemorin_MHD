!
!      module pickup_node_4_viewer
!
      module pickup_node_4_viewer
!
!      Written by Kemorin in Jan., 2007
!
      use m_precision
!
      use m_pickup_table_4_viewer
!
      implicit none
!
!      subroutine mark_used_node_4_viewer
!      subroutine count_used_node_4_viewer
!      subroutine set_node_cvt_table_viewer
!
!      subroutine renumber_surf_connect_4_viewer
!      subroutine set_node_position_4_viewer
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine mark_used_node_4_viewer
!
      use m_geometry_data
      use m_geometry_data_4_merge
      use m_surface_mesh_4_merge
!
      integer(kind = kint) :: inum, isurf, inod, k1
!
!
      do isurf = 1, surfpetot_viewer
        do k1 = 1, surf1%nnod_4_surf
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
      subroutine count_used_node_4_viewer
!
      use m_geometry_data_4_merge
      use m_surface_mesh_4_merge
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
      subroutine set_node_cvt_table_viewer
!
      use m_geometry_data_4_merge
      use m_surf_geometry_4_merge
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
!------------------------------------------------------------------
!
      subroutine renumber_surf_connect_4_viewer
!
      use m_geometry_data
      use m_surface_mesh_4_merge
!
      integer(kind = kint) :: isurf, k1, inod
!
!
      do isurf = 1, surfpetot_viewer
        do k1 = 1, surf1%nnod_4_surf
          inod = ie_sf_viewer(isurf,k1)
          ie_sf_viewer(isurf,k1) = inod_merge2viewer(inod)
        end do
      end do
!
      end subroutine renumber_surf_connect_4_viewer
!
!------------------------------------------------------------------
!
      subroutine set_node_position_4_viewer
!
      use m_geometry_data_4_merge
      use m_surface_mesh_4_merge
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
