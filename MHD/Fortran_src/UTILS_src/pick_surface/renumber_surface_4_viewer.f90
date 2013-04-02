!
!      module renumber_surface_4_viewer
!
      module renumber_surface_4_viewer
!
!      Written by Kemorin in Jan., 2007
!
      use m_precision
!
      use m_surface_mesh_4_merge
!
      implicit none
!
!      subroutine set_surf_domain_item_viewer
!      subroutine set_element_group_item_viewer
!      subroutine set_surface_group_item_viewer
!      subroutine set_node_group_item_viewer
!
!      subroutine set_surf_domain_stack_viewer
!      subroutine set_element_group_stack_viewer
!      subroutine set_surface_group_stack_viewer
!      subroutine set_node_group_stack_viewer
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine set_surf_domain_item_viewer
!
      use m_surf_geometry_4_merge
      use m_pickup_table_4_viewer
!
      integer(kind = kint) :: inum, isurf
!
!
      do inum = 1, nsurf_domain_sf
        isurf = abs( merged_surf%isf_isolate(inum) )
        isurf_domain_sf(inum) = isf_merge2viewer(isurf)                 &
     &                    * (merged_surf%isf_isolate(inum) / isurf)
      end do
!
      end subroutine set_surf_domain_item_viewer
!
!------------------------------------------------------------------
!
      subroutine set_element_group_item_viewer
!
      use m_grp_data_merged_surfaces
      use m_pickup_table_4_viewer
!
      integer(kind = kint) :: inum, isurf
!
!
      do inum = 1, nele_ele_sf
        isurf = abs( isf_isolate_ele_grp_m(inum) )
        ele_item_sf(inum) = isf_merge2viewer(isurf)                     &
     &                     * (isf_isolate_ele_grp_m(inum) / isurf)
      end do
!
      end subroutine set_element_group_item_viewer
!
!------------------------------------------------------------------
!
      subroutine set_surface_group_item_viewer
!
      use m_grp_data_merged_surfaces
      use m_pickup_table_4_viewer
!
      integer(kind = kint) :: inum, isurf
!
!
      do inum = 1, nsurf_surf_sf
        isurf = abs( isf_surf_grp_m(inum) )
        surf_item_sf(inum) = isf_merge2viewer(isurf)                    &
     &                     * (isf_surf_grp_m(inum) / isurf)
      end do
!
      end subroutine set_surface_group_item_viewer
!
!------------------------------------------------------------------
!
      subroutine set_node_group_item_viewer
!
      use m_geometry_data_4_merge
      use m_pickup_table_4_viewer
!
      integer(kind = kint) :: inum, inod
!
!
      do inum = 1, nnod_nod_sf
        inod = merged_grp%nod_grp%item_grp(inum)
        nod_item_sf(inum) = inod_merge2viewer(inod)
      end do
!
      end subroutine set_node_group_item_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_surf_domain_stack_viewer
!
      integer(kind = kint) :: ip, iref, ist, inum, isurf
!
!
      do ip = 1, num_pe_sf
        iref = isurf_sf_stack(ip)
        ist = isurf_stack_domain_sf(ip-1) + 1
        isurf_stack_domain_sf(ip) = isurf_stack_domain_sf(ip-1)
        do inum = ist, nsurf_domain_sf
          isurf = abs( isurf_domain_sf(inum) )
          if ( isurf .gt. iref ) exit
          isurf_stack_domain_sf(ip) = inum
        end do
      end do
!
      end subroutine set_surf_domain_stack_viewer
!
!------------------------------------------------------------------
!
      subroutine set_element_group_stack_viewer
!
      use m_grp_data_merged_surfaces
!
      integer(kind = kint) :: igrp, ip, idx, iref, ist, inum, isurf
!
!
      do igrp = 1, ngrp_ele_sf
        do ip = 1, num_pe_sf
          idx = ip + (igrp-1) * num_pe_sf
          iref = isurf_sf_stack(ip)
          ist = ele_stack_sf(idx-1) + 1
          ele_stack_sf(idx) = ele_stack_sf(idx-1)
          do inum = ist, istack_sf_iso_ele_grp_m(igrp)
            isurf = abs( ele_item_sf(inum) )
            if ( isurf .gt. iref ) exit
            ele_stack_sf(idx) = inum
          end do
        end do
      end do
!
      end subroutine set_element_group_stack_viewer
!
!------------------------------------------------------------------
!
      subroutine set_surface_group_stack_viewer
!
      use m_geometry_data_4_merge
!
      integer(kind = kint) :: igrp, ip, idx, iref, ist, ied
      integer(kind = kint) :: inum, isurf
!
!
      do igrp = 1, ngrp_surf_sf
        do ip = 1, num_pe_sf
          idx = ip + (igrp-1) * num_pe_sf
          iref = isurf_sf_stack(ip)
          ist = surf_stack_sf(idx-1) + 1
          ied = merged_grp%surf_grp%istack_grp(igrp)
!
          surf_stack_sf(idx) = surf_stack_sf(idx-1)
          do inum = ist, ied
            isurf = abs( surf_item_sf(inum) )
            if ( isurf .gt. iref ) exit
            surf_stack_sf(idx) = inum
          end do
        end do
      end do
!
      end subroutine set_surface_group_stack_viewer
!
!------------------------------------------------------------------
!
      subroutine set_node_group_stack_viewer
!
      use m_geometry_data_4_merge
!
      integer(kind = kint) :: igrp, ip, idx, iref, ist, ied, inum, inod
!
!
      do igrp = 1, ngrp_nod_sf
        do ip = 1, num_pe_sf
          idx = ip + (igrp-1) * num_pe_sf
          iref = inod_sf_stack(ip)
          ist = nod_stack_sf(idx-1) + 1
          ied = merged_grp%nod_grp%istack_grp(igrp)
!
          nod_stack_sf(idx) = nod_stack_sf(idx-1)
          do inum = ist, ied
            inod = abs( nod_item_sf(inum) )
            if ( inod .gt. iref ) exit
            nod_stack_sf(idx:(num_pe_sf*ngrp_nod_sf)) = inum
          end do
        end do
      end do
!
      end subroutine set_node_group_stack_viewer
!
!------------------------------------------------------------------
!
      end module renumber_surface_4_viewer
