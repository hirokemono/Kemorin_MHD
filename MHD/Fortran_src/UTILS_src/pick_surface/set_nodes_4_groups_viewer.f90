!
!      module set_nodes_4_groups_viewer
!
!      Written by Kemorin on Jan., 2007
!
!!      subroutine s_set_nodes_4_groups_viewer(nnod_4_surf, nnod_4_edge)
!
      module set_nodes_4_groups_viewer
!
      use m_precision
!
      use m_machine_parameter
      use m_pickup_table_4_viewer
      use const_node_list_4_viewer
!
      implicit none
!
      private :: set_nod_4_domain_viewer
      private :: set_nod_4_ele_group_viewer
      private :: set_nod_4_surf_group_viewer
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine s_set_nodes_4_groups_viewer(nnod_4_surf, nnod_4_edge)
!
      use m_surface_mesh_4_merge
!
      integer(kind = kint), intent(in) :: nnod_4_surf, nnod_4_edge
!
!
      if(iflag_debug .gt. 0) write(*,*) 'allocate_imark_node'
      call allocate_imark_node(nodpetot_viewer)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_nod_4_domain_viewer'
      call set_nod_4_domain_viewer(nnod_4_surf, nnod_4_edge)
!
      call alloc_merged_group_item(ele_nod_grp)
      call alloc_merged_group_item(sf_nod_grp)
      if(iflag_debug .gt. 0) write(*,*) 'set_nod_4_ele_group_viewer'
      call set_nod_4_ele_group_viewer(nnod_4_surf, nnod_4_edge)
      if(iflag_debug .gt. 0) write(*,*) 'set_nod_4_surf_group_viewer'
      call set_nod_4_surf_group_viewer(nnod_4_surf, nnod_4_edge)
!
      call deallocate_imark_node
!
      end subroutine s_set_nodes_4_groups_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_nod_4_domain_viewer(nnod_4_surf, nnod_4_edge)
!
      use m_surface_mesh_4_merge
!
      integer(kind = kint), intent(in) :: nnod_4_surf, nnod_4_edge
!
!
      call mark_node_4_domain_viewer(nnod_4_surf, nnod_4_edge)
      call count_nod_stack_4_domain_viewer
      call alloc_merged_group_item(domain_nod_grp)
      call const_nod_4_domain_viewer
!
      end subroutine set_nod_4_domain_viewer
!
!------------------------------------------------------------------
!
      subroutine set_nod_4_ele_group_viewer(nnod_4_surf, nnod_4_edge)
!
      use m_surface_mesh_4_merge
!
      integer(kind = kint), intent(in) :: nnod_4_surf, nnod_4_edge
!
      integer(kind = kint) :: igrp, ied
!
!
      do igrp = 1, ngrp_ele_sf
        call mark_node_4_ele_grp_viewer(igrp, nnod_4_surf, nnod_4_edge)
!
        call allocate_ele_gp_nod_item_tmp
        ied = ele_nod_grp%num_item
        ele_nod_item_tmp(1:ied) = ele_nod_grp%item_sf(1:ied)
        call dealloc_merged_group_item(ele_nod_grp)
!
        call count_nod_stack_4_ele_gp_viewer(igrp)
!
        call alloc_merged_group_item(ele_nod_grp)
        ele_nod_grp%item_sf(1:ied) = ele_nod_item_tmp(1:ied)
        call deallocate_ele_gp_nod_item_tmp
!
        call const_nod_4_ele_gp_viewer(igrp)
      end do
!
      end subroutine set_nod_4_ele_group_viewer
!
!------------------------------------------------------------------
!
      subroutine set_nod_4_surf_group_viewer(nnod_4_surf, nnod_4_edge)
!
      use m_surface_mesh_4_merge

      integer(kind = kint), intent(in) :: nnod_4_surf, nnod_4_edge
!
      integer(kind = kint) :: igrp, ied
!
!
      do igrp = 1, view_sf_grps%num_grp
        call mark_node_4_surf_grp_viewer                                &
     &     (igrp, nnod_4_surf, nnod_4_edge, view_sf_grps%surf_grp)
!
        call allocate_sf_gp_nod_item_tmp
        ied = sf_nod_grp%num_item
        surf_nod_item_tmp(1:ied) = sf_nod_grp%item_sf(1:ied)
        call dealloc_merged_group_item(sf_nod_grp)
!
        call count_nod_stack_4_sf_gp_viewer(igrp)
!
        call alloc_merged_group_item(sf_nod_grp)
        sf_nod_grp%item_sf(1:ied) = surf_nod_item_tmp(1:ied)
        call deallocate_sf_gp_nod_item_tmp
!
        call const_nod_4_sf_gp_viewer(igrp)
      end do
!
      end subroutine set_nod_4_surf_group_viewer
!
!------------------------------------------------------------------
!
      end module set_nodes_4_groups_viewer
