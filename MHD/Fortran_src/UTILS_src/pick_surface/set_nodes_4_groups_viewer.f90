!
!      module set_nodes_4_groups_viewer
!
      module set_nodes_4_groups_viewer
!
!      Written by Kemorin on Jan., 2007
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
!      subroutine s_set_nodes_4_groups_viewer
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine s_set_nodes_4_groups_viewer
!
      use m_surface_mesh_4_merge
!
!
      if(iflag_debug .gt. 0) write(*,*) 'allocate_imark_node'
      call allocate_imark_node(nodpetot_viewer)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_nod_4_domain_viewer'
      call set_nod_4_domain_viewer
!
      call allocate_ele_gp_nod_item_sf
      call allocate_sf_gp_nod_item_sf
      if(iflag_debug .gt. 0) write(*,*) 'set_nod_4_ele_group_viewer'
      call set_nod_4_ele_group_viewer
      if(iflag_debug .gt. 0) write(*,*) 'set_nod_4_surf_group_viewer'
      call set_nod_4_surf_group_viewer
!
      call deallocate_imark_node
!
      end subroutine s_set_nodes_4_groups_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_nod_4_domain_viewer
!
!
      call mark_node_4_domain_viewer
      call count_nod_stack_4_domain_viewer
      call allocate_domain_nod_item_sf
      call const_nod_4_domain_viewer
!
      end subroutine set_nod_4_domain_viewer
!
!------------------------------------------------------------------
!
      subroutine set_nod_4_ele_group_viewer
!
      integer(kind = kint) :: igrp, ied
!
!
      do igrp = 1, ngrp_ele_sf
        call mark_node_4_ele_grp_viewer(igrp)
!
        call allocate_ele_gp_nod_item_tmp
        ied = nnod_ele_sf
        ele_nod_item_tmp(1:ied) = ele_nod_item_sf(1:ied)
        call deallocate_ele_gp_nod_item_sf
!
        call count_nod_stack_4_ele_gp_viewer(igrp)
!
        call allocate_ele_gp_nod_item_sf
        ele_nod_item_sf(1:ied) = ele_nod_item_tmp(1:ied)
        call deallocate_ele_gp_nod_item_tmp
!
        call const_nod_4_ele_gp_viewer(igrp)
      end do
!
      end subroutine set_nod_4_ele_group_viewer
!
!------------------------------------------------------------------
!
      subroutine set_nod_4_surf_group_viewer
!
      integer(kind = kint) :: igrp, ied
!
!
      do igrp = 1, ngrp_surf_sf
        call mark_node_4_surf_grp_viewer(igrp)
!
        call allocate_sf_gp_nod_item_tmp
        ied = nnod_surf_sf
        surf_nod_item_tmp(1:ied) = surf_nod_item_sf(1:ied)
        call deallocate_sf_gp_nod_item_sf
!
        call count_nod_stack_4_sf_gp_viewer(igrp)
!
        call allocate_sf_gp_nod_item_sf
        surf_nod_item_sf(1:ied) = surf_nod_item_tmp(1:ied)
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
