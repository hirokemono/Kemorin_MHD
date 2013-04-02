!
!      module set_nodes_4_viewer
!
      module set_nodes_4_viewer
!
!      Written by Kemorin in Jan., 2007
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_data_4_merge
      use m_surface_mesh_4_merge
!
      implicit none
!
      private :: s_set_nod_grp_4_viewer_surface
!
!      subroutine s_set_nodes_4_viewer
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine s_set_nodes_4_viewer
!
      use m_pickup_table_4_viewer
      use pickup_node_4_viewer
!
!
      if(iflag_debug .gt. 0) write(*,*) 'allocate_imark_node'
      call allocate_imark_node(merged%node%numnod)
      call mark_used_node_4_viewer
!
      call count_used_node_4_viewer
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &           'allocate_nod_cvt_table_viewer'
      call allocate_nod_cvt_table_viewer
      call set_node_cvt_table_viewer
!
      if(iflag_debug .gt. 0) write(*,*) 'deallocate_imark_node'
      call deallocate_imark_node
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &           'allocate_nod_position_viewer'
      call allocate_nod_position_viewer
      call set_node_position_4_viewer
!
      call renumber_surf_connect_4_viewer
!
      call s_set_nod_grp_4_viewer_surface
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &           'deallocate_nod_cvt_table_viewer'
      call deallocate_nod_cvt_table_viewer
!
      call dealloc_merged_node_group
!
      end subroutine s_set_nodes_4_viewer
!
!------------------------------------------------------------------
!
      subroutine s_set_nod_grp_4_viewer_surface
!
      use m_surf_geometry_4_merge
!
      use renumber_surface_4_viewer
!
!
      ngrp_nod_sf = merged_grp%nod_grp%num_grp
      nnod_nod_sf = merged_grp%nod_grp%num_item
      call allocate_nod_grp_stack_4_surf
      call allocate_nod_grp_item_4_surf
!
      nod_gp_name_sf(1:ngrp_nod_sf)                                     &
     &     = merged_grp%nod_grp%grp_name(1:ngrp_nod_sf)
!
      call set_node_group_item_viewer
      call set_node_group_stack_viewer
!
      end subroutine s_set_nod_grp_4_viewer_surface
!
!------------------------------------------------------------------
!
      end module set_nodes_4_viewer
