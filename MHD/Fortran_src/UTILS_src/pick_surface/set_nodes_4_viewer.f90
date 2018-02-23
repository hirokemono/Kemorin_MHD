!
!      module set_nodes_4_viewer
!
!      Written by Kemorin in Jan., 2007
!
!      subroutine s_set_nodes_4_viewer(nnod_4_surf)
!
      module set_nodes_4_viewer
!
      use m_precision
!
      use m_machine_parameter
      use m_surface_mesh_4_merge
!
      implicit none
!
      private :: s_set_nod_grp_4_viewer_surface
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine s_set_nodes_4_viewer(nnod_4_surf)
!
      use m_geometry_data_4_merge
      use m_pickup_table_4_viewer
      use pickup_node_4_viewer
!
      integer(kind = kint), intent(in) :: nnod_4_surf
!
!
      if(iflag_debug .gt. 0) write(*,*) 'allocate_imark_node'
      call allocate_imark_node(mgd_mesh1%merged%node%numnod)
      call mark_used_node_4_viewer(nnod_4_surf, mgd_mesh1%merged_grp)
!
      call count_used_node_4_viewer(mgd_mesh1%merge_tbl)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &           'allocate_nod_cvt_table_viewer'
      call allocate_nod_cvt_table_viewer(mgd_mesh1%merged)
      call set_node_cvt_table_viewer(mgd_mesh1%merged)
!
      if(iflag_debug .gt. 0) write(*,*) 'deallocate_imark_node'
      call deallocate_imark_node
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &           'allocate_nod_position_viewer'
      call allocate_nod_position_viewer
      call set_node_position_4_viewer(mgd_mesh1%merged)
!
      call renumber_surf_connect_4_viewer(nnod_4_surf)
!
      call s_set_nod_grp_4_viewer_surface(mgd_mesh1%merged_grp)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &           'deallocate_nod_cvt_table_viewer'
      call deallocate_nod_cvt_table_viewer
!
      call deallocate_grp_type(mgd_mesh1%merged_grp%nod_grp)
!
      end subroutine s_set_nodes_4_viewer
!
!------------------------------------------------------------------
!
      subroutine s_set_nod_grp_4_viewer_surface(merged_grp)
!
      use t_mesh_data
      use m_surf_geometry_4_merge
!
      use renumber_surface_4_viewer
!
      type(mesh_groups), intent(in) :: merged_grp
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
      call set_node_group_item_viewer(merged_grp)
      call set_node_group_stack_viewer(merged_grp)
!
      end subroutine s_set_nod_grp_4_viewer_surface
!
!------------------------------------------------------------------
!
      end module set_nodes_4_viewer
