!
!      module set_nodes_4_viewer
!
!      Written by Kemorin in Jan., 2007
!
!!      subroutine s_set_nodes_4_viewer(nnod_4_surf, mgd_mesh, num_pe,  &
!!     &          nnod_sf, inod_sf_stack, view_mesh, view_nod_grps)
!!        type(merged_mesh), intent(inout) :: mgd_mesh
!!        type(viewer_mesh_data), intent(inout) :: view_mesh
!!        type(viewer_node_groups), intent(inout) :: view_nod_grps
!
      module set_nodes_4_viewer
!
      use m_precision
!
      use m_machine_parameter
      use t_viewer_mesh
      use pickup_node_4_viewer
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
      subroutine s_set_nodes_4_viewer(nnod_4_surf, mgd_mesh, num_pe,    &
     &          nnod_sf, inod_sf_stack, view_mesh, view_nod_grps)
!
      use t_mesh_data_4_merge
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: num_pe
!
      integer(kind = kint), intent(inout) :: nnod_sf(num_pe)
      integer(kind = kint), intent(inout) :: inod_sf_stack(0:num_pe)
      type(merged_mesh), intent(inout) :: mgd_mesh
      type(viewer_mesh_data), intent(inout) :: view_mesh
      type(viewer_node_groups), intent(inout) :: view_nod_grps
!
!
      if(iflag_debug .gt. 0) write(*,*) 'allocate_imark_nod_pick_node'
      call allocate_imark_nod_pick_node(mgd_mesh%merged)
      call mark_used_node_4_viewer                                      &
     &   (nnod_4_surf, mgd_mesh%merged_grp, view_mesh)
!
      call count_used_node_4_viewer                                     &
     &   (mgd_mesh%merge_tbl, num_pe, nnod_sf)
      call s_cal_total_and_stacks(num_pe, nnod_sf, izero,               &
     &    inod_sf_stack, view_mesh%nnod_viewer)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &           'allocate_nod_cvt_table_viewer'
      call allocate_nod_cvt_table_viewer(mgd_mesh%merged, view_mesh)
      call set_node_cvt_table_viewer(mgd_mesh%merged)
!
      call deallocate_imark_nod_pick_node
!
      if(iflag_debug .gt. 0) write(*,*) 'alloc_nod_position_viewer'
      call alloc_nod_position_viewer(view_mesh)
!      call set_node_position_4_viewer(mgd_mesh%merged, view_mesh)
!
      call renumber_surf_connect_4_viewer(nnod_4_surf, view_mesh)
!
      call s_set_nod_grp_4_viewer_surface(mgd_mesh%merged_grp,          &
     &    num_pe, inod_sf_stack, view_nod_grps)
!
      if(iflag_debug .gt. 0) write(*,*)                                 &
     &           'deallocate_nod_cvt_table_viewer'
      call deallocate_nod_cvt_table_viewer
!
      call deallocate_grp_type(mgd_mesh%merged_grp%nod_grp)
!
      end subroutine s_set_nodes_4_viewer
!
!------------------------------------------------------------------
!
      subroutine s_set_nod_grp_4_viewer_surface                         &
     &         (merged_grp, num_pe, inod_sf_stack, view_nod_grps)
!
      use t_mesh_data
!
      use renumber_surface_4_viewer
!
      integer(kind = kint), intent(in) :: num_pe
      integer(kind = kint), intent(in) :: inod_sf_stack(0:num_pe)
      type(mesh_groups), intent(in) :: merged_grp
!
      type(viewer_node_groups), intent(inout) :: view_nod_grps
!
!
      view_nod_grps%num_grp = merged_grp%nod_grp%num_grp
      view_nod_grps%node_grp%num_item = merged_grp%nod_grp%num_item
      call alloc_merged_node_grps_stack(num_pe, view_nod_grps)
      call alloc_merged_group_item(view_nod_grps%node_grp)
!
      view_nod_grps%grp_name(1:view_nod_grps%num_grp)                   &
     &     = merged_grp%nod_grp%grp_name(1:view_nod_grps%num_grp)
!
      call set_node_group_item_viewer(merged_grp,                       &
     &    view_nod_grps%node_grp)
      call set_node_group_stack_viewer(num_pe, inod_sf_stack,           &
     &    merged_grp, view_nod_grps%num_grp, view_nod_grps%node_grp)
!
      end subroutine s_set_nod_grp_4_viewer_surface
!
!------------------------------------------------------------------
!
      end module set_nodes_4_viewer
