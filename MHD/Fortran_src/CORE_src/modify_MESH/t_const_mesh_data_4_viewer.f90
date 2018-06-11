!> @file  t_const_mesh_data_4_viewer.f90
!!      module t_const_mesh_data_4_viewer
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Mark node, surface, edge for viewer mesh
!!
!!@verbatim
!!      subroutine alloc_index_list_pick_surf                           &
!!     &         (node, surf, edge, idx_lst)
!!      subroutine dealloc_index_list_pick_surf(idx_lst)
!!        type(node_data), intent(in) :: node
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data), intent(in) :: edge
!!        type(index_list_4_pick_surface), intent(inout) :: idx_lst
!!      type(index_list_4_pick_surface), intent(inout) :: idx_lst
!!
!!      subroutine const_index_list_4_viewer                            &
!!     &         (node, surf, edge, nod_grp, ele_grp, surf_grp,         &
!!     &          idx_lst, view_mesh)
!!        type(node_data), intent(in) :: node
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data), intent(in) :: edge
!!        type(group_data), intent(in) :: nod_grp, ele_grp
!!        type(surface_group_data), intent(in) :: surf_grp
!!        type(index_list_4_pick_surface), intent(inout) :: idx_lst
!!        type(viewer_mesh_data), intent(inout) :: view_mesh
!!
!!      subroutine const_mesh_data_4_viewer                             &
!!     &         (node, surf, edge, idx_lst, view_mesh)
!!        type(node_data), intent(in) :: node
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data), intent(in) :: edge
!!        type(index_list_4_pick_surface), intent(in) :: idx_lst
!!        type(viewer_mesh_data), intent(inout) :: view_mesh
!!@endverbatim
!
      module t_const_mesh_data_4_viewer
!
      use m_precision
      use m_constants
      use m_geometry_constants
!
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_viewer_mesh
!
      implicit none
!
      type index_list_4_pick_surface
        integer(kind = kint) :: numnod
        integer(kind = kint) :: numsurf
        integer(kind = kint) :: numedge
!
        integer(kind = kint), allocatable :: inod_ksm(:)
        integer(kind = kint), allocatable :: isurf_ksm(:)
        integer(kind = kint), allocatable :: iedge_ksm(:)
!
        integer(kind = kint), allocatable :: iflag_node(:)
        integer(kind = kint), allocatable :: iflag_surf(:)
        integer(kind = kint), allocatable :: iflag_edge(:)
      end type index_list_4_pick_surface
!
      private :: set_surf_domain_id_viewer
      private :: set_nod_sf_edge_list_4_ksm
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine alloc_index_list_pick_surf                             &
     &         (node, surf, edge, idx_lst)
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      type(index_list_4_pick_surface), intent(inout) :: idx_lst
!
!
      idx_lst%numnod =  node%numnod
      idx_lst%numsurf = surf%numsurf
      idx_lst%numedge = edge%numedge
      allocate(idx_lst%inod_ksm(idx_lst%numnod))
      allocate(idx_lst%isurf_ksm(idx_lst%numsurf))
      allocate(idx_lst%iedge_ksm(idx_lst%numedge))
!
      allocate(idx_lst%iflag_node(idx_lst%numnod))
      allocate(idx_lst%iflag_surf(idx_lst%numsurf))
      allocate(idx_lst%iflag_edge(idx_lst%numedge))
!
      if(idx_lst%numnod .gt.  0) idx_lst%inod_ksm =  0
      if(idx_lst%numsurf .gt. 0) idx_lst%isurf_ksm = 0
      if(idx_lst%numedge .gt. 0) idx_lst%iedge_ksm = 0
!
      if(idx_lst%numnod .gt.  0) idx_lst%iflag_node = 0
      if(idx_lst%numsurf .gt. 0) idx_lst%iflag_surf = 0
      if(idx_lst%numedge .gt. 0) idx_lst%iflag_edge = 0
!
      end subroutine alloc_index_list_pick_surf
!
!------------------------------------------------------------------
!
      subroutine dealloc_index_list_pick_surf(idx_lst)
!
      type(index_list_4_pick_surface), intent(inout) :: idx_lst
!
      deallocate(idx_lst%inod_ksm)
      deallocate(idx_lst%isurf_ksm, idx_lst%iedge_ksm)
!
      deallocate(idx_lst%iflag_node)
      deallocate(idx_lst%iflag_surf, idx_lst%iflag_edge)
!
      end subroutine dealloc_index_list_pick_surf
!
!------------------------------------------------------------------
!
      subroutine set_surf_domain_id_viewer(surf, view_mesh)
!
      type(surface_data), intent(in) :: surf
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
!
      call alloc_surf_type_viewer(view_mesh)
!
      if ( surf%nnod_4_surf .eq. 4) then
        view_mesh%surftyp_viewer(1:view_mesh%nsurf_viewer) = 221
      else if ( surf%nnod_4_surf .eq. 8) then
        view_mesh%surftyp_viewer(1:view_mesh%nsurf_viewer) = 222
      else if ( surf%nnod_4_surf .eq. 9) then
        view_mesh%surftyp_viewer(1:view_mesh%nsurf_viewer) = 223
      end if
!
      end subroutine set_surf_domain_id_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine const_index_list_4_viewer                              &
     &         (node, surf, edge, nod_grp, ele_grp, surf_grp,           &
     &          idx_lst, view_mesh)
!
      use set_index_4_viewer_mesh
      use pickup_surface_4_viewer
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      type(group_data), intent(in) :: nod_grp, ele_grp
      type(surface_group_data), intent(in) :: surf_grp
!
      type(index_list_4_pick_surface), intent(inout) :: idx_lst
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      integer(kind = kint) :: icou_nod, icou_surf, icou_edge
!
      integer(kind = kint) :: i, ist, num
!
      icou_nod =  0
      icou_surf = 0
      icou_edge = 0
!
      call mark_isolate_surface(surf, idx_lst%iflag_surf)
!
      call node_edge_flag_by_sf_flag(node, surf, edge,                  &
     &    idx_lst%iflag_surf, idx_lst%iflag_node, idx_lst%iflag_edge)
      call set_nod_sf_edge_list_4_ksm(node, surf, edge,                 &
     &    icou_nod, icou_surf, icou_edge, idx_lst)
!
!
      do i = 1, nod_grp%num_grp
        ist = nod_grp%istack_grp(i-1) + 1
        num = nod_grp%istack_grp(i) - nod_grp%istack_grp(i-1)
        call mark_node_in_each_node_grp                                 &
     &     (num, nod_grp%item_grp(ist), node, idx_lst%iflag_node)
        call set_node_list_4_ksm(node%numnod, idx_lst%iflag_node,       &
     &      icou_nod, idx_lst%inod_ksm)
      end do
!
!
      do i = 1, ele_grp%num_grp
        call mark_isolate_sf_in_ele_grp                                 &
     &     (i, ele_grp, surf, idx_lst%iflag_surf)
        call node_edge_flag_by_sf_flag(node, surf, edge,                &
     &      idx_lst%iflag_surf, idx_lst%iflag_node, idx_lst%iflag_edge)
!
        call set_nod_sf_edge_list_4_ksm(node, surf, edge,               &
     &      icou_nod, icou_surf, icou_edge, idx_lst)
      end do
!
!
      do i = 1, surf_grp%num_grp
        call mark_isolate_sf_in_surf_grp                                &
     &     (i, surf_grp, surf, idx_lst%iflag_surf)
        call node_edge_flag_by_sf_flag(node, surf, edge,                &
     &      idx_lst%iflag_surf, idx_lst%iflag_node, idx_lst%iflag_edge)
!
        call set_nod_sf_edge_list_4_ksm(node, surf, edge,               &
     &      icou_nod, icou_surf, icou_edge, idx_lst)
      end do
!
      view_mesh%nnod_viewer =  icou_nod
      view_mesh%nsurf_viewer = icou_surf
      view_mesh%nedge_viewer = icou_edge
!
      end subroutine const_index_list_4_viewer
!
!------------------------------------------------------------------
!
      subroutine const_mesh_data_4_viewer                               &
     &         (node, surf, edge, idx_lst, view_mesh)
!
      use pickup_surface_4_viewer
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
!
      type(index_list_4_pick_surface), intent(in) :: idx_lst
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
!
      call alloc_nod_position_viewer(view_mesh)
      call set_node_position_4_viewer                                   &
     &   (node, idx_lst%inod_ksm, view_mesh)
!
      call alloc_surf_connect_viewer                                    &
     &   (surf%nnod_4_surf, view_mesh)
      call set_surf_connect_viewer(node, surf,                          &
     &    idx_lst%inod_ksm, idx_lst%isurf_ksm, view_mesh)
      call set_surf_domain_id_viewer(surf, view_mesh)
!
      call alloc_edge_data_4_sf                                         &
     &   (edge%nnod_4_edge, view_mesh)
      call set_edge_connect_viewer(node, surf, edge,                    &
     &    idx_lst%inod_ksm, idx_lst%isurf_ksm, idx_lst%iedge_ksm,       &
     &    view_mesh)
!
      end subroutine const_mesh_data_4_viewer
!
!------------------------------------------------------------------
!
      subroutine set_nod_sf_edge_list_4_ksm(node, surf, edge,           &
     &          icou_nod, icou_surf, icou_edge, idx_lst)
!
      use pickup_surface_4_viewer
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
!
      integer(kind = kint), intent(inout) :: icou_nod, icou_surf
      integer(kind = kint), intent(inout) :: icou_edge
      type(index_list_4_pick_surface), intent(inout) :: idx_lst
!
!
      call set_node_list_4_ksm(node%numnod, idx_lst%iflag_node,         &
     &    icou_nod, idx_lst%inod_ksm)
      call set_node_list_4_ksm(surf%numsurf, idx_lst%iflag_surf,        &
     &    icou_surf, idx_lst%isurf_ksm)
      call set_node_list_4_ksm(edge%numedge, idx_lst%iflag_edge,        &
     &    icou_edge, idx_lst%iedge_ksm)
!
      end subroutine set_nod_sf_edge_list_4_ksm
!
!------------------------------------------------------------------
!
      end module t_const_mesh_data_4_viewer
