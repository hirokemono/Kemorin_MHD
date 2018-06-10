!> @file  const_mesh_list_4_viewer.f90
!!      module const_mesh_list_4_viewer
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Mark node, surface, edge for viewer mesh
!!
!!@verbatim
!!      subroutine const_domain_groups_4_viewer(node, surf, edge,       &
!!     &          idx_lst, domain_grps)
!!      subroutine const_node_groups_4_viewer                           &
!!     &         (node, nod_grp, inod_ksm, iflag_node, view_grps)
!!      subroutine const_element_groups_4_viewer(node, surf, edge,      &
!!     &          ele_grp, idx_lst, view_grps)
!!      subroutine const_surface_groups_4_viewer(node, surf, edge,      &
!!     &          surf_grp, idx_lst, view_grps)
!!        type(node_data), intent(in) :: node
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data), intent(in) :: edge
!!        type(mesh_groups), intent(in) :: group
!!        type(index_list_4_pick_surface), intent(inout) :: idx_lst
!!        type(viewer_surface_groups), intent(inout) :: domain_grps
!!        type(viewer_node_groups), intent(inout) :: view_nod_grps
!!        type(viewer_surface_groups), intent(inout) :: view_ele_grps
!!        type(viewer_surface_groups), intent(inout) :: view_sf_grps
!!@endverbatim
!
      module const_mesh_list_4_viewer
!
      use m_precision
      use m_constants
      use m_geometry_constants
      use calypso_mpi
!
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_group_data
      use t_viewer_mesh
      use t_const_mesh_data_4_viewer
!
      implicit none
!
      private :: count_ele_sf_grp_num_4_viewer
      private :: set_ele_sf_grp_item_4_viewer
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine const_domain_groups_4_viewer(node, surf, edge,         &
     &          idx_lst, domain_grps)
!
      use set_index_4_viewer_mesh
      use pickup_surface_4_viewer
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
!
      type(index_list_4_pick_surface), intent(inout) :: idx_lst
      type(viewer_surface_groups), intent(inout) :: domain_grps
!
!
      call alloc_viewer_surf_grps_stack(ione, domain_grps)
      domain_grps%grp_name(1) = 'Domain'
!
      call mark_isolate_surface(surf, idx_lst%iflag_surf)
      call node_edge_flag_by_sf_flag(node, surf, edge,                  &
     &    idx_lst%iflag_surf, idx_lst%iflag_node, idx_lst%iflag_edge)
!
      call count_ele_sf_grp_num_4_viewer                                &
     &  (ione, node, surf, edge, idx_lst, domain_grps%node_grp,         &
     &   domain_grps%surf_grp, domain_grps%edge_grp)
!
      domain_grps%surf_grp%num_item                                     &
     &      = domain_grps%surf_grp%istack_sf(domain_grps%num_grp)
      domain_grps%node_grp%num_item                                     &
     &      = domain_grps%node_grp%istack_sf(domain_grps%num_grp)
      domain_grps%edge_grp%num_item                                     &
     &      = domain_grps%edge_grp%istack_sf(domain_grps%num_grp)
!
      call alloc_merged_group_item(domain_grps%surf_grp)
      call alloc_merged_group_item(domain_grps%node_grp)
      call alloc_merged_group_item(domain_grps%edge_grp)
!
!
      call mark_isolate_surface(surf, idx_lst%iflag_surf)
      call node_edge_flag_by_sf_flag(node, surf, edge,                  &
     &    idx_lst%iflag_surf, idx_lst%iflag_node, idx_lst%iflag_edge)
!
      call set_ele_sf_grp_item_4_viewer                                 &
     &   (ione, node, surf, edge, idx_lst, domain_grps%node_grp,        &
     &    domain_grps%surf_grp, domain_grps%edge_grp)
!
      end subroutine const_domain_groups_4_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine const_node_groups_4_viewer                             &
     &         (node, nod_grp, inod_ksm, iflag_node, view_grps)
!
      use pickup_surface_4_viewer
      use set_index_4_viewer_mesh
!
      type(node_data), intent(in) :: node
      type(group_data), intent(in) :: nod_grp
!
      integer(kind = kint), intent(in) :: inod_ksm(node%numnod)
!
      integer(kind = kint), intent(inout) :: iflag_node(node%numnod)
!
      type(viewer_node_groups), intent(inout) :: view_grps
!
      integer(kind = kint) :: i, ist, num
!
!
      call alloc_viewer_node_grps_stack(nod_grp%num_grp, view_grps)
      do i = 1, view_grps%num_grp
        view_grps%grp_name(i) = nod_grp%grp_name(i)
      end do
!
      do i = 1, view_grps%num_grp
        ist = nod_grp%istack_grp(i-1) + 1
        num = nod_grp%istack_grp(i) - nod_grp%istack_grp(i-1)
        call mark_node_in_each_node_grp                                 &
     &     (num, nod_grp%item_grp(ist), node, iflag_node)
!
        view_grps%node_grp%istack_sf(i)                                 &
     &    = count_group_item_4_viewer(node%numnod, iflag_node,          &
     &                            view_grps%node_grp%istack_sf(i-1))
      end do
      view_grps%node_grp%num_item                                       &
     &      = view_grps%node_grp%istack_sf(view_grps%num_grp)
!
      call alloc_merged_group_item(view_grps%node_grp)
!
      do i = 1, view_grps%num_grp
        ist = nod_grp%istack_grp(i-1) + 1
        num = nod_grp%istack_grp(i) - nod_grp%istack_grp(i-1)
        call mark_node_in_each_node_grp                                 &
     &     (num, nod_grp%item_grp(ist), node, iflag_node)
!
        call set_group_item_4_viewer(node%numnod, iflag_node, inod_ksm, &
     &      view_grps%node_grp%num_item,                                &
     &      view_grps%node_grp%istack_sf(i-1),                          &
     &      view_grps%node_grp%item_sf)
      end do
!
      end subroutine const_node_groups_4_viewer
!
!------------------------------------------------------------------
!
      subroutine const_element_groups_4_viewer(node, surf, edge,        &
     &          ele_grp, idx_lst, view_grps)
!
      use set_index_4_viewer_mesh
      use pickup_surface_4_viewer
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      type(group_data), intent(in) :: ele_grp
!
      type(index_list_4_pick_surface), intent(inout) :: idx_lst
      type(viewer_surface_groups), intent(inout) :: view_grps
!
      integer(kind = kint) :: i
!
!
      call alloc_viewer_surf_grps_stack(ele_grp%num_grp, view_grps)
      do i = 1, view_grps%num_grp
        view_grps%grp_name(i) = ele_grp%grp_name(i)
      end do
!
      do i = 1, view_grps%num_grp
        call mark_isolate_sf_in_ele_grp                                 &
     &     (i, ele_grp, surf, idx_lst%iflag_surf)
        call node_edge_flag_by_sf_flag(node, surf, edge,                &
     &      idx_lst%iflag_surf, idx_lst%iflag_node, idx_lst%iflag_edge)
!
        call count_ele_sf_grp_num_4_viewer                              &
     &     (i, node, surf, edge, idx_lst, view_grps%node_grp,           &
     &      view_grps%surf_grp, view_grps%edge_grp)
      end do
      view_grps%surf_grp%num_item                                       &
     &      = view_grps%surf_grp%istack_sf(view_grps%num_grp)
      view_grps%node_grp%num_item                                       &
     &      = view_grps%node_grp%istack_sf(view_grps%num_grp)
      view_grps%edge_grp%num_item                                       &
     &      = view_grps%edge_grp%istack_sf(view_grps%num_grp)
!
      call alloc_merged_group_item(view_grps%surf_grp)
      call alloc_merged_group_item(view_grps%node_grp)
      call alloc_merged_group_item(view_grps%edge_grp)
!
      do i = 1, view_grps%num_grp
        call mark_isolate_sf_in_ele_grp                                 &
     &     (i, ele_grp, surf, idx_lst%iflag_surf)
        call node_edge_flag_by_sf_flag(node, surf, edge,                &
     &      idx_lst%iflag_surf, idx_lst%iflag_node, idx_lst%iflag_edge)
!
        call set_ele_sf_grp_item_4_viewer                               &
     &     (i, node, surf, edge, idx_lst, view_grps%node_grp,           &
     &       view_grps%surf_grp, view_grps%edge_grp)
      end do
!
      end subroutine const_element_groups_4_viewer
!
!------------------------------------------------------------------
!
      subroutine const_surface_groups_4_viewer(node, surf, edge,        &
     &          surf_grp, idx_lst, view_grps)
!
      use set_index_4_viewer_mesh
      use pickup_surface_4_viewer
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      type(surface_group_data), intent(in) :: surf_grp
!
      type(index_list_4_pick_surface), intent(inout) :: idx_lst
      type(viewer_surface_groups), intent(inout) :: view_grps
!
      integer(kind = kint) :: i
!
!
      call alloc_viewer_surf_grps_stack(surf_grp%num_grp, view_grps)
      do i = 1, view_grps%num_grp
        view_grps%grp_name(i) = surf_grp%grp_name(i)
      end do
!
      do i = 1, view_grps%num_grp
        call mark_isolate_sf_in_surf_grp                                &
     &     (i, surf_grp, surf, idx_lst%iflag_surf)
        call node_edge_flag_by_sf_flag(node, surf, edge,                &
     &      idx_lst%iflag_surf, idx_lst%iflag_node, idx_lst%iflag_edge)
!
        call count_ele_sf_grp_num_4_viewer                              &
     &     (i, node, surf, edge, idx_lst, view_grps%node_grp,           &
     &      view_grps%surf_grp, view_grps%edge_grp)
      end do
      view_grps%surf_grp%num_item                                       &
     &      = view_grps%surf_grp%istack_sf(view_grps%num_grp)
      view_grps%node_grp%num_item                                       &
     &      = view_grps%node_grp%istack_sf(view_grps%num_grp)
      view_grps%edge_grp%num_item                                       &
     &      = view_grps%edge_grp%istack_sf(view_grps%num_grp)
!
      call alloc_merged_group_item(view_grps%surf_grp)
      call alloc_merged_group_item(view_grps%node_grp)
      call alloc_merged_group_item(view_grps%edge_grp)
!
      do i = 1, view_grps%num_grp
        call mark_isolate_sf_in_surf_grp                                &
     &     (i, surf_grp, surf, idx_lst%iflag_surf)
        call node_edge_flag_by_sf_flag(node, surf, edge,                &
     &      idx_lst%iflag_surf, idx_lst%iflag_node, idx_lst%iflag_edge)
!
        call set_ele_sf_grp_item_4_viewer                               &
     &     (i, node, surf, edge, idx_lst, view_grps%node_grp,           &
     &       view_grps%surf_grp, view_grps%edge_grp)
      end do
!
      end subroutine const_surface_groups_4_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine count_ele_sf_grp_num_4_viewer(igrp, node, surf, edge,  &
     &          idx_lst, node_grp, surf_grp, edge_grp)
!
      use pickup_surface_4_viewer
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      integer(kind = kint), intent(in) :: igrp
      type(index_list_4_pick_surface), intent(in) :: idx_lst
!
      type(viewer_group_data), intent(inout) :: node_grp
      type(viewer_group_data), intent(inout) :: surf_grp
      type(viewer_group_data), intent(inout) :: edge_grp
!
!
      node_grp%istack_sf(igrp)                                          &
     &    = count_group_item_4_viewer(node%numnod, idx_lst%iflag_node,  &
     &                                node_grp%istack_sf(igrp-1))
      surf_grp%istack_sf(igrp)                                          &
     &    = count_group_item_4_viewer(surf%numsurf, idx_lst%iflag_surf, &
     &                                surf_grp%istack_sf(igrp-1))
      edge_grp%istack_sf(igrp)                                          &
     &    = count_group_item_4_viewer(edge%numedge, idx_lst%iflag_edge, &
     &                                edge_grp%istack_sf(igrp-1))
!
      end subroutine count_ele_sf_grp_num_4_viewer
!
!------------------------------------------------------------------
!
      subroutine set_ele_sf_grp_item_4_viewer(igrp, node, surf, edge,   &
     &          idx_lst, node_grp, surf_grp, edge_grp)
!
      use pickup_surface_4_viewer
!
      integer(kind = kint), intent(in) :: igrp
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      type(index_list_4_pick_surface), intent(in) :: idx_lst
!
      type(viewer_group_data), intent(inout) :: node_grp
      type(viewer_group_data), intent(inout) :: surf_grp
      type(viewer_group_data), intent(inout) :: edge_grp
!
!
      call set_group_item_4_viewer                                      &
     &   (node%numnod, idx_lst%iflag_node, idx_lst%inod_ksm,            &
     &    node_grp%num_item, node_grp%istack_sf(igrp-1),                &
     &    node_grp%item_sf)
      call set_group_item_4_viewer                                      &
     &   (surf%numsurf, idx_lst%iflag_surf, idx_lst%isurf_ksm,          &
     &    surf_grp%num_item, surf_grp%istack_sf(igrp-1),                &
     &    surf_grp%item_sf)
      call set_group_item_4_viewer                                      &
     &   (edge%numedge, idx_lst%iflag_edge, idx_lst%iedge_ksm,          &
     &    edge_grp%num_item, edge_grp%istack_sf(igrp-1),                &
     &    edge_grp%item_sf)
!
      end subroutine set_ele_sf_grp_item_4_viewer
!
!------------------------------------------------------------------
!
      end module const_mesh_list_4_viewer
