!> @file  const_mesh_list_4_viewer.f90
!!      module const_mesh_list_4_viewer
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Mark node, surface, edge for viewer mesh
!!
!!@verbatim
!!      subroutine s_const_mesh_list_4_viewer                           &
!!     &        (node, surf, edge, nod_grp, ele_grp, surf_grp,          &
!!     &         inod_ksm, isurf_ksm, iedge_ksm,                        &
!!     &         numnod_ksm, numsurf_ksm, numedge_ksm)
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
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine s_const_mesh_list_4_viewer                             &
     &        (node, surf, edge, nod_grp, ele_grp, surf_grp,            &
     &         inod_ksm, isurf_ksm, iedge_ksm,                          &
     &         numnod_ksm, numsurf_ksm, numedge_ksm)
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      type(group_data), intent(in) :: nod_grp, ele_grp
      type(surface_group_data), intent(in) :: surf_grp
!
      integer(kind = kint), intent(inout) :: inod_ksm(node%numnod)
      integer(kind = kint), intent(inout) :: isurf_ksm(surf%numsurf)
      integer(kind = kint), intent(inout) :: iedge_ksm(edge%numedge)
      integer(kind = kint), intent(inout) :: numnod_ksm
      integer(kind = kint), intent(inout) :: numsurf_ksm
      integer(kind = kint), intent(inout) :: numedge_ksm
!
      integer(kind = kint) :: icou_nod, icou_surf, icou_edge
      integer(kind = kint), allocatable :: iflag_node(:)
      integer(kind = kint), allocatable :: iflag_surf(:)
      integer(kind = kint), allocatable :: iflag_edge(:)
!
      integer(kind = kint) :: i, ist, num
!
      icou_nod =  0
      icou_surf = 0
      icou_edge = 0
!
      allocate(iflag_node(node%numnod))
      allocate(iflag_surf(surf%numsurf))
      allocate(iflag_edge(edge%numedge))
!
      call mark_isolate_surface(surf, iflag_surf)
!
      call node_edge_flag_by_sf_flag                                    &
     &   (node, surf, edge, iflag_surf, iflag_node, iflag_edge)
      call set_nod_sf_edge_list_4_ksm(node, surf, edge,                 &
     &          iflag_node, iflag_surf, iflag_edge,                     &
     &          icou_nod, icou_surf, icou_edge,                         &
     &          inod_ksm, isurf_ksm, iedge_ksm)
!
!
      do i = 1, nod_grp%num_grp
        ist = nod_grp%istack_grp(i-1) + 1
        num = nod_grp%istack_grp(i) - nod_grp%istack_grp(i-1)
        call mark_node_in_each_node_grp                                 &
     &     (num, nod_grp%item_grp(ist), node, iflag_node)
        call set_node_list_4_ksm                                        &
     &     (node%numnod, iflag_node, icou_nod, inod_ksm)
      end do
!
!
      do i = 1, ele_grp%num_grp
        call mark_isolate_sf_in_ele_grp(i, ele_grp, surf, iflag_surf)
        call node_edge_flag_by_sf_flag                                  &
     &     (node, surf, edge, iflag_surf, iflag_node, iflag_edge)
!
        call set_nod_sf_edge_list_4_ksm(node, surf, edge,               &
     &          iflag_node, iflag_surf, iflag_edge,                     &
     &          icou_nod, icou_surf, icou_edge,                         &
     &          inod_ksm, isurf_ksm, iedge_ksm)
      end do
!
!
      do i = 1, surf_grp%num_grp
        call mark_isolate_sf_in_surf_grp(i, surf_grp, surf, iflag_surf)
        call node_edge_flag_by_sf_flag                                  &
     &     (node, surf, edge, iflag_surf, iflag_node, iflag_edge)
!
        call set_nod_sf_edge_list_4_ksm(node, surf, edge,               &
     &          iflag_node, iflag_surf, iflag_edge,                     &
     &          icou_nod, icou_surf, icou_edge,                         &
     &          inod_ksm, isurf_ksm, iedge_ksm)
      end do
!
      numnod_ksm = icou_nod
      numsurf_ksm = icou_surf
      numedge_ksm = icou_edge
!
      deallocate(iflag_node, iflag_surf, iflag_edge)
!
      end subroutine s_const_mesh_list_4_viewer
!
!------------------------------------------------------------------
!
      subroutine const_domain_groups_4_viewer(node, surf, edge,         &
     &          inod_ksm, isurf_ksm, iedge_ksm,                         &
     &          iflag_node, iflag_surf, iflag_edge, domain_grps)
!
      use t_viewer_mesh
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
!
      integer(kind = kint), intent(in) :: inod_ksm(node%numnod)
      integer(kind = kint), intent(in) :: isurf_ksm(surf%numsurf)
      integer(kind = kint), intent(in) :: iedge_ksm(edge%numedge)
!
      integer(kind = kint), intent(inout) :: iflag_node(node%numnod)
      integer(kind = kint), intent(inout) :: iflag_surf(surf%numsurf)
      integer(kind = kint), intent(inout) :: iflag_edge(edge%numedge)
!
      type(viewer_surface_groups), intent(inout) :: domain_grps
!
!
      call alloc_viewer_surf_grps_stack(ione, domain_grps)
      domain_grps%grp_name(1) = 'Domain'
!
      call mark_isolate_surface(surf, iflag_surf)
      call node_edge_flag_by_sf_flag                                    &
     &   (node, surf, edge, iflag_surf, iflag_node, iflag_edge)
!
      call count_ele_sf_grp_num_4_viewer                                &
     &  (ione, node, surf, edge, iflag_node, iflag_surf, iflag_edge,    &
     &   domain_grps%node_grp, domain_grps%surf_grp,                    &
     &   domain_grps%edge_grp)
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
      call mark_isolate_surface(surf, iflag_surf)
      call node_edge_flag_by_sf_flag                                    &
     &   (node, surf, edge, iflag_surf, iflag_node, iflag_edge)
!
      call set_ele_sf_grp_item_4_viewer                                 &
     &   (ione, node, surf, edge, iflag_node, iflag_surf, iflag_edge,   &
     &    inod_ksm, isurf_ksm, iedge_ksm,                               &
     &    domain_grps%node_grp, domain_grps%surf_grp,                   &
     &    domain_grps%edge_grp)
!
      end subroutine const_domain_groups_4_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine const_node_groups_4_viewer(node, surf, edge,           &
     &          nod_grp, inod_ksm, iflag_node, view_grps)
!
      use t_viewer_mesh
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
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
     &          ele_grp, inod_ksm, isurf_ksm, iedge_ksm,                &
     &          iflag_node, iflag_surf, iflag_edge, view_grps)
!
      use t_viewer_mesh
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      type(group_data), intent(in) :: ele_grp
!
      integer(kind = kint), intent(in) :: inod_ksm(node%numnod)
      integer(kind = kint), intent(in) :: isurf_ksm(surf%numsurf)
      integer(kind = kint), intent(in) :: iedge_ksm(edge%numedge)
!
      integer(kind = kint), intent(inout) :: iflag_node(node%numnod)
      integer(kind = kint), intent(inout) :: iflag_surf(surf%numsurf)
      integer(kind = kint), intent(inout) :: iflag_edge(edge%numedge)
!
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
        call mark_isolate_sf_in_ele_grp(i, ele_grp, surf, iflag_surf)
        call node_edge_flag_by_sf_flag                                  &
     &     (node, surf, edge, iflag_surf, iflag_node, iflag_edge)
!
        call count_ele_sf_grp_num_4_viewer                              &
     &     (i, node, surf, edge, iflag_node, iflag_surf, iflag_edge,    &
     &      view_grps%node_grp, view_grps%surf_grp, view_grps%edge_grp)
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
        call mark_isolate_sf_in_ele_grp(i, ele_grp, surf, iflag_surf)
        call node_edge_flag_by_sf_flag                                  &
     &     (node, surf, edge, iflag_surf, iflag_node, iflag_edge)
!
        call set_ele_sf_grp_item_4_viewer                               &
     &     (i, node, surf, edge, iflag_node, iflag_surf, iflag_edge,    &
     &      inod_ksm, isurf_ksm, iedge_ksm, view_grps%node_grp,         &
     &       view_grps%surf_grp, view_grps%edge_grp)
      end do
!
      end subroutine const_element_groups_4_viewer
!
!------------------------------------------------------------------
!
      subroutine const_surface_groups_4_viewer(node, surf, edge,        &
     &          surf_grp, inod_ksm, isurf_ksm, iedge_ksm,               &
     &          iflag_node, iflag_surf, iflag_edge, view_grps)
!
      use t_viewer_mesh
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      type(surface_group_data), intent(in) :: surf_grp
!
      integer(kind = kint), intent(in) :: inod_ksm(node%numnod)
      integer(kind = kint), intent(in) :: isurf_ksm(surf%numsurf)
      integer(kind = kint), intent(in) :: iedge_ksm(edge%numedge)
!
      integer(kind = kint), intent(inout) :: iflag_node(node%numnod)
      integer(kind = kint), intent(inout) :: iflag_surf(surf%numsurf)
      integer(kind = kint), intent(inout) :: iflag_edge(edge%numedge)
!
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
        call mark_isolate_sf_in_surf_grp(i, surf_grp, surf, iflag_surf)
        call node_edge_flag_by_sf_flag                                  &
     &     (node, surf, edge, iflag_surf, iflag_node, iflag_edge)
!
        call count_ele_sf_grp_num_4_viewer                              &
     &     (i, node, surf, edge, iflag_node, iflag_surf, iflag_edge,    &
     &      view_grps%node_grp, view_grps%surf_grp, view_grps%edge_grp)
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
        call mark_isolate_sf_in_surf_grp(i, surf_grp, surf, iflag_surf)
        call node_edge_flag_by_sf_flag                                  &
     &     (node, surf, edge, iflag_surf, iflag_node, iflag_edge)
!
        call set_ele_sf_grp_item_4_viewer                               &
     &     (i, node, surf, edge, iflag_node, iflag_surf, iflag_edge,    &
     &      inod_ksm, isurf_ksm, iedge_ksm, view_grps%node_grp,         &
     &       view_grps%surf_grp, view_grps%edge_grp)
      end do
!
      end subroutine const_surface_groups_4_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine count_ele_sf_grp_num_4_viewer(igrp,                    &
     &          node, surf, edge, iflag_node, iflag_surf, iflag_edge,   &
     &          node_grp, surf_grp, edge_grp)
!
      use t_viewer_mesh
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      integer(kind = kint), intent(in) :: igrp
      integer(kind = kint), intent(in) :: iflag_node(node%numnod)
      integer(kind = kint), intent(in) :: iflag_surf(surf%numsurf)
      integer(kind = kint), intent(in) :: iflag_edge(edge%numedge)
!
      type(viewer_group_data), intent(inout) :: node_grp
      type(viewer_group_data), intent(inout) :: surf_grp
      type(viewer_group_data), intent(inout) :: edge_grp
!
!
      node_grp%istack_sf(igrp)                                          &
     &    = count_group_item_4_viewer(node%numnod, iflag_node,          &
     &                                node_grp%istack_sf(igrp-1))
      surf_grp%istack_sf(igrp)                                          &
     &    = count_group_item_4_viewer(surf%numsurf, iflag_surf,         &
     &                                surf_grp%istack_sf(igrp-1))
      edge_grp%istack_sf(igrp)                                          &
     &    = count_group_item_4_viewer(edge%numedge, iflag_edge,         &
     &                                edge_grp%istack_sf(igrp-1))
!
      end subroutine count_ele_sf_grp_num_4_viewer
!
!------------------------------------------------------------------
!
      subroutine set_ele_sf_grp_item_4_viewer(igrp, node, surf, edge,   &
     &          iflag_node, iflag_surf, iflag_edge,                     &
     &          inod_ksm, isurf_ksm, iedge_ksm,                         &
     &          node_grp, surf_grp, edge_grp)
!
      use t_viewer_mesh
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      integer(kind = kint), intent(in) :: igrp
      integer(kind = kint), intent(in) :: iflag_node(node%numnod)
      integer(kind = kint), intent(in) :: iflag_surf(surf%numsurf)
      integer(kind = kint), intent(in) :: iflag_edge(edge%numedge)
      integer(kind = kint), intent(in) :: inod_ksm(node%numnod)
      integer(kind = kint), intent(in) :: isurf_ksm(surf%numsurf)
      integer(kind = kint), intent(in) :: iedge_ksm(edge%numedge)
!
      type(viewer_group_data), intent(inout) :: node_grp
      type(viewer_group_data), intent(inout) :: surf_grp
      type(viewer_group_data), intent(inout) :: edge_grp
!
!
      call set_group_item_4_viewer(node%numnod, iflag_node, inod_ksm,   &
     &    node_grp%num_item, node_grp%istack_sf(igrp-1),                &
     &    node_grp%item_sf)
      call set_group_item_4_viewer(surf%numsurf, iflag_surf, isurf_ksm, &
     &    surf_grp%num_item, surf_grp%istack_sf(igrp-1),                &
     &    surf_grp%item_sf)
      call set_group_item_4_viewer(edge%numedge, iflag_edge, iedge_ksm, &
     &    edge_grp%num_item, edge_grp%istack_sf(igrp-1),                &
     &    edge_grp%item_sf)
!
      end subroutine set_ele_sf_grp_item_4_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mark_isolate_surface(surf, iflag_surf)
!
      type(surface_data), intent(in) :: surf
!
      integer(kind = kint), intent(inout) :: iflag_surf(surf%numsurf)
!
      integer(kind = kint) :: isurf, inum
!
!
!$omp parallel workshare
      iflag_surf(1:surf%numsurf) = 0
!$omp end parallel workshare
!
      do inum = 1, surf%numsurf_iso
        isurf = abs(surf%isf_isolate(inum))
        iflag_surf(isurf) = 1
      end do
!
      end subroutine mark_isolate_surface
!
!------------------------------------------------------------------
!
      subroutine mark_node_in_each_node_grp                             &
     &         (num_item, item_nod, node, iflag_node)
!
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: num_item
      integer(kind = kint), intent(in) :: item_nod(num_item)
!
      integer(kind = kint), intent(inout) :: iflag_node(node%numnod)
!
      integer(kind = kint) :: inum, inod
!
!
!$omp parallel workshare
      iflag_node(1:node%numnod) = 0
!$omp end parallel workshare
!
      do inum = 1, num_item
        inod = item_nod(inum)
        iflag_node(inod) = 1
      end do
!
      end subroutine mark_node_in_each_node_grp
!
!------------------------------------------------------------------
!
      subroutine mark_isolate_sf_in_ele_grp                             &
     &         (igrp, ele_grp, surf, iflag_surf)
!
      integer(kind = kint), intent(in) :: igrp
      type(surface_data), intent(in) :: surf
      type(group_data), intent(in) :: ele_grp
!
      integer(kind = kint), intent(inout) :: iflag_surf(surf%numsurf)
!
      integer(kind = kint) :: iele, k1, ist, ied, inum, isurf
!
!
!$omp parallel workshare
      iflag_surf(1:surf%numsurf) = 0
!$omp end parallel workshare
!
      ist = ele_grp%istack_grp(igrp-1) + 1
      ied = ele_grp%istack_grp(igrp)
      do inum = ist, ied
        iele = ele_grp%item_grp(inum)
        do k1 = 1, nsurf_4_ele
          isurf = abs(surf%isf_4_ele(iele,k1))
          iflag_surf(isurf) = iflag_surf(isurf)                         &
     &                       + surf%isf_4_ele(iele,k1) / isurf
        end do
      end do
!
      end subroutine mark_isolate_sf_in_ele_grp
!
!------------------------------------------------------------------
!
      subroutine mark_isolate_sf_in_surf_grp                            &
     &         (igrp, surf_grp, surf, iflag_surf)
!
      integer(kind = kint), intent(in) :: igrp
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: surf_grp
!
      integer(kind = kint), intent(inout) :: iflag_surf(surf%numsurf)
!
      integer(kind = kint) :: iele, isurf, k1, ist, ied, inum
!
!
!$omp parallel workshare
      iflag_surf(1:surf%numsurf) = 0
!$omp end parallel workshare
!
      ist = surf_grp%istack_grp(igrp-1) + 1
      ied = surf_grp%istack_grp(igrp)
      do inum = ist, ied
        iele = surf_grp%item_sf_grp(1,igrp)
        k1 =   surf_grp%item_sf_grp(2,igrp)
        isurf = abs(surf%isf_4_ele(iele,k1))
        iflag_surf(isurf) = iflag_surf(isurf)                           &
     &                     + surf%isf_4_ele(iele,k1) / isurf
      end do
!
      end subroutine mark_isolate_sf_in_surf_grp
!
!------------------------------------------------------------------
!
      subroutine node_edge_flag_by_sf_flag                              &
     &         (node, surf, edge, iflag_surf, iflag_node, iflag_edge)
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      integer(kind = kint), intent(in) :: iflag_surf(surf%numsurf)
!
      integer(kind = kint), intent(inout) :: iflag_node(node%numnod)
      integer(kind = kint), intent(inout) :: iflag_edge(edge%numedge)
!
      integer(kind = kint) :: isurf, k1, inod, iedge
!
!
!$omp parallel workshare
      iflag_node(1:node%numnod) = 0
!$omp end parallel workshare
      do isurf = 1, surf%numsurf
        if(abs(iflag_surf(isurf)) .eq. 1) then
          do k1 = 1, surf%nnod_4_surf
            inod = surf%ie_surf(isurf,k1)
            iflag_node(inod) = 1
          end do
        end if
      end do
!
!$omp parallel workshare
      iflag_edge(1:edge%numedge) = 0
!$omp end parallel workshare
      do isurf = 1, surf%numsurf
        if(abs(iflag_surf(isurf)) .eq. 1) then
          do k1 = 1, nedge_4_surf
            iedge = abs(edge%iedge_4_sf(isurf,k1))
            iflag_edge(iedge) = 1
          end do
        end if
      end do
!
      end subroutine node_edge_flag_by_sf_flag
!
!------------------------------------------------------------------
!
      subroutine set_nod_sf_edge_list_4_ksm(node, surf, edge,           &
     &          iflag_node, iflag_surf, iflag_edge,                     &
     &          icou_nod, icou_surf, icou_edge,                         &
     &          inod_ksm, isurf_ksm, iedge_ksm)
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      integer(kind = kint), intent(in) :: iflag_node(node%numnod)
      integer(kind = kint), intent(in) :: iflag_surf(surf%numsurf)
      integer(kind = kint), intent(in) :: iflag_edge(edge%numedge)
!
      integer(kind = kint), intent(inout) :: icou_nod, icou_surf
      integer(kind = kint), intent(inout) :: icou_edge
      integer(kind = kint), intent(inout) :: inod_ksm(node%numnod)
      integer(kind = kint), intent(inout) :: isurf_ksm(surf%numsurf)
      integer(kind = kint), intent(inout) :: iedge_ksm(edge%numedge)
!
!
      call set_node_list_4_ksm                                          &
     &   (node%numnod, iflag_node, icou_nod, inod_ksm)
      call set_node_list_4_ksm                                          &
     &   (surf%numsurf, iflag_surf, icou_surf, isurf_ksm)
      call set_node_list_4_ksm                                          &
     &   (edge%numedge, iflag_edge, icou_edge, iedge_ksm)
!
      end subroutine set_nod_sf_edge_list_4_ksm
!
!------------------------------------------------------------------
!
      subroutine set_node_list_4_ksm                                    &
     &         (numnod, iflag_node, icou_nod, inod_ksm)
!
      integer(kind = kint), intent(in) :: numnod
      integer(kind = kint), intent(in) :: iflag_node(numnod)
!
      integer(kind = kint), intent(inout) :: icou_nod
      integer(kind = kint), intent(inout) :: inod_ksm(numnod)
!
      integer(kind = kint) :: inod
!
!
      do inod = 1, numnod
        if(iflag_node(inod) .ne. 0 .and. inod_ksm(inod) .eq. 0) then
          icou_nod = icou_nod + 1
          inod_ksm(inod) = icou_nod
        end if
      end do
!
      end subroutine set_node_list_4_ksm
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      integer(kind = kint) function count_group_item_4_viewer           &
     &                   (numnod, iflag_node, istack_pre)
!
      integer(kind = kint), intent(in)  :: numnod
      integer(kind = kint), intent(in)  :: iflag_node(numnod)
      integer(kind = kint), intent(in)  :: istack_pre
!
      integer(kind = kint) :: inod, icou
!
!
      icou = istack_pre
      do inod = 1, numnod
        icou = icou + iflag_node(inod)
      end do
      count_group_item_4_viewer = icou
!
      end function count_group_item_4_viewer
!
!------------------------------------------------------------------
!
      subroutine set_group_item_4_viewer(numnod, iflag_node, inod_ksm,  &
     &          num_item, istack_pre, item)
!
      integer(kind = kint), intent(in)  :: numnod
      integer(kind = kint), intent(in)  :: iflag_node(numnod)
      integer(kind = kint), intent(in)  :: inod_ksm(numnod)
      integer(kind = kint), intent(in)  :: num_item, istack_pre
!
      integer(kind = kint), intent(inout) :: item(num_item)
!
      integer(kind = kint) :: inod, icou
!
!
      icou = istack_pre
      do inod = 1, numnod
        if(iflag_node(inod) .gt. 0) then
          icou = icou + 1
          item(icou) = inod_ksm(inod)
        end if
      end do
!
      end subroutine set_group_item_4_viewer
!
!------------------------------------------------------------------
!
      end module const_mesh_list_4_viewer
