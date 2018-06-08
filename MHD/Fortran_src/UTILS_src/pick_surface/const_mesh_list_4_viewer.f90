!> @file  const_mesh_list_4_viewer.f90
!!      module const_mesh_list_4_viewer
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Mark node, surface, edge for viewer mesh
!!
!!@verbatim
!!      subroutine s_const_mesh_list_4_viewer(iflag_add_comm_tbl,       &
!!     &         node, nod_comm, surf, edge, nod_grp, ele_grp, surf_grp,&
!!     &         inod_ksm, isurf_ksm, iedge_ksm,                        &
!!     &         numnod_ksm, numsurf_ksm, numedge_ksm)
!!@endverbatim
!
      module const_mesh_list_4_viewer
!
      use m_precision
      use m_geometry_constants
      use calypso_mpi
!
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_comm_table
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
      subroutine s_const_mesh_list_4_viewer(iflag_add_comm_tbl,         &
     &         node, nod_comm, surf, edge, nod_grp, ele_grp, surf_grp,  &
     &         inod_ksm, isurf_ksm, iedge_ksm,                          &
     &         numnod_ksm, numsurf_ksm, numedge_ksm)
!
      integer(kind = kint), intent(in) :: iflag_add_comm_tbl
      type(communication_table), intent(in) :: nod_comm
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
        call set_node_list_4_ksm(node, iflag_node, icou_nod, inod_ksm)
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
!
      if(iflag_add_comm_tbl .gt. 0) then
        call add_comm_table_2_mesh_list(node, nod_comm%num_neib,        &
    &       nod_comm%istack_import, nod_comm%item_import,               &
    &       nod_comm%istack_export, nod_comm%item_export,               &
    &       iflag_node, icou_nod, inod_ksm)
      end if
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
      subroutine add_comm_table_2_mesh_list(node, num_neib,             &
     &          istack_import, item_import, istack_export, item_export, &
     &          iflag_node, icou_nod, inod_ksm)
!
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: num_neib
      integer(kind = kint), intent(in) :: istack_import(0:num_neib)
      integer(kind = kint), intent(in)                                  &
     &                     :: item_import(istack_import(num_neib))
      integer(kind = kint), intent(in) :: istack_export(0:num_neib)
      integer(kind = kint), intent(in)                                  &
     &                     :: item_export(istack_export(num_neib))
!
      integer(kind = kint), intent(inout) :: iflag_node(node%numnod)
      integer(kind = kint), intent(inout) :: inod_ksm(node%numnod)
      integer(kind = kint), intent(inout) :: icou_nod
!
      integer(kind = kint) :: i, ist, num
!
!
      do i = 1, num_neib
        ist = istack_import(i-1) + 1
        num = istack_import(i) - istack_import(i-1)
        call mark_node_in_each_node_grp                                 &
     &     (num, item_import(ist), node, iflag_node)
        call set_node_list_4_ksm(node, iflag_node, icou_nod, inod_ksm)
      end do
!
      do i = 1, num_neib
        ist = istack_export(i-1) + 1
        num = istack_export(i) - istack_export(i-1)
        call mark_node_in_each_node_grp                                 &
     &     (num, item_export(ist), node, iflag_node)
        call set_node_list_4_ksm(node, iflag_node, icou_nod, inod_ksm)
      end do
!
      end subroutine add_comm_table_2_mesh_list
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
        iele = ele_grp%item_grp(igrp)
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
      integer(kind = kint) :: isurf, iedge
!
!
      call set_node_list_4_ksm(node, iflag_node, icou_nod, inod_ksm)
!
      do isurf = 1, surf%numsurf
        if(iflag_surf(isurf) .ne. 0 .and. isurf_ksm(isurf) .eq. 0) then
          icou_surf = icou_surf + 1
          isurf_ksm(isurf) = icou_surf
        end if
      end do
      do iedge = 1, edge%numedge
        if(iflag_edge(iedge) .ne. 0 .and. iedge_ksm(iedge) .eq. 0) then
          icou_edge = icou_edge + 1
          iedge_ksm(isurf) = icou_edge
        end if
      end do
!
      end subroutine set_nod_sf_edge_list_4_ksm
!
!------------------------------------------------------------------
!
      subroutine set_node_list_4_ksm                                    &
     &         (node, iflag_node, icou_nod, inod_ksm)
!
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: iflag_node(node%numnod)
!
      integer(kind = kint), intent(inout) :: icou_nod
      integer(kind = kint), intent(inout) :: inod_ksm(node%numnod)
!
      integer(kind = kint) :: inod
!
      do inod = 1, node%numnod
        if(iflag_node(inod) .ne. 0 .and. inod_ksm(inod) .eq. 0) then
          icou_nod = icou_nod + 1
          inod_ksm(inod) = icou_nod
        end if
      end do
!
      end subroutine set_node_list_4_ksm
!
!------------------------------------------------------------------
!
      end module const_mesh_list_4_viewer
