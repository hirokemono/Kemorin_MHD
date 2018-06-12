!> @file  set_index_4_viewer_mesh.f90
!!      module set_index_4_viewer_mesh
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Mark node, surface, edge for viewer mesh
!!
!!@verbatim
!!      subroutine mark_isolate_surface(surf, iflag_surf)
!!      subroutine mark_node_in_each_node_grp                           &
!!     &         (num_item, item_nod, node, iflag_node)
!!      subroutine mark_isolate_sf_in_ele_grp                           &
!!     &         (igrp, ele_grp, surf, iflag_surf)
!!      subroutine mark_isolate_sf_in_surf_grp                          &
!!     &         (igrp, surf_grp, surf, iflag_surf)
!!        type(node_data), intent(in) :: node
!!        type(surface_data), intent(in) :: surf
!!        type(group_data), intent(in) :: ele_grp
!!        type(surface_group_data), intent(in) :: surf_grp
!!
!!      subroutine node_edge_flag_by_sf_flag                            &
!!     &         (node, surf, edge, iflag_surf, iflag_node, iflag_edge)
!!        type(node_data), intent(in) :: node
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data), intent(in) :: edge
!!@endverbatim
!
      module set_index_4_viewer_mesh
!
      use m_precision
      use m_constants
      use m_geometry_constants
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
        iele = surf_grp%item_sf_grp(1,inum)
        k1 =   surf_grp%item_sf_grp(2,inum)
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
      end module set_index_4_viewer_mesh
