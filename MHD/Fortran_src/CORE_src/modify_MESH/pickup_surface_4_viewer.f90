!> @file  pickup_surface_4_viewer.f90
!!      module pickup_surface_4_viewer
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Mark node, surface, edge for viewer mesh
!!
!!@verbatim
!!      subroutine set_node_list_4_ksm                                  &
!!     &         (numnod, iflag_node, icou_nod, inod_ksm)
!!
!!      subroutine set_node_position_4_viewer(node, inod_ksm, view_mesh)
!!      subroutine set_surf_connect_viewer                              &
!!     &         (node, surf, inod_ksm, isurf_ksm, view_mesh)
!!      subroutine set_edge_connect_viewer(node, surf, edge,            &
!!     &          inod_ksm, isurf_ksm, iedge_ksm, view_mesh)
!!        type(node_data), intent(in) :: node
!!        type(surface_data), intent(in) :: surf
!!        type(viewer_mesh_data), intent(inout) :: view_mesh
!!        type(edge_data), intent(in) :: edge
!!
!!      integer(kind = kint) function count_group_item_4_viewer         &
!!     &                   (numnod, iflag_node, istack_pre)
!!      subroutine set_group_item_4_viewer(numnod, iflag_node, inod_ksm,&
!!     &          num_item, istack_pre, item)
!!@endverbatim
!
      module pickup_surface_4_viewer
!
      use m_precision
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_viewer_mesh
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
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
      subroutine set_node_position_4_viewer(node, inod_ksm, view_mesh)
!
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: inod_ksm(node%numnod)
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      integer(kind = kint) :: inum, inod
!
!
!$omp parallel do
      do inum = 1, view_mesh%nnod_viewer
        view_mesh%inod_gl_view(inum) = inum
      end do
!$omp end parallel do
!
      do inod = 1, node%numnod
        inum = inod_ksm(inod)
        if(inum .gt. 0) view_mesh%xx_view(inum,1:3) = node%xx(inod,1:3)
      end do
!
      end subroutine set_node_position_4_viewer
!
!------------------------------------------------------------------
!
      subroutine set_surf_connect_viewer                                &
     &         (node, surf, inod_ksm, isurf_ksm, view_mesh)
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      integer(kind = kint), intent(in) :: inod_ksm(node%numnod)
      integer(kind = kint), intent(in) :: isurf_ksm(surf%numsurf)
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      integer(kind = kint) :: inum, isurf, k1, inod
!
!$omp parallel do
      do inum = 1, view_mesh%nsurf_viewer
        view_mesh%isurf_gl_view(inum) = inum
      end do
!$omp end parallel do
!
      do isurf = 1, surf%numsurf
        inum = isurf_ksm(isurf)
        if(inum .gt. 0) then
          do k1 = 1, surf%nnod_4_surf
            inod = surf%ie_surf(isurf,k1)
            view_mesh%ie_sf_viewer(inum,k1) = inod_ksm(inod)
            if(inod_ksm(inod) .le. 0) write(*,*)                        &
     &               'Wrong table in inod_ksm', inod
          end do
        end if
      end do
!
      end subroutine set_surf_connect_viewer
!
!------------------------------------------------------------------
!
      subroutine set_edge_connect_viewer(node, surf, edge,              &
     &          inod_ksm, isurf_ksm, iedge_ksm, view_mesh)
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
!
      integer(kind = kint), intent(in) :: inod_ksm(node%numnod)
      integer(kind = kint), intent(in) :: isurf_ksm(surf%numsurf)
      integer(kind = kint), intent(in) :: iedge_ksm(edge%numedge)
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      integer(kind = kint) :: iedge, isurf, inod, inum, k1
!
!
!$omp parallel do
      do inum = 1, view_mesh%nedge_viewer
        view_mesh%iedge_gl_view(inum) = inum
      end do
!$omp end parallel do
!
      do iedge = 1, edge%numedge
        inum = iedge_ksm(iedge)
        if(inum .gt. 0) then
          do k1 = 1, edge%nnod_4_edge
            inod = edge%ie_edge(iedge,k1)
            view_mesh%ie_edge_viewer(inum,k1) = inod_ksm(inod)
              if(inod_ksm(inod) .le. 0) write(*,*)                      &
     &               'Wrong table in inod_ksm', inod
          end do
        end if
      end do
!
      do isurf = 1, surf%numsurf
        inum = isurf_ksm(isurf)
        if(inum .gt. 0) then
          do k1 = 1, nedge_4_surf
            iedge = abs(edge%iedge_4_sf(isurf,k1))
            view_mesh%iedge_sf_viewer(inum,k1) = iedge_ksm(iedge)
            if(iedge_ksm(iedge) .le. 0) write(*,*)                      &
     &              'Wrong table in iedge_ksm', iedge, iedge_ksm(iedge)
          end do
        end if
      end do
!
      end subroutine set_edge_connect_viewer
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
        icou = icou + abs(iflag_node(inod))
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
        if(abs(iflag_node(inod)) .gt. 0) then
          icou = icou + 1
          item(icou) = inod_ksm(inod) * iflag_node(inod)
          if(item(icou) .eq. 0) write(*,*)                              &
     &                        'Wrong at', icou, inod, inod_ksm(inod)
        end if
      end do
!
      end subroutine set_group_item_4_viewer
!
!------------------------------------------------------------------
!
      end module pickup_surface_4_viewer
