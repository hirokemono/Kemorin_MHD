!set_local_connectivities.f90
!      module set_local_connectivities
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine s_set_local_connectivities(ele, surf, edge)
!      subroutine set_local_connectivity_4_ele(ele)
!
      module set_local_connectivities
!
      use m_precision
!
      use m_geometry_parameter
      use m_geometry_data
      use m_2nd_geometry_param
      use m_domain_group_4_partition
!
      implicit  none
!
!
      private :: set_local_surf_4_ele, set_local_edge_4_ele
      private :: set_local_connectivity_4_surf
      private :: set_local_edge_4_surf, set_local_connect_4_edge
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine s_set_local_connectivities(ele, surf, edge)
!
      use t_geometry_data
      use t_surface_data
      use t_edge_data
!
      type(element_data), intent(inout) :: ele
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
!
!
      call set_local_connectivity_4_ele(ele)
!
      call set_local_connectivity_4_surf(surf)
      call set_local_surf_4_ele(ele, surf)
!
      call set_local_connect_4_edge(edge)
      call set_local_edge_4_ele(ele, edge)
      call set_local_edge_4_surf(surf, edge)
!
      end subroutine s_set_local_connectivities
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_local_connectivity_4_ele(ele)
!
      use t_geometry_data
!
      type(element_data), intent(inout) :: ele
!
      integer(kind = kint) :: inum, iele, inod_g, k1
!
!
      do inum = 1, ele%numele
        iele = ele%iele_global(inum)
        do k1 = 1, ele%nodelm(inum)
          inod_g = ie(iele,k1)
          ele%ie(inum,k1) = inod_local_part(inod_g)
        end do
      end do
!
      end subroutine set_local_connectivity_4_ele
!
!   --------------------------------------------------------------------
!
      subroutine set_local_surf_4_ele(ele, surf)
!
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(inout) :: surf
!
      integer(kind = kint) :: inum, iele, isurf, k1
!
!
      do inum = 1, ele%numele
        iele = ele%iele_global(inum)
        do k1 = 1, nsurf_4_ele
          isurf = abs(isf_4_ele(iele,k1))
          surf%isf_4_ele(inum,k1) = isurf_local_part(isurf)             &
     &                              * (isf_4_ele(iele,k1) / isurf)
        end do
      end do
!
      end subroutine set_local_surf_4_ele
!
!   --------------------------------------------------------------------
!
      subroutine set_local_edge_4_ele(ele, edge)
!
      use m_geometry_constants
!
      use t_geometry_data
      use t_edge_data
!
      type(element_data), intent(in) :: ele
      type(edge_data), intent(inout) :: edge
      integer(kind = kint) :: inum, iele, iedge, k1
!
!
      do inum = 1, ele%numele
        iele = ele%iele_global(inum)
        do k1 = 1, nedge_4_ele
          iedge = abs(iedge_4_ele(iele,k1))
          edge%iedge_4_ele(inum,k1) = iedge_local_part(iedge)           &
     &                               * (iedge_4_ele(iele,k1) / iedge)
        end do
      end do
!
      end subroutine set_local_edge_4_ele
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_local_connectivity_4_surf(surf)
!
      use t_surface_data
!
      type(surface_data), intent(inout) :: surf
!
      integer(kind = kint) :: inum, isurf, inod_g, k1
!
!
      do inum = 1, surf%numsurf
        isurf = surf%isurf_global(inum)
        do k1 = 1, nnod_4_surf
          inod_g = ie_surf(isurf,k1)
          surf%ie_surf(inum,k1) = inod_local_part(inod_g)
        end do
      end do
!
      end subroutine set_local_connectivity_4_surf
!
!   --------------------------------------------------------------------
!
      subroutine set_local_edge_4_surf(surf, edge)
!
      use m_geometry_constants
      use t_surface_data
      use t_edge_data
!
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
      integer(kind = kint) :: inum, isurf, iedge, k1
!
!
      do inum = 1, surf%numsurf
        isurf = surf%isurf_global(inum)
        do k1 = 1, nedge_4_surf
          iedge = abs(iedge_4_sf(isurf,k1))
          edge%iedge_4_sf(inum,k1) = iedge_local_part(iedge)            &
     &                             * (iedge_4_sf(isurf,k1) / iedge)
        end do
      end do
!
      end subroutine set_local_edge_4_surf
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_local_connect_4_edge(edge)
!
      use t_edge_data
!
      type(edge_data), intent(inout) :: edge
      integer(kind = kint) :: inum, iedge, inod_g, k1
!
!
      do inum = 1, edge%numedge
        iedge = edge%iedge_global(inum)
        do k1 = 1, nnod_4_edge
          inod_g = ie_edge(iedge,k1)
          edge%ie_edge(inum,k1) = inod_local_part(inod_g)
        end do
      end do
!
      end subroutine set_local_connect_4_edge
!
!   --------------------------------------------------------------------
!
      end module set_local_connectivities
