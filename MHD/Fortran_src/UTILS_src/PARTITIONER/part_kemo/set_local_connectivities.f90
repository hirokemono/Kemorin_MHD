!set_local_connectivities.f90
!      module set_local_connectivities
!
!     Written by H. Matsui on Aug., 2007
!
!!      subroutine s_set_local_connectivities                           &
!!     &      (ele_org, surf_org, edge_org, ele_new, surf_new, edge_new)
!      subroutine set_local_connectivity_4_ele(ele_org, ele_new)
!
      module set_local_connectivities
!
      use m_precision
!
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
      subroutine s_set_local_connectivities                             &
     &      (ele_org, surf_org, edge_org, ele_new, surf_new, edge_new)
!
      use t_geometry_data
      use t_surface_data
      use t_edge_data
!
      type(element_data), intent(in) :: ele_org
      type(surface_data), intent(in) :: surf_org
      type(edge_data), intent(in) :: edge_org
!
      type(element_data), intent(inout) :: ele_new
      type(surface_data), intent(inout) :: surf_new
      type(edge_data), intent(inout) :: edge_new
!
!
      call set_local_connectivity_4_ele(ele_org, ele_new)
!
      call set_local_connectivity_4_surf(surf_org, surf_new)
      call set_local_surf_4_ele(ele_new, surf_org, surf_new)
!
      call set_local_connect_4_edge(edge_org, edge_new)
      call set_local_edge_4_ele(ele_new, edge_org, edge_new)
      call set_local_edge_4_surf(surf_new, edge_org, edge_new)
!
      end subroutine s_set_local_connectivities
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_local_connectivity_4_ele(ele_org, ele_new)
!
      use t_geometry_data
!
      type(element_data), intent(in) :: ele_org
      type(element_data), intent(inout) :: ele_new
!
      integer(kind = kint) :: inum, inod_g, k1
      integer(kind = kint_gl) :: iele
!
!
      do inum = 1, ele_new%numele
        iele = ele_new%iele_global(inum)
        do k1 = 1, ele_new%nodelm(inum)
          inod_g = ele_org%ie(iele,k1)
          ele_new%ie(inum,k1) = inod_local_part(inod_g)
        end do
      end do
!
      end subroutine set_local_connectivity_4_ele
!
!   --------------------------------------------------------------------
!
      subroutine set_local_surf_4_ele(ele_new, surf_org, surf_new)
!
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
!
      type(element_data), intent(in) :: ele_new
      type(surface_data), intent(in) :: surf_org
      type(surface_data), intent(inout) :: surf_new
!
      integer(kind = kint) :: inum, isurf, k1
      integer(kind = kint_gl) :: iele
!
!
      do inum = 1, ele_new%numele
        iele = ele_new%iele_global(inum)
        do k1 = 1, nsurf_4_ele
          isurf = abs(surf_org%isf_4_ele(iele,k1))
          surf_new%isf_4_ele(inum,k1) = isurf_local_part(isurf)         &
     &                       * (surf_org%isf_4_ele(iele,k1) / isurf)
        end do
      end do
!
      end subroutine set_local_surf_4_ele
!
!   --------------------------------------------------------------------
!
      subroutine set_local_edge_4_ele(ele_new, edge_org, edge_new)
!
      use m_geometry_constants
!
      use t_geometry_data
      use t_edge_data
!
      type(element_data), intent(in) :: ele_new
      type(edge_data), intent(in) :: edge_org
      type(edge_data), intent(inout) :: edge_new
      integer(kind = kint) :: inum, iedge, k1
      integer(kind = kint_gl) :: iele
!
!
      do inum = 1, ele_new%numele
        iele = ele_new%iele_global(inum)
        do k1 = 1, nedge_4_ele
          iedge = abs(edge_org%iedge_4_ele(iele,k1))
          edge_new%iedge_4_ele(inum,k1)                                 &
     &                      = edge_d_grp1%id_local_part(iedge)          &
     &                       * (edge_org%iedge_4_ele(iele,k1) / iedge)
        end do
      end do
!
      end subroutine set_local_edge_4_ele
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_local_connectivity_4_surf(surf_org, surf_new)
!
      use t_surface_data
!
      type(surface_data), intent(in) :: surf_org
      type(surface_data), intent(inout) :: surf_new
!
      integer(kind = kint) :: inum, inod_g, k1
      integer(kind = kint_gl) :: isurf
!
!
      do inum = 1, surf_new%numsurf
        isurf = surf_new%isurf_global(inum)
        do k1 = 1, surf_org%nnod_4_surf
          inod_g = surf_org%ie_surf(isurf,k1)
          surf_new%ie_surf(inum,k1) = inod_local_part(inod_g)
        end do
      end do
!
      end subroutine set_local_connectivity_4_surf
!
!   --------------------------------------------------------------------
!
      subroutine set_local_edge_4_surf(surf_new, edge_org, edge_new)
!
      use m_geometry_constants
      use t_surface_data
      use t_edge_data
!
      type(surface_data), intent(in) :: surf_new
      type(edge_data), intent(in) :: edge_org
      type(edge_data), intent(inout) :: edge_new
      integer(kind = kint) :: inum, iedge, k1
      integer(kind = kint_gl) :: isurf
!
!
      do inum = 1, surf_new%numsurf
        isurf = surf_new%isurf_global(inum)
        do k1 = 1, nedge_4_surf
          iedge = abs(edge_org%iedge_4_sf(isurf,k1))
          edge_new%iedge_4_sf(inum,k1)                                  &
     &                     = edge_d_grp1%id_local_part(iedge)           &
     &                      * (edge_org%iedge_4_sf(isurf,k1) / iedge)
        end do
      end do
!
      end subroutine set_local_edge_4_surf
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_local_connect_4_edge(edge_org, edge_new)
!
      use t_edge_data
!
      type(edge_data), intent(in) :: edge_org
      type(edge_data), intent(inout) :: edge_new
!
      integer(kind = kint) :: inum, inod_g, k1
      integer(kind = kint_gl) :: iedge
!
!
      do inum = 1, edge_new%numedge
        iedge = edge_new%iedge_global(inum)
        do k1 = 1, edge_org%nnod_4_edge
          inod_g = edge_org%ie_edge(iedge,k1)
          edge_new%ie_edge(inum,k1) = inod_local_part(inod_g)
        end do
      end do
!
      end subroutine set_local_connect_4_edge
!
!   --------------------------------------------------------------------
!
      end module set_local_connectivities
