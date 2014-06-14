!set_local_connectivities.f90
!      module set_local_connectivities
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine s_set_local_connectivities
!
!      subroutine set_local_connectivity_4_ele
!
!      subroutine set_local_connectivity_4_surf
!      subroutine set_local_surf_4_ele
!
!      subroutine set_local_edge_4_ele
!
      module set_local_connectivities
!
      use m_precision
!
      use m_geometry_parameter
      use m_geometry_data
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_domain_group_4_partition
!
      implicit  none
!
!
!      private :: set_local_connectivity_4_ele
!      private :: set_local_connectivity_4_surf
      private :: set_local_edge_4_surf, set_local_connect_4_edge
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine s_set_local_connectivities
!
      use m_2nd_geometry_data
!
!
      call set_local_connectivity_4_ele
!
      call set_local_connectivity_4_surf
      call set_local_surf_4_ele
!
      call set_local_connect_4_edge(edge_2nd)
      call set_local_edge_4_ele
      call set_local_edge_4_surf(edge_2nd)
!
      end subroutine s_set_local_connectivities
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_local_connectivity_4_ele
!
      integer(kind = kint) :: inum, iele, inod_g, k1
!
!
      do inum = 1, nele_2nd
        iele = globalelmid_2nd(inum)
        do k1 = 1, nodelm_2nd(inum)
          inod_g = ie(iele,k1)
          ie_2nd(inum,k1) = inod_local_part(inod_g)
        end do
      end do
!
      end subroutine set_local_connectivity_4_ele
!
!   --------------------------------------------------------------------
!
      subroutine set_local_surf_4_ele
!
      use m_geometry_constants
!
!
      integer(kind = kint) :: inum, iele, isurf, k1
!
!
      do inum = 1, nele_2nd
        iele = globalelmid_2nd(inum)
        do k1 = 1, nsurf_4_ele
          isurf = abs(isf_4_ele(iele,k1))
          surf_2nd%isf_4_ele(inum,k1) = isurf_local_part(isurf)         &
     &                              * (isf_4_ele(iele,k1) / isurf)
        end do
      end do
!
      end subroutine set_local_surf_4_ele
!
!   --------------------------------------------------------------------
!
      subroutine set_local_edge_4_ele
!
      use m_geometry_constants
!
      integer(kind = kint) :: inum, iele, iedge, k1
!
!
      do inum = 1, nele_2nd
        iele = globalelmid_2nd(inum)
        do k1 = 1, nedge_4_ele
          iedge = abs(iedge_4_ele(iele,k1))
          edge_2nd%iedge_4_ele(inum,k1) = iedge_local_part(iedge)       &
     &                                * (iedge_4_ele(iele,k1) / iedge)
        end do
      end do
!
      end subroutine set_local_edge_4_ele
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_local_connectivity_4_surf
!
      integer(kind = kint) :: inum, isurf, inod_g, k1
!
!
      do inum = 1, surf_2nd%numsurf
        isurf = surf_2nd%isurf_global(inum)
        do k1 = 1, nnod_4_surf
          inod_g = ie_surf(isurf,k1)
          surf_2nd%ie_surf(inum,k1) = inod_local_part(inod_g)
        end do
      end do
!
      end subroutine set_local_connectivity_4_surf
!
!   --------------------------------------------------------------------
!
      subroutine set_local_edge_4_surf(edge)
!
      use m_geometry_constants
      use t_edge_data
!
      type(edge_data), intent(inout) :: edge
      integer(kind = kint) :: inum, isurf, iedge, k1
!
!
      do inum = 1, surf_2nd%numsurf
        isurf = surf_2nd%isurf_global(inum)
        do k1 = 1, nedge_4_surf
          iedge = abs(iedge_4_sf(isurf,k1))
          edge%iedge_4_sf(inum,k1) = iedge_local_part(iedge)            &
     &                               * (iedge_4_sf(isurf,k1) / iedge)
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
