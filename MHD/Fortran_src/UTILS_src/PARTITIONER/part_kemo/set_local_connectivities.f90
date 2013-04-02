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
!      subroutine set_local_connectivity_4_edge
!      subroutine set_local_edge_4_ele
!      subroutine set_local_edge_4_surf
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
!      private :: set_local_connectivity_4_edge
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine s_set_local_connectivities
!
!
      call set_local_connectivity_4_ele
!
      call set_local_connectivity_4_surf
      call set_local_surf_4_ele
!
      call set_local_connectivity_4_edge
      call set_local_edge_4_ele
      call set_local_edge_4_surf
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
          isf_4_ele_2nd(inum,k1) = isurf_local_part(isurf)              &
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
          iedge_4_ele_2nd(inum,k1) = iedge_local_part(iedge)            &
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
      do inum = 1, nsurf_2nd
        isurf = globalsurfid_2nd(inum)
        do k1 = 1, nnod_4_surf
          inod_g = ie_surf(isurf,k1)
          ie_surf_2nd(inum,k1) = inod_local_part(inod_g)
        end do
      end do
!
      end subroutine set_local_connectivity_4_surf
!
!   --------------------------------------------------------------------
!
      subroutine set_local_edge_4_surf
!
      use m_geometry_constants
!
      integer(kind = kint) :: inum, isurf, iedge, k1
!
!
      do inum = 1, nsurf_2nd
        isurf = globalsurfid_2nd(inum)
        do k1 = 1, nedge_4_surf
          iedge = abs(iedge_4_sf(isurf,k1))
          iedge_4_sf_2nd(inum,k1) = iedge_local_part(iedge)             &
     &                               * (iedge_4_sf(isurf,k1) / iedge)
        end do
      end do
!
      end subroutine set_local_edge_4_surf
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_local_connectivity_4_edge
!
      integer(kind = kint) :: inum, iedge, inod_g, k1
!
!
      do inum = 1, nedge_2nd
        iedge = globaledgeid_2nd(inum)
        do k1 = 1, nnod_4_edge
          inod_g = ie_edge(iedge,k1)
          ie_edge_2nd(inum,k1) = inod_local_part(inod_g)
        end do
      end do
!
      end subroutine set_local_connectivity_4_edge
!
!   --------------------------------------------------------------------
!
      end module set_local_connectivities
