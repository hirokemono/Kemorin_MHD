!set_local_by_subdomain_tbl.f90
!      module set_local_by_subdomain_tbl
!
      module set_local_by_subdomain_tbl
!
!     Written by H. Matsui on Aug., 2007
!
      use m_precision
!
      use m_geometry_data
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_internal_4_partitioner
      use m_domain_group_4_partition
!
      implicit  none
!
!      subroutine set_local_node(ip)
!      subroutine set_local_element(ip)
!      subroutine set_local_surface(ip)
!      subroutine set_local_edge(ip)
!
!      subroutine set_local_node_4_export(ip)
!      subroutine set_local_element_4_export(ip)
!      subroutine set_local_surface_4_export(ip)
!      subroutine set_local_edge_4_export(ip)
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_local_node(ip)
!
      integer(kind = kint), intent(in) :: ip
      integer(kind = kint) :: ist, inum, inod
!
      inod_local_part(1:nnod_s_domin) = 0
      ist = istack_numnod_sub(ip-1)
      do inum = 1, numnod_4_subdomain(ip)
        inod = inod_4_subdomain(inum+ist)
        globalnodid_2nd(inum) = inod
        xx_2nd(inum,1:3) = xx(inod,1:3)
!
        inod_local_part(inod)= inum
      end do
!
      end subroutine set_local_node
!
!   --------------------------------------------------------------------
!
      subroutine set_local_element(ip)
!
      integer(kind = kint), intent(in) :: ip
      integer(kind = kint) :: ist, inum, iele
!
      iele_local_part(1:nele_s_domin) = 0
      ist = istack_numele_sub(ip-1)
      do inum = 1, numele_4_subdomain(ip)
        iele = iele_4_subdomain(inum+ist)
        globalelmid_2nd(inum) = iele
        nodelm_2nd(inum) = nodelm(iele)
        elmtyp_2nd(inum) = elmtyp(iele)
!
        iele_local_part(iele)= inum 
      end do
!
      end subroutine set_local_element
!
!   --------------------------------------------------------------------
!
      subroutine set_local_surface(ip)
!
      integer(kind = kint), intent(in) :: ip
      integer(kind = kint) :: ist, inum, isurf
!
      isurf_local_part(1:nsurf_s_domin) = 0
      ist = istack_numsurf_sub(ip-1)
      do inum = 1, numsurf_4_subdomain(ip)
        isurf = isurf_4_subdomain(inum+ist)
        globalsurfid_2nd(inum) = isurf
!
        isurf_local_part(isurf)= inum 
      end do
!
      end subroutine set_local_surface
!
!   --------------------------------------------------------------------
!
      subroutine set_local_edge(ip)
!
      integer(kind = kint), intent(in) :: ip
      integer(kind = kint) :: ist, inum, iedge
!
      iedge_local_part(1:nedge_s_domin) = 0
      ist = istack_numedge_sub(ip-1)
      do inum = 1, numedge_4_subdomain(ip)
        iedge = iedge_4_subdomain(inum+ist)
        globaledgeid_2nd(inum) = iedge
!
        iedge_local_part(iedge)= inum 
      end do
!
      end subroutine set_local_edge
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_local_node_4_export(ip)
!
      integer(kind = kint), intent(in) :: ip
      integer(kind = kint) :: ist, inum, inod
!
      inod_local_part(1:nnod_s_domin) = 0
      ist = istack_numnod_sub(ip-1)
      do inum = 1, numnod_4_subdomain(ip)
        inod = inod_4_subdomain(inum+ist)
        inod_local_part(inod)= inum
      end do
!
      end subroutine set_local_node_4_export
!
!   --------------------------------------------------------------------
!
      subroutine set_local_element_4_export(ip)
!
      integer(kind = kint), intent(in) :: ip
      integer(kind = kint) :: ist, inum, iele
!
      iele_local_part(1:nele_s_domin) = 0
      ist = istack_numele_sub(ip-1)
      do inum = 1, numele_4_subdomain(ip)
        iele = iele_4_subdomain(inum+ist)
        iele_local_part(iele)= inum 
      end do
!
      end subroutine set_local_element_4_export
!
!   --------------------------------------------------------------------
!
      subroutine set_local_surface_4_export(ip)
!
      integer(kind = kint), intent(in) :: ip
      integer(kind = kint) :: ist, inum, isurf
!
      isurf_local_part(1:nsurf_s_domin) = 0
      ist = istack_numsurf_sub(ip-1)
      do inum = 1, numsurf_4_subdomain(ip)
        isurf = isurf_4_subdomain(inum+ist)
        isurf_local_part(isurf)= inum 
      end do
!
      end subroutine set_local_surface_4_export
!
!   --------------------------------------------------------------------
!
      subroutine set_local_edge_4_export(ip)
!
      integer(kind = kint), intent(in) :: ip
      integer(kind = kint) :: ist, inum, iedge
!
      iedge_local_part(1:nedge_s_domin) = 0
      ist = istack_numedge_sub(ip-1)
      do inum = 1, numedge_4_subdomain(ip)
        iedge = iedge_4_subdomain(inum+ist)
        iedge_local_part(iedge)= inum 
      end do
!
      end subroutine set_local_edge_4_export
!
!   --------------------------------------------------------------------
!
      end module set_local_by_subdomain_tbl
