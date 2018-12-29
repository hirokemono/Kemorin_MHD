!set_local_by_subdomain_tbl.f90
!      module set_local_by_subdomain_tbl
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine set_local_node(ip, org_node, new_node)
!      subroutine set_local_element(ip, org_ele, new_ele)
!      subroutine set_local_surface(ip, new_surf)
!      subroutine set_local_edge(ip, new_edge)
!
!      subroutine set_local_node_4_export(ip)
!      subroutine set_local_element_4_export(ip)
!      subroutine set_local_surface_4_export(ip)
!      subroutine set_local_edge_4_export(ip)
!
!      use m_precision
!
      module set_local_by_subdomain_tbl
!
      use m_internal_4_partitioner
      use m_domain_group_4_partition
!
      implicit  none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_local_node(ip, org_node, new_node)
!
      use t_geometry_data
!
      integer(kind = kint), intent(in) :: ip
      type(node_data), intent(in) :: org_node
      type(node_data), intent(inout) :: new_node
!
      integer(kind = kint) :: ist, inum, inod
!
!$omp parallel workshare
      inod_local_part(1:nod_d_grp1%num_s_domin) = 0
!$omp end parallel workshare
!
      ist = istack_numnod_sub(ip-1)
      do inum = 1, numnod_4_subdomain(ip)
        inod = inod_4_subdomain(inum+ist)
        new_node%inod_global(inum) = inod
        new_node%xx(inum,1:3) = org_node%xx(inod,1:3)
!
        inod_local_part(inod)= inum
      end do
!
      end subroutine set_local_node
!
!   --------------------------------------------------------------------
!
      subroutine set_local_element(ip, org_ele, new_ele)
!
      use t_geometry_data
!
      integer(kind = kint), intent(in) :: ip
      type(element_data), intent(in) :: org_ele
      type(element_data), intent(inout) :: new_ele
!
      integer(kind = kint) :: ist, inum, iele
!
      iele_local_part(1:nele_s_domin) = 0
      ist = istack_numele_sub(ip-1)
      do inum = 1, numele_4_subdomain(ip)
        iele = iele_4_subdomain(inum+ist)
        new_ele%iele_global(inum) = iele
        new_ele%nodelm(inum) = org_ele%nodelm(iele)
        new_ele%elmtyp(inum) = org_ele%elmtyp(iele)
!
        iele_local_part(iele)= inum 
      end do
!
      end subroutine set_local_element
!
!   --------------------------------------------------------------------
!
      subroutine set_local_surface(ip, new_surf)
!
      use t_surface_data
!
      integer(kind = kint), intent(in) :: ip
      type(surface_data), intent(inout) :: new_surf
!
      integer(kind = kint) :: ist, inum, isurf
!
      isurf_local_part(1:nsurf_s_domin) = 0
      ist = istack_numsurf_sub(ip-1)
      do inum = 1, numsurf_4_subdomain(ip)
        isurf = isurf_4_subdomain(inum+ist)
        new_surf%isurf_global(inum) = isurf
!
        isurf_local_part(isurf)= inum 
      end do
!
      end subroutine set_local_surface
!
!   --------------------------------------------------------------------
!
      subroutine set_local_edge(ip, new_edge)
!
      use t_edge_data
!
      integer(kind = kint), intent(in) :: ip
      type(edge_data), intent(inout) :: new_edge
!
      integer(kind = kint) :: ist, inum, iedge
!
      iedge_local_part(1:nedge_s_domin) = 0
      ist = istack_numedge_sub(ip-1)
      do inum = 1, numedge_4_subdomain(ip)
        iedge = iedge_4_subdomain(inum+ist)
        new_edge%iedge_global(inum) = iedge
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
!$omp parallel workshare
      inod_local_part(1:nod_d_grp1%num_s_domin) = 0
!$omp end parallel workshare
!
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
