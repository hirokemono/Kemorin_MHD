!set_local_by_subdomain_tbl.f90
!      module set_local_by_subdomain_tbl
!
!     Written by H. Matsui on Aug., 2007
!
!!      subroutine set_local_node(ip, org_node, new_node, nod_d_grp)
!!      subroutine set_local_element(ip, org_ele, new_ele, ele_d_grp)
!!        type(domain_group_4_partition), intent(inout) :: ele_d_grp
!!      subroutine set_local_surface(ip, new_surf, surf_d_grp)
!!        type(domain_group_4_partition), intent(inout) :: surf_d_grp
!!      subroutine set_local_edge(ip, new_edge, edge_d_grp)
!!        type(domain_group_4_partition), intent(inout) :: edge_d_grp
!!
!!      subroutine set_local_node_4_export(ip, nod_d_grp)
!!        type(domain_group_4_partition), intent(inout) :: nod_d_grp
!!      subroutine set_local_element_4_export(ip, ele_d_grp)
!!        type(domain_group_4_partition), intent(inout) :: ele_d_grp
!!      subroutine set_local_surface_4_export(ip, surf_d_grp)
!!        type(domain_group_4_partition), intent(inout) :: surf_d_grp
!!      subroutine set_local_edge_4_export(ip, edge_d_grp)
!!        type(domain_group_4_partition), intent(inout) :: edge_d_grp
!
!
      module set_local_by_subdomain_tbl
!
      use m_internal_4_partitioner
      use t_domain_group_4_partition
!
      implicit  none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_local_node(ip, org_node, new_node, nod_d_grp)
!
      use t_geometry_data
!
      integer(kind = kint), intent(in) :: ip
      type(node_data), intent(in) :: org_node
      type(node_data), intent(inout) :: new_node
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
!
      integer(kind = kint) :: ist, inum, inod
!
!$omp parallel workshare
      nod_d_grp%id_local_part(1:nod_d_grp%num_s_domin) = 0
!$omp end parallel workshare
!
      ist = itl_nod_part%istack_4_subdomain(ip-1)
      do inum = 1, itl_nod_part%num_4_subdomain(ip)
        inod = itl_nod_part%id_4_subdomain(inum+ist)
        new_node%inod_global(inum) = inod
        new_node%xx(inum,1:3) = org_node%xx(inod,1:3)
!
        nod_d_grp%id_local_part(inod)= inum
      end do
!
      end subroutine set_local_node
!
!   --------------------------------------------------------------------
!
      subroutine set_local_element(ip, org_ele, new_ele, ele_d_grp)
!
      use t_geometry_data
!
      integer(kind = kint), intent(in) :: ip
      type(element_data), intent(in) :: org_ele
!
      type(element_data), intent(inout) :: new_ele
      type(domain_group_4_partition), intent(inout) :: ele_d_grp
!
      integer(kind = kint) :: ist, inum, iele
!
!$omp parallel workshare
      ele_d_grp%id_local_part(1:ele_d_grp%num_s_domin) = 0
!$omp end parallel workshare
!
      ist = itl_ele_part%istack_4_subdomain(ip-1)
      do inum = 1, itl_ele_part%num_4_subdomain(ip)
        iele = itl_ele_part%id_4_subdomain(inum+ist)
        new_ele%iele_global(inum) = iele
        new_ele%nodelm(inum) = org_ele%nodelm(iele)
        new_ele%elmtyp(inum) = org_ele%elmtyp(iele)
!
        ele_d_grp%id_local_part(iele)= inum 
      end do
!
      end subroutine set_local_element
!
!   --------------------------------------------------------------------
!
      subroutine set_local_surface(ip, new_surf, surf_d_grp)
!
      use t_surface_data
!
      integer(kind = kint), intent(in) :: ip
      type(surface_data), intent(inout) :: new_surf
      type(domain_group_4_partition), intent(inout) :: surf_d_grp
!
      integer(kind = kint) :: ist, inum, isurf
!
!$omp parallel workshare
      surf_d_grp%id_local_part(1:surf_d_grp%num_s_domin) = 0
!$omp end parallel workshare
!
      ist = itl_surf_part%istack_4_subdomain(ip-1)
      do inum = 1, itl_surf_part%num_4_subdomain(ip)
        isurf = itl_surf_part%id_4_subdomain(inum+ist)
        new_surf%isurf_global(inum) = isurf
!
        surf_d_grp%id_local_part(isurf)= inum 
      end do
!
      end subroutine set_local_surface
!
!   --------------------------------------------------------------------
!
      subroutine set_local_edge(ip, new_edge, edge_d_grp)
!
      use t_edge_data
!
      integer(kind = kint), intent(in) :: ip
      type(edge_data), intent(inout) :: new_edge
      type(domain_group_4_partition), intent(inout) :: edge_d_grp
!
      integer(kind = kint) :: ist, inum, iedge
!
!$omp parallel workshare
      edge_d_grp%id_local_part(1:edge_d_grp%num_s_domin) = 0
!$omp end parallel workshare
!
      ist = itl_edge_part%istack_4_subdomain(ip-1)
      do inum = 1, itl_edge_part%num_4_subdomain(ip)
        iedge = itl_edge_part%id_4_subdomain(inum+ist)
        new_edge%iedge_global(inum) = iedge
!
        edge_d_grp%id_local_part(iedge)= inum 
      end do
!
      end subroutine set_local_edge
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine set_local_node_4_export(ip, nod_d_grp)
!
      integer(kind = kint), intent(in) :: ip
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
!
      integer(kind = kint) :: ist, inum, inod
!
!$omp parallel workshare
      nod_d_grp%id_local_part(1:nod_d_grp%num_s_domin) = 0
!$omp end parallel workshare
!
      ist = itl_nod_part%istack_4_subdomain(ip-1)
      do inum = 1, itl_nod_part%num_4_subdomain(ip)
        inod = itl_nod_part%id_4_subdomain(inum+ist)
        nod_d_grp%id_local_part(inod) = inum
      end do
!
      end subroutine set_local_node_4_export
!
!   --------------------------------------------------------------------
!
      subroutine set_local_element_4_export(ip, ele_d_grp)
!
      integer(kind = kint), intent(in) :: ip
      type(domain_group_4_partition), intent(inout) :: ele_d_grp
!
      integer(kind = kint) :: ist, inum, iele
!
!$omp parallel workshare
      ele_d_grp%id_local_part(1:ele_d_grp%num_s_domin) = 0
!$omp end parallel workshare
!
      ist = itl_ele_part%istack_4_subdomain(ip-1)
      do inum = 1, itl_ele_part%num_4_subdomain(ip)
        iele = itl_ele_part%id_4_subdomain(inum+ist)
        ele_d_grp%id_local_part(iele)= inum 
      end do
!
      end subroutine set_local_element_4_export
!
!   --------------------------------------------------------------------
!
      subroutine set_local_surface_4_export(ip, surf_d_grp)
!
      integer(kind = kint), intent(in) :: ip
      type(domain_group_4_partition), intent(inout) :: surf_d_grp
!
      integer(kind = kint) :: ist, inum, isurf
!
!$omp parallel workshare
      surf_d_grp%id_local_part(1:surf_d_grp%num_s_domin) = 0
!$omp end parallel workshare
!
      ist = itl_surf_part%istack_4_subdomain(ip-1)
      do inum = 1, itl_surf_part%num_4_subdomain(ip)
        isurf = itl_surf_part%id_4_subdomain(inum+ist)
        surf_d_grp%id_local_part(isurf)= inum 
      end do
!
      end subroutine set_local_surface_4_export
!
!   --------------------------------------------------------------------
!
      subroutine set_local_edge_4_export(ip, edge_d_grp)
!
      integer(kind = kint), intent(in) :: ip
      type(domain_group_4_partition), intent(inout) :: edge_d_grp
!
      integer(kind = kint) :: ist, inum, iedge
!
!$omp parallel workshare
      edge_d_grp%id_local_part(1:edge_d_grp%num_s_domin) = 0
!$omp end parallel workshare
!
      ist = itl_edge_part%istack_4_subdomain(ip-1)
      do inum = 1, itl_edge_part%num_4_subdomain(ip)
        iedge = itl_edge_part%id_4_subdomain(inum+ist)
        edge_d_grp%id_local_part(iedge)= inum 
      end do
!
      end subroutine set_local_edge_4_export
!
!   --------------------------------------------------------------------
!
      end module set_local_by_subdomain_tbl
