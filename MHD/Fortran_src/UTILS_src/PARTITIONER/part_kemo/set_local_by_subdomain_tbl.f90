!set_local_by_subdomain_tbl.f90
!      module set_local_by_subdomain_tbl
!
!     Written by H. Matsui on Aug., 2007
!
!!      subroutine set_local_node                                       &
!!     &         (ip, org_node, itl_nod_part, new_node, nod_d_grp)
!!        type(node_data), intent(in) :: org_node
!!        type(internal_4_partitioner), intent(in) :: itl_nod_part
!!        type(node_data), intent(inout) :: new_node
!!        type(domain_group_4_partition), intent(inout) :: nod_d_grp
!!      subroutine set_local_element                                    &
!!     &         (ip, org_ele, itl_ele_part, new_ele, ele_d_grp)
!!        type(element_data), intent(in) :: org_ele
!!        type(internal_4_partitioner), intent(in) :: itl_ele_part
!!        type(element_data), intent(inout) :: new_ele
!!        type(domain_group_4_partition), intent(inout) :: ele_d_grp
!!      subroutine set_local_surface                                    &
!!     &         (ip, itl_surf_part, new_surf, surf_d_grp)
!!        type(internal_4_partitioner), intent(in) :: itl_surf_part
!!        type(surface_data), intent(inout) :: new_surf
!!        type(domain_group_4_partition), intent(inout) :: surf_d_grp
!!      subroutine set_local_edge                                       &
!!     &         (ip, itl_edge_part, new_edge, edge_d_grp)
!!        type(internal_4_partitioner), intent(in) :: itl_edge_part
!!        type(edge_data), intent(inout) :: new_edge
!!        type(domain_group_4_partition), intent(inout) :: edge_d_grp
!!
!!      subroutine set_local_id_4_export(ip, itl_part, domain_grp)
!!        type(internal_4_partitioner), intent(in) :: itl_part
!!        type(domain_group_4_partition), intent(inout) :: domain_grp
!
!
      module set_local_by_subdomain_tbl
!
      use t_internal_4_partitioner
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
      subroutine set_local_node                                         &
     &         (ip, org_node, itl_nod_part, new_node, nod_d_grp)
!
      use t_geometry_data
!
      integer(kind = kint), intent(in) :: ip
      type(node_data), intent(in) :: org_node
      type(internal_4_partitioner), intent(in) :: itl_nod_part
!
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
      subroutine set_local_element                                      &
     &         (ip, org_ele, itl_ele_part, new_ele, ele_d_grp)
!
      use t_geometry_data
!
      integer(kind = kint), intent(in) :: ip
      type(element_data), intent(in) :: org_ele
      type(internal_4_partitioner), intent(in) :: itl_ele_part
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
      subroutine set_local_surface                                      &
     &         (ip, itl_surf_part, new_surf, surf_d_grp)
!
      use t_surface_data
!
      integer(kind = kint), intent(in) :: ip
      type(surface_data), intent(inout) :: new_surf
      type(internal_4_partitioner), intent(in) :: itl_surf_part
!
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
      subroutine set_local_edge                                         &
     &         (ip, itl_edge_part, new_edge, edge_d_grp)
!
      use t_edge_data
!
      integer(kind = kint), intent(in) :: ip
      type(internal_4_partitioner), intent(in) :: itl_edge_part
!
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
      subroutine set_local_id_4_export(ip, itl_part, domain_grp)
!
      integer(kind = kint), intent(in) :: ip
      type(internal_4_partitioner), intent(in) :: itl_part
!
      type(domain_group_4_partition), intent(inout) :: domain_grp
!
      integer(kind = kint) :: ist, inum, i
!
!$omp parallel workshare
      domain_grp%id_local_part(1:domain_grp%num_s_domin) = 0
!$omp end parallel workshare
!
      ist = itl_part%istack_4_subdomain(ip-1)
      do inum = 1, itl_part%num_4_subdomain(ip)
        i = itl_part%id_4_subdomain(inum+ist)
        domain_grp%id_local_part(i)= inum 
      end do
!
      end subroutine set_local_id_4_export
!
!   --------------------------------------------------------------------
!
      end module set_local_by_subdomain_tbl
