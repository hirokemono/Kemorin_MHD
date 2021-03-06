!
!     module const_local_mesh_id
!
!     written by H. Matsui on Sep., 2007
!
!!      subroutine s_const_local_meshes(ip, org_node, org_ele,          &
!!     &          internals_part, nod_d_grp, ele_d_grp, newmesh)
!!        type(node_data), intent(in) :: org_node
!!        type(element_data), intent(in) :: org_ele
!!        type(internals_4_part), intent(in) :: internals_part
!!        type(domain_group_4_partition), intent(inout)  :: nod_d_grp
!!        type(domain_group_4_partition), intent(inout)  :: ele_d_grp
!!        type(mesh_geometry), intent(inout) :: newmesh
!!      subroutine const_local_mesh_sf_ele(ip, org_node, org_ele,       &
!!     &          org_surf, org_edge, internals_part,                   &
!!     &          domain_grp, newmesh, new_surf, new_edge)
!!        type(node_data), intent(in) :: org_node
!!        type(element_data), intent(in) :: org_ele
!!        type(surface_data), intent(in) :: org_surf
!!        type(edge_data), intent(in) ::    org_edge
!!        type(domain_groups_4_partitioner), intent(ionut) :: domain_grp
!!        type(mesh_geometry), intent(inout) :: newmesh
!!        type(surface_data), intent(inout) :: new_surf
!!        type(edge_data), intent(inout) :: new_edge
!
      module const_local_mesh_id
!
      use m_precision
!
      use t_internal_4_partitioner
      use t_domain_group_4_partition
      use set_local_by_subdomain_tbl
!
      implicit  none
!
      private :: const_local_node_position
      private :: const_local_element, const_local_surface
      private :: const_local_edge
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine s_const_local_meshes(ip, org_node, org_ele,            &
     &          internals_part, nod_d_grp, ele_d_grp, newmesh)
!
      use t_mesh_data
      use t_geometry_data
!
      integer(kind = kint), intent(in) :: ip
      type(node_data), intent(in) :: org_node
      type(element_data), intent(in) :: org_ele
      type(internals_4_part), intent(in) :: internals_part
!
      type(domain_group_4_partition), intent(inout)  :: nod_d_grp
      type(domain_group_4_partition), intent(inout)  :: ele_d_grp
      type(mesh_geometry), intent(inout) :: newmesh
!
!
      call const_local_node_position(ip, org_node,                      &
     &    internals_part%itl_nod_part, newmesh%node, nod_d_grp)
      call const_local_element(ip, org_ele,                             &
     &    internals_part%itl_ele_part, newmesh%ele, ele_d_grp)
!
      end subroutine s_const_local_meshes
!
!   --------------------------------------------------------------------
!
      subroutine const_local_mesh_sf_ele(ip, org_node, org_ele,         &
     &          org_surf, org_edge, internals_part,                     &
     &          domain_grp, newmesh, new_surf, new_edge)
!
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_edge_data
!
      integer(kind = kint), intent(in) :: ip
      type(node_data), intent(in) :: org_node
      type(element_data), intent(in) :: org_ele
      type(surface_data), intent(in) :: org_surf
      type(edge_data), intent(in) ::    org_edge
      type(internals_4_part), intent(in) :: internals_part
!
      type(domain_groups_4_partitioner), intent(inout) :: domain_grp
      type(mesh_geometry), intent(inout) :: newmesh
      type(surface_data), intent(inout) :: new_surf
      type(edge_data), intent(inout) :: new_edge
!
!
      call const_local_node_position                                    &
     &   (ip, org_node, internals_part%itl_nod_part,                    &
     &    newmesh%node, domain_grp%nod_d_grp)
      call const_local_element                                          &
     &   (ip, org_ele, internals_part%itl_ele_part,                     &
     &    newmesh%ele, domain_grp%ele_d_grp)
!
      call const_local_surface(ip, org_surf%nnod_4_surf, newmesh%ele,   &
     &    internals_part%itl_surf_part, new_surf,                       &
     &    domain_grp%surf_d_grp)
      call const_local_edge                                             &
     &   (ip, org_edge%nnod_4_edge, newmesh%ele, new_surf,              &
     &    internals_part%itl_edge_part, new_edge,                       &
     &    domain_grp%edge_d_grp)
!
      end subroutine const_local_mesh_sf_ele
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine const_local_node_position                              &
     &         (ip, org_node, itl_nod_part, new_node, nod_d_grp)
!
      use t_geometry_data
!
      integer(kind = kint), intent(in) :: ip
      type(node_data), intent(in) :: org_node
      type(internal_4_partitioner), intent(in) :: itl_nod_part
!
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
      type(node_data), intent(inout) :: new_node
!
!
      new_node%numnod = itl_nod_part%num_4_subdomain(ip)
      new_node%internal_node = itl_nod_part%num_inter_sub(ip)
      call alloc_node_geometry_w_sph(new_node)
      call set_local_node                                               &
     &    (ip, org_node, itl_nod_part, new_node, nod_d_grp)
!
      end subroutine const_local_node_position
!
!   --------------------------------------------------------------------
!
      subroutine const_local_element                                    &
     &         (ip, org_ele, itl_ele_part, new_ele, ele_d_grp)
!
      use t_geometry_data
!
      integer(kind = kint), intent(in) :: ip
      type(element_data), intent(in) :: org_ele
      type(internal_4_partitioner), intent(in) :: itl_ele_part
!
      type(domain_group_4_partition), intent(inout) :: ele_d_grp
      type(element_data), intent(inout) :: new_ele
!
!
      new_ele%numele =     itl_ele_part%num_4_subdomain(ip)
      new_ele%nnod_4_ele = org_ele%nnod_4_ele
      call alloc_ele_connect(new_ele)
!
      call set_local_element                                            &
     &   (ip, org_ele, itl_ele_part, new_ele, ele_d_grp)
!
      end subroutine const_local_element
!
!   --------------------------------------------------------------------
!
      subroutine const_local_surface(ip, nnod_4_surf, new_ele,          &
     &          itl_surf_part, new_surf, surf_d_grp)
!
      use t_geometry_data
      use t_surface_data
!
      integer(kind = kint), intent(in) :: ip
      integer(kind = kint), intent(in) :: nnod_4_surf
      type(element_data), intent(in) :: new_ele
      type(internal_4_partitioner), intent(in) :: itl_surf_part
!
      type(domain_group_4_partition), intent(inout) :: surf_d_grp
      type(surface_data), intent(inout) :: new_surf
!
!
      new_surf%numsurf =     itl_surf_part%num_4_subdomain(ip)
      new_surf%nnod_4_surf = nnod_4_surf
      call allocate_surface_connect_type(new_surf, new_ele%numele)
!
      call set_local_surface(ip, itl_surf_part, new_surf, surf_d_grp)
!
      end subroutine const_local_surface
!
!   --------------------------------------------------------------------
!
      subroutine const_local_edge(ip, nnod_4_edge, new_ele, new_surf,   &
     &          itl_edge_part, new_edge, edge_d_grp)
!
      use t_geometry_data
      use t_surface_data
      use t_edge_data
!
      integer(kind = kint), intent(in) :: ip
      integer(kind = kint), intent(in) :: nnod_4_edge
      type(element_data), intent(in) :: new_ele
      type(surface_data), intent(in) :: new_surf
      type(internal_4_partitioner), intent(in) :: itl_edge_part
!
      type(domain_group_4_partition), intent(inout) :: edge_d_grp
      type(edge_data), intent(inout) :: new_edge
!
!
      new_edge%numedge = itl_edge_part%num_4_subdomain(ip)
      new_edge%nnod_4_edge = nnod_4_edge
      call alloc_edge_connect(new_edge, new_surf%numsurf)
      call alloc_edge_4_ele(new_edge, new_ele%numele)
!
      call set_local_edge(ip, itl_edge_part, new_edge, edge_d_grp)
!
      end subroutine const_local_edge
!
!   --------------------------------------------------------------------
!
      end module const_local_mesh_id
