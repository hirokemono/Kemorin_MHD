!
!     module const_local_mesh_id
!
!     written by H. Matsui on Sep., 2007
!
!      subroutine s_const_local_meshes(ip, newmesh)
!      subroutine const_local_mesh_sf_ele                               &
!     &         (ip, newmesh, new_surf, new_edge)
!
!      subroutine const_local_node_position(ip, new_node)
!      subroutine const_local_element(ip, new_ele)
!      subroutine const_local_surface(ip, new_ele, new_surf)
!      subroutine const_local_edge(ip, new_surf, new_edge)
!
      module const_local_mesh_id
!
      use m_precision
!
      use m_geometry_parameter
      use m_internal_4_partitioner
      use set_local_by_subdomain_tbl
!
      implicit  none
!
!      private :: const_local_node_position
!      private :: const_local_element, const_local_surface
!      private :: const_local_edge
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine s_const_local_meshes(ip, newmesh)
!
      use t_mesh_data
!
      integer(kind = kint), intent(in) :: ip
      type(mesh_geometry), intent(inout) :: newmesh
!
!
      call const_local_node_position(ip, newmesh%node)
      call const_local_element(ip, newmesh%ele)
!
      end subroutine s_const_local_meshes
!
!   --------------------------------------------------------------------
!
      subroutine const_local_mesh_sf_ele                                &
     &         (ip, newmesh, new_surf, new_edge)
!
      use t_mesh_data
      use t_surface_data
      use t_edge_data
!
      integer(kind = kint), intent(in) :: ip
      type(mesh_geometry), intent(inout) :: newmesh
      type(surface_data), intent(inout) :: new_surf
      type(edge_data), intent(inout) :: new_edge
!
!
      call const_local_node_position(ip, newmesh%node)
      call const_local_element(ip, newmesh%ele)
!
      call const_local_surface(ip, newmesh%ele, new_surf)
      call const_local_edge(ip, newmesh%ele, new_surf, new_edge)
!
      end subroutine const_local_mesh_sf_ele
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine const_local_node_position(ip, new_node)
!
      use t_geometry_data
!
      integer(kind = kint), intent(in) :: ip
      type(node_data), intent(inout) :: new_node
!
!
      new_node%numnod = numnod_4_subdomain(ip)
      new_node%internal_node = num_intnod_sub(ip)
      call allocate_node_geometry_type(new_node)
      call set_local_node(ip, new_node)
!
      end subroutine const_local_node_position
!
!   --------------------------------------------------------------------
!
      subroutine const_local_element(ip, new_ele)
!
      use t_geometry_data
!
      integer(kind = kint), intent(in) :: ip
      type(element_data), intent(inout) :: new_ele
!
!
      new_ele%numele =       numele_4_subdomain(ip)
      new_ele%nnod_4_ele = nnod_4_ele
      call allocate_ele_connect_type(new_ele)
!
      call set_local_element(ip, new_ele)
!
      end subroutine const_local_element
!
!   --------------------------------------------------------------------
!
      subroutine const_local_surface(ip, new_ele, new_surf)
!
      use t_geometry_data
      use t_surface_data
!
      integer(kind = kint), intent(in) :: ip
      type(element_data), intent(in) :: new_ele
      type(surface_data), intent(inout) :: new_surf
!
!
      new_surf%numsurf =       numsurf_4_subdomain(ip)
      new_surf%nnod_4_surf = nnod_4_surf
      call allocate_surface_connect_type(new_surf, new_ele%numele)
!
      call set_local_surface(ip, new_surf)
!
      end subroutine const_local_surface
!
!   --------------------------------------------------------------------
!
      subroutine const_local_edge(ip, new_ele, new_surf, new_edge)
!
      use t_geometry_data
      use t_surface_data
      use t_edge_data
!
      integer(kind = kint), intent(in) :: ip
      type(element_data), intent(in) :: new_ele
      type(surface_data), intent(in) :: new_surf
      type(edge_data), intent(inout) :: new_edge
!
!
      new_edge%numedge = numedge_4_subdomain(ip)
      new_edge%nnod_4_edge = nnod_4_edge
      call allocate_edge_connect_type(new_edge, new_surf%numsurf)
      call allocate_edge_4_ele_type(new_edge, new_ele%numele)
!
      call set_local_edge(ip, new_edge)
!
      end subroutine const_local_edge
!
!   --------------------------------------------------------------------
!
      end module const_local_mesh_id
