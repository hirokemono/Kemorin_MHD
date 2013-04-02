!
!     module const_local_mesh_id
!
!     written by H. Matsui on Sep., 2007
!
!      subroutine s_const_local_meshes(ip)
!      subroutine const_local_mesh_sf_ele(ip)
!
!      subroutine const_local_node_position(ip)
!      subroutine const_local_element(ip)
!      subroutine const_local_surface(ip)
!      subroutine const_local_edge(ip)
!
      module const_local_mesh_id
!
      use m_precision
!
      use m_geometry_parameter
      use m_2nd_geometry_param
      use m_2nd_geometry_data
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
      subroutine s_const_local_meshes(ip)
!
      integer(kind = kint), intent(in) :: ip
!
!
      call const_local_node_position(ip)
      call const_local_element(ip)
!
      end subroutine s_const_local_meshes
!
!   --------------------------------------------------------------------
!
      subroutine const_local_mesh_sf_ele(ip)
!
      integer(kind = kint), intent(in) :: ip
!
!
      call const_local_node_position(ip)
      call const_local_element(ip)
!
      call const_local_surface(ip)
      call const_local_edge(ip)
!
      end subroutine const_local_mesh_sf_ele
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine const_local_node_position(ip)
!
      integer(kind = kint), intent(in) :: ip
!
!
      nnod_2nd =         numnod_4_subdomain(ip)
      internal_nod_2nd = num_intnod_sub(ip)
      call allocate_2nd_node_position
      call set_local_node(ip)
!
      end subroutine const_local_node_position
!
!   --------------------------------------------------------------------
!
      subroutine const_local_element(ip)
!
      integer(kind = kint), intent(in) :: ip
!
!
      nele_2nd =       numele_4_subdomain(ip)
      nnod_4_ele_2nd = nnod_4_ele
      call allocate_2nd_element_connect
!
      call set_local_element(ip)
!
      end subroutine const_local_element
!
!   --------------------------------------------------------------------
!
      subroutine const_local_surface(ip)
!
      integer(kind = kint), intent(in) :: ip
!
!
      nsurf_2nd =       numsurf_4_subdomain(ip)
      nnod_4_surf_2nd = nnod_4_surf
      call allocate_2nd_surface_connect
!
      call set_local_surface(ip)
!
      end subroutine const_local_surface
!
!   --------------------------------------------------------------------
!
      subroutine const_local_edge(ip)
!
      integer(kind = kint), intent(in) :: ip
!
!
      nedge_2nd =        numedge_4_subdomain(ip)
      nnod_4_edge_2nd = nnod_4_edge
      call allocate_2nd_edge_connect
      call allocate_2nd_edge_4_ele
!
      call set_local_edge(ip)
!
      end subroutine const_local_edge
!
!   --------------------------------------------------------------------
!
      end module const_local_mesh_id
