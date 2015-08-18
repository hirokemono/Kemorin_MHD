!const_edge_data.f90
!      module const_edge_data
!
!     Written by H. Matsui on Apr., 2006
!
!      subroutine construct_edge_data(my_rank)
!      subroutine const_element_list_4_edge
!      subroutine const_surface_list_4_edge
!!
!!      subroutine deallocate_ele_4_edge_item
!!      subroutine deallocate_surf_4_edge_item
!
      module const_edge_data
!
      use m_precision
!
      use m_geometry_constants
      use t_sum_hash
!
      implicit none
!
      type(sum_hash_tbl), save, private :: edge_ele_tbl
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine construct_edge_data(id_rank)
!
      use m_machine_parameter
      use m_geometry_data
!
      use set_edge_hash_by_ele
      use set_edge_data_by_ele
!
      use check_geometries
!
      integer(kind = kint), intent(in) :: id_rank
!
!
      call alloc_sum_hash(node1%numnod, ele1%numele, nedge_4_ele,       &
     &    edge1%nnod_4_edge, edge_ele_tbl)
!
!   set hash data for edge elements using sum of local node ID
!
      if (iflag_debug.eq.1) write(*,*) 'const_edge_hash_4_ele'
      call const_edge_hash_4_ele(node1%numnod,                          &
     &    ele1%numele, ele1%nnod_4_ele, edge1%nnod_4_edge, ele1%ie,     &
     &    edge_ele_tbl%num_hash, edge_ele_tbl%istack_hash,              &
     &    edge_ele_tbl%iend_hash, edge_ele_tbl%id_hash,                 &
     &    edge_ele_tbl%iflag_hash)
!
      if (iflag_debug.eq.1) write(*,*) 'count_num_edges_by_ele'
      call count_num_edges_by_ele                                       &
     &   (node1%numnod, ele1%numele, edge1%nnod_4_edge,                 &
     &    edge_ele_tbl%istack_hash, edge_ele_tbl%iend_hash,             &
     &    edge_ele_tbl%iflag_hash, edge1%numedge)
!
      call allocate_edge_connect_type(edge1, surf1%numsurf)
      call allocate_edge_4_ele_type(edge1, ele1%numele)
!
      if (iflag_debug.eq.1) write(*,*) 'set_edges_connect_by_ele'
      call set_edges_connect_by_ele(node1%numnod, ele1%numele,          &
     &    edge1%numedge, ele1%nnod_4_ele, edge1%nnod_4_edge, ele1%ie,   &
     &    edge_ele_tbl%istack_hash, edge_ele_tbl%iend_hash,             &
     &    edge_ele_tbl%id_hash, edge_ele_tbl%iflag_hash,                &
     &    edge1%ie_edge, edge1%iedge_4_ele, edge1%node_on_edge)
!
      if (iflag_debug.eq.1) write(*,*) 'set_edges_connect_4_sf'
      call set_edges_connect_4_sf(node1%numnod, ele1%numele,            &
     &    surf1%numsurf, edge1%numedge, surf1%nnod_4_surf,              &
     &    edge1%nnod_4_edge, surf1%ie_surf, edge1%iedge_4_ele,          &
     &    edge_ele_tbl%istack_hash, edge_ele_tbl%id_hash,               &
     &    edge_ele_tbl%iflag_hash, edge1%ie_edge, edge1%iedge_4_sf)
!
      call dealloc_sum_hash(edge_ele_tbl)
!
!
!      call check_edge_data(id_rank)
!      call check_edge_hexa_data(id_rank)
!
      end subroutine construct_edge_data
!
!------------------------------------------------------------------
!
      subroutine const_element_list_4_edge
!
      use m_geometry_data
      use set_element_list_4_surface
!
!
      call alloc_ele_4_edge_num_type(edge1)
      call count_ele_list_4_edge                                        &
     &   (ele1%numele, edge1%numedge, nedge_4_ele, edge1%iedge_4_ele,   &
     &    edge1%ntot_iele_4_edge, edge1%num_iele_4_edge,                &
     &    edge1%istack_iele_4_edge)
!
      call alloc_ele_4_edge_item_type(edge1)
      call set_ele_list_4_edge(ele1%numele, edge1%numedge, nedge_4_ele, &
     &    edge1%iedge_4_ele, edge1%ntot_iele_4_edge,                    &
     &    edge1%num_iele_4_edge, edge1%istack_iele_4_edge,              &
     &    edge1%iele_4_edge)
!
      end subroutine const_element_list_4_edge
!
!------------------------------------------------------------------
!
      subroutine const_surface_list_4_edge
!
      use m_geometry_data
      use set_element_list_4_surface
!
!
      call alloc_surf_4_edge_num_type(edge1)
      call count_ele_list_4_edge(surf1%numsurf, edge1%numedge,          &
     &    nedge_4_surf, edge1%iedge_4_sf, edge1%ntot_isurf_4_edge,      &
     &    edge1%num_isurf_4_edge, edge1%istack_isurf_4_edge)
!
      call alloc_surf_4_edge_item_type(edge1)
      call set_ele_list_4_edge(surf1%numsurf, edge1%numedge,            &
     &    nedge_4_surf, edge1%iedge_4_sf, edge1%ntot_isurf_4_edge,      &
     &    edge1%num_isurf_4_edge, edge1%istack_isurf_4_edge,            &
     &    edge1%isurf_4_edge)
!
      end subroutine const_surface_list_4_edge
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_ele_4_edge_item
!
      use m_geometry_data
!
      call dealloc_ele_4_edge_item_type(edge1)
!
      end subroutine deallocate_ele_4_edge_item
!
! ------------------------------------------------------
!
      subroutine deallocate_surf_4_edge_item
!
      use m_geometry_data
!
      call dealloc_surf_4_edge_item_type(edge1)
!
      end subroutine deallocate_surf_4_edge_item
!
! ------------------------------------------------------
!
      end module const_edge_data
