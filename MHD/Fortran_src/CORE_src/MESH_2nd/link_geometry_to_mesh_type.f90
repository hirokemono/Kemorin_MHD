!link_geometry_to_mesh_type.f90
!     module link_geometry_to_mesh_type
!
!      Written by H. Matsui on Sep., 2006
!
!       subroutine link_2nd_1ele_list_type(surf, edge)
!       subroutine link_2nd_node_type(node)
!       subroutine link_2nd_ele_connect_type(ele)
!       subroutine link_2nd_surf_connect_type(surf)
!       subroutine link_2nd_edge_connect_type(edge)
!
!       subroutine link_2nd_ele_geom_to_type(ele)
!         type(node_data), intent(in) :: node
!         type(element_data), intent(in) :: ele
!         type(surface_data), intent(in) :: surf
!         type(edge_data), intent(in) :: edge
!
      module link_geometry_to_mesh_type
!
      use m_precision
!
      use m_2nd_geometry_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
       subroutine link_2nd_1ele_list_type(surf, edge)
!
       use t_surface_data
       use t_edge_data
!
       type(surface_data), intent(in) :: surf
       type(edge_data), intent(in) :: edge
!
       surf_2nd%node_on_sf =>   surf%node_on_sf
       surf_2nd%node_on_sf_n => surf%node_on_sf_n
!
       edge_2nd%node_on_edge =>    edge%node_on_edge
       edge_2nd%node_on_edge_sf => edge%node_on_edge_sf
!
       end subroutine link_2nd_1ele_list_type
!
!  ---------------------------------------------------------------------
!
       subroutine link_2nd_node_type(node)
!
       use t_geometry_data
!
       type(node_data), intent(in) :: node
!
!
       call link_new_nod_geometry_type(node, node_2nd)
!
       end subroutine link_2nd_node_type
!
!  ---------------------------------------------------------------------
!
      subroutine link_2nd_ele_connect_type(ele)
!
      use t_geometry_data
!
      type(element_data), intent(in) :: ele
!
!
      call link_new_ele_connect_type(ele, ele_2nd)
      call link_new_ele_connect_type(ele, ele_2nd)
!
       end subroutine link_2nd_ele_connect_type
!
!  ---------------------------------------------------------------------
!
       subroutine link_2nd_surf_connect_type(surf)
!
       use t_surface_data
!
       type(surface_data), intent(in) :: surf
!
!
       surf_2nd%numsurf =       surf%numsurf
       surf_2nd%nnod_4_surf =   surf%nnod_4_surf
       surf_2nd%internal_surf = surf%internal_surf
!
       surf_2nd%max_surf_smp =    surf%max_surf_smp
       surf_2nd%istack_surf_smp => surf%istack_surf_smp
!
       surf_2nd%isurf_global =>  surf%isurf_global
       surf_2nd%ie_surf =>       surf%ie_surf
       surf_2nd%isf_4_ele =>     surf%isf_4_ele
       surf_2nd%interior_surf => surf%interior_surf
!
       end subroutine link_2nd_surf_connect_type
!
! ----------------------------------------------------------------------
!
       subroutine link_2nd_edge_connect_type(edge)
!
       use t_edge_data
!
       type(edge_data), intent(in) :: edge
!
!
       edge_2nd%numedge =    edge%numedge
       edge_2nd%nnod_4_edge = edge%nnod_4_edge
       edge_2nd%internal_edge =    edge%internal_edge
!
       edge_2nd%max_edge_smp =    edge%max_edge_smp
       edge_2nd%istack_edge_smp => edge%istack_edge_smp
!
       edge_2nd%iedge_global =>  edge%iedge_global
       edge_2nd%ie_edge =>       edge%ie_edge
       edge_2nd%iedge_4_sf =>    edge%iedge_4_sf
       edge_2nd%iedge_4_ele =>   edge%iedge_4_ele
       edge_2nd%interior_edge => edge%interior_edge
!
       end subroutine link_2nd_edge_connect_type
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine link_2nd_ele_geom_to_type(ele)
!
       use t_geometry_data
!
       type(element_data), intent(in) :: ele
!
!
       call link_new_ele_geometry_type(ele, ele_2nd)
!
       end subroutine link_2nd_ele_geom_to_type
!
!  ---------------------------------------------------------------------
!
      end module link_geometry_to_mesh_type
