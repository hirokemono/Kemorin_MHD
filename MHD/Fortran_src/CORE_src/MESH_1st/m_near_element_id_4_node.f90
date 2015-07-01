!
!      module m_near_element_id_4_node
!
!      Written by H. Matsui on Aug., 2006
!
      module m_near_element_id_4_node
!
      use m_precision
      use t_near_mesh_id_4_node
!
      implicit none
!
!> structure of surrounded element for each node
        type(near_mesh) :: near_ele1_tbl
!> structure of surrounded element for each node
        type(near_mesh) :: near_ele1_wide
!
!
!> structure of surrounded surface for each node
        type(near_mesh), save :: near_surf1_tbl
!> structure of surrounded surface for each node
        type(near_mesh), save :: near_surf1_wide
!
!
!> structure of surrounded edge for each node
        type(near_mesh), save :: near_edge_tbl
!> structure of surrounded edge for each node
        type(near_mesh), save :: near_edge_wide
!
      end module m_near_element_id_4_node
