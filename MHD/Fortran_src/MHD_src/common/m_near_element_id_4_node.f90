!
!      module m_near_element_id_4_node
!
!      Written by H. Matsui on Aug., 2006
!
      module m_near_element_id_4_node
!
      use m_precision
      use t_near_mesh_id_4_node
      use t_hanging_mesh_data
      use t_next_node_ele_4_node
!
      implicit none
!
!
!> structure of surrounded node, element, surface, edge for each node
      type(fem_near_mesh), save :: near_mesh1
!
!>  Structure for hanging nodes on mesh
      type(hanging_mesh), save :: hang1
!>  Structure of neighbouring node list for hanging node
      type(next_nod_id_4_nod), save :: neib_hang1
!
      end module m_near_element_id_4_node
