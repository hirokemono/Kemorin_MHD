!
!     module link_geometry_to_1st_mesh
!
!      Written by H. Matsui on Sep., 2006
!
!      subroutine link_single_ele_list
!      subroutine link_node_data
!      subroutine link_element_data
!      subroutine link_surface_data
!      subroutine link_edge_data
!
!      subroutine link_element_geometry
!
!      subroutine link_mesh_parameter_4_smp
!
      module link_geometry_to_1st_mesh
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine link_single_ele_list
!
      use m_2nd_geometry_data
      use link_data_type_to_1st_mesh
!
      call link_single_ele_list_type(surf_2nd, edge_2nd)
!    
      end subroutine link_single_ele_list
!
!  ---------------------------------------------------------------------
!
      subroutine link_node_data
!
      use m_2nd_geometry_data
      use link_data_type_to_1st_mesh
!
!
      call link_node_data_type(node_2nd)
!
      end subroutine link_node_data
!
!  ---------------------------------------------------------------------
!
      subroutine link_element_data
!
      use m_2nd_geometry_data
      use link_data_type_to_1st_mesh
!
!
      call link_element_data_type(ele_2nd)
!
      end subroutine link_element_data
!
!  ---------------------------------------------------------------------
!
      subroutine link_surface_data
!
      use m_2nd_geometry_data
      use link_data_type_to_1st_mesh
!
      call link_surface_data_type(surf_2nd)
!
      end subroutine link_surface_data
!
! ----------------------------------------------------------------------
!
      subroutine link_edge_data
!
      use m_2nd_geometry_data
      use link_data_type_to_1st_mesh
!
      call link_edge_data_type(edge_2nd)
!
      end subroutine link_edge_data
!
! ----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine link_element_geometry
!
      use m_2nd_geometry_data
      use link_data_type_to_1st_mesh
!
      call link_ele_geometry_type(ele_2nd)
!
      end subroutine link_element_geometry
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine link_mesh_parameter_4_smp
!
      use m_2nd_geometry_data
      use link_data_type_to_1st_mesh
!
!
      call link_smp_param_type(node_2nd, ele_2nd, surf_2nd, edge_2nd)
!
      end subroutine link_mesh_parameter_4_smp
!
! -------------------------------------------------------------------
!
      end module link_geometry_to_1st_mesh
