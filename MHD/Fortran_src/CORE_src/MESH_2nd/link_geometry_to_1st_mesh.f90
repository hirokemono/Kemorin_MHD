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
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_geometry_parameter
      use m_geometry_data
!
      nnod_2nd =         numnod
      internal_nod_2nd = internal_node
!
      globalnodid_2nd => globalnodid
      xx_2nd =>  xx
!
      radius_2nd =>   radius
      a_radius_2nd => a_radius
      theta_2nd =>    colatitude
      phi_2nd =>      longitude
      s_cyl_2nd =>    s_cylinder
      a_s_cyl_2nd =>  a_s_cylinder
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
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_geometry_parameter
      use m_geometry_data
!
      ele_2nd%numele = numele
!
      ele_2nd%x_ele =>  x_ele
!
      ele_2nd%r_ele =>     r_ele
      ele_2nd%ar_ele =>    ar_ele
      ele_2nd%phi_ele =>   phi_ele
      ele_2nd%theta_ele => theta_ele
      ele_2nd%s_ele =>     s_ele
      ele_2nd%as_ele =>    as_ele
!
      ele_2nd%volume_ele => volume_ele
      ele_2nd%a_vol_ele =>  a_vol_ele
!
      end subroutine link_element_geometry
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine link_mesh_parameter_4_smp
!
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_geometry_parameter
!
!
      inod_smp_stack_2nd =>  inod_smp_stack
      inter_smp_stack_2nd => inter_smp_stack
      ele_2nd%istack_ele_smp =>  iele_smp_stack
      surf_2nd%istack_surf_smp => isurf_smp_stack
      edge_2nd%istack_edge_smp => iedge_smp_stack
!
      maxnod_4_smp_2nd =     maxnod_4_smp
      max_in_nod_4_smp_2nd = max_in_nod_4_smp
      ele_2nd%max_ele_smp =     maxele_4_smp
      surf_2nd%max_surf_smp =    maxsurf_4_smp
      edge_2nd%max_edge_smp =    maxedge_4_smp
!
      end subroutine link_mesh_parameter_4_smp
!
! -------------------------------------------------------------------
!
      end module link_geometry_to_1st_mesh
