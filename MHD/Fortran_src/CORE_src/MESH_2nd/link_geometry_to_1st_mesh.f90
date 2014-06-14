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
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_geometry_parameter
      use m_geometry_data
!
      nele_2nd = numele
      nnod_4_ele_2nd = nnod_4_ele
!
      ie_2nd =>           ie
      interior_ele_2nd => interior_ele
      e_multi_2nd =>      e_multi
!
      globalelmid_2nd => globalelmid
      elmtyp_2nd => elmtyp
      nodelm_2nd => nodelm
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
      use m_2nd_element_geometry_data
      use m_geometry_parameter
      use m_geometry_data
!
      nele_2nd = numele
!
      x_ele_2nd =>  x_ele
!
      r_ele_2nd =>     r_ele
      ar_ele_2nd =>    ar_ele
      phi_ele_2nd =>   phi_ele
      theta_ele_2nd => theta_ele
      s_ele_2nd =>     s_ele
      as_ele_2nd =>    as_ele
!
      volume_ele_2nd => volume_ele
      a_vol_ele_2nd =>  a_vol_ele
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
      iele_smp_stack_2nd =>  iele_smp_stack
      surf_2nd%istack_surf_smp => isurf_smp_stack
      edge_2nd%istack_edge_smp => iedge_smp_stack
!
      maxnod_4_smp_2nd =     maxnod_4_smp
      max_in_nod_4_smp_2nd = max_in_nod_4_smp
      maxele_4_smp_2nd =     maxele_4_smp
      surf_2nd%max_surf_smp =    maxsurf_4_smp
      edge_2nd%max_edge_smp =    maxedge_4_smp
!
      end subroutine link_mesh_parameter_4_smp
!
! -------------------------------------------------------------------
!
      end module link_geometry_to_1st_mesh
