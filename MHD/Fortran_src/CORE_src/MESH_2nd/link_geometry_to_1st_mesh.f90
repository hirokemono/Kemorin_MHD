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
      use m_2nd_geometry_param
      use m_geometry_parameter
!
      node_on_sf_2nd =>   node_on_sf
      node_on_sf_n_2nd => node_on_sf_n
!
      node_on_edge_2nd =>    node_on_edge
      node_on_edge_sf_2nd => node_on_edge_sf
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
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_geometry_parameter
      use m_geometry_data
!
      nsurf_2nd =       numsurf
      nnod_4_surf_2nd = nnod_4_surf
!
      ie_surf_2nd =>       ie_surf
      isf_4_ele_2nd =>     isf_4_ele
      interior_surf_2nd => interior_surf
!
      end subroutine link_surface_data
!
! ----------------------------------------------------------------------
!
      subroutine link_edge_data
!
      use m_2nd_geometry_param
      use m_2nd_geometry_data
      use m_geometry_parameter
      use m_geometry_data
!
      nedge_2nd =       numedge
      nnod_4_edge_2nd = nnod_4_edge
!
      ie_edge_2nd =>      ie_edge
      iedge_4_sf_2nd =>    iedge_4_sf
      iedge_4_ele_2nd =>   iedge_4_ele
      interior_edge_2nd => interior_edge
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
      use m_geometry_parameter
!
!
      inod_smp_stack_2nd =>  inod_smp_stack
      inter_smp_stack_2nd => inter_smp_stack
      iele_smp_stack_2nd =>  iele_smp_stack
      isurf_smp_stack_2nd => isurf_smp_stack
      iedge_smp_stack_2nd => iedge_smp_stack
!
      maxnod_4_smp_2nd =     maxnod_4_smp
      max_in_nod_4_smp_2nd = max_in_nod_4_smp
      maxele_4_smp_2nd =     maxele_4_smp
      maxsurf_4_smp_2nd =    maxsurf_4_smp
      maxedge_4_smp_2nd =    maxedge_4_smp
!
      end subroutine link_mesh_parameter_4_smp
!
! -------------------------------------------------------------------
!
      end module link_geometry_to_1st_mesh
