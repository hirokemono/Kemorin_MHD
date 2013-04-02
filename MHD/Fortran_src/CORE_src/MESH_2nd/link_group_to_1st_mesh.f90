!
!     module link_group_to_1st_mesh
!
!      Written by H. Matsui on Sep., 2006
!
!      subroutine link_node_group
!      subroutine link_element_group
!      subroutine link_surface_group
!
!      subroutine link_element_group_data
!      subroutine link_surface_group_data
!
      module link_group_to_1st_mesh
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
      subroutine link_node_group
!
      use m_2nd_group_data
      use m_node_group
!
      num_bc_2nd = num_bc
      num_nod_bc_2nd = num_nod_bc
!
      bc_name_2nd =>   bc_name
      bc_istack_2nd => bc_istack
      bc_item_2nd =>   bc_item
!
      end subroutine link_node_group
!
!  ---------------------------------------------------------------------
!
      subroutine link_element_group
!
      use m_2nd_group_data
      use m_element_group
!
      num_mat_2nd = num_mat
      num_mat_bc_2nd = num_mat_bc
!
      mat_name_2nd =>   mat_name
      mat_istack_2nd => mat_istack
      mat_item_2nd =>   mat_item
!
      end subroutine link_element_group
!
!  ---------------------------------------------------------------------
!
      subroutine link_surface_group
!
      use m_2nd_group_data
      use m_surface_group
!
      num_surf_2nd = num_surf
      num_surf_bc_2nd = num_surf_bc
!
      surf_name_2nd =>   surf_name
      surf_istack_2nd => surf_istack
      surf_item_2nd =>   surf_item
!
!
      end subroutine link_surface_group
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine link_element_group_data
!
      use m_element_group_connect
      use m_2nd_ele_group_data
!
      ntot_surf_ele_grp_2nd = ntot_surf_ele_grp
      ntot_edge_ele_grp_2nd = ntot_edge_ele_grp
      ntot_node_ele_grp_2nd = ntot_node_ele_grp
!
      nsurf_ele_grp_2nd =>       nsurf_ele_grp
      isurf_stack_ele_grp_2nd => isurf_stack_ele_grp
      isurf_ele_grp_2nd =>       isurf_ele_grp
!
      nedge_ele_grp_2nd =>       nedge_ele_grp
      iedge_stack_ele_grp_2nd => iedge_stack_ele_grp
      iedge_ele_grp_2nd =>       iedge_ele_grp
!
      nnod_ele_grp_2nd =>        nnod_ele_grp
      inod_stack_ele_grp_2nd =>  inod_stack_ele_grp
      inod_ele_grp_2nd =>        inod_ele_grp
!
      end subroutine link_element_group_data
!
!  ---------------------------------------------------------------------
!
      subroutine link_surface_group_data
!
      use m_surface_group_connect
      use m_2nd_surf_group_data
!
      ntot_edge_sf_grp_2nd = ntot_edge_sf_grp
      ntot_node_sf_grp_2nd = ntot_node_sf_grp
!
      isurf_grp_2nd =>          isurf_grp
      isurf_grp_n_2nd =>        isurf_grp_n
!
      nedge_sf_grp_2nd =>       nedge_sf_grp
      iedge_stack_sf_grp_2nd => iedge_stack_sf_grp
      iedge_surf_grp_2nd =>     iedge_surf_grp
!
      nnod_sf_grp_2nd =>        nnod_sf_grp
      inod_stack_sf_grp_2nd =>  inod_stack_sf_grp
      inod_surf_grp_2nd =>      inod_surf_grp
!
      end subroutine link_surface_group_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      end module link_group_to_1st_mesh
