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
      use m_2nd_group_data
      use link_group_type_2_1st_mesh
!
!
      call link_1st_ele_grp_connect_type(ele_grp_tbl_2nd)
!
      end subroutine link_element_group_data
!
!  ---------------------------------------------------------------------
!
      subroutine link_surface_group_data
!
      use m_2nd_group_data
      use link_group_type_2_1st_mesh
!
!
      call link_1st_surf_grp_conn_type(sf_grp_tbl_2nd)
!
      end subroutine link_surface_group_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      end module link_group_to_1st_mesh
