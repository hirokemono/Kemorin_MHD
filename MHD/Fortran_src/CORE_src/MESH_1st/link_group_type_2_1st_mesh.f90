!link_group_type_2_1st_mesh.f90
!     module link_group_type_2_1st_mesh
!
!      Written by H. Matsui on Sep., 2006
!
!      subroutine link_node_group_to_type(nod_grp)
!      subroutine link_element_group_to_type(ele_grp)
!      subroutine link_surface_group_to_type(sf_grp)
!
      module link_group_type_2_1st_mesh
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
      subroutine link_node_group_to_type(nod_grp)
!
      use t_group_data
      use m_node_group
!
      type(group_data), intent(inout) :: nod_grp
!
      nod_grp%num_grp = num_bc
      nod_grp%num_item = num_nod_bc
!
      nod_grp%grp_name =>  bc_name
      nod_grp%istack_grp => bc_istack
      nod_grp%item_grp =>   bc_item
!
      end subroutine link_node_group_to_type
!
!  ---------------------------------------------------------------------
!
      subroutine link_element_group_to_type(ele_grp)
!
      use t_group_data
      use m_element_group
!
      type(group_data), intent(inout) :: ele_grp
!
      ele_grp%num_grp =  num_mat
      ele_grp%num_item = num_mat_bc
!
      ele_grp%grp_name =>   mat_name
      ele_grp%istack_grp => mat_istack
      ele_grp%item_grp =>   mat_item
!
      end subroutine link_element_group_to_type
!
!  ---------------------------------------------------------------------
!
      subroutine link_surface_group_to_type(sf_grp)
!
      use t_group_data
      use m_surface_group
!
      type(surface_group_data), intent(inout) :: sf_grp
!
      sf_grp%num_grp =  sf_grp1%num_grp
      sf_grp%num_item = sf_grp1%num_item
!
      sf_grp%grp_name =>   surf_name
      sf_grp%istack_grp => surf_istack
      sf_grp%item_sf_grp => surf_item
!
      end subroutine link_surface_group_to_type
!
!  ---------------------------------------------------------------------
!
      end module link_group_type_2_1st_mesh
