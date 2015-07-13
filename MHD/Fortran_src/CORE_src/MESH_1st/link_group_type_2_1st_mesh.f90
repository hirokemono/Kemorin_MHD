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
      nod_grp%num_grp =  nod_grp1%num_grp
      nod_grp%num_item = nod_grp1%num_item
!
      nod_grp%grp_name =>  nod_grp1%grp_name
      nod_grp%istack_grp => nod_grp1%istack_grp
      nod_grp%item_grp =>   nod_grp1%item_grp
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
      ele_grp%num_grp =  ele_grp1%num_grp
      ele_grp%num_item = ele_grp1%num_item
!
      ele_grp%grp_name =>   ele_grp1%grp_name
      ele_grp%istack_grp => ele_grp1%istack_grp
      ele_grp%item_grp =>   ele_grp1%item_grp
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
      sf_grp%grp_name =>   sf_grp1%grp_name
      sf_grp%istack_grp => sf_grp1%istack_grp
      sf_grp%item_sf_grp => sf_grp1%item_sf_grp
!
      end subroutine link_surface_group_to_type
!
!  ---------------------------------------------------------------------
!
      end module link_group_type_2_1st_mesh
