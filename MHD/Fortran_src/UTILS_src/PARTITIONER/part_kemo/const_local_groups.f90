!const_local_groups.f90
!      module const_local_groups
!
!      Written by H. Matsui on Aug., 2007
!
!      subroutine s_const_local_groups
!
      module const_local_groups
!
      use m_precision
!
      use set_group_4_subdomain
!
      implicit none
!
!
      private :: const_local_nod_group, const_local_ele_group
      private :: const_local_surf_group
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_const_local_groups(newgroup)
!
      use t_mesh_data
!
      type(mesh_groups), intent(inout) :: newgroup
!
!
      call const_local_nod_group(newgroup%nod_grp)
      call const_local_ele_group(newgroup%ele_grp)
      call const_local_surf_group(newgroup%surf_grp)
!
      end subroutine s_const_local_groups
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine const_local_nod_group(new_nod_grp)
!
      use m_node_group
      use t_group_data
!
      type(group_data), intent(inout) :: new_nod_grp
!
!
      new_nod_grp%num_grp = num_bc
      call allocate_grp_type_num(new_nod_grp)
      call count_local_node_group(new_nod_grp)
!
      call allocate_grp_type_item(new_nod_grp)
      call set_local_node_group(new_nod_grp)
!
      end subroutine const_local_nod_group
!
!  ---------------------------------------------------------------------
!
      subroutine const_local_ele_group(new_ele_grp)
!
      use m_element_group
      use t_group_data
!
      type(group_data), intent(inout) :: new_ele_grp
!
!
      new_ele_grp%num_grp = ele_grp1%num_grp
      call allocate_grp_type_num(new_ele_grp)
      call count_local_ele_group(new_ele_grp)
!
      call allocate_grp_type_item(new_ele_grp)
      call set_local_ele_group(new_ele_grp)
!
      end subroutine const_local_ele_group
!
!  ---------------------------------------------------------------------
!
      subroutine const_local_surf_group(new_sf_grp)
!
      use m_surface_group
      use t_group_data
!
      type(surface_group_data), intent(inout) :: new_sf_grp
!
!
      new_sf_grp%num_grp = sf_grp1%num_grp
      call allocate_sf_grp_type_num(new_sf_grp)
      call count_local_surf_group(new_sf_grp)
!
      call allocate_sf_grp_type_item(new_sf_grp)
      call set_local_surf_group(new_sf_grp)
!
      end subroutine const_local_surf_group
!
!  ---------------------------------------------------------------------
!
      end module const_local_groups
