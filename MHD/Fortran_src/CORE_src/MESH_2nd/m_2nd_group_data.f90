!
!      module m_2nd_group_data
!
!      Written by H. Matsui on Mar., 2006
!
!
!      subroutine allocate_2nd_node_group
!      subroutine allocate_2nd_element_group
!      subroutine allocate_2nd_surface_group
!
!      subroutine allocate_2nd_node_grp_num
!      subroutine allocate_2nd_ele_grp_num
!      subroutine allocate_2nd_surf_grp_num
!      subroutine allocate_2nd_node_grp_item
!      subroutine allocate_2nd_ele_grp_item
!      subroutine allocate_2nd_surf_grp_item
!
!      subroutine deallocate_2nd_groups
!      subroutine deallocate_2nd_node_group
!      subroutine deallocate_2nd_element_group
!      subroutine deallocate_2nd_surface_group
!
!      subroutine disconnect_2nd_group
!      subroutine disconnect_2nd_node_group
!      subroutine disconnect_2nd_element_group
!      subroutine disconnect_2nd_surface_group
!
!       subroutine allocate_2nd_node_grp_num_smp
!       subroutine allocate_2nd_ele_grp_num_smp
!       subroutine allocate_2nd_surf_grp_num_smp
!
!       subroutine deallocate_2nd_node_grp_num_smp
!       subroutine deallocate_2nd_ele_grp_num_smp
!       subroutine deallocate_2nd_surf_grp_num_smp
!
      module m_2nd_group_data
!
      use m_precision
      use t_group_data
      use t_group_connects
!
      implicit none
!
!
      type(group_data), save :: nod_grp_2nd
!
      type(group_data), save :: ele_grp_2nd
!
      type(surface_group_data), save :: sf_grp_2nd
!
      type(element_group_table), save :: ele_grp_tbl_2nd
!
      type(surface_group_table), save :: sf_grp_tbl_2nd
!
!
      end module  m_2nd_group_data
