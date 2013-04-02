!const_local_groups.f90
!      module const_local_groups
!
      module const_local_groups
!
!      Written by H. Matsui on Aug., 2007
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
!      subroutine s_const_local_groups
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_const_local_groups
!
      call const_local_nod_group
      call const_local_ele_group
      call const_local_surf_group
!
      end subroutine s_const_local_groups
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine const_local_nod_group
!
      use m_node_group
      use m_2nd_group_data
!
      num_bc_2nd = num_bc
      call allocate_2nd_node_grp_num
      call count_local_node_group
!
      call allocate_2nd_node_grp_item
      call set_local_node_group
!
      end subroutine const_local_nod_group
!
!  ---------------------------------------------------------------------
!
      subroutine const_local_ele_group
!
      use m_element_group
      use m_2nd_group_data
!
      num_mat_2nd = num_mat
      call allocate_2nd_ele_grp_num
      call count_local_ele_group
!
      call allocate_2nd_ele_grp_item
      call set_local_ele_group
!
      end subroutine const_local_ele_group
!
!  ---------------------------------------------------------------------
!
      subroutine const_local_surf_group
!
      use m_surface_group
      use m_2nd_group_data
!
      num_surf_2nd = num_surf
      call allocate_2nd_surf_grp_num
      call count_local_surf_group
!
      call allocate_2nd_surf_grp_item
      call set_local_surf_group
!
      end subroutine const_local_surf_group
!
!  ---------------------------------------------------------------------
!
      end module const_local_groups
