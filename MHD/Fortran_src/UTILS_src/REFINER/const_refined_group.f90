!const_refined_group.f90
!      module const_refined_group
!
!      Writen by H. Matsui on Oct., 2007
!
!      subroutine s_set_refined_group_data
!
      module const_refined_group
!
      use m_precision
!
      implicit none
!
      private :: const_refined_node_group
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_const_refined_group
!
!
      call const_refined_node_group
!
      call const_refined_ele_group
!
      call const_refined_surf_group
!
      end subroutine s_const_refined_group
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine const_refined_node_group
!
      use m_node_group
      use m_2nd_group_data
      use set_refined_node_group
      use find_hanging_surface
!
!
      call allocate_mark_refine_nod_grp
!
      num_bc_2nd = num_bc
      call add_hanging_node_group_num
      call allocate_2nd_node_grp_num
!
      call count_refined_node_group
      call add_hanging_node_group_name
      call allocate_2nd_node_grp_item
!
      write(*,*) 's_set_refined_node_group'
      call s_set_refined_node_group
      call add_hanging_node_group_item
      call deallocate_mark_refine_nod_grp
!
      end subroutine const_refined_node_group
!
!  ---------------------------------------------------------------------
!
      subroutine const_refined_ele_group
!
      use m_element_group
      use m_2nd_group_data
      use set_refined_ele_group
!
!
      num_mat_2nd = num_mat
      call allocate_2nd_ele_grp_num
!
      call count_refined_ele_group
      call allocate_2nd_ele_grp_item
!
      write(*,*) 's_set_refined_ele_group'
      call s_set_refined_ele_group
!
      end subroutine const_refined_ele_group
!
!  ---------------------------------------------------------------------
!
      subroutine const_refined_surf_group
!
      use m_surface_group
      use m_2nd_group_data
      use set_refined_surf_group
!
!
      call allocate_mark_refine_sf_grp
!
      num_surf_2nd = num_surf
      call allocate_2nd_surf_grp_num
!
      call count_refined_surf_group
      call allocate_2nd_surf_grp_item
!
      write(*,*) 's_set_refined_surf_group'
      call s_set_refined_surf_group
      call deallocate_mark_refine_sf_grp
!
      end subroutine const_refined_surf_group
!
!  ---------------------------------------------------------------------
!
      end module const_refined_group
