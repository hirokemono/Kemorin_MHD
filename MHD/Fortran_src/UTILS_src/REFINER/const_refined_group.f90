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
      nod_grp_2nd%num_grp = num_bc
      call add_hanging_node_group_num
      call allocate_grp_type_num(nod_grp_2nd)
!
      call count_refined_node_group
      call add_hanging_node_group_name
      call allocate_grp_type_item(nod_grp_2nd)
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
      ele_grp_2nd%num_grp = num_mat
      call allocate_grp_type_num(ele_grp_2nd)
!
      call count_refined_ele_group
      call allocate_grp_type_item(ele_grp_2nd)
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
      sf_grp_2nd%num_grp = num_surf
      call allocate_sf_grp_type_num(sf_grp_2nd)
!
      call count_refined_surf_group
      call allocate_sf_grp_type_item(sf_grp_2nd)
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
