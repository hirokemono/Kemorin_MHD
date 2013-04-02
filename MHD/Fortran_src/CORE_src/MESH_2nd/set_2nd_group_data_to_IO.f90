!
!     module set_2nd_group_data_to_IO
!
!      written by H. Matsui on June, 2007
!
!      subroutine s_set_2nd_group_data_to_IO
!
!      subroutine set_2nd_node_group_2_IO
!      subroutine set_2nd_element_group_2_IO
!      subroutine set_2nd_surface_group_2_IO
!
      module set_2nd_group_data_to_IO
!
      use m_precision
!
      use m_2nd_group_data
      use m_read_boundary_data
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_set_2nd_group_data_to_IO
!
      call set_2nd_node_group_2_IO
      call set_2nd_element_group_2_IO
      call set_2nd_surface_group_2_IO
!
      end subroutine s_set_2nd_group_data_to_IO
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_2nd_node_group_2_IO
!
!
      num_bc_dummy = num_bc_2nd
      num_nod_bc_dummy = num_nod_bc_2nd
      call allocate_bc_stack_dummy
!
      bc_name_dummy(1:num_bc_2nd) = bc_name_2nd(1:num_bc_2nd)
      bc_istack_dummy(0:num_bc_2nd) = bc_istack_2nd(0:num_bc_2nd)
!
      call allocate_bc_item_dummy
      bc_item_dummy(1:num_nod_bc_2nd) = bc_item_2nd(1:num_nod_bc_2nd)
!
      end subroutine set_2nd_node_group_2_IO
!
!-----------------------------------------------------------------------
!
      subroutine set_2nd_element_group_2_IO
!
!
      num_mat_dummy = num_mat_2nd
      num_mat_bc_dummy = num_mat_bc_2nd
      call allocate_bc_ele_stack_dummy
!
      mat_name_dummy(1:num_mat_2nd) = mat_name_2nd(1:num_mat_2nd)
      mat_istack_dummy(0:num_mat_2nd) = mat_istack_2nd(0:num_mat_2nd)
!
      call allocate_bc_ele_item_dummy
      mat_item_dummy(1:num_mat_bc_2nd) = mat_item_2nd(1:num_mat_bc_2nd)
!
      end subroutine set_2nd_element_group_2_IO
!
!-----------------------------------------------------------------------
!
      subroutine set_2nd_surface_group_2_IO
!
!
      num_surf_dummy = num_surf_2nd
      num_surf_bc_dummy = num_surf_bc_2nd
      call allocate_bc_sf_stack_dummy
!
      surf_name_dummy(1:num_surf_2nd) = surf_name_2nd(1:num_surf_2nd)
      surf_istack_dummy(0:num_surf_2nd)                                 &
     &      = surf_istack_2nd(0:num_surf_2nd)
!
      call allocate_bc_sf_item_dummy
      surf_item_dummy(1:num_surf_bc_2nd,1)                              &
     &      = surf_item_2nd(1,1:num_surf_bc_2nd)
      surf_item_dummy(1:num_surf_bc_2nd,2)                              &
     &      = surf_item_2nd(2,1:num_surf_bc_2nd)
!
      end subroutine set_2nd_surface_group_2_IO
!
!-----------------------------------------------------------------------
!
      end module set_2nd_group_data_to_IO
