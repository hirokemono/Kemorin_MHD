!
!     module set_2nd_group_data_from_IO
!
!      written by H. Matsui on June, 2007
!
!      subroutine s_set_2nd_group_data_from_IO
!
!      subroutine set_2nd_node_group_from_IO
!      subroutine set_2nd_element_group_from_IO
!      subroutine set_2nd_surface_group_from_IO
!
      module set_2nd_group_data_from_IO
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
      subroutine s_set_2nd_group_data_from_IO
!
      call set_2nd_node_group_from_IO
      call set_2nd_element_group_from_IO
      call set_2nd_surface_group_from_IO
!
      end subroutine s_set_2nd_group_data_from_IO
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_2nd_node_group_from_IO
!
!   set node group
!
      num_bc_2nd = num_bc_dummy
      if (num_bc_2nd/=0) then
!
        num_nod_bc_2nd = num_nod_bc_dummy
        call allocate_2nd_node_group
!
        bc_name_2nd(1:num_bc_2nd) =     bc_name_dummy(1:num_bc_2nd)
        bc_istack_2nd(0:num_bc_2nd) =   bc_istack_dummy(0:num_bc_2nd)
        bc_item_2nd(1:num_nod_bc_2nd) = bc_item_dummy(1:num_nod_bc_2nd)
      end if
      call deallocate_bc_item_dummy
!
      end subroutine set_2nd_node_group_from_IO
!
!-----------------------------------------------------------------------
!
      subroutine set_2nd_element_group_from_IO
!
!    set element group
!
      num_mat_2nd =     num_mat_dummy
      if (num_mat_2nd/=0) then
!
        num_mat_bc_2nd = num_mat_bc_dummy
        call allocate_2nd_element_group
!
        mat_name_2nd(1:num_mat_2nd)                                     &
     &        = mat_name_dummy(1:num_mat_2nd)
        mat_istack_2nd(0:num_mat_2nd)                                   &
     &        = mat_istack_dummy(0:num_mat_2nd)
        mat_item_2nd(1:num_mat_bc_2nd)                                  &
     &        = mat_item_dummy(1:num_mat_bc_2nd)
      end if
      call deallocate_bc_ele_item_dummy
!
      end subroutine set_2nd_element_group_from_IO
!
!-----------------------------------------------------------------------
!
      subroutine set_2nd_surface_group_from_IO
!
!   set surface group
!
      num_surf_2nd = num_surf_dummy
      if (num_surf_2nd/=0) then
!
        num_surf_bc_2nd = num_surf_bc_dummy
        call allocate_2nd_surface_group
!
        surf_name_2nd(1:num_surf_2nd)                                   &
     &        = surf_name_dummy(1:num_surf_2nd)
        surf_istack_2nd(0:num_surf_2nd)                                 &
     &        = surf_istack_dummy(0:num_surf_2nd)
        surf_item_2nd(1,1:num_surf_bc_2nd)                              &
     &        = surf_item_dummy(1:num_surf_bc_2nd,1)
        surf_item_2nd(2,1:num_surf_bc_2nd)                              &
     &        = surf_item_dummy(1:num_surf_bc_2nd,2)
      end if
      call deallocate_bc_sf_item_dummy
!
      end subroutine set_2nd_surface_group_from_IO
!
!  ---------------------------------------------------------------------
!
      end module set_2nd_group_data_from_IO
