!
!     module set_group_data_from_2nd
!
!      written by H. Matsui on June, 2007
!
      module set_group_data_from_2nd
!
      use m_precision
!
      use m_2nd_group_data
!
      implicit  none
!
      private ::  set_node_group_from_2nd, set_element_group_from_2nd
      private ::  set_surface_group_from_2nd
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_set_group_data_from_2nd
!
!
      call set_node_group_from_2nd
      call set_element_group_from_2nd
      call set_surface_group_from_2nd
!
      call deallocate_2nd_node_group
      call deallocate_2nd_element_group
      call deallocate_2nd_surface_group
!
      end subroutine s_set_group_data_from_2nd
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_node_group_from_2nd
!
      use m_node_group
!
!   set node group
!
      num_bc = num_bc_2nd
      if (num_bc .gt. 0) then
        num_nod_bc = num_nod_bc_2nd
        call allocate_boundary_data
!
        bc_name(1:num_bc) =     bc_name_2nd(1:num_bc)
        bc_istack(0:num_bc) =   bc_istack_2nd(0:num_bc)
        bc_item(1:num_nod_bc) = bc_item_2nd(1:num_nod_bc)
      end if
!
      end subroutine set_node_group_from_2nd
!
!-----------------------------------------------------------------------
!
      subroutine set_element_group_from_2nd
!
      use m_element_group
!
!    set element group
!
      num_mat =     num_mat_2nd
      if (num_mat .gt. 0) then
        num_mat_bc = num_mat_bc_2nd
        call allocate_material_data
!
        mat_name(1:num_mat) =    mat_name_2nd(1:num_mat)
        mat_istack(0:num_mat) =  mat_istack_2nd(0:num_mat)
        mat_item(1:num_mat_bc) = mat_item_2nd(1:num_mat_bc)
      end if
!
      end subroutine set_element_group_from_2nd
!
!-----------------------------------------------------------------------
!
      subroutine set_surface_group_from_2nd
!
      use m_surface_group
!
!   set surface group
!
      num_surf = num_surf_2nd
      if (num_surf .gt. 0) then
        num_surf_bc = num_surf_bc_2nd
        call allocate_surface_data
!
        surf_name(1:num_surf) =    surf_name_2nd(1:num_surf)
        surf_istack(0:num_surf) =  surf_istack_2nd(0:num_surf)
        surf_item(1,1:num_surf_bc) = surf_item_2nd(1,1:num_surf_bc)
        surf_item(2,1:num_surf_bc) = surf_item_2nd(2,1:num_surf_bc)
      end if
!
      end subroutine set_surface_group_from_2nd
!
!  ---------------------------------------------------------------------
!
      end module set_group_data_from_2nd
