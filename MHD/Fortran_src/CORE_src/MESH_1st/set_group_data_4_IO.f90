!set_group_data_4_IO.f90
!     module set_group_data_4_IO
!
!      written by H. Matsui on Dec., 2006
!
!      subroutine copy_group_data_from_IO
!      subroutine copy_group_data_to_IO
!
      module set_group_data_4_IO
!
      use m_precision
!
      implicit  none
!
      private :: copy_node_group_from_IO, set_node_group_to_IO
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_group_data_from_IO
!
      use m_element_group
      use m_surface_group
      use set_group_types_4_IO
!
!
      call copy_node_group_from_IO
      call set_ele_grp_type_from_IO(ele_grp1)
      call set_surf_grp_type_from_IO(sf_grp1)
!
      end subroutine copy_group_data_from_IO
!
!-----------------------------------------------------------------------
!
      subroutine copy_group_data_to_IO
!
      use m_element_group
      use m_surface_group
      use set_group_types_4_IO
!
!
      call set_node_group_to_IO
      call set_ele_grp_type_to_IO(ele_grp1)
      call set_surface_grp_type_to_IO(sf_grp1)
!
      call deallocate_grp_type(ele_grp1)
      call deallocate_sf_grp_type(sf_grp1)
!
      end subroutine copy_group_data_to_IO
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_node_group_from_IO
!
      use m_read_boundary_data
      use m_node_group
!
!   set node group
!
      num_bc = num_bc_dummy
      if (num_bc/=0) then
!
        num_nod_bc = num_nod_bc_dummy
        call allocate_boundary_data
!
        bc_name(1:num_bc) =     bc_name_dummy(1:num_bc)
        bc_istack(0:num_bc) =   bc_istack_dummy(0:num_bc)
        bc_item(1:num_nod_bc) = bc_item_dummy(1:num_nod_bc)
      end if
      call deallocate_bc_item_dummy
!
      end subroutine copy_node_group_from_IO
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_node_group_to_IO
!
      use m_node_group
      use m_read_boundary_data
!
!
      num_bc_dummy = num_bc
      num_nod_bc_dummy = num_nod_bc
      call allocate_bc_stack_dummy
!
      bc_name_dummy(1:num_bc) = bc_name(1:num_bc)
      bc_istack_dummy(0:num_bc) = bc_istack(0:num_bc)
!
      call allocate_bc_item_dummy
      bc_item_dummy(1:num_nod_bc) = bc_item(1:num_nod_bc)
!
      call deallocate_boundary_data
!
      end subroutine set_node_group_to_IO
!
!-----------------------------------------------------------------------
!
      end module set_group_data_4_IO
