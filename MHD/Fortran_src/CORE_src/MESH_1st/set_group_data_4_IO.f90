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
      nod_grp1%num_grp = num_bc_dummy
      if (nod_grp1%num_grp .gt. 0) then
!
        nod_grp1%num_item = num_nod_bc_dummy
        call allocate_boundary_data
!
        bc_name(1:nod_grp1%num_grp) = bc_name_dummy(1:nod_grp1%num_grp)
        nod_grp1%istack_grp(0:nod_grp1%num_grp)                         &
     &      = bc_istack_dummy(0:nod_grp1%num_grp)
        bc_item(1:nod_grp1%num_item)                                    &
     &      = bc_item_dummy(1:nod_grp1%num_item)
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
      num_bc_dummy = nod_grp1%num_grp
      num_nod_bc_dummy = nod_grp1%num_item
      call allocate_bc_stack_dummy
!
      bc_name_dummy(1:nod_grp1%num_grp) = bc_name(1:nod_grp1%num_grp)
      bc_istack_dummy(0:nod_grp1%num_grp)                               &
     &            = nod_grp1%istack_grp(0:nod_grp1%num_grp)
!
      call allocate_bc_item_dummy
      bc_item_dummy(1:nod_grp1%num_item) = bc_item(1:nod_grp1%num_item)
!
      call deallocate_boundary_data
!
      end subroutine set_node_group_to_IO
!
!-----------------------------------------------------------------------
!
      end module set_group_data_4_IO
