!
!     module set_group_data_from_type
!
!      written by H. Matsui on June, 2007
!
!      subroutine group_data_from_type(group)
!
      module set_group_data_from_type
!
      use m_precision
!
      implicit  none
!
      private ::  node_group_from_type, element_group_from_type
      private ::  surface_group_from_type
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine group_data_from_type(group)
!
      use t_mesh_data
!
      type(mesh_groups), intent(inout) :: group
!
!
      call node_group_from_type(group%nod_grp)
      call element_group_from_type(group%ele_grp)
      call surface_group_from_type(group%surf_grp)
!
      call dealloc_groups_data(group)
!
      end subroutine group_data_from_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine node_group_from_type(nod_grp)
!
      use m_node_group
      use t_group_data
!
      type(group_data), intent(inout) :: nod_grp
!
!
      num_bc = nod_grp%num_grp
      if (num_bc .gt. 0) then
        num_nod_bc = nod_grp%num_item
        call allocate_boundary_data
!
        bc_name(1:num_bc) =     nod_grp%grp_name(1:num_bc)
        bc_istack(0:num_bc) =   nod_grp%istack_grp(0:num_bc)
        bc_item(1:num_nod_bc) = nod_grp%item_grp(1:num_nod_bc)
      end if
!
      end subroutine node_group_from_type
!
!-----------------------------------------------------------------------
!
      subroutine element_group_from_type(ele_grp)
!
      use m_element_group
      use t_group_data
!
      type(group_data), intent(inout) :: ele_grp
!
!
      num_mat =     ele_grp%num_grp
      if (num_mat .gt. 0) then
        num_mat_bc = ele_grp%num_item
        call allocate_material_data
!
        mat_name(1:num_mat) =    ele_grp%grp_name(1:num_mat)
        mat_istack(0:num_mat) =  ele_grp%istack_grp(0:num_mat)
        mat_item(1:num_mat_bc) = ele_grp%item_grp(1:num_mat_bc)
      end if
!
      end subroutine element_group_from_type
!
!-----------------------------------------------------------------------
!
      subroutine surface_group_from_type(sf_grp)
!
      use m_surface_group
      use t_group_data
!
      type(surface_group_data), intent(inout) :: sf_grp
!
!
      num_surf = sf_grp%num_grp
      if (num_surf .gt. 0) then
        num_surf_bc = sf_grp%num_item
        call allocate_surface_data
!
        surf_name(1:num_surf) =    sf_grp%grp_name(1:num_surf)
        surf_istack(0:num_surf) =  sf_grp%istack_grp(0:num_surf)
        surf_item(1,1:num_surf_bc)= sf_grp%item_sf_grp(1,1:num_surf_bc)
        surf_item(2,1:num_surf_bc)= sf_grp%item_sf_grp(2,1:num_surf_bc)
      end if
!
      end subroutine surface_group_from_type
!
!  ---------------------------------------------------------------------
!
      end module set_group_data_from_type
