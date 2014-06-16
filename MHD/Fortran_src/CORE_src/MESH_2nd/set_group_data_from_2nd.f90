!
!     module set_group_data_from_2nd
!
!      written by H. Matsui on June, 2007
!
      module set_group_data_from_2nd
!
      use m_precision
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
      subroutine s_set_group_data_from_2nd(group)
!
      use t_mesh_data
!
      type(mesh_groups), intent(inout) :: group
!
!
      call set_node_group_from_2nd(group%nod_grp)
      call set_element_group_from_2nd(group%ele_grp)
      call set_surface_group_from_2nd(group%surf_grp)
!
!
      call deallocate_sf_grp_type_num(group%surf_grp)
      call deallocate_sf_grp_type_item(group%surf_grp)
!
      call deallocate_grp_type_num(group%ele_grp)
      call deallocate_grp_type_item(group%ele_grp)
!
      call deallocate_grp_type_num(group%nod_grp)
      call deallocate_grp_type_item(group%nod_grp)
!
!
      end subroutine s_set_group_data_from_2nd
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_node_group_from_2nd(nod_grp)
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
      end subroutine set_node_group_from_2nd
!
!-----------------------------------------------------------------------
!
      subroutine set_element_group_from_2nd(ele_grp)
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
      end subroutine set_element_group_from_2nd
!
!-----------------------------------------------------------------------
!
      subroutine set_surface_group_from_2nd(sf_grp)
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
      end subroutine set_surface_group_from_2nd
!
!  ---------------------------------------------------------------------
!
      end module set_group_data_from_2nd
