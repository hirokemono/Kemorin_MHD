!
!      module const_merged_groups
!
!      subroutine const_merged_mesh_groups
!
!      Written by H. Matsui on july, 2005
!
      module const_merged_groups
!
      use m_precision
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_merged_mesh_groups
!
      use m_geometry_data_4_merge
      use count_merged_groups
      use set_merged_groups
!
!
!    count merged number of groups
!
      call count_num_merged_grp(num_pe, sub_nod_grp,                    &
     &    istack_bc_pe, merged_grp%nod_grp%num_grp)
      call count_num_merged_grp(num_pe, sub_ele_grp,                    &
     &    istack_mat_pe, merged_grp%ele_grp%num_grp)
!
      call count_num_merged_sf_grp(num_pe, sub_surf_grp,                &
     &    istack_surf_pe, merged_grp%surf_grp%num_grp)
!
!     allocate group names
!
       call allocate_grp_type_num(merged_grp%nod_grp)
       call allocate_grp_type_num(merged_grp%ele_grp)
       call allocate_sf_grp_type_num(merged_grp%surf_grp)
!
!    set merged group names
!
      call set_merged_grp_name(num_pe, sub_nod_grp,                     &
     &    istack_bc_pe, merged_grp%nod_grp)
      call set_merged_grp_name(num_pe, sub_ele_grp,                     &
     &    istack_mat_pe, merged_grp%ele_grp)
!
      call set_merged_grp_sf_name(num_pe, sub_surf_grp,                 &
     &    istack_surf_pe, merged_grp%surf_grp)
!
!    count merged group items
!
      call allocate_flags_merged_grp
!
      call count_merged_node_group
      call count_merged_element_group
      call count_merged_surface_group
!
!    allocate merged group items
!
       call allocate_grp_type_item(merged_grp%nod_grp)
       call allocate_grp_type_item(merged_grp%ele_grp)
       call allocate_sf_grp_type_item(merged_grp%surf_grp)
!
!    set merged group data
!
      call set_merged_node_group
      call set_merged_element_group
      call set_merged_surface_group
!
      call deallocate_flags_merged_grp
!
      end subroutine const_merged_mesh_groups
!
!-----------------------------------------------------------------------
!
      end module const_merged_groups
