!
!      module const_merged_groups
!
!!      subroutine allocate_subdomain_grp_stack(num_pe)
!!      subroutine deallocate_subdomain_grp_stack
!!
!!      subroutine count_num_group_w_overlap(num_pe,                    &
!!     &          sub_nod_grp, sub_ele_grp, sub_surf_grp)
!!      subroutine count_merged_mesh_groups(num_pe,                     &
!!     &          sub_nod_grp, sub_ele_grp, sub_surf_grp, merged_grp)
!!
!!      subroutine const_merged_mesh_groups                             &
!!     &         (num_pe, subdomain, merged, merge_tbl,                 &
!!     &          sub_nod_grp, sub_ele_grp, sub_surf_grp, merged_grp)
!!      subroutine const_merged_overlapped_groups(num_pe,               &
!!     &          sub_nod_grp, sub_ele_grp, sub_surf_grp, merged_grp)
!!        type(merged_stacks), intent(in) :: merge_tbl
!!        type(mesh_geometry), intent(in) :: subdomain(num_pe)
!!        type(group_data), intent(in) :: sub_nod_grp(num_pe)
!!        type(group_data), intent(in) :: sub_ele_grp(num_pe)
!!        type(surface_group_data), intent(in) :: sub_surf_grp(num_pe)
!!        type(mesh_groups), intent(inout) :: merged_grp
!
!      Written by H. Matsui on july, 2005
!
      module const_merged_groups
!
      use m_precision
!
      implicit none
!
!
!>      stacks of node group data
      integer (kind=kint), allocatable :: istack_bc_pe(:)
!>      stacks of element group data
      integer (kind=kint), allocatable :: istack_mat_pe(:)
!>      stacks of surface group data
      integer (kind=kint), allocatable :: istack_surf_pe(:)
!
      private :: istack_bc_pe, istack_mat_pe, istack_surf_pe
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_subdomain_grp_stack(num_pe)
!
      integer, intent(in) :: num_pe
!
       allocate( istack_bc_pe(0:num_pe) )
       allocate( istack_mat_pe(0:num_pe) )
       allocate( istack_surf_pe(0:num_pe) )
!
       istack_bc_pe = 0
       istack_mat_pe = 0
       istack_surf_pe = 0
!
       end subroutine allocate_subdomain_grp_stack
!
!------------------------------------------------------------------
!
      subroutine deallocate_subdomain_grp_stack
!
!
      deallocate(istack_bc_pe, istack_mat_pe, istack_surf_pe)
!
      end subroutine deallocate_subdomain_grp_stack
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine count_num_group_w_overlap(num_pe,                      &
     &          sub_nod_grp, sub_ele_grp, sub_surf_grp)
!
      use t_group_data
      use count_number_with_overlap
!
      integer, intent(in)  :: num_pe
      type(group_data), intent(in) :: sub_nod_grp(num_pe)
      type(group_data), intent(in) :: sub_ele_grp(num_pe)
      type(surface_group_data), intent(in) :: sub_surf_grp(num_pe)
!
!
      call count_subdomain_ngrp_stack                                   &
     &   (num_pe, sub_nod_grp, istack_bc_pe)
      call count_subdomain_ngrp_stack                                   &
     &   (num_pe, sub_ele_grp, istack_mat_pe)
!
      call count_subdomain_sf_ngrp_stack                                &
     &   (num_pe, sub_surf_grp, istack_surf_pe)
!
      end subroutine count_num_group_w_overlap
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_merged_mesh_groups(num_pe,                       &
     &          sub_nod_grp, sub_ele_grp, sub_surf_grp, merged_grp)
!
      use t_mesh_data
      use t_group_data
      use count_merged_groups
!
      integer, intent(in)  :: num_pe
      type(group_data), intent(in) :: sub_nod_grp(num_pe)
      type(group_data), intent(in) :: sub_ele_grp(num_pe)
      type(surface_group_data), intent(in) :: sub_surf_grp(num_pe)
!
      type(mesh_groups), intent(inout) :: merged_grp
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
      call set_merged_grp_name                                          &
     &   (num_pe, sub_nod_grp, istack_bc_pe, merged_grp%nod_grp)
      call set_merged_grp_name                                          &
     &   (num_pe, sub_ele_grp, istack_mat_pe, merged_grp%ele_grp)
!
      call set_merged_grp_sf_name                                       &
     &   (num_pe, sub_surf_grp, istack_surf_pe, merged_grp%surf_grp)
!
      end subroutine count_merged_mesh_groups
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_merged_mesh_groups                               &
     &         (num_pe, subdomain, merged, merge_tbl,                   &
     &          sub_nod_grp, sub_ele_grp, sub_surf_grp, merged_grp)
!
      use t_mesh_data
      use t_group_data
      use t_merged_geometry_data
      use count_merged_groups
      use set_merged_groups
!
      integer, intent(in)  :: num_pe
      type(mesh_geometry), intent(in) :: merged
      type(merged_stacks), intent(in) :: merge_tbl
      type(mesh_geometry), intent(in) :: subdomain(num_pe)
      type(group_data), intent(in) :: sub_nod_grp(num_pe)
      type(group_data), intent(in) :: sub_ele_grp(num_pe)
      type(surface_group_data), intent(in) :: sub_surf_grp(num_pe)
!
      type(mesh_groups), intent(inout) :: merged_grp
!
!    count merged group items
!
      call allocate_flags_merged_grp(merged)
!
      call count_merged_node_group(num_pe, merge_tbl,                   &
     &    subdomain, sub_nod_grp, merged_grp)
      call count_merged_element_group(num_pe, merge_tbl,                &
     &    subdomain, sub_ele_grp, merged_grp)
      call count_merged_surface_group(num_pe, merge_tbl,                &
     &    subdomain, sub_surf_grp, merged_grp)
!
!    allocate merged group items
!
       call allocate_grp_type_item(merged_grp%nod_grp)
       call allocate_grp_type_item(merged_grp%ele_grp)
       call allocate_sf_grp_type_item(merged_grp%surf_grp)
!
!    set merged group data
!
      call set_merged_node_group(num_pe, merge_tbl,                     &
     &    subdomain, sub_nod_grp, merged_grp)
      call set_merged_element_group(num_pe, merge_tbl,                  &
     &    subdomain, sub_ele_grp, merged_grp)
      call set_merged_surface_group(num_pe, merge_tbl,                  &
     &    subdomain, sub_surf_grp, merged_grp)
!
      call deallocate_flags_merged_grp
!
      end subroutine const_merged_mesh_groups
!
!-----------------------------------------------------------------------
!
      subroutine const_merged_overlapped_groups(num_pe,                 &
     &          sub_nod_grp, sub_ele_grp, sub_surf_grp, merged_grp)
!
      use t_mesh_data
      use t_group_data
      use count_merged_groups
      use set_overlap_groups
!
      integer, intent(in)  :: num_pe
      type(group_data), intent(in) :: sub_nod_grp(num_pe)
      type(group_data), intent(in) :: sub_ele_grp(num_pe)
      type(surface_group_data), intent(in) :: sub_surf_grp(num_pe)
!
      type(mesh_groups), intent(inout) :: merged_grp
!
!    count merged group items
!
!      write(*,*) 'count_group_w_overlap'
      call count_group_w_overlap                                        &
     &    (num_pe, sub_nod_grp, merged_grp%nod_grp)
!      write(*,*) 'count_group_w_overlap'
      call count_group_w_overlap                                        &
     &    (num_pe, sub_ele_grp, merged_grp%ele_grp)
!
!      write(*,*) 'count_surf_group_w_overlap'
      call count_surf_group_w_overlap                                   &
     &   (num_pe, sub_surf_grp, merged_grp%surf_grp)
!
!    allocate merged group items
!
       call allocate_grp_type_item(merged_grp%nod_grp)
       call allocate_grp_type_item(merged_grp%ele_grp)
       call allocate_sf_grp_type_item(merged_grp%surf_grp)
!
!    set merged group data
!
!      write(*,*) 'set_group_w_overlap'
      call set_group_w_overlap                                          &
     &   (num_pe, sub_nod_grp, merged_grp%nod_grp)
!      write(*,*) 'set_group_w_overlap'
      call set_group_w_overlap                                          &
     &   (num_pe, sub_ele_grp, merged_grp%ele_grp)
!
!      write(*,*) 'set_surf_group_w_overlap'
      call set_surf_group_w_overlap                                     &
     &   (num_pe, sub_surf_grp, merged_grp%surf_grp)
!
      end subroutine const_merged_overlapped_groups
!
!-----------------------------------------------------------------------
!
      end module const_merged_groups
