!
!      module const_merged_groups
!
!!      subroutine deallocate_subdomain_grp_stack
!!
!!      subroutine count_num_group_w_overlap
!!      subroutine count_merged_mesh_groups
!!
!!      subroutine const_merged_mesh_groups
!!      subroutine const_merged_overlapped_groups
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
       subroutine allocate_subdomain_grp_stack
!
      use m_geometry_data_4_merge
!
       allocate( istack_bc_pe(0:mgd_mesh1%num_pe) )
       allocate( istack_mat_pe(0:mgd_mesh1%num_pe) )
       allocate( istack_surf_pe(0:mgd_mesh1%num_pe) )
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
      subroutine count_num_group_w_overlap
!
      use m_geometry_data_4_merge
      use count_number_with_overlap
!
!
      call count_subdomain_ngrp_stack(mgd_mesh1%num_pe,                 &
     &    mgd_mesh1%sub_nod_grp, istack_bc_pe)
      call count_subdomain_ngrp_stack(mgd_mesh1%num_pe,                 &
     &    mgd_mesh1%sub_ele_grp, istack_mat_pe)
!
      call count_subdomain_sf_ngrp_stack(mgd_mesh1%num_pe,              &
     &    mgd_mesh1%sub_surf_grp, istack_surf_pe)
!
      end subroutine count_num_group_w_overlap
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_merged_mesh_groups
!
      use m_geometry_data_4_merge
      use count_merged_groups
!
!
!    count merged number of groups
!
      call count_num_merged_grp                                         &
     &   (mgd_mesh1%num_pe, mgd_mesh1%sub_nod_grp,                      &
     &    istack_bc_pe, merged_grp%nod_grp%num_grp)
      call count_num_merged_grp                                         &
     &   (mgd_mesh1%num_pe, mgd_mesh1%sub_ele_grp,                      &
     &    istack_mat_pe, merged_grp%ele_grp%num_grp)
!
      call count_num_merged_sf_grp                                      &
     &   (mgd_mesh1%num_pe, mgd_mesh1%sub_surf_grp,                     &
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
     &   (mgd_mesh1%num_pe, mgd_mesh1%sub_nod_grp,                      &
     &    istack_bc_pe, merged_grp%nod_grp)
      call set_merged_grp_name                                          &
     &   (mgd_mesh1%num_pe, mgd_mesh1%sub_ele_grp,                      &
     &    istack_mat_pe, merged_grp%ele_grp)
!
      call set_merged_grp_sf_name                                       &
     &   (mgd_mesh1%num_pe, mgd_mesh1%sub_surf_grp,                     &
     &    istack_surf_pe, merged_grp%surf_grp)
!
      end subroutine count_merged_mesh_groups
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine const_merged_mesh_groups
!
      use m_geometry_data_4_merge
      use count_merged_groups
      use set_merged_groups
!
!    count merged group items
!
      call allocate_flags_merged_grp
!
      call count_merged_node_group(mgd_mesh1%num_pe, merge_tbl,         &
     &    subdomain, mgd_mesh1%sub_nod_grp, merged_grp)
      call count_merged_element_group(mgd_mesh1%num_pe, merge_tbl,      &
     &    subdomain, mgd_mesh1%sub_ele_grp, merged_grp)
      call count_merged_surface_group(mgd_mesh1%num_pe, merge_tbl,      &
     &    subdomain, mgd_mesh1%sub_surf_grp, merged_grp)
!
!    allocate merged group items
!
       call allocate_grp_type_item(merged_grp%nod_grp)
       call allocate_grp_type_item(merged_grp%ele_grp)
       call allocate_sf_grp_type_item(merged_grp%surf_grp)
!
!    set merged group data
!
      call set_merged_node_group(mgd_mesh1%num_pe, merge_tbl,           &
     &    subdomain, mgd_mesh1%sub_nod_grp, merged_grp)
      call set_merged_element_group(mgd_mesh1%num_pe, merge_tbl,        &
     &    subdomain, mgd_mesh1%sub_ele_grp, merged_grp)
      call set_merged_surface_group(mgd_mesh1%num_pe, merge_tbl,        &
     &    subdomain, mgd_mesh1%sub_surf_grp, merged_grp)
!
      call deallocate_flags_merged_grp
!
      end subroutine const_merged_mesh_groups
!
!-----------------------------------------------------------------------
!
      subroutine const_merged_overlapped_groups
!
      use m_geometry_data_4_merge
      use count_merged_groups
      use set_overlap_groups
!
!
!    count merged group items
!
!      write(*,*) 'count_group_w_overlap'
      call count_group_w_overlap                                        &
     &    (mgd_mesh1%num_pe, mgd_mesh1%sub_nod_grp,                     &
     &     merged_grp%nod_grp)
!      write(*,*) 'count_group_w_overlap'
      call count_group_w_overlap                                        &
     &    (mgd_mesh1%num_pe, mgd_mesh1%sub_ele_grp,                     &
     &     merged_grp%ele_grp)
!
!      write(*,*) 'count_surf_group_w_overlap'
      call count_surf_group_w_overlap                                   &
     &   (mgd_mesh1%num_pe, mgd_mesh1%sub_surf_grp,                     &
     &    merged_grp%surf_grp)
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
     &   (mgd_mesh1%num_pe, mgd_mesh1%sub_nod_grp, merged_grp%nod_grp)
!      write(*,*) 'set_group_w_overlap'
      call set_group_w_overlap                                          &
     &   (mgd_mesh1%num_pe, mgd_mesh1%sub_ele_grp, merged_grp%ele_grp)
!
!      write(*,*) 'set_surf_group_w_overlap'
      call set_surf_group_w_overlap                                     &
     &   (mgd_mesh1%num_pe, mgd_mesh1%sub_surf_grp,                     &
     &    merged_grp%surf_grp)
!
      end subroutine const_merged_overlapped_groups
!
!-----------------------------------------------------------------------
!
      end module const_merged_groups
