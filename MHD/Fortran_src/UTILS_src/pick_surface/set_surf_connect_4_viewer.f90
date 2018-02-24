!
!      module set_surf_connect_4_viewer
!
!      Written by Kemorin in Jan., 2007
!
!!      subroutine s_set_surf_connect_4_viewer                          &
!!     &         (nnod_4_surf, mgd_mesh, mgd_sf_grp)
!!        type(merged_mesh), intent(inout) :: mgd_mesh
!!        type(group_data_merged_surf), intent(inout) :: mgd_sf_grp
!
      module set_surf_connect_4_viewer
!
      use m_precision
!
      use m_surface_mesh_4_merge
!
      implicit none
!
      private :: s_set_groups_4_viewer_surface
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine s_set_surf_connect_4_viewer                            &
     &         (nnod_4_surf, mgd_mesh, mgd_sf_grp)
!
      use t_mesh_data_4_merge
      use t_surface_data
      use t_grp_data_merged_surfaces
      use m_pickup_table_4_viewer
      use pickup_surface_4_viewer
!
      integer(kind = kint), intent(in) :: nnod_4_surf
      type(merged_mesh), intent(inout) :: mgd_mesh
      type(group_data_merged_surf), intent(inout) :: mgd_sf_grp
!
!
       write(*,*) 'allocate_imark_surf'
      call allocate_imark_surf(mgd_mesh%merged_surf)
      call mark_used_surface_4_viewer                                   &
     &   (mgd_mesh%merged_grp, mgd_mesh%merged_surf, mgd_sf_grp)
!
       write(*,*) 'count_used_surface_4_viewer'
      call count_used_surface_4_viewer                                  &
     &   (mgd_mesh%num_pe, mgd_mesh%istack_surfpe)
!
       write(*,*) 'allocate_sf_cvt_table_viewer'
      call allocate_sf_cvt_table_viewer(mgd_mesh%merged_surf)
      call set_surf_cvt_table_viewer(mgd_mesh%merged_surf)
!
       write(*,*) 'deallocate_imark_surf'
      call deallocate_imark_surf
!
       write(*,*) 'allocate_surf_connect_viewer'
      call allocate_surf_connect_viewer(nnod_4_surf)
      call set_surf_connect_viewer(mgd_mesh%merged_surf)
!
      call s_set_groups_4_viewer_surface                                &
     &   (mgd_mesh%merged_grp, mgd_mesh%merged_surf, mgd_sf_grp)
!
       write(*,*) 'deallocate_sf_cvt_table_viewer'
      call deallocate_sf_cvt_table_viewer
!
      call deallocate_sf_grp_type(mgd_mesh%merged_grp%surf_grp)
      call deallocate_grp_type(mgd_mesh%merged_grp%ele_grp)
!
      call dealloc_surf_connect_merge(mgd_mesh)
      call dealloc_num_surface_merge(mgd_mesh)
!
      end subroutine s_set_surf_connect_4_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine s_set_groups_4_viewer_surface                          &
     &         (merged_grp, merged_surf, mgd_sf_grp)
!
      use t_mesh_data
      use t_surface_data
      use t_grp_data_merged_surfaces
!
      use renumber_surface_4_viewer
!
      type(mesh_groups), intent(in) :: merged_grp
      type(surface_data), intent(in) :: merged_surf
      type(group_data_merged_surf), intent(in) :: mgd_sf_grp
!
!     renumber domain boundary
!
      domain_surf_grp%num_item = merged_surf%numsurf_iso
       write(*,*) 'allocate_domain_stack_4_surf'
      call allocate_domain_stack_4_surf
      call alloc_merged_group_item(domain_surf_grp)
!
      call set_surf_domain_item_viewer(merged_surf)
      call set_surf_domain_stack_viewer
!
!     renumber element group boundary
!
      ngrp_ele_sf = merged_grp%ele_grp%num_grp
      ele_surf_grp%num_item = mgd_sf_grp%ntot_sf_iso_ele_grp_m
       write(*,*) 'allocate_ele_grp_stack_4_surf'
      call allocate_ele_grp_stack_4_surf
      call alloc_merged_group_item(ele_surf_grp)
!
      ele_gp_name_sf(1:ngrp_ele_sf)                                     &
     &     = merged_grp%ele_grp%grp_name(1:ngrp_ele_sf)
!
      call set_element_group_item_viewer(mgd_sf_grp)
      call set_element_group_stack_viewer(mgd_sf_grp)
!
!     renumber surface boundary
!
      ngrp_surf_sf = merged_grp%surf_grp%num_grp
      sf_surf_grp%num_item = merged_grp%surf_grp%num_item
!
      call allocate_surf_grp_stack_4_surf
      call alloc_merged_group_item(sf_surf_grp)
!
      surf_gp_name_sf(1:ngrp_surf_sf)                                   &
     &        = merged_grp%surf_grp%grp_name(1:ngrp_surf_sf)
!
      call set_surface_group_item_viewer(mgd_sf_grp)
      call set_surface_group_stack_viewer(merged_grp)
!
      end subroutine s_set_groups_4_viewer_surface
!
!------------------------------------------------------------------
!
      end module set_surf_connect_4_viewer
