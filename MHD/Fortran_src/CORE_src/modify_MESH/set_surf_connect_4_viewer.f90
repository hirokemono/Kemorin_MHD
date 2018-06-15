!
!      module set_surf_connect_4_viewer
!
!      Written by Kemorin in Jan., 2007
!
!!      subroutine s_set_surf_connect_4_viewer                          &
!!     &         (nnod_4_surf, mgd_mesh, mgd_sf_grp,                    &
!!     &          view_mesh, domain_grps, view_ele_grps, view_sf_grps)
!!        type(merged_mesh), intent(inout) :: mgd_mesh
!!        type(group_data_merged_surf), intent(inout) :: mgd_sf_grp
!
      module set_surf_connect_4_viewer
!
      use m_precision
      use m_constants
!
      use t_viewer_mesh
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
     &         (nnod_4_surf, mgd_mesh, mgd_sf_grp,                      &
     &          view_mesh, domain_grps, view_ele_grps, view_sf_grps)
!
      use t_mesh_data_4_merge
      use t_surface_data
      use t_viewer_ele_grp_surface
      use pickup_surface_4_viewer
!
      integer(kind = kint), intent(in) :: nnod_4_surf
!
      type(merged_mesh), intent(inout) :: mgd_mesh
      type(group_data_merged_surf), intent(inout) :: mgd_sf_grp
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
      type(viewer_surface_groups), intent(inout) :: domain_grps
      type(viewer_surface_groups), intent(inout) :: view_ele_grps
      type(viewer_surface_groups), intent(inout) :: view_sf_grps
!
!
       write(*,*) 'allocate_imark_surf'
      call allocate_imark_surf(mgd_mesh%merged_surf)
      call mark_used_surface_4_viewer                                   &
     &   (mgd_mesh%merged_grp, mgd_mesh%merged_surf, mgd_sf_grp)
!
       write(*,*) 'count_used_surface_4_viewer'
      call count_used_surface_4_viewer                                  &
     &   (mgd_mesh%istack_surfpe, view_mesh%nsurf_viewer)
!
       write(*,*) 'allocate_sf_cvt_table_viewer'
      call allocate_sf_cvt_table_viewer                                 &
     &   (mgd_mesh%merged_surf, view_mesh)
      call set_surf_cvt_table_viewer(mgd_mesh%merged_surf)
!
       write(*,*) 'deallocate_imark_surf'
      call deallocate_imark_surf
!
       write(*,*) 'alloc_surf_connect_viewer'
      call alloc_surf_connect_viewer(nnod_4_surf, view_mesh)
      call set_surf_connect_viewer(mgd_mesh%merged_surf, view_mesh)
!
      call s_set_groups_4_viewer_surface                                &
     &   (mgd_mesh%merged_grp, mgd_mesh%merged_surf, mgd_sf_grp,        &
     &    view_mesh%nsurf_viewer, domain_grps,                          &
     &    view_ele_grps, view_sf_grps)
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
     &         (merged_grp, merged_surf, mgd_sf_grp,                    &
     &          nsurf_viewer, domain_grps, view_ele_grps, view_sf_grps)
!
      use t_mesh_data
      use t_surface_data
      use t_viewer_ele_grp_surface
!
      use renumber_surface_4_viewer
      use pickup_surface_4_viewer
!
      integer(kind = kint), intent(in) :: nsurf_viewer
      type(mesh_groups), intent(in) :: merged_grp
      type(surface_data), intent(in) :: merged_surf
      type(group_data_merged_surf), intent(in) :: mgd_sf_grp
!
      type(viewer_surface_groups), intent(inout) :: domain_grps
      type(viewer_surface_groups), intent(inout) :: view_ele_grps
      type(viewer_surface_groups), intent(inout) :: view_sf_grps
!
!     renumber domain boundary
!
      domain_grps%surf_grp%num_item = merged_surf%numsurf_iso
       write(*,*) 'alloc_domain_stack_4_surf'
      call alloc_domain_stack_4_surf(ione, domain_grps)
      call alloc_merged_group_item(domain_grps%surf_grp)
!
      call set_surf_domain_item_viewer                                  &
     &   (merged_surf, domain_grps%surf_grp)
      call set_surf_domain_stack_viewer                                 &
     &   (nsurf_viewer, domain_grps%surf_grp)
!
!     renumber element group boundary
!
      view_ele_grps%num_grp = merged_grp%ele_grp%num_grp
      view_ele_grps%surf_grp%num_item                                   &
     &     = mgd_sf_grp%ntot_sf_iso_ele_grp
      call alloc_merged_surf_grps_stack(ione, view_ele_grps)
      call alloc_merged_group_item(view_ele_grps%surf_grp)
!
      view_ele_grps%grp_name(1:view_ele_grps%num_grp)                   &
     &     = merged_grp%ele_grp%grp_name(1:view_ele_grps%num_grp)
!
      call set_element_group_item_viewer                                &
     &   (mgd_sf_grp, view_ele_grps%surf_grp)
      call set_element_group_stack_viewer(nsurf_viewer,                 &
     &    mgd_sf_grp, view_ele_grps%num_grp, view_ele_grps%surf_grp)
!
!     renumber surface boundary
!
      view_sf_grps%num_grp = merged_grp%surf_grp%num_grp
      view_sf_grps%surf_grp%num_item = merged_grp%surf_grp%num_item
!
      call alloc_merged_surf_grps_stack(ione, view_sf_grps)
      call alloc_merged_group_item(view_sf_grps%surf_grp)
!
      view_sf_grps%grp_name(1:view_sf_grps%num_grp)                     &
     &        = merged_grp%surf_grp%grp_name(1:view_sf_grps%num_grp)
!
      call set_surface_group_item_viewer                                &
     &   (mgd_sf_grp, view_sf_grps%surf_grp)
      call set_surface_group_stack_viewer(nsurf_viewer,                 &
     &    merged_grp,  view_sf_grps%num_grp, view_sf_grps%surf_grp)
!
      end subroutine s_set_groups_4_viewer_surface
!
!------------------------------------------------------------------
!
      end module set_surf_connect_4_viewer
