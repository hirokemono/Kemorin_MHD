!
!      module set_surf_connect_4_viewer
!
!      Written by Kemorin in Jan., 2007
!
!!      subroutine s_set_surf_connect_4_viewer                          &
!!     &         (nnod_4_surf, mgd_mesh, mgd_sf_grp, num_pe,            &
!!     &          nsurf_sf, isurf_sf_stack, view_mesh, domain_grps,     &
!!     &          view_ele_grps, view_sf_grps)
!!        type(merged_mesh), intent(inout) :: mgd_mesh
!!        type(group_data_merged_surf), intent(inout) :: mgd_sf_grp
!
      module set_surf_connect_4_viewer
!
      use m_precision
!
      use t_viewer_mesh
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
!      subroutine s_set_surf_connect_4_viewer                            &
!     &         (nnod_4_surf, mgd_mesh, mgd_sf_grp, num_pe,              &
!     &          nsurf_sf, isurf_sf_stack, view_mesh, domain_grps,       &
!     &          view_ele_grps, view_sf_grps)
!
!
!      call allocate_imark_surf(mgd_mesh%merged_surf)
!      call mark_used_surface_4_viewer                                   &
!     &   (mgd_mesh%merged_grp, mgd_mesh%merged_surf, mgd_sf_grp)
!
!       write(*,*) 'count_used_surface_4_viewer'
!      call count_used_surface_4_viewer                                  &
!     &   (mgd_mesh%num_pe, mgd_mesh%istack_surfpe, nsurf_sf)
!
!      call s_cal_total_and_stacks(num_pe, nsurf_sf, izero,              &
!     &    isurf_sf_stack, view_mesh%nsurf_viewer)
!
!       write(*,*) 'allocate_sf_cvt_table_viewer'
!      call allocate_sf_cvt_table_viewer                                 &
!     &   (mgd_mesh%merged_surf, view_mesh)
!      call set_surf_cvt_table_viewer(mgd_mesh%merged_surf)
!
!       write(*,*) 'deallocate_imark_surf'
!      call deallocate_imark_surf
!
!      call alloc_surf_connect_viewer(nnod_4_surf, view_mesh)
!      call set_surf_connect_viewer(mgd_mesh%merged_surf, view_mesh)
!
!      call s_set_groups_4_viewer_surface                                &
!     &   (mgd_mesh%merged_grp, mgd_mesh%merged_surf, mgd_sf_grp,        &
!     &    num_pe, isurf_sf_stack, domain_grps,                          &
!     &    view_ele_grps, view_sf_grps)
!
!       write(*,*) 'deallocate_sf_cvt_table_viewer'
!      call deallocate_sf_cvt_table_viewer
!
!      call deallocate_sf_grp_type(mgd_mesh%merged_grp%surf_grp)
!      call deallocate_grp_type(mgd_mesh%merged_grp%ele_grp)
!
!      call dealloc_surf_connect_merge(mgd_mesh)
!
!      end subroutine s_set_surf_connect_4_viewer
!
!------------------------------------------------------------------
!
      end module set_surf_connect_4_viewer
