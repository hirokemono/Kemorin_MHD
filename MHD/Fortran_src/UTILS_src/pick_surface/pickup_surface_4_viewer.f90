!
!      module pickup_surface_4_viewer
!
!      Written by Kemorin in Jan., 2007
!
!!      subroutine allocate_imark_surf(merged_surf)
!!      subroutine allocate_sf_cvt_table_viewer(merged_surf)
!!        type(mesh_geometry), intent(in) :: merged
!!        type(surface_data), intent(in) :: merged_surf
!!      subroutine deallocate_imark_surf
!!      subroutine deallocate_sf_cvt_table_viewer
!!
!!      subroutine mark_used_surface_4_viewer                           &
!!     &         (merged_grp, merged_surf, mgd_sf_grp)
!!        type(mesh_groups), intent(in) :: merged_grp
!!        type(surface_data), intent(in) :: merged_surf
!!        type(group_data_merged_surf), intent(in) :: mgd_sf_grp
!!      subroutine count_used_surface_4_viewer(num_pe, istack_surfpe)
!!
!!      subroutine set_surf_cvt_table_viewer(merged_surf, imark_surf)
!!      subroutine set_surf_connect_viewer(merged_surf)
!!        type(surface_data), intent(in) :: merged_surf
!!      subroutine set_surf_domain_item_viewer                          &
!!     &         (merged_surf, domain_surf_grp)
!!        type(viewer_group_data), intent(inout) :: domain_surf_grp
!!      subroutine set_element_group_item_viewer                        &
!!     &         (mgd_sf_grp, ele_surf_grp)
!!        type(group_data_merged_surf), intent(in) :: mgd_sf_grp
!!      subroutine set_surface_group_item_viewer                        &
!!     &         (mgd_sf_grp, sf_surf_grp)
!!        type(mesh_groups), intent(in) :: merged_grp
!!        type(group_data_merged_surf), intent(in) :: mgd_sf_grp
!!        type(mesh_groups), intent(in) :: merged_grp
!
      module pickup_surface_4_viewer
!
      use m_precision
      use t_surface_data
      use t_surface_mesh_4_merge
!
      implicit none
!
!
      integer(kind = kint), allocatable :: imark_surf(:)
      integer(kind = kint), allocatable :: isf_merge2viewer(:)
      integer(kind = kint), allocatable :: isf_viewer2merge(:)
!
      private :: imark_surf, isf_merge2viewer, isf_viewer2merge
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_imark_surf(merged_surf)
!
      type(surface_data), intent(in) :: merged_surf
!
      allocate( imark_surf(merged_surf%numsurf) )
      imark_surf = 0
!
      end subroutine allocate_imark_surf
!
!------------------------------------------------------------------
!
      subroutine allocate_sf_cvt_table_viewer(merged_surf)
!
      use m_surface_mesh_4_merge
!
      type(surface_data), intent(in) :: merged_surf
!
      allocate( isf_merge2viewer(merged_surf%numsurf) )
      allocate( isf_viewer2merge(surfpetot_viewer) )
      isf_merge2viewer = 0
      isf_viewer2merge = 0
!
      end subroutine allocate_sf_cvt_table_viewer
!
!------------------------------------------------------------------
!
      subroutine deallocate_imark_surf
!
      deallocate( imark_surf )
!
      end subroutine deallocate_imark_surf
!
!------------------------------------------------------------------
!
      subroutine deallocate_sf_cvt_table_viewer
!
      deallocate( isf_merge2viewer )
      deallocate( isf_viewer2merge )
!
      end subroutine deallocate_sf_cvt_table_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mark_used_surface_4_viewer                             &
     &         (merged_grp, merged_surf, mgd_sf_grp)
!
      use t_mesh_data
      use t_surface_data
      use t_grp_data_merged_surfaces
!
      type(mesh_groups), intent(in) :: merged_grp
      type(surface_data), intent(in) :: merged_surf
      type(group_data_merged_surf), intent(in) :: mgd_sf_grp
!
      integer(kind = kint) :: inum, isurf
!
!
      do inum = 1, merged_surf%numsurf_iso
        isurf = abs( merged_surf%isf_isolate(inum) )
        imark_surf(isurf) = 1
      end do
!
      do inum = 1, mgd_sf_grp%ntot_sf_iso_ele_grp_m
        isurf = abs( mgd_sf_grp%isf_isolate_ele_grp_m(inum) )
        imark_surf(isurf) = 1
      end do
!
      do inum = 1, merged_grp%surf_grp%num_item
        isurf = abs( mgd_sf_grp%isf_surf_grp_m(inum) )
        imark_surf(isurf) = 1
      end do
!
      end subroutine mark_used_surface_4_viewer
!
! ------------------------------------------------------
!
      subroutine count_used_surface_4_viewer(num_pe, istack_surfpe)
!
      use m_surface_mesh_4_merge
!
      integer(kind = kint), intent(in) :: num_pe
      integer(kind = kint), intent(in) :: istack_surfpe(0:num_pe)
!
      integer(kind = kint) :: ip, ist, ied, isurf
!
      do ip = 1, num_pe
        ist = istack_surfpe(ip-1) + 1
        ied = istack_surfpe(ip)
        isurf_sf_stack(ip) = isurf_sf_stack(ip-1)
        do isurf = ist, ied
          isurf_sf_stack(ip) = isurf_sf_stack(ip) + imark_surf(isurf)
        end do
      end do
      surfpetot_viewer = isurf_sf_stack(num_pe)
!
      end subroutine count_used_surface_4_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine set_surf_cvt_table_viewer(merged_surf)
!
      use t_surface_data
!
      type(surface_data), intent(in) :: merged_surf
!
      integer(kind = kint) :: isurf, inum
!
!
      inum = 0
      do isurf = 1, merged_surf%numsurf
        if ( imark_surf(isurf) .gt. 0 ) then
          inum = inum + 1
          isf_merge2viewer(isurf) = inum
          isf_viewer2merge(inum) = isurf
        end if
      end do
!
      end subroutine set_surf_cvt_table_viewer
!
! ------------------------------------------------------
!
      subroutine set_surf_connect_viewer(merged_surf)
!
      use t_surface_data
      use m_surface_mesh_4_merge
!
      type(surface_data), intent(in) :: merged_surf
!
      integer(kind = kint) :: inum, isurf
!
!
      do inum = 1, surfpetot_viewer
        isurf = isf_viewer2merge(inum)
        ie_sf_viewer(inum,1:merged_surf%nnod_4_surf)                    &
     &         = merged_surf%ie_surf(isurf,1:merged_surf%nnod_4_surf)
      end do
!
      end subroutine set_surf_connect_viewer
!
! ------------------------------------------------------
! ------------------------------------------------------
!
      subroutine set_surf_domain_item_viewer                            &
     &         (merged_surf, domain_surf_grp)
!
      use t_surface_data
!
      type(surface_data), intent(in) :: merged_surf
      type(viewer_group_data), intent(inout) :: domain_surf_grp
!
      integer(kind = kint) :: inum, isurf
!
!
      do inum = 1, domain_surf_grp%num_item
        isurf = abs( merged_surf%isf_isolate(inum) )
        domain_surf_grp%item_sf(inum) = isf_merge2viewer(isurf)         &
     &                    * (merged_surf%isf_isolate(inum) / isurf)
      end do
!
      end subroutine set_surf_domain_item_viewer
!
!------------------------------------------------------------------
!
      subroutine set_element_group_item_viewer                          &
     &         (mgd_sf_grp, ele_surf_grp)
!
      use t_grp_data_merged_surfaces

!
      type(group_data_merged_surf), intent(in) :: mgd_sf_grp
      type(viewer_group_data), intent(inout) :: ele_surf_grp
!
      integer(kind = kint) :: inum, isurf
!
!
      do inum = 1, ele_surf_grp%num_item
        isurf = abs( mgd_sf_grp%isf_isolate_ele_grp_m(inum) )
        ele_surf_grp%item_sf(inum) = isf_merge2viewer(isurf)            &
     &         * (mgd_sf_grp%isf_isolate_ele_grp_m(inum) / isurf)
      end do
!
      end subroutine set_element_group_item_viewer
!
!------------------------------------------------------------------
!
      subroutine set_surface_group_item_viewer                          &
     &         (mgd_sf_grp, sf_surf_grp)
!
      use t_grp_data_merged_surfaces
!
      type(group_data_merged_surf), intent(in) :: mgd_sf_grp
      type(viewer_group_data), intent(inout)  :: sf_surf_grp
!
      integer(kind = kint) :: inum, isurf
!
!
      do inum = 1, sf_surf_grp%num_item
        isurf = abs( mgd_sf_grp%isf_surf_grp_m(inum) )
        sf_surf_grp%item_sf(inum) = isf_merge2viewer(isurf)             &
     &                     * (mgd_sf_grp%isf_surf_grp_m(inum) / isurf)
      end do
!
      end subroutine set_surface_group_item_viewer
!
!------------------------------------------------------------------
!
      end module pickup_surface_4_viewer
