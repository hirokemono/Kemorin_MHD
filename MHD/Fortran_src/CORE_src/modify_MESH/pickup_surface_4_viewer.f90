!
!      module pickup_surface_4_viewer
!
!      Written by Kemorin in Jan., 2007
!
!!      subroutine allocate_imark_surf(merged_surf)
!!      subroutine allocate_sf_cvt_table_viewer(merged_surf, view_mesh)
!!        type(viewer_mesh_data), intent(in) :: view_mesh
!!        type(surface_data), intent(in) :: merged_surf
!!      subroutine deallocate_imark_surf
!!      subroutine deallocate_sf_cvt_table_viewer
!!
!!      subroutine mark_used_surface_4_viewer                           &
!!     &         (merged_grp, merged_surf, mgd_sf_grp)
!!        type(mesh_groups), intent(in) :: merged_grp
!!        type(surface_data), intent(in) :: merged_surf
!!        type(viewer_ele_grp_surface), intent(in) :: mgd_sf_grp
!!      subroutine count_used_surface_4_viewer(numsurf, nsurf_viewer)
!!
!!      subroutine set_surf_cvt_table_viewer(merged_surf)
!!      subroutine set_surf_connect_viewer(merged_surf, view_mesh)
!!        type(surface_data), intent(in) :: merged_surf
!!        type(viewer_mesh_data), intent(inout) :: view_mesh
!!      subroutine set_surf_domain_item_viewer                          &
!!     &         (merged_surf, domain_surf_grp)
!!        type(viewer_group_data), intent(inout) :: domain_surf_grp
!!      subroutine set_element_group_item_viewer                        &
!!     &         (mgd_sf_grp, ele_surf_grp)
!!        type(viewer_ele_grp_surface), intent(in) :: mgd_sf_grp
!!      subroutine set_surface_group_item_viewer                        &
!!     &         (mgd_sf_grp, sf_surf_grp)
!!        type(mesh_groups), intent(in) :: merged_grp
!!        type(viewer_ele_grp_surface), intent(in) :: mgd_sf_grp
!!        type(mesh_groups), intent(in) :: merged_grp
!
      module pickup_surface_4_viewer
!
      use m_precision
      use t_surface_data
      use t_viewer_mesh
      use t_viewer_group
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
      subroutine allocate_sf_cvt_table_viewer(merged_surf, view_mesh)
!
      type(surface_data), intent(in) :: merged_surf
      type(viewer_mesh_data), intent(in) :: view_mesh
!
      allocate( isf_merge2viewer(merged_surf%numsurf) )
      allocate( isf_viewer2merge(view_mesh%nsurf_viewer) )
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
      use t_viewer_ele_grp_surface
!
      type(mesh_groups), intent(in) :: merged_grp
      type(surface_data), intent(in) :: merged_surf
      type(viewer_ele_grp_surface), intent(in) :: mgd_sf_grp
!
      integer(kind = kint) :: inum, isurf
!
!
      do inum = 1, merged_surf%numsurf_iso
        isurf = abs( merged_surf%isf_isolate(inum) )
        imark_surf(isurf) = 1
      end do
!
      do inum = 1, mgd_sf_grp%ntot_sf_iso_ele_grp
        isurf = abs( mgd_sf_grp%isf_isolate_ele_grp(inum) )
        imark_surf(isurf) = 1
      end do
!
      do inum = 1, merged_grp%surf_grp%num_item
        isurf = abs( mgd_sf_grp%isf_surf_grp(inum) )
        imark_surf(isurf) = 1
      end do
!
      end subroutine mark_used_surface_4_viewer
!
! ------------------------------------------------------
!
      subroutine count_used_surface_4_viewer(numsurf, nsurf_viewer)
!
      integer(kind = kint), intent(in) :: numsurf
!
      integer(kind = kint), intent(inout) :: nsurf_viewer
!
      integer(kind = kint) :: ip, ist, ied, isurf
!
      nsurf_viewer = 0
      do isurf = 1, numsurf
        nsurf_viewer = nsurf_viewer + imark_surf(isurf)
      end do
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
      subroutine set_surf_connect_viewer(merged_surf, view_mesh)
!
      use t_surface_data
!
      type(surface_data), intent(in) :: merged_surf
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      integer(kind = kint) :: inum, isurf
!
!
      do inum = 1, view_mesh%nsurf_viewer
        isurf = isf_viewer2merge(inum)
        view_mesh%ie_sf_viewer(inum,1:merged_surf%nnod_4_surf)          &
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
      use t_viewer_ele_grp_surface

!
      type(viewer_ele_grp_surface), intent(in) :: mgd_sf_grp
      type(viewer_group_data), intent(inout) :: ele_surf_grp
!
      integer(kind = kint) :: inum, isurf
!
!
      do inum = 1, ele_surf_grp%num_item
        isurf = abs( mgd_sf_grp%isf_isolate_ele_grp(inum) )
        ele_surf_grp%item_sf(inum) = isf_merge2viewer(isurf)            &
     &         * (mgd_sf_grp%isf_isolate_ele_grp(inum) / isurf)
      end do
!
      end subroutine set_element_group_item_viewer
!
!------------------------------------------------------------------
!
      subroutine set_surface_group_item_viewer                          &
     &         (mgd_sf_grp, sf_surf_grp)
!
      use t_viewer_ele_grp_surface
!
      type(viewer_ele_grp_surface), intent(in) :: mgd_sf_grp
      type(viewer_group_data), intent(inout)  :: sf_surf_grp
!
      integer(kind = kint) :: inum, isurf
!
!
      do inum = 1, sf_surf_grp%num_item
        isurf = abs( mgd_sf_grp%isf_surf_grp(inum) )
        sf_surf_grp%item_sf(inum) = isf_merge2viewer(isurf)             &
     &                     * (mgd_sf_grp%isf_surf_grp(inum) / isurf)
      end do
!
      end subroutine set_surface_group_item_viewer
!
!------------------------------------------------------------------
!
      end module pickup_surface_4_viewer
