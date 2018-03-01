!
!      module merge_to_viewer_surf_tbl
!
!      Written by Kemorin in Jan., 2007
!
!!      subroutine allocate_sf_cvt_table_viewer(merged_surf)
!!        type(mesh_geometry), intent(in) :: merged
!!        type(surface_data), intent(in) :: merged_surf
!!      subroutine deallocate_sf_cvt_table_viewer
!!
!!      subroutine set_surf_cvt_table_viewer(merged_surf)
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
      module merge_to_viewer_surf_tbl
!
      use m_precision
      use t_surface_data
!
      implicit none
!
!
      integer(kind = kint), allocatable :: isf_merge2viewer(:)
      integer(kind = kint), allocatable :: isf_viewer2merge(:)
!
      private :: isf_merge2viewer, isf_viewer2merge
!
!------------------------------------------------------------------
!
      contains
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
      use m_pickup_table_4_viewer
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
      use m_pickup_table_4_viewer
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
      use m_pickup_table_4_viewer
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
      end module merge_to_viewer_surf_tbl
