!
!      module pickup_surface_4_viewer
!
!      Written by Kemorin in Jan., 2007
!
!!      subroutine allocate_imark_surf(merged_surf)
!!      subroutine allocate_sf_cvt_table_viewer(merged_surf, view_mesh)
!!        type(mesh_geometry), intent(in) :: merged
!!        type(viewer_mesh_data), intent(in) :: view_mesh
!!        type(surface_data), intent(in) :: merged_surf
!!      subroutine deallocate_imark_surf
!!      subroutine deallocate_sf_cvt_table_viewer
!!
!!      subroutine mark_used_surface_4_viewer                           &
!!     &         (merged_grp, merged_surf, mgd_sf_grp)
!!        type(mesh_groups), intent(in) :: merged_grp
!!        type(surface_data), intent(in) :: merged_surf
!!        type(group_data_merged_surf), intent(in) :: mgd_sf_grp
!!      subroutine count_used_surface_4_viewer                          &
!!     &         (num_pe, istack_surfpe, nsurf_sf)
!!        type(viewer_mesh_data), intent(inout) :: view_mesh
!!
!!      subroutine set_surf_cvt_table_viewer(merged_surf)
!!      subroutine set_surf_connect_viewer                              &
!!     &         (node, surf, inod_ksm, isurf_ksm, view_mesh)
!!        type(surface_data), intent(in) :: merged_surf
!!        type(viewer_mesh_data), intent(inout) :: view_mesh
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
      use t_geometry_data
      use t_surface_data
      use t_viewer_mesh
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
      subroutine count_used_surface_4_viewer                            &
     &         (num_pe, istack_surfpe, nsurf_sf)
!
      integer(kind = kint), intent(in) :: num_pe
      integer(kind = kint), intent(in) :: istack_surfpe(0:num_pe)
!
      integer(kind = kint), intent(inout) :: nsurf_sf(num_pe)
!
      integer(kind = kint) :: ip, ist, ied, isurf
!
      do ip = 1, num_pe
        ist = istack_surfpe(ip-1) + 1
        ied = istack_surfpe(ip)
        nsurf_sf(ip) = 0
        do isurf = ist, ied
          nsurf_sf(ip) = nsurf_sf(ip) + imark_surf(isurf)
        end do
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
      subroutine set_surf_connect_viewer                                &
     &         (node, surf, inod_ksm, isurf_ksm, view_mesh)
!
      use t_surface_data
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      integer(kind = kint), intent(in) :: inod_ksm(node%numnod)
      integer(kind = kint), intent(in) :: isurf_ksm(surf%numsurf)
!
      type(viewer_mesh_data), intent(inout) :: view_mesh
!
      integer(kind = kint) :: inum, isurf, k1, inod
!
!$omp parallel do
      do inum = 1, view_mesh%nsurf_viewer
        view_mesh%isurf_gl_view(inum) = inum
      end do
!$omp end parallel do
!
      do isurf = 1, surf%numsurf
        inum = isurf_ksm(isurf)
        if(inum .gt. 0) then
          do k1 = 1, surf%nnod_4_surf
            inod = surf%ie_surf(isurf,k1)
            view_mesh%ie_sf_viewer(inum,k1) = inod_ksm(inod)
            if(inod_ksm(inod) .le. 0) write(*,*)                        &
     &               'Wrong table in inod_ksm', inod
          end do
        end if
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
