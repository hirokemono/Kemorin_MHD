!
!      module pickup_surface_4_viewer
!
!      Written by Kemorin in Jan., 2007
!
!!      subroutine mark_used_surface_4_viewer(merged_grp)
!!        type(mesh_groups), intent(in) :: merged_grp
!!      subroutine count_used_surface_4_viewer
!!      subroutine set_surf_cvt_table_viewer
!!      subroutine set_surf_connect_viewer
!
!
      module pickup_surface_4_viewer
!
      use m_precision
!
      use m_pickup_table_4_viewer
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine mark_used_surface_4_viewer(merged_grp)
!
      use t_mesh_data
      use m_surf_geometry_4_merge
      use m_grp_data_merged_surfaces
!
      type(mesh_groups), intent(in) :: merged_grp
!
      integer(kind = kint) :: inum, isurf
!
!
      do inum = 1, merged_surf%numsurf_iso
        isurf = abs( merged_surf%isf_isolate(inum) )
        imark_surf(isurf) = 1
      end do
!
      do inum = 1, ntot_sf_iso_ele_grp_m
        isurf = abs( isf_isolate_ele_grp_m(inum) )
        imark_surf(isurf) = 1
      end do
!
      do inum = 1, merged_grp%surf_grp%num_item
        isurf = abs( isf_surf_grp_m(inum) )
        imark_surf(isurf) = 1
      end do
!
      end subroutine mark_used_surface_4_viewer
!
! ------------------------------------------------------
!
      subroutine count_used_surface_4_viewer
!
      use m_surf_geometry_4_merge
      use m_surface_mesh_4_merge
!
      integer(kind = kint) :: ip, ist, ied, isurf
!
      do ip = 1, num_pe_sf
        ist = istack_surfpe(ip-1) + 1
        ied = istack_surfpe(ip)
        isurf_sf_stack(ip) = isurf_sf_stack(ip-1)
        do isurf = ist, ied
          isurf_sf_stack(ip) = isurf_sf_stack(ip) + imark_surf(isurf)
        end do
      end do
      surfpetot_viewer = isurf_sf_stack(num_pe_sf)
!
      end subroutine count_used_surface_4_viewer
!
! ------------------------------------------------------
!
      subroutine set_surf_cvt_table_viewer
!
      use m_surf_geometry_4_merge
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
      subroutine set_surf_connect_viewer
!
      use m_surf_geometry_4_merge
      use m_surface_mesh_4_merge
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
!
      end module pickup_surface_4_viewer
