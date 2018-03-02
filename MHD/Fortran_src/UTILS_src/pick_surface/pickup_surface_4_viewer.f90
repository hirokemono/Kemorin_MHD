!
!      module pickup_surface_4_viewer
!
!      Written by Kemorin in Jan., 2007
!
!!      subroutine mark_used_surface_4_viewer                           &
!!     &         (merged_grp, merged_surf, mgd_sf_grp)
!!        type(mesh_groups), intent(in) :: merged_grp
!!        type(surface_data), intent(in) :: merged_surf
!!        type(group_data_merged_surf), intent(in) :: mgd_sf_grp
!!      subroutine count_used_surface_4_viewer(num_pe, istack_surfpe)
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
! ------------------------------------------------------
!
      end module pickup_surface_4_viewer
