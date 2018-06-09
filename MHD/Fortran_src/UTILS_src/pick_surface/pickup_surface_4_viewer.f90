!
!      module pickup_surface_4_viewer
!
!      Written by Kemorin in Jan., 2007
!
!!      subroutine set_surf_connect_viewer                              &
!!     &         (node, surf, inod_ksm, isurf_ksm, view_mesh)
!!        type(surface_data), intent(in) :: merged_surf
!!        type(viewer_mesh_data), intent(inout) :: view_mesh
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
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
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
!------------------------------------------------------------------
!
      end module pickup_surface_4_viewer
