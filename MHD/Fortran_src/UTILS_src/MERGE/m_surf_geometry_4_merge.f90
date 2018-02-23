!
!      module m_surf_geometry_4_merge
!
!      Written by H. Matsui on Jan., 2007
!
!!      subroutine deallocate_surf_connect_merge
!!      subroutine deallocate_iso_surf_merge
!!
!!      subroutine check_merged_surface_data(mgd_mesh)
!!        type(merged_mesh), intent(in) :: mgd_mesh
!!      subroutine check_merged_iso_surf
!
      module m_surf_geometry_4_merge
!
      use m_precision
      use t_surface_data
!
      implicit    none
!
!
      type(surface_data) :: merged_surf
!
!      integer(kind=kint ), allocatable :: istack_surfpe(:)
!      number of surface stack
!
! ------------------------------------------------------
!
      contains
!
! ------------------------------------------------------
! ------------------------------------------------------
!
      subroutine deallocate_surf_connect_merge
!
      call deallocate_surface_connect_type(merged_surf)
!
      end subroutine deallocate_surf_connect_merge
!
! ------------------------------------------------------
!
      subroutine deallocate_iso_surf_merge
!
      call deallocate_iso_surface_type(merged_surf)
!
      end subroutine deallocate_iso_surf_merge
!
! ------------------------------------------------------
! ------------------------------------------------------
!
      subroutine check_merged_surface_data(mgd_mesh)
!
      use t_mesh_data_4_merge
      use m_geometry_constants
!
      type(merged_mesh), intent(in) :: mgd_mesh
!
      integer(kind = kint) :: isurf, iele
!
!
      write(50,*) 'surfpetot, nnod_4_surf'
      write(50,'(2i16)') merged_surf%numsurf, merged_surf%nnod_4_surf
!
      write(50,*) 'istack_surfpe'
      write(50,'(8i16)') mgd_mesh%istack_surfpe
!
      write(50,*) 'isurf, connection'
      do isurf = 1, merged_surf%numsurf
        write(50,'(9i16)') isurf,                                       &
     &      merged_surf%ie_surf(isurf,1:merged_surf%nnod_4_surf)
      end do
!
      write(50,*) 'elmpetot, nsurf_4_ele'
      write(50,'(2i16)') mgd_mesh%merged%ele%numele, nsurf_4_ele
      write(50,*) 'iele, edge ID for surface'
!
      do iele = 1, mgd_mesh%merged%ele%numele
        write(50,'(7i16)')                                              &
     &            iele, merged_surf%isf_4_ele(iele,1:nsurf_4_ele)
      end do
!
!
      end subroutine check_merged_surface_data
!
!   ---------------------------------------------------------------------
!
      subroutine check_merged_iso_surf
!
      write(50,*) 'numsurf_iso', merged_surf%numsurf_iso
!
      write(50,*) 'isf_isolate'
      write(50,'(8i16)')                                                &
     &     merged_surf%isf_isolate(1:merged_surf%numsurf_iso)
!
      end subroutine check_merged_iso_surf
!
!   ---------------------------------------------------------------------
!
      end module m_surf_geometry_4_merge
