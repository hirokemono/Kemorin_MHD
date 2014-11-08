!
!      module m_surf_geometry_4_merge
!
!      Written by H. Matsui on Jan., 2007
!
!      subroutine allocate_num_surface_merge
!
!      subroutine deallocate_num_surface_merge
!      subroutine deallocate_surf_connect_merge
!      subroutine deallocate_iso_surf_merge
!
!      subroutine check_merged_surface_data
!      subroutine check_merged_iso_surf
!
      module m_surf_geometry_4_merge
!
      use t_surface_data
!
      use m_precision
!
      implicit    none
!
!
      type(surface_data) :: merged_surf
!
      integer(kind=kint ), allocatable :: istack_surfpe(:)
!      number of surface stack
!
! ------------------------------------------------------
!
      contains
!
! ------------------------------------------------------
!
      subroutine allocate_num_surface_merge
!
      use m_geometry_data_4_merge
!
      allocate( istack_surfpe(0:num_pe) )
      istack_surfpe = 0
!
      end subroutine allocate_num_surface_merge
!
! ------------------------------------------------------
! ------------------------------------------------------
!
      subroutine deallocate_num_surface_merge
!
      deallocate( istack_surfpe )
!
      end subroutine deallocate_num_surface_merge
!
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
      subroutine check_merged_surface_data
!
      use m_geometry_constants
      use m_geometry_data_4_merge
!
      integer(kind = kint) :: isurf, iele
!
!
      write(50,*) 'surfpetot, nnod_4_surf'
      write(50,'(2i15)') merged_surf%numsurf, merged_surf%nnod_4_surf
!
      write(50,*) 'istack_surfpe'
      write(50,'(8i10)') istack_surfpe
!
      write(50,*) 'isurf, connection'
      do isurf = 1, merged_surf%numsurf
        write(50,'(30i10)') isurf,                                      &
     &      merged_surf%ie_surf(isurf,1:merged_surf%nnod_4_surf)
      end do
!
      write(50,*) 'elmpetot, nsurf_4_ele'
      write(50,'(2i15)') merged%ele%numele, nsurf_4_ele
      write(50,*) 'iele, edge ID for surface'
!
      do iele = 1, merged%ele%numele
        write(50,'(30i10)')                                             &
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
      write(50,'(8i10)')                                                &
     &     merged_surf%isf_isolate(1:merged_surf%numsurf_iso)
!
      end subroutine check_merged_iso_surf
!
!   ---------------------------------------------------------------------
!
      end module m_surf_geometry_4_merge
