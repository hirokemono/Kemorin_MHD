
!
!      module check_mesh_data_4_merge
!
!      Written by H. Matsui on Jan., 2007
!
!!
!!      subroutine check_boundary_data_m(ip, mgd_mesh)
!!      subroutine check_material_data_m(ip, mgd_mesh)
!!      subroutine check_surface_data_m(ip, mgd_mesh)
!!
!!      subroutine check_merged_surface_data(mgd_mesh)
!!        type(merged_mesh), intent(in) :: mgd_mesh
!!      subroutine check_merged_iso_surf(merged_surf)
!
      module check_mesh_data_4_merge
!
      use m_precision
      use t_mesh_data_4_merge
!
      implicit    none
!
! ------------------------------------------------------
!
      contains
!
! ------------------------------------------------------
!
      subroutine check_boundary_data_m(ip, mgd_mesh)
!
       integer(kind = kint), intent(in) :: ip
       type(merged_mesh), intent(in) :: mgd_mesh
!
!
      call check_group_type_data(izero, mgd_mesh%sub_nod_grp(ip))
!
      end subroutine check_boundary_data_m
!
!-----------------------------------------------------------------------
!
      subroutine check_material_data_m(ip, mgd_mesh)
!
       integer(kind = kint), intent(in) :: ip
       type(merged_mesh), intent(in) :: mgd_mesh
!
!
      call check_group_type_data(ip, mgd_mesh%sub_ele_grp(ip))
!
      end subroutine check_material_data_m
!
!-----------------------------------------------------------------------
!
       subroutine check_surface_data_m(ip, mgd_mesh)
!
       integer(kind = kint), intent(in) :: ip
       type(merged_mesh), intent(in) :: mgd_mesh
!
!
       call check_surf_grp_type_data(ip, mgd_mesh%sub_surf_grp(ip))
!
       end subroutine check_surface_data_m
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine check_merged_surface_data(mgd_mesh)
!
      use t_mesh_data_4_merge
      use m_geometry_constants
!
      type(merged_mesh), intent(in) :: mgd_mesh
!
      integer(kind = kint) :: isurf, iele, i
!
!
      write(50,*) 'surfpetot, nnod_4_surf'
      write(50,'(2i16)') mgd_mesh%merged_surf%numsurf,                  &
     &                   mgd_mesh%merged_surf%nnod_4_surf
!
      write(50,*) 'istack_surfpe'
      write(50,'(8i16)') mgd_mesh%istack_surfpe
!
      write(50,*) 'isurf, connection'
      do isurf = 1, mgd_mesh%merged_surf%numsurf
        write(50,'(9i16)') isurf,                                       &
     &     (mgd_mesh%merged_surf%ie_surf(isurf,i),                      &
     &      i=1,mgd_mesh%merged_surf%nnod_4_surf)
      end do
!
      write(50,*) 'elmpetot, nsurf_4_ele'
      write(50,'(2i16)') mgd_mesh%merged%ele%numele, nsurf_4_ele
      write(50,*) 'iele, edge ID for surface'
!
      do iele = 1, mgd_mesh%merged%ele%numele
        write(50,'(7i16)')                                              &
     &       iele, mgd_mesh%merged_surf%isf_4_ele(iele,1:nsurf_4_ele)
      end do
!
!
      end subroutine check_merged_surface_data
!
!   ---------------------------------------------------------------------
!
      subroutine check_merged_iso_surf(merged_surf)
!
      type(surface_data), intent(in) :: merged_surf
!
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
      end module check_mesh_data_4_merge
