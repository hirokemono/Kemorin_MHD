!set_plane_geometries.f90
!     module set_plane_geometries
!
!     Written by H. Matsui
!     modified by H. Matsui on Aug., 2007
!
!      subroutine s_set_plane_geometries(elm_type)
!
      module set_plane_geometries
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_set_plane_geometries(elm_type)
!
      use m_precision
!
      use m_cube_files_data
      use m_size_of_cube
      use m_size_4_plane
      use m_cube_position
      use m_grp_data_cub_kemo
      use m_filtering_nod_4_cubmesh
!
      implicit none
!
      integer(kind = kint), intent(in) :: elm_type
!
!
      mesh_type_plane = elm_type
!
      neib = ndepth
!
      xmin = xmin - ndepth*xsize / dble(nx_all)
      xmax = xmax - ndepth*xsize / dble(nx_all)
      ymin = ymin - ndepth*ysize / dble(ny_all)
      ymax = ymax - ndepth*ysize / dble(ny_all)
!
      call set_plane_resolution(ndepth, c_size1)
!
      end subroutine s_set_plane_geometries
!
! ----------------------------------------------------------------------
!
      end module set_plane_geometries
