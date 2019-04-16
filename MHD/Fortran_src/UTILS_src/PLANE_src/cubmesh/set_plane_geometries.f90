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
      subroutine s_set_plane_geometries(elm_type, c_size)
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
      type(size_of_cube), intent(inout) :: c_size
!
!
      mesh_type_plane = elm_type
!
      neib = ndepth
      c_size%ndepth = ndepth
!
      c_size%xmin = c_size%xmin                                         &
     &             - c_size%ndepth * c_size%xsize / dble(c_size%nx_all)
      c_size%xmax = c_size%xmax                                         &
     &             - c_size%ndepth * c_size%xsize / dble(c_size%nx_all)
      c_size%ymin = c_size%ymin                                         &
     &             - c_size%ndepth * c_size%ysize / dble(c_size%ny_all)
      c_size%ymax = c_size%ymax                                         &
     &             - c_size%ndepth * c_size%ysize / dble(c_size%ny_all)
!
      call set_plane_resolution(ndepth, c_size)
!
      end subroutine s_set_plane_geometries
!
! ----------------------------------------------------------------------
!
      end module set_plane_geometries
