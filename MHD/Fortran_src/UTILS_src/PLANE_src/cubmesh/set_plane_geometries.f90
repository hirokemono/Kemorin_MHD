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
      subroutine set_plane_range_w_sleeve(elm_type, c_size)
!
      use t_size_of_cube
      use m_grp_data_cub_kemo
!
      implicit none
!
      integer(kind = kint), intent(in) :: elm_type
      type(size_of_cube), intent(inout) :: c_size
!
!
      neib = c_size%ndepth
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
      end subroutine set_plane_range_w_sleeve
!
! ----------------------------------------------------------------------
!
      end module set_plane_geometries
