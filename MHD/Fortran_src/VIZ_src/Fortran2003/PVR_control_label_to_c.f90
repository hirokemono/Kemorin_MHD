!>@file   PVR_control_label_to_c.f90
!!@brief  module PVR_control_label_to_c
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control parameter for volume rendering
!!
!!@verbatim
!!      integer(c_int) function num_label_pvr_modelview_f() bind(c)
!!      integer(c_int) function num_label_pvr_pixels_f() bind(c)
!!      integer(c_int) function num_label_pvr_streo_f() bind(c)
!!      subroutine set_label_pvr_modelview_f(names)  bind(c)
!!      subroutine set_label_pvr_pixels_f(names)  bind(c)
!!      subroutine set_label_pvr_streo_f(names)  bind(c)
!!
!!      integer(c_int) function num_label_pvr_light_f() bind(c)
!!      subroutine set_label_pvr_light_f(names)  bind(c)
!!
!!      integer(c_int) function num_label_pvr_colormap_f() bind(c)
!!      subroutine set_label_pvr_colormap_f(names)  bind(c)
!!
!!      integer(c_int) function num_label_pvr_colorbar_f() bind(c)
!!      subroutine set_label_pvr_colorbar_f(names)  bind(c)
!!
!!      integer(c_int) function num_label_pvr_movie_f() bind(c)
!!      subroutine set_label_pvr_movie_f(names)  bind(c)
!!@endverbatim
!
      module PVR_control_label_to_c
!
      use ISO_C_BINDING
!
      use m_precision
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      integer(c_int) function num_label_pvr_modelview_f() bind(c)
!
      use t_ctl_data_4_view_transfer
!
      num_label_pvr_modelview_f = num_label_pvr_modelview()
      return
      end function num_label_pvr_modelview_f
!
! ----------------------------------------------------------------------
!
      integer(c_int) function num_label_pvr_pixels_f() bind(c)
!
      use t_ctl_data_4_view_transfer
!
      num_label_pvr_pixels_f = num_label_pvr_pixels()
      return
      end function num_label_pvr_pixels_f
!
! ----------------------------------------------------------------------
!
      integer(c_int) function num_label_pvr_streo_f() bind(c)
!
      use t_ctl_data_4_view_transfer
!
      num_label_pvr_streo_f = num_label_pvr_streo()
      return
      end function num_label_pvr_streo_f
!
!  ---------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_label_pvr_modelview_f(names)  bind(c)
!
      use t_ctl_data_4_view_transfer
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_label_pvr_modelview(names)
      end subroutine set_label_pvr_modelview_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_pvr_pixels_f(names)  bind(c)
!
      use t_ctl_data_4_view_transfer
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_label_pvr_pixels(names)
      end subroutine set_label_pvr_pixels_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_pvr_streo_f(names)  bind(c)
!
      use t_ctl_data_4_view_transfer
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_label_pvr_streo(names)
      end subroutine set_label_pvr_streo_f
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(c_int) function num_label_pvr_light_f() bind(c)
!
      use t_ctl_data_pvr_light
!
      num_label_pvr_light_f = num_label_pvr_light()
      return
      end function num_label_pvr_light_f
!
! ----------------------------------------------------------------------
!
      subroutine set_label_pvr_light_f(names)  bind(c)
!
      use t_ctl_data_pvr_light
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_label_pvr_light(names)
      end subroutine set_label_pvr_light_f
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(c_int) function num_label_pvr_colormap_f() bind(c)
!
      use t_ctl_data_pvr_colormap
!
      num_label_pvr_colormap_f = num_label_pvr_colormap()
      return
      end function num_label_pvr_colormap_f
!
! ----------------------------------------------------------------------
!
      subroutine set_label_pvr_colormap_f(names)  bind(c)
!
      use t_ctl_data_pvr_colormap
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_label_pvr_colormap(names)
      end subroutine set_label_pvr_colormap_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_label_pvr_colorbar_f() bind(c)
!
      use t_ctl_data_pvr_colorbar
!
      num_label_pvr_colorbar_f = num_label_pvr_colorbar()
      return
      end function num_label_pvr_colorbar_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_pvr_colorbar_f(names)  bind(c)
!
      use t_ctl_data_pvr_colorbar
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_label_pvr_colorbar(names)
      end subroutine set_label_pvr_colorbar_f
!
!  ---------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_label_pvr_movie_f() bind(c)
!
      use t_control_data_pvr_movie
!
      num_label_pvr_movie_f = num_label_pvr_movie()
      return
      end function num_label_pvr_movie_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_pvr_movie_f(names)  bind(c)
!
      use t_control_data_pvr_movie
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_label_pvr_movie(names)
      end subroutine set_label_pvr_movie_f
!
!  ---------------------------------------------------------------------
!
      end module PVR_control_label_to_c
