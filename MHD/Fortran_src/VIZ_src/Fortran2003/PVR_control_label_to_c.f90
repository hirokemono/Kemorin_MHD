!>@file   PVR_control_label_to_c.f90
!!@brief  module PVR_control_label_to_c
!!
!!@date  Programmed by H.Matsui in May, 2006
!
!>@brief control parameter for volume rendering
!!
!!@verbatim
!!      integer(c_int) function num_label_pvr_ctl_f() bind(c)
!!      integer(c_int) function num_label_pvr_ctl_w_dup_f() bind(c)
!!      subroutine set_label_pvr_ctl_w_dup_f(names)  bind(c)
!!
!!      integer(c_int) function num_label_pvr_modelview_f() bind(c)
!!      integer(c_int) function num_label_pvr_pixels_f() bind(c)
!!      integer(c_int) function num_label_pvr_streo_f() bind(c)
!!      subroutine set_label_pvr_modelview_f(names)  bind(c)
!!      subroutine set_label_pvr_pixels_f(names)  bind(c)
!!      subroutine set_label_pvr_streo_f(names)  bind(c)
!!
!!      integer(c_int) function num_label_pvr_projection_f() bind(c)
!!      subroutine set_label_pvr_projection_f(names)  bind(c)
!!
!!      integer(c_int) function num_label_pvr_area_f() bind(c)
!!      subroutine set_label_pvr_area_f(names)  bind(c)
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
!!      integer(c_int) function num_label_pvr_cmap_bar_f() bind(c)
!!      subroutine set_label_pvr_cmap_bar_f(names)  bind(c)
!!
!!      integer(c_int) function num_label_pvr_section_f() bind(c)
!!      subroutine set_label_pvr_section_f(names)  bind(c)
!!
!!      integer(c_int) function num_label_pvr_isosurface_f() bind(c)
!!      subroutine set_label_pvr_isosurface_f(names)  bind(c)
!!
!!      integer(c_int) function num_label_pvr_movie_f() bind(c)
!!      subroutine set_label_pvr_movie_f(names)  bind(c)
!!
!!      integer(c_int) function num_flag_pvr_movie_mode_f() bind(c)
!!      subroutine set_flag_pvr_movie_mode_f(names)  bind(c)
!!
!!      integer(c_int) function num_flag_pvr_isosurf_dir_f() bind(c)
!!      subroutine set_flag_pvr_isosurf_dir_f(names)  bind(c)
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
! ----------------------------------------------------------------------
!
      integer(c_int) function num_label_pvr_ctl_f() bind(c)
!
      use read_pvr_control
!
      num_label_pvr_ctl_f = num_label_pvr_ctl()
      return
      end function num_label_pvr_ctl_f
!
! ----------------------------------------------------------------------
!
      integer(c_int) function num_label_pvr_ctl_w_dup_f() bind(c)
!
      use read_pvr_control
!
      num_label_pvr_ctl_w_dup_f = num_label_pvr_ctl_w_dup()
      return
      end function num_label_pvr_ctl_w_dup_f
!
! ----------------------------------------------------------------------
!
      subroutine set_label_pvr_ctl_w_dup_f(names)  bind(c)
!
      use read_pvr_control
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_label_pvr_ctl_w_dup(names)
      end subroutine set_label_pvr_ctl_w_dup_f
!
!  ---------------------------------------------------------------------
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
      use t_ctl_data_4_screen_pixel
!
      num_label_pvr_pixels_f = num_label_pvr_pixels()
      return
      end function num_label_pvr_pixels_f
!
! ----------------------------------------------------------------------
!
      integer(c_int) function num_label_pvr_streo_f() bind(c)
!
      use t_ctl_data_4_streo_view
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
      use t_ctl_data_4_screen_pixel
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
      use t_ctl_data_4_streo_view
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_label_pvr_streo(names)
      end subroutine set_label_pvr_streo_f
!
!  ---------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_label_pvr_projection_f() bind(c)
!
      use t_ctl_data_4_projection
!
      num_label_pvr_projection_f = num_label_pvr_projection()
      return
      end function num_label_pvr_projection_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_pvr_projection_f(names)  bind(c)
!
      use t_ctl_data_4_projection
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_label_pvr_projection(names)
      end subroutine set_label_pvr_projection_f
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(c_int) function num_label_pvr_area_f() bind(c)
!
      use t_control_data_pvr_area
!
      num_label_pvr_area_f = num_label_pvr_area()
      return
      end function num_label_pvr_area_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_pvr_area_f(names)  bind(c)
!
      use t_control_data_pvr_area
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_label_pvr_area(names)
      end subroutine set_label_pvr_area_f
!
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_label_pvr_cmap_bar_f() bind(c)
!
      use t_ctl_data_pvr_colormap_bar
!
      num_label_pvr_cmap_bar_f = num_label_pvr_cmap_bar()
      return
      end function num_label_pvr_cmap_bar_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_pvr_cmap_bar_f(names)  bind(c)
!
      use t_ctl_data_pvr_colormap_bar
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_label_pvr_cmap_bar(names)
      end subroutine set_label_pvr_cmap_bar_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_label_pvr_isosurface_f() bind(c)
!
      use t_control_data_pvr_isosurfs
!
      num_label_pvr_isosurface_f = num_label_pvr_isosurface()
      return
      end function num_label_pvr_isosurface_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_pvr_isosurface_f(names)  bind(c)
!
      use t_control_data_pvr_isosurfs
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_label_pvr_isosurface(names)
      end subroutine set_label_pvr_isosurface_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_label_pvr_section_f() bind(c)
!
      use t_control_data_pvr_sections
!
      num_label_pvr_section_f = num_label_pvr_section()
      return
      end function num_label_pvr_section_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_pvr_section_f(names)  bind(c)
!
      use t_control_data_pvr_sections
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_label_pvr_section(names)
      end subroutine set_label_pvr_section_f
!
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_flag_pvr_movie_mode_f() bind(c)
!
      use t_control_params_4_pvr
!
      num_flag_pvr_movie_mode_f = num_flag_pvr_movie_mode()
      return
      end function num_flag_pvr_movie_mode_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_flag_pvr_movie_mode_f(names)  bind(c)
!
      use t_control_params_4_pvr
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_flag_pvr_movie_mode(names)
      end subroutine set_flag_pvr_movie_mode_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_flag_pvr_isosurf_dir_f() bind(c)
!
      use pvr_surface_enhancement
!
      num_flag_pvr_isosurf_dir_f = num_flag_pvr_isosurf_dir()
      return
      end function num_flag_pvr_isosurf_dir_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_flag_pvr_isosurf_dir_f(names)  bind(c)
!
      use pvr_surface_enhancement
!
      character(C_CHAR), intent(inout) :: names(*)
!
      call set_flag_pvr_isosurf_dir(names)
      end subroutine set_flag_pvr_isosurf_dir_f
!
! ----------------------------------------------------------------------
!
      end module PVR_control_label_to_c
