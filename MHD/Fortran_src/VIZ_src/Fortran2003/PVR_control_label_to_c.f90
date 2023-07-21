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
!!      subroutine set_label_pvr_ctl_w_dup_f(names_c)  bind(c)
!!
!!      integer(c_int) function num_label_pvr_modelview_f() bind(c)
!!      integer(c_int) function num_label_pvr_pixels_f() bind(c)
!!      integer(c_int) function num_label_pvr_streo_f() bind(c)
!!      subroutine set_label_pvr_modelview_f(names_c)  bind(c)
!!      subroutine set_label_pvr_pixels_f(names_c)  bind(c)
!!      subroutine set_label_pvr_streo_f(names_c)  bind(c)
!!
!!      integer(c_int) function num_label_pvr_projection_f() bind(c)
!!      subroutine set_label_pvr_projection_f(names_c)  bind(c)
!!
!!      integer(c_int) function num_label_pvr_area_f() bind(c)
!!      subroutine set_label_pvr_area_f(names_c)  bind(c)
!!
!!      integer(c_int) function num_label_pvr_light_f() bind(c)
!!      subroutine set_label_pvr_light_f(names_c)  bind(c)
!!
!!      integer(c_int) function num_label_pvr_colormap_f() bind(c)
!!      subroutine set_label_pvr_colormap_f(names_c)  bind(c)
!!
!!      integer(c_int) function num_label_pvr_colorbar_f() bind(c)
!!      subroutine set_label_pvr_colorbar_f(names_c)  bind(c)
!!
!!      integer(c_int) function num_label_pvr_cmap_bar_f() bind(c)
!!      subroutine set_label_pvr_cmap_bar_f(names_c)  bind(c)
!!
!!      integer(c_int) function num_label_pvr_section_f() bind(c)
!!      subroutine set_label_pvr_section_f(names_c)  bind(c)
!!
!!      integer(c_int) function num_label_pvr_isosurface_f() bind(c)
!!      subroutine set_label_pvr_isosurface_f(names_c)  bind(c)
!!
!!      integer(c_int) function num_label_pvr_movie_f() bind(c)
!!      subroutine set_label_pvr_movie_f(names_c)  bind(c)
!!
!!      integer(c_int) function num_label_quilt_image_f() bind(c)
!!      subroutine set_label_quilt_image_f(names_c)  bind(c)
!!
!!      integer(c_int) function num_flag_pvr_movie_mode_f() bind(c)
!!      subroutine set_flag_pvr_movie_mode_f(names_c)  bind(c)
!!
!!      integer(c_int) function num_flag_pvr_isosurf_dir_f() bind(c)
!!      subroutine set_flag_pvr_isosurf_dir_f(names_c)  bind(c)
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
      use ctl_data_each_pvr_IO
!
      num_label_pvr_ctl_f = num_label_pvr_ctl()
      return
      end function num_label_pvr_ctl_f
!
! ----------------------------------------------------------------------
!
      integer(c_int) function num_label_pvr_ctl_w_dup_f() bind(c)
!
      use ctl_data_each_pvr_IO
!
      num_label_pvr_ctl_w_dup_f = num_label_pvr_ctl_w_dup()
      return
      end function num_label_pvr_ctl_w_dup_f
!
! ----------------------------------------------------------------------
!
      subroutine set_label_pvr_ctl_w_dup_f(names_c)  bind(c)
!
      use ctl_data_each_pvr_IO
!
      type(C_ptr), value :: names_c
!
      character(len=kchara), pointer :: name_f(:)
!
      call c_f_pointer(names_c, name_f, [num_label_pvr_ctl_w_dup()])
      call set_label_pvr_ctl_w_dup(name_f)
      end subroutine set_label_pvr_ctl_w_dup_f
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(c_int) function num_label_pvr_modelview_f() bind(c)
!
      use ctl_data_view_transfer_IO
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
      subroutine set_label_pvr_modelview_f(names_c)  bind(c)
!
      use ctl_data_view_transfer_IO
!
      type(C_ptr), value :: names_c
!
      character(len=kchara), pointer :: name_f(:)
!
      call c_f_pointer(names_c, name_f, [num_label_pvr_modelview()])
      call set_label_pvr_modelview(name_f)
      end subroutine set_label_pvr_modelview_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_pvr_pixels_f(names_c)  bind(c)
!
      use t_ctl_data_4_screen_pixel
!
      type(C_ptr), value :: names_c
!
      character(len=kchara), pointer :: name_f(:)
!
      call c_f_pointer(names_c, name_f, [num_label_pvr_pixels()])
      call set_label_pvr_pixels(name_f)
      end subroutine set_label_pvr_pixels_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_pvr_streo_f(names_c)  bind(c)
!
      use t_ctl_data_4_streo_view
!
      type(C_ptr), value :: names_c
!
      character(len=kchara), pointer :: name_f(:)
!
      call c_f_pointer(names_c, name_f, [num_label_pvr_streo()])
      call set_label_pvr_streo(name_f)
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
      subroutine set_label_pvr_projection_f(names_c)  bind(c)
!
      use t_ctl_data_4_projection
!
      type(C_ptr), value :: names_c
!
      character(len=kchara), pointer :: name_f(:)
!
      call c_f_pointer(names_c, name_f, [num_label_pvr_projection()])
      call set_label_pvr_projection(name_f)
      end subroutine set_label_pvr_projection_f
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(c_int) function num_label_pvr_area_f() bind(c)
!
      use t_ctl_data_pvr_area
!
      num_label_pvr_area_f = num_label_pvr_area()
      return
      end function num_label_pvr_area_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_pvr_area_f(names_c)  bind(c)
!
      use t_ctl_data_pvr_area
!
      type(C_ptr), value :: names_c
!
      character(len=kchara), pointer :: name_f(:)
!
      call c_f_pointer(names_c, name_f, [num_label_pvr_area()])
      call set_label_pvr_area(name_f)
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
      subroutine set_label_pvr_light_f(names_c)  bind(c)
!
      use t_ctl_data_pvr_light
!
      type(C_ptr), value :: names_c
!
      character(len=kchara), pointer :: name_f(:)
!
      call c_f_pointer(names_c, name_f, [num_label_pvr_light()])
      call set_label_pvr_light(name_f)
      end subroutine set_label_pvr_light_f
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(c_int) function num_label_pvr_colormap_f() bind(c)
!
      use ctl_data_pvr_colormap_IO
!
      num_label_pvr_colormap_f = num_label_pvr_colormap()
      return
      end function num_label_pvr_colormap_f
!
! ----------------------------------------------------------------------
!
      subroutine set_label_pvr_colormap_f(names_c)  bind(c)
!
      use ctl_data_pvr_colormap_IO
!
      type(C_ptr), value :: names_c
!
      character(len=kchara), pointer :: name_f(:)
!
      call c_f_pointer(names_c, name_f, [num_label_pvr_colormap()])
      call set_label_pvr_colormap(name_f)
      end subroutine set_label_pvr_colormap_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_label_pvr_colorbar_f() bind(c)
!
      use ctl_data_pvr_colorbar_IO
!
      num_label_pvr_colorbar_f = num_label_pvr_colorbar()
      return
      end function num_label_pvr_colorbar_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_pvr_colorbar_f(names_c)  bind(c)
!
      use ctl_data_pvr_colorbar_IO
!
      type(C_ptr), value :: names_c
!
      character(len=kchara), pointer :: name_f(:)
!
      call c_f_pointer(names_c, name_f, [num_label_pvr_colorbar()])
      call set_label_pvr_colorbar(name_f)
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
      subroutine set_label_pvr_cmap_bar_f(names_c)  bind(c)
!
      use t_ctl_data_pvr_colormap_bar
!
      type(C_ptr), value :: names_c
!
      character(len=kchara), pointer :: name_f(:)
!
      call c_f_pointer(names_c, name_f, [num_label_pvr_cmap_bar()])
      call set_label_pvr_cmap_bar(name_f)
      end subroutine set_label_pvr_cmap_bar_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_label_pvr_isosurface_f() bind(c)
!
      use t_ctl_data_pvr_isosurface
!
      num_label_pvr_isosurface_f = num_label_pvr_isosurface()
      return
      end function num_label_pvr_isosurface_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_pvr_isosurface_f(names_c)  bind(c)
!
      use t_ctl_data_pvr_isosurface
!
      type(C_ptr), value :: names_c
!
      character(len=kchara), pointer :: name_f(:)
!
      call c_f_pointer(names_c, name_f, [num_label_pvr_isosurface()])
      call set_label_pvr_isosurface(name_f)
      end subroutine set_label_pvr_isosurface_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_label_pvr_section_f() bind(c)
!
      use t_ctl_data_pvr_section
!
      num_label_pvr_section_f = num_label_pvr_section()
      return
      end function num_label_pvr_section_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_pvr_section_f(names_c)  bind(c)
!
      use t_ctl_data_pvr_section
!
      type(C_ptr), value :: names_c
!
      character(len=kchara), pointer :: name_f(:)
!
      call c_f_pointer(names_c, name_f, [num_label_pvr_section()])
      call set_label_pvr_section(name_f)
      end subroutine set_label_pvr_section_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_label_pvr_movie_f() bind(c)
!
      use ctl_data_pvr_movie_IO
!
      num_label_pvr_movie_f = num_label_pvr_movie()
      return
      end function num_label_pvr_movie_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_pvr_movie_f(names_c)  bind(c)
!
      use ctl_data_pvr_movie_IO
!
      type(C_ptr), value :: names_c
!
      character(len=kchara), pointer :: name_f(:)
!
      call c_f_pointer(names_c, name_f, [num_label_pvr_movie()])
      call set_label_pvr_movie(name_f)
      end subroutine set_label_pvr_movie_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_label_quilt_image_f() bind(c)
!
      use t_ctl_data_quilt_image
!
      num_label_quilt_image_f = num_label_quilt_image()
      return
      end function num_label_quilt_image_f
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_quilt_image_f(names_c)  bind(c)
!
      use t_ctl_data_quilt_image
!
      type(C_ptr), value :: names_c
!
      character(len=kchara), pointer :: name_f(:)
!
      call c_f_pointer(names_c, name_f, [num_label_quilt_image()])
      call set_label_quilt_image(name_f)
      end subroutine set_label_quilt_image_f
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
      subroutine set_flag_pvr_movie_mode_f(names_c)  bind(c)
!
      use t_control_params_4_pvr
!
      type(C_ptr), value :: names_c
!
      character(len=kchara), pointer :: name_f(:)
!
      call c_f_pointer(names_c, name_f, [num_flag_pvr_movie_mode()])
      call set_flag_pvr_movie_mode(name_f)
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
      subroutine set_flag_pvr_isosurf_dir_f(names_c)  bind(c)
!
      use pvr_surface_enhancement
!
      type(C_ptr), value :: names_c
!
      character(len=kchara), pointer :: name_f(:)
!
      call c_f_pointer(names_c, name_f, [num_flag_pvr_isosurf_dir()])
      call set_flag_pvr_isosurf_dir(name_f)
      end subroutine set_flag_pvr_isosurf_dir_f
!
! ----------------------------------------------------------------------
!
      end module PVR_control_label_to_c
