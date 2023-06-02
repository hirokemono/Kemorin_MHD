!>@file   t_map_rendering_data.f90
!!@brief  module t_map_rendering_data
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!@n    modified in July, 2014
!
!>@brief Structure for cross sectioning
!!
!!@verbatim
!!      subroutine alloc_scalar_on_map(num_pixel, map_data)
!!      subroutine dealloc_scalar_on_map(map_data)
!!        integer(kind = kint), intent(in) :: num_pixel
!!        type(map_rendering_data), intent(inout) :: map_data
!!
!!      subroutine init_map_rendering_data                              &
!!     &         (view_param, cbar_param, pvr_rgb, map_data)
!!        type(pvr_view_parameter), intent(in):: view_param
!!        type(pvr_colorbar_parameter), intent(in) :: cbar_param
!!        type(pvr_image_type), intent(in) :: pvr_rgb
!!        type(map_rendering_data), intent(inout) :: map_data
!!      subroutine cal_map_rendering_data(time_d, psf_nod, psf_ele,     &
!!     &          psf_phys, color_param, cbar_param, map_data, pvr_rgb)
!!        type(time_data), intent(in) :: time_d
!!        type(pvr_view_parameter), intent(in):: view_param
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        type(pvr_colorbar_parameter), intent(in) :: cbar_param
!!        type(phys_data), intent(in) :: psf_phys
!!        type(node_data), intent(in) :: psf_nod
!!        type(element_data), intent(in) :: psf_ele
!!        type(map_rendering_data), intent(inout) :: map_data
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!@endverbatim
      module t_map_rendering_data
!
      use calypso_mpi
      use m_precision
!
      use t_geometry_data
      use t_phys_data
      use t_control_params_4_pvr
      use t_pvr_colormap_parameter
!
      implicit  none
!
      real(kind= kreal), parameter, private :: xframe = 2.4
      real(kind= kreal), parameter, private :: yframe = 1.8
!
      type map_rendering_data
        real(kind= kreal) :: xmin_frame = -xframe
        real(kind= kreal) :: xmax_frame =  xframe
        real(kind= kreal) :: ymin_frame = -yframe
        real(kind= kreal) :: ymax_frame =  yframe
!
        real(kind = kreal) :: tangent_cylinder_theta(2)
!
        real(kind = kreal), allocatable :: d_map(:)
      end type map_rendering_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_scalar_on_map(num_pixel, map_data)
!
      integer(kind = kint), intent(in) :: num_pixel
      type(map_rendering_data), intent(inout) :: map_data
!
      allocate(map_data%d_map(num_pixel))
!
      if(num_pixel .le. 0) return
!$omp parallel workshare
      map_data%d_map(num_pixel) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_scalar_on_map
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_scalar_on_map(map_data)
!
      type(map_rendering_data), intent(inout) :: map_data
!
      deallocate(map_data%d_map)
!
      end subroutine dealloc_scalar_on_map
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine init_map_rendering_data                                &
     &         (view_param, cbar_param, pvr_rgb, map_data)
!
      use t_psf_patch_data
      use t_pvr_image_array
!
      type(pvr_view_parameter), intent(in):: view_param
      type(pvr_colorbar_parameter), intent(in) :: cbar_param
      type(pvr_image_type), intent(in) :: pvr_rgb
!
      type(map_rendering_data), intent(inout) :: map_data
!
      real(kind = kreal) :: xtmp, ytmp
      real(kind = kreal) :: aspect, pi
!
!
      if(my_rank .ne. pvr_rgb%irank_image_file) return
!
        pi = four*atan(one)
        map_data%tangent_cylinder_theta(1)                              &
     &            = asin(cbar_param%tangent_cylinder_radius(2)          &
     &                 / cbar_param%tangent_cylinder_radius(1))
        map_data%tangent_cylinder_theta(2)                              &
     &            = pi - map_data%tangent_cylinder_theta(1)
!
      aspect =  view_param%perspective_xy_ratio
!
      ytmp = xframe / aspect
      xtmp = yframe * aspect
      if(ytmp .le. 1.0) then
        map_data%xmin_frame = -xtmp
        map_data%xmax_frame =  xtmp
        map_data%ymin_frame = -yframe
        map_data%ymax_frame =  yframe
      else
        map_data%xmin_frame = -xframe
        map_data%xmax_frame =  xframe
        map_data%ymin_frame = -ytmp
        map_data%ymax_frame =  ytmp
      end if
!
      end subroutine init_map_rendering_data
!
!  ---------------------------------------------------------------------
!
      subroutine cal_map_rendering_data(time_d, psf_nod, psf_ele,       &
     &          psf_phys, color_param, cbar_param, map_data, pvr_rgb)
!
      use t_psf_patch_data
      use t_time_data
      use t_file_IO_parameter
      use t_map_patch_from_1patch
      use t_pvr_image_array
!
      use set_ucd_data_to_type
      use ucd_IO_select
!
      use draw_aitoff_map
      use draw_lines_on_map
      use draw_pvr_colorbar
!
      type(time_data), intent(in) :: time_d
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(pvr_colorbar_parameter), intent(in) :: cbar_param
      type(phys_data), intent(in) :: psf_phys
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
!
      type(map_rendering_data), intent(inout) :: map_data
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
      type(map_patches_for_1patch) :: map_e1
!
!
      if(my_rank .ne. pvr_rgb%irank_image_file) return
!
      call alloc_map_patch_from_1patch(ione, map_e1)
      call set_scalar_on_map_image(psf_nod, psf_ele, psf_phys,          &
     &    map_data%xmin_frame, map_data%xmax_frame,                     &
     &    map_data%ymin_frame, map_data%ymax_frame,                     &
     &    pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),                 &
     &    pvr_rgb%num_pixel_xy, map_data%d_map, pvr_rgb%rgba_real_gl,   &
     &    map_e1)
      call map_value_to_rgb                                             &
     &   (color_param, pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),    &
     &    pvr_rgb%num_pixel_xy, map_data%d_map, pvr_rgb%rgba_real_gl)
!
        call draw_aitoff_map_zeroline                                   &
     &     (pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      pvr_rgb%num_pixel_xy, map_data%d_map, pvr_rgb%rgba_real_gl)
      call dealloc_map_patch_from_1patch(map_e1)
!
!      if(cbar_param%flag_draw_tangent_cylinder) then
        call draw_aitoff_lat_line                                       &
     &     (map_data%xmin_frame, map_data%xmax_frame,                   &
     &      map_data%ymin_frame, map_data%ymax_frame,                   &
     &      map_data%tangent_cylinder_theta(1),                         &
     &      cbar_param%tangent_cylinder_rgba,                           &
     &      pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      pvr_rgb%num_pixel_xy, pvr_rgb%rgba_real_gl)
        call draw_aitoff_lat_line                                       &
     &     (map_data%xmin_frame, map_data%xmax_frame,                   &
     &      map_data%ymin_frame, map_data%ymax_frame,                   &
     &      map_data%tangent_cylinder_theta(2),                         &
     &      cbar_param%tangent_cylinder_rgba,                           &
     &      pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      pvr_rgb%num_pixel_xy, pvr_rgb%rgba_real_gl)
!      end if
!
      if(cbar_param%flag_draw_mapgrid) then
        call draw_aitoff_map_frame                                      &
     &     (map_data%xmin_frame, map_data%xmax_frame,                   &
     &      map_data%ymin_frame, map_data%ymax_frame,                   &
     &      pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      pvr_rgb%num_pixel_xy, pvr_rgb%rgba_real_gl)
      end if
!
      if(cbar_param%flag_pvr_colorbar) then
        call set_pvr_colorbar(pvr_rgb%num_pixel_xy, pvr_rgb%num_pixels, &
     &      color_param, cbar_param, pvr_rgb%rgba_real_gl(1,1))
      end if
!
      if(cbar_param%flag_draw_time) then
        call set_pvr_timelabel                                          &
     &     (time_d%time, pvr_rgb%num_pixel_xy, pvr_rgb%num_pixels,      &
     &      cbar_param, pvr_rgb%rgba_real_gl(1,1))
      end if
      call dealloc_scalar_on_map(map_data)
!
      end subroutine cal_map_rendering_data
!
!  ---------------------------------------------------------------------
!
      end module t_map_rendering_data
