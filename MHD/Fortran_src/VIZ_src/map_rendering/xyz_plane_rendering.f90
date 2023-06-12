!>@file   xyz_plane_rendering.f90
!!@brief  module xyz_plane_rendering
!!
!!@author H. Matsui
!!@date Programmed in July, 2023
!
!>@brief Subroutines to draw lines on map
!!
!!@verbatim
!!      subroutine xy_plane_rendering(time_d, psf_nod, psf_ele,         &
!!     &          psf_phys, color_param, cbar_param, map_data, pvr_rgb)
!!      subroutine xz_plane_rendering(time_d, psf_nod, psf_ele,         &
!!     &          psf_phys, color_param, cbar_param, map_data, pvr_rgb)
!!      subroutine yz_plane_rendering(time_d, psf_nod, psf_ele,         &
!!     &          psf_phys, color_param, cbar_param, map_data, pvr_rgb)
!!        type(time_data), intent(in) :: time_d
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        type(pvr_colorbar_parameter), intent(in) :: cbar_param
!!        type(phys_data), intent(in) :: psf_phys
!!        type(node_data), intent(in) :: psf_nod
!!        type(element_data), intent(in) :: psf_ele
!!        type(map_rendering_data), intent(inout) :: map_data
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!@endverbatim
      module xyz_plane_rendering
!
      use m_precision
      use m_constants
!
      use t_psf_patch_data
      use t_time_data
      use t_file_IO_parameter
      use t_map_patch_from_1patch
      use t_pvr_image_array
      use t_map_rendering_data
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine xy_plane_rendering(time_d, psf_nod, psf_ele,           &
     &          psf_phys, color_param, cbar_param, map_data, pvr_rgb)
!
      use set_scalar_on_xyz_plane
      use set_map_values_for_grids
      use draw_pixels_on_map
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
      real(kind = kreal) :: plane_z = 0.0
      real(kind = kreal) :: ref_r = 0.0
!
      logical :: fill_flag = .TRUE.
      integer(kind = kint) :: num_line = 0
!
!
      if(my_rank .ne. pvr_rgb%irank_image_file) return
!
      call alloc_map_patch_from_1patch(map_e1)
      call set_scalar_on_xy_plane(psf_nod, psf_ele, psf_phys,           &
     &    map_data%xmin_frame, map_data%xmax_frame,                     &
     &    map_data%ymin_frame, map_data%ymax_frame,                     &
     &    pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),                 &
     &    pvr_rgb%num_pixel_xy, map_data%d_map, pvr_rgb%rgba_real_gl,   &
     &    map_e1)
      call dealloc_map_patch_from_1patch(map_e1)
!
      if(fill_flag) then
        call map_value_to_rgb                                           &
     &     (color_param, pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),  &
     &      map_data%d_map, pvr_rgb%rgba_real_gl)
      end if
!
      if(map_data%flag_zeroline) then
        call draw_zeroline                                              &
     &     (pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      map_data%d_map, pvr_rgb%rgba_real_gl)
      end if
!
      if(num_line .gt. 0) then
        call draw_isolines                                              &
     &     (pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2), color_param,  &
     &      num_line, map_data%d_map, pvr_rgb%rgba_real_gl)
      end if
!
      call map_value_to_projected_r                                     &
     &   (map_data%xmin_frame, map_data%xmax_frame,                     &
     &    map_data%ymin_frame, map_data%ymax_frame,                     &
     &    pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2), map_data%d_map)
!
      if(plane_z .lt. map_data%tangent_cylinder_radius(1)) then
        ref_r = sqrt(map_data%tangent_cylinder_radius(1)**2             &
     &             - plane_z**2)
        call draw_radius_grid                                           &
     &     (pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2), ref_r,        &
     &      map_data%d_map, pvr_rgb%rgba_real_gl)
      end if
      ref_r = sqrt(map_data%tangent_cylinder_radius(2)**2               &
     &           - plane_z**2)
      call draw_radius_grid                                             &
     &   (pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2), ref_r,          &
     &    map_data%d_map, pvr_rgb%rgba_real_gl)
!
      call fill_background                                              &
     &   (pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),                 &
     &    color_param%bg_rgba_real, pvr_rgb%rgba_real_gl)
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
!
      end subroutine xy_plane_rendering
!
!  ---------------------------------------------------------------------
!
      subroutine xz_plane_rendering(time_d, psf_nod, psf_ele,           &
     &          psf_phys, color_param, cbar_param, map_data, pvr_rgb)
!
      use set_scalar_on_xyz_plane
      use set_map_values_for_grids
      use draw_pixels_on_map
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
      logical :: fill_flag = .TRUE.
      integer(kind = kint) :: num_line = 0
!
!
      if(my_rank .ne. pvr_rgb%irank_image_file) return
!
      call alloc_map_patch_from_1patch(map_e1)
      call set_scalar_on_xz_plane(psf_nod, psf_ele, psf_phys,           &
     &    map_data%xmin_frame, map_data%xmax_frame,                     &
     &    map_data%ymin_frame, map_data%ymax_frame,                     &
     &    pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),                 &
     &    pvr_rgb%num_pixel_xy, map_data%d_map, pvr_rgb%rgba_real_gl,   &
     &    map_e1)
      call dealloc_map_patch_from_1patch(map_e1)
!
      if(fill_flag) then
        call map_value_to_rgb                                           &
     &     (color_param, pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),  &
     &      map_data%d_map, pvr_rgb%rgba_real_gl)
      end if
!
      if(map_data%flag_zeroline) then
        call draw_zeroline                                              &
     &     (pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      map_data%d_map, pvr_rgb%rgba_real_gl)
      end if
!
      if(num_line .gt. 0) then
        call draw_isolines                                              &
     &     (pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2), color_param,  &
     &      num_line, map_data%d_map, pvr_rgb%rgba_real_gl)
      end if
!
      if(map_data%flag_tangent_cylinder) then
        call map_value_to_projected_x                                   &
     &     (map_data%xmin_frame, map_data%xmax_frame,                   &
     &      map_data%ymin_frame, map_data%ymax_frame,                   &
     &      pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      map_data%d_map)
        call draw_med_tangent_cyl_grid                                  &
     &     (pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      map_data%tangent_cylinder_radius(1), map_data%d_map,        &
     &      pvr_rgb%rgba_real_gl)
      end if
!
      call map_value_to_projected_r                                     &
     &   (map_data%xmin_frame, map_data%xmax_frame,                     &
     &    map_data%ymin_frame, map_data%ymax_frame,                     &
     &    pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2), map_data%d_map)
!
      call draw_radius_grid                                           &
     &   (pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),                 &
     &    map_data%tangent_cylinder_radius(1), map_data%d_map,          &
     &    pvr_rgb%rgba_real_gl)
      call draw_radius_grid                                             &
     &   (pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),                 &
     &    map_data%tangent_cylinder_radius(2), map_data%d_map,          &
     &    pvr_rgb%rgba_real_gl)
!
      call fill_background                                              &
     &   (pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),                 &
     &    color_param%bg_rgba_real, pvr_rgb%rgba_real_gl)
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
!
      end subroutine xz_plane_rendering
!
!  ---------------------------------------------------------------------
!
      subroutine yz_plane_rendering(time_d, psf_nod, psf_ele,           &
     &          psf_phys, color_param, cbar_param, map_data, pvr_rgb)
!
      use set_scalar_on_xyz_plane
      use set_map_values_for_grids
      use draw_pixels_on_map
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
      logical :: fill_flag = .TRUE.
      integer(kind = kint) :: num_line = 0
!
!
      if(my_rank .ne. pvr_rgb%irank_image_file) return
!
      call alloc_map_patch_from_1patch(map_e1)
      call set_scalar_on_yz_plane(psf_nod, psf_ele, psf_phys,           &
     &    map_data%xmin_frame, map_data%xmax_frame,                     &
     &    map_data%ymin_frame, map_data%ymax_frame,                     &
     &    pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),                 &
     &    pvr_rgb%num_pixel_xy, map_data%d_map, pvr_rgb%rgba_real_gl,   &
     &    map_e1)
      call dealloc_map_patch_from_1patch(map_e1)
!
      if(fill_flag) then
        call map_value_to_rgb                                           &
     &     (color_param, pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),  &
     &      map_data%d_map, pvr_rgb%rgba_real_gl)
      end if
!
      if(map_data%flag_zeroline) then
        call draw_zeroline                                              &
     &     (pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      map_data%d_map, pvr_rgb%rgba_real_gl)
      end if
!
      if(num_line .gt. 0) then
        call draw_isolines                                              &
     &     (pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2), color_param,  &
     &      num_line, map_data%d_map, pvr_rgb%rgba_real_gl)
      end if
!
      if(map_data%flag_tangent_cylinder) then
        call map_value_to_projected_x                                   &
     &     (map_data%xmin_frame, map_data%xmax_frame,                   &
     &      map_data%ymin_frame, map_data%ymax_frame,                   &
     &      pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      map_data%d_map)
        call draw_med_tangent_cyl_grid                                  &
     &     (pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      map_data%tangent_cylinder_radius(1), map_data%d_map,        &
     &      pvr_rgb%rgba_real_gl)
      end if
!
      call map_value_to_projected_r                                     &
     &   (map_data%xmin_frame, map_data%xmax_frame,                     &
     &    map_data%ymin_frame, map_data%ymax_frame,                     &
     &    pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2), map_data%d_map)
!
      call draw_radius_grid                                           &
     &   (pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),                 &
     &    map_data%tangent_cylinder_radius(1), map_data%d_map,          &
     &    pvr_rgb%rgba_real_gl)
      call draw_radius_grid                                             &
     &   (pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),                 &
     &    map_data%tangent_cylinder_radius(2), map_data%d_map,          &
     &    pvr_rgb%rgba_real_gl)
!
      call fill_background                                              &
     &   (pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),                 &
     &    color_param%bg_rgba_real, pvr_rgb%rgba_real_gl)
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
!
      end subroutine yz_plane_rendering
!
!  ---------------------------------------------------------------------
!
      end module xyz_plane_rendering
