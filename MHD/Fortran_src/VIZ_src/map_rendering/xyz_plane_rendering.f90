!>@file   xyz_plane_rendering.f90
!!@brief  module xyz_plane_rendering
!!
!!@author H. Matsui
!!@date Programmed in July, 2023
!
!>@brief Subroutines to draw lines on map
!!
!!@verbatim
!!      subroutine aitoff_projection_rendering(time_d, psf_nod, psf_ele,&
!!     &          psf_phys, color_param, cbar_param, map_data, pvr_rgb)
!!      subroutine s_xyz_plane_rendering(time_d, psf_nod, psf_ele,      &
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
      subroutine aitoff_projection_rendering(time_d, psf_nod, psf_ele,  &
     &          psf_phys, color_param, cbar_param, map_data, pvr_rgb)
!
      use set_ucd_data_to_type
      use ucd_IO_select
!
      use draw_aitoff_map
      use draw_lines_on_map
      use draw_pvr_colorbar
      use draw_pixels_on_map
      use set_map_values_for_grids
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
      real(kind = kreal), parameter                                     &
     &                   :: black(4) = (/zero,zero,zero,one/)
      real(kind = kreal), parameter                                     &
     &                   :: white(4) = (/one,one,one,one/)
!
!
      if(my_rank .ne. pvr_rgb%irank_image_file) return
!
!
      call alloc_map_patch_from_1patch(map_e1)
      if(map_data%fill_flag) then
        call set_scalar_on_map_image                                    &
     &     (color_param, psf_nod, psf_ele, psf_phys%d_fld(1,1),         &
     &      map_data, pvr_rgb, map_e1)
!
        if(map_data%flag_zeroline .and. (map_data%num_line.le.0)) then
          call draw_isoline_on_map_image                                &
     &       (psf_nod, psf_ele, psf_phys%d_fld(1,1), 3, 0,              &
     &        map_data%xmin_frame, map_data%xmax_frame,                 &
     &        map_data%ymin_frame, map_data%ymax_frame,                 &
     &        pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),             &
     &        zero, black, pvr_rgb%rgba_real_gl, map_e1)
        end if
      else
        call fill_map_one_color                                         &
     &      (pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),              &
     &       color_param%bg_rgba_real, pvr_rgb%rgba_real_gl)
      end if
!
      if(map_data%num_line .gt. 0) then
        call draw_aitoff_map_isolines                                   &
     &     (psf_nod, psf_ele, psf_phys%d_fld(1,2), map_data,            &
     &      color_param, pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),  &
     &      pvr_rgb%rgba_real_gl, map_e1)
!
        if(map_data%flag_zeroline                                       &
     &        .and. (map_data%fill_flag.eqv. .FALSE.)) then
          call draw_isoline_on_map_image                                &
     &       (psf_nod, psf_ele, psf_phys%d_fld(1,2), 2, 0,              &
     &        map_data%xmin_frame, map_data%xmax_frame,                 &
     &        map_data%ymin_frame, map_data%ymax_frame,                 &
     &        pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),             &
     &        zero, white, pvr_rgb%rgba_real_gl, map_e1)
        end if
      end if
!
      call map_value_to_colatitude                                      &
     &   (map_data%xmin_frame, map_data%xmax_frame,                     &
     &    map_data%ymin_frame, map_data%ymax_frame,                     &
     &    pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2), map_data%d_map)
      call draw_latitude_grid(map_data, color_param%bg_rgba_real,       &
     &                        pvr_rgb)
      if(map_data%flag_tangent_cylinder) then
        call draw_map_tangent_cyl_grid                                  &
     &     (map_data, color_param%bg_rgba_real,                         &
     &      map_data%tangent_cylinder_theta, pvr_rgb)
      end if
!
      call map_value_to_longitude                                       &
     &   (map_data%xmin_frame, map_data%xmax_frame,                     &
     &    map_data%ymin_frame, map_data%ymax_frame,                     &
     &    pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2), map_data%d_map)
      call draw_longitude_grid(map_data, color_param%bg_rgba_real,      &
     &                         pvr_rgb)
      call draw_mapflame(map_data, color_param%bg_rgba_real,            &
     &                   pvr_rgb)
      call dealloc_map_patch_from_1patch(map_e1)
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
      end subroutine aitoff_projection_rendering
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine s_xyz_plane_rendering(time_d, psf_nod, psf_ele,        &
     &          psf_phys, color_param, cbar_param, map_data, pvr_rgb)
!
      use set_scalar_on_xyz_plane
      use set_map_values_for_grids
      use draw_pixels_on_map
      use draw_lines_on_map
      use draw_pvr_colorbar
      use cal_mesh_position
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
      real(kind = kreal), parameter                                     &
     &                   :: black(4) = (/zero,zero,zero,one/)
      real(kind = kreal), parameter                                     &
     &                   :: white(4) = (/one,one,one,one/)
!
!
      if(my_rank .ne. pvr_rgb%irank_image_file) return
!
      call alloc_map_patch_from_1patch(map_e1)
!
      if(map_data%fill_flag) then
        call sel_scalar_on_xyz_plane                                    &
     &     (color_param, psf_nod, psf_ele, psf_phys%d_fld(1,1),         &
     &      map_data, pvr_rgb, map_e1)
!
        if(map_data%flag_zeroline .and. (map_data%num_line.le.0)) then
          call sel_draw_isoline_on_xyz_plane                            &
     &       (psf_nod, psf_ele, psf_phys%d_fld(1,1), 3, 0,              &
     &        map_data, zero, black, pvr_rgb, map_e1)
        end if
      else
        call fill_map_one_color                                         &
     &      (pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),              &
     &       color_param%bg_rgba_real, pvr_rgb%rgba_real_gl)
      end if
!
      if(map_data%num_line .gt. 0) then
        call draw_xyz_plane_isolines                                    &
     &     (psf_nod, psf_ele, psf_phys%d_fld(1,2), map_data,            &
     &      color_param, pvr_rgb, map_e1)
        if(map_data%flag_zeroline                                       &
     &        .and. (map_data%fill_flag.eqv. .FALSE.)) then
          call sel_draw_isoline_on_xyz_plane                            &
     &       (psf_nod, psf_ele, psf_phys%d_fld(1,1), 3, 0,              &
     &        map_data, zero, white, pvr_rgb, map_e1)
        end if
      end if
!
      if(map_data%iflag_2d_projection_mode .eq. iflag_xz_plane          &
     &   .or. map_data%iflag_2d_projection_mode .eq. iflag_xz_plane     &
     &  ) then
        if(map_data%flag_tangent_cylinder) then
          call draw_med_tangent_cyl_grid(psf_nod, psf_ele, map_data,    &
     &        color_param%bg_rgba_real, map_data%fill_flag,             &
     &        map_data%tangent_cylinder_radius(2), pvr_rgb, map_e1)
        end if
      end if
!
      call draw_radius_grid(psf_nod, psf_ele, map_data,                 &
     &    color_param%bg_rgba_real, map_data%fill_flag,                 &
     &    map_data%tangent_cylinder_radius(2), pvr_rgb, map_e1)
      call draw_radius_grid(psf_nod, psf_ele, map_data,                 &
     &    color_param%bg_rgba_real, map_data%fill_flag,                 &
     &    map_data%tangent_cylinder_radius(1), pvr_rgb, map_e1)
      call dealloc_map_patch_from_1patch(map_e1)
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
      end subroutine s_xyz_plane_rendering
!
!  ---------------------------------------------------------------------
!
      end module xyz_plane_rendering
