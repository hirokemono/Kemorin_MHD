!>@file  write_PVR_image.f90
!!       module write_PVR_image
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine rendering_image                                      &
!!     &         (istep_pvr, node, ele, surf, color_param,              &
!!     &          cbar_param, field_pvr, view_param, pvr_screen,        &
!!     &          pvr_start, rgba_real_gl, pvr_rgb)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(pvr_projected_field), intent(in) :: field_pvr
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        type(pvr_colorbar_parameter), intent(in) :: cbar_param
!!        type(pvr_view_parameter), intent(in) :: view_param
!!        type(pvr_projected_position), intent(in) :: pvr_screen
!!        type(pvr_ray_start_type), intent(inout) :: pvr_start
!!        type(pvr_stencil_buffer), intent(inout) :: pvr_stencil
!!        type(pvr_segmented_img), intent(inout) :: pvr_img
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!
!!      subroutine sel_write_pvr_image_file(i_rot, istep_pvr, pvr_rgb)
!!      subroutine sel_write_pvr_local_img(index, istep_pvr, pvr_rgb)
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!@endverbatim
!
      module write_PVR_image
!
      use m_precision
      use m_work_time
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine rendering_image                                        &
     &         (istep_pvr, node, ele, surf, color_param,                &
     &          cbar_param, field_pvr, view_param, pvr_screen,          &
     &          pvr_start, pvr_stencil, pvr_rgb)
!
      use m_geometry_constants
      use m_elapsed_labels_4_VIZ
      use t_geometry_data
      use t_surface_data
      use t_control_params_4_pvr
      use t_geometries_in_pvr_screen
      use t_pvr_image_array
      use t_pvr_ray_startpoints
      use t_pvr_stencil_buffer
      use ray_trace_4_each_image
      use draw_pvr_colorbar
      use pvr_axis_label
!      use composit_by_segmentad_image
!
      integer(kind = kint), intent(in) :: istep_pvr
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(pvr_projected_field), intent(in) :: field_pvr
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(pvr_colorbar_parameter), intent(in) :: cbar_param
      type(pvr_view_parameter), intent(in) :: view_param
      type(pvr_projected_position), intent(in) :: pvr_screen
!
      type(pvr_ray_start_type), intent(inout) :: pvr_start
      type(pvr_stencil_buffer), intent(inout) :: pvr_stencil
!      type(pvr_segmented_img), intent(inout) :: pvr_img
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
      integer(kind = kint) :: i, j, k, ipix
!
!
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+3)
      if(iflag_debug .gt. 0) write(*,*) 's_ray_trace_4_each_image'
      call s_ray_trace_4_each_image                                     &
     &   (node, ele, surf, pvr_screen, field_pvr,                       &
     &    color_param, view_param%viewpoint_vec, ray_vec,               &
     &    pvr_start%num_pvr_ray, pvr_start%id_pixel_check,              &
     &    pvr_start%icount_pvr_trace, pvr_start%isf_pvr_ray_start,      &
     &    pvr_start%xi_pvr_start, pvr_start%xx_pvr_start,               &
     &    pvr_start%xx_pvr_ray_start, pvr_start%rgba_ray)
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+3)
!
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+4)
      if(iflag_debug .gt. 0) write(*,*) 'collect_rendering_image'
      call collect_rendering_image(pvr_start,                           &
     &    pvr_rgb%num_pixel_actual, pvr_rgb%rgba_real_gl, pvr_stencil)
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+4)
!
!      call composit_by_segmentad_image                                 &
!     &   (istep_pvr, iflag_PVR_time, ist_elapsed_PVR,                  &
!     &    pvr_start, pvr_stencil, pvr_img, pvr_rgb)
!
      if(my_rank .eq. pvr_rgb%irank_image_file) then
        if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+3)
        call set_pvr_colorbar(pvr_rgb%num_pixel_xy, pvr_rgb%num_pixels, &
     &      color_param, cbar_param, pvr_rgb%rgba_real_gl)
        call set_pvr_axislabel(cbar_param%iflag_pvr_axis,               &
     &      pvr_rgb%num_pixel_xy, pvr_rgb%num_pixels,                   &
     &      pvr_screen, pvr_rgb%rgba_real_gl)
        if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+3)
      end if
!
      end subroutine rendering_image
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine sel_write_pvr_image_file(i_rot, istep_pvr, pvr_rgb)
!
      use t_pvr_image_array
      use t_control_params_4_pvr
      use output_image_sel_4_png
      use set_parallel_file_name
      use convert_real_rgb_2_bite
!
      integer(kind = kint), intent(in) :: i_rot, istep_pvr
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
      character(len=kchara) :: tmpchara, img_head
!
!
      if(my_rank .ne. pvr_rgb%irank_image_file) return
!      write(*,*) 'output file from ', my_rank
!
      if(istep_pvr .ge. 0) then
        tmpchara = add_int_suffix(istep_pvr, pvr_rgb%pvr_prefix)
      else
        tmpchara = pvr_rgb%pvr_prefix
      end if
!
      if(i_rot .gt. 0) then
        img_head = add_int_suffix(i_rot, tmpchara)
      else
        img_head = tmpchara
      end if
!
      if(pvr_rgb%id_pvr_transparent .eq. 1) then
          call cvt_double_rgba_to_char_rgba(pvr_rgb%num_pixel_xy,       &
     &        pvr_rgb%rgba_real_gl,  pvr_rgb%rgba_chara_gl)
          call sel_rgba_image_file(pvr_rgb%id_pvr_file_type,            &
     &        img_head, pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),   &
     &        pvr_rgb%rgba_chara_gl)
      else
          call cvt_double_rgba_to_char_rgb(pvr_rgb%num_pixel_xy,        &
     &        pvr_rgb%rgba_real_gl,  pvr_rgb%rgb_chara_gl)
          call sel_output_image_file(pvr_rgb%id_pvr_file_type,          &
     &        img_head, pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),   &
     &        pvr_rgb%rgb_chara_gl)
      end if
!
      end subroutine sel_write_pvr_image_file
!
!  ---------------------------------------------------------------------
!
      subroutine sel_write_pvr_local_img(index, istep_pvr, pvr_rgb)
!
      use t_pvr_image_array
      use t_control_params_4_pvr
      use output_image_sel_4_png
      use set_parallel_file_name
      use convert_real_rgb_2_bite
!
      integer(kind = kint), intent(in) :: index, istep_pvr
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
      character(len=kchara) :: tmpchara, img_head
!
!
      if(istep_pvr .ge. 0) then
        tmpchara = add_int_suffix(istep_pvr, pvr_rgb%pvr_prefix)
      else
        tmpchara = pvr_rgb%pvr_prefix
      end if
      img_head = add_int_suffix(index, tmpchara)
!
      call cvt_double_rgba_to_char_rgb(pvr_rgb%num_pixel_xy,            &
     &    pvr_rgb%rgba_real_lc, pvr_rgb%rgb_chara_lc)
!
      call sel_output_image_file(pvr_rgb%id_pvr_file_type,              &
     &    img_head, pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),       &
     &    pvr_rgb%rgb_chara_lc)
!
      end subroutine sel_write_pvr_local_img
!
!  ---------------------------------------------------------------------
!
      end module write_PVR_image
