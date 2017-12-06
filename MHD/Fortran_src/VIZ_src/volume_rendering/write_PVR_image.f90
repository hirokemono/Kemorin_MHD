!>@file  write_PVR_image.f90
!!       module write_PVR_image
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine rendering_image(node, ele, surf, color_param,        &
!!     &          cbar_param, field_pvr, pvr_screen, pvr_start, pvr_img)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(pvr_projected_field), intent(in) :: field_pvr
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        type(pvr_colorbar_parameter), intent(in) :: cbar_param
!!        type(pvr_projected_data), intent(in) :: pvr_screen
!!        type(pvr_ray_start_type), intent(inout) :: pvr_start
!!      type(pvr_image_type), intent(inout) :: pvr_rgb
!!
!!      subroutine sel_write_pvr_image_file                             &
!!     &       (file_param, i_rot, istep_pvr, isel_projection, pvr_rgb)
!!      subroutine sel_write_pvr_local_img                              &
!!     &        (file_param, index, istep_pvr, pvr_rgb)
!!      type(pvr_image_type), intent(inout) :: pvr_rgb
!!@endverbatim
!
      module write_PVR_image
!
      use m_precision
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
      subroutine rendering_image(node, ele, surf, color_param,          &
     &          cbar_param, field_pvr, pvr_screen, pvr_start,           &
     &          pvr_img, pvr_rgb)
!
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
      use t_control_params_4_pvr
      use t_geometries_in_pvr_screen
      use t_pvr_image_array
      use t_pvr_ray_startpoints
      use ray_trace_4_each_image
      use composite_pvr_images
      use draw_pvr_colorbar
      use composite_pvr_images
      use PVR_image_transfer
      use pvr_axis_label
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(pvr_projected_field), intent(in) :: field_pvr
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(pvr_colorbar_parameter), intent(in) :: cbar_param
      type(pvr_projected_data), intent(in) :: pvr_screen
!
      type(pvr_ray_start_type), intent(inout) :: pvr_start
      type(pvr_segmented_img), intent(inout) :: pvr_img
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
!>       MPI rank for image output
      integer(kind = kint), parameter :: irank_tgt = 0
!
!      integer(kind = kint) :: i, j, k, ipix
!
!
      if(iflag_debug .gt. 0) write(*,*) 's_ray_trace_4_each_image'
      call s_ray_trace_4_each_image(node, ele, surf,                    &
     &    pvr_screen, field_pvr, color_param, ray_vec,                  &
     &    pvr_start%num_pvr_ray, pvr_start%id_pixel_check,              &
     &    pvr_start%icount_pvr_trace, pvr_start%isf_pvr_ray_start,      &
     &    pvr_start%xi_pvr_start, pvr_start%xx_pvr_start,               &
     &    pvr_start%xx_pvr_ray_start, pvr_start%rgba_ray)
!
      if(iflag_debug .gt. 0) write(*,*) 'copy_segmented_image'
      call copy_segmented_image(pvr_start%num_pvr_ray,                  &
     &    pvr_start%id_pixel_start, pvr_start%rgba_ray,                 &
     &    pvr_img%num_overlap, pvr_rgb%num_pixel_xy,                    &
     &    pvr_img%npixel_img, pvr_img%iflag_img_pe,                     &
     &    pvr_img%iflag_mapped, pvr_img%rgba_lc)
!
!      do i = 1, pvr_img%num_overlap
!        j = pvr_img%istack_overlap(my_rank) + i
!        do k = 1, pvr_img%npixel_img
!          ipix = pvr_img%ipixel_small(k)
!          pvr_rgb%rgba_real_lc(1:4,ipix) = pvr_img%rgba_lc(1:4,j,k)
!        end do
!        call sel_write_pvr_local_img(file_param, j, istep_pvr,         &
!     &      pvr_rgb)
!      end do
!
      call distribute_segmented_images                                  &
     &   (pvr_img%num_overlap, pvr_img%istack_overlap,                  &
     &    pvr_img%ntot_overlap, pvr_img%npixel_img,                     &
     &    pvr_img%istack_pixel, pvr_img%npixel_img_local,               &
     &    pvr_img%rgba_lc, pvr_img%rgba_recv, pvr_img%rgba_part,        &
     &    pvr_img%COMM)
!
      call blend_image_over_segments                                    &
     &   (pvr_img%ntot_overlap, pvr_img%npixel_img_local,               &
     &    pvr_img%ip_closer, pvr_img%rgba_part, pvr_img%rgba_whole)
!
      call collect_segmented_images                                     &
     &   (irank_tgt, pvr_img%npixel_img_local, pvr_img%istack_pixel,    &
     &    pvr_img%npixel_img, pvr_rgb%num_pixel_xy,                     &
     &    pvr_img%ipixel_small, pvr_img%rgba_whole,                     &
     &    pvr_img%rgba_rank0, pvr_rgb%rgba_real_gl, pvr_img%COMM)
!
      if(my_rank .eq. irank_tgt) then
        call set_pvr_colorbar(pvr_rgb%num_pixel_xy, pvr_rgb%num_pixels, &
     &      color_param, cbar_param, pvr_rgb%rgba_real_gl)
        call set_pvr_axislabel(cbar_param%iflag_pvr_axis,               &
     &      pvr_rgb%num_pixel_xy, pvr_rgb%num_pixels,                   &
     &      pvr_screen, cbar_param, pvr_rgb%rgba_real_gl)
      end if
!
      end subroutine rendering_image
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine sel_write_pvr_image_file                               &
     &        (file_param, i_rot, istep_pvr, isel_projection, pvr_rgb)
!
      use t_pvr_image_array
      use t_control_params_4_pvr
      use output_image_sel_4_png
      use set_parallel_file_name
      use convert_real_rgb_2_bite
!
      type(pvr_output_parameter), intent(in) :: file_param
      integer(kind = kint), intent(in) :: i_rot, istep_pvr
      integer(kind = kint), intent(in) :: isel_projection
!
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
      character(len=kchara) :: tmpchara, img_head
!
!
      if(my_rank .ne. 0) return
!
      if(isel_projection .eq. IFLAG_LEFT) then
        call add_left_label(file_param%pvr_prefix, img_head)
      else if(isel_projection .eq. IFLAG_RIGHT) then
        call add_right_label(file_param%pvr_prefix, img_head)
      else
        img_head = file_param%pvr_prefix
      end if
!
      if(istep_pvr .ge. 0) then
        call add_int_suffix(istep_pvr, img_head, tmpchara)
      else
        tmpchara = img_head
      end if
!
      if(i_rot .gt. 0) then
        call add_int_suffix(i_rot, tmpchara, img_head)
      else
        img_head = tmpchara
      end if
!
      if(file_param%id_pvr_transparent .eq. 1) then
          call cvt_double_rgba_to_char_rgba(pvr_rgb%num_pixel_xy,       &
     &        pvr_rgb%rgba_real_gl,  pvr_rgb%rgba_chara_gl)
          call sel_rgba_image_file(file_param%id_pvr_file_type,         &
     &        img_head, pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),   &
     &        pvr_rgb%rgba_chara_gl)
      else
          call cvt_double_rgba_to_char_rgb(pvr_rgb%num_pixel_xy,        &
     &        pvr_rgb%rgba_real_gl,  pvr_rgb%rgb_chara_gl)
          call sel_output_image_file(file_param%id_pvr_file_type,       &
     &        img_head, pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),   &
     &        pvr_rgb%rgb_chara_gl)
      end if
!
      end subroutine sel_write_pvr_image_file
!
!  ---------------------------------------------------------------------
!
      subroutine sel_write_pvr_local_img                                &
     &         (file_param, index, istep_pvr, pvr_rgb)
!
      use t_pvr_image_array
      use t_control_params_4_pvr
      use output_image_sel_4_png
      use set_parallel_file_name
      use convert_real_rgb_2_bite
!
      type(pvr_output_parameter), intent(in) :: file_param
      integer(kind = kint), intent(in) :: index, istep_pvr
!
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
      character(len=kchara) :: tmpchara, img_head
!
!
      if(istep_pvr .ge. 0) then
        call add_int_suffix(istep_pvr, file_param%pvr_prefix, tmpchara)
      else
        tmpchara = file_param%pvr_prefix
      end if
      call add_int_suffix(index, tmpchara, img_head)
!
      call cvt_double_rgba_to_char_rgb(pvr_rgb%num_pixel_xy,            &
     &    pvr_rgb%rgba_real_lc, pvr_rgb%rgb_chara_lc)
!
      call sel_output_image_file(file_param%id_pvr_file_type,           &
     &    img_head, pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),       &
     &    pvr_rgb%rgb_chara_lc)
!
      end subroutine sel_write_pvr_local_img
!
!  ---------------------------------------------------------------------
!
      end module write_PVR_image
