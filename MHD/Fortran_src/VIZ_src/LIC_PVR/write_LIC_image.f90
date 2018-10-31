!>@file  write_LIC_image.f90
!!       module write_LIC_image
!!
!!@author  Y. Liao and H. Matsui
!!@date Programmed in Feb., 2018
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine rendering_image_4_lic                                &
!!     &         (istep_pvr,  node, ele, surf, lic_p, color_param,      &
!!     &          cbar_param, field_pvr, view_param, pvr_screen,        &
!!     &          pvr_start, pvr_img, pvr_rgb)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(pvr_projected_field), intent(in) :: field_pvr
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        type(pvr_colorbar_parameter), intent(in) :: cbar_param
!!        type(pvr_view_parameter), intent(in) :: view_param
!!        type(pvr_projected_position), intent(in) :: pvr_screen
!!        type(pvr_ray_start_type), intent(inout) :: pvr_start
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!@endverbatim
!
      module write_LIC_image
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
      subroutine rendering_image_4_lic                                  &
     &         (istep_pvr,  node, ele, surf, lic_p, color_param,        &
     &          cbar_param, field_pvr, view_param, pvr_screen,          &
     &          pvr_start, pvr_img, pvr_rgb)
!
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
      use t_control_params_4_pvr
      use t_control_param_LIC
      use t_geometries_in_pvr_screen
      use t_pvr_image_array
      use t_pvr_ray_startpoints
      use ray_trace_LIC_image
      use composite_pvr_images
      use draw_pvr_colorbar
      use composite_pvr_images
      use PVR_image_transfer
      use pvr_axis_label
      use write_PVR_image
!
      integer(kind = kint), intent(in) :: istep_pvr
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(lic_parameters), intent(in) :: lic_p
      type(pvr_projected_field), intent(in) :: field_pvr
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(pvr_colorbar_parameter), intent(in) :: cbar_param
      type(pvr_view_parameter), intent(in) :: view_param
      type(pvr_projected_position), intent(in) :: pvr_screen
!
      type(pvr_ray_start_type), intent(inout) :: pvr_start
      type(pvr_segmented_img), intent(inout) :: pvr_img
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
      integer(kind = kint) :: i, j, k, ipix
!
!
      call start_elapsed_time(78)
      if(iflag_debug .gt. 0) write(*,*) 'ray_trace_each_lic_image'
      call ray_trace_each_lic_image                                     &
     &   (node, ele, surf, lic_p, pvr_screen, field_pvr,                &
     &    color_param, view_param%viewpoint_vec, ray_vec,               &
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
      call end_elapsed_time(78)
!
!       Outut semented image
      if(i_debug .gt. 0) then
        do i = 1, pvr_img%num_overlap
          j = pvr_img%istack_overlap(my_rank) + i
          do k = 1, pvr_img%npixel_img
            ipix = pvr_img%ipixel_small(k)
            pvr_rgb%rgba_real_lc(1:4,ipix) = pvr_img%rgba_lc(1:4,j,k)
          end do
          call sel_write_pvr_local_img(j, istep_pvr,  pvr_rgb)
        end do
      end if
!
      call start_elapsed_time(79)
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
      call collect_segmented_images(pvr_rgb%irank_image_file,           &
     &    pvr_img%npixel_img_local, pvr_img%istack_pixel,               &
     &    pvr_img%npixel_img, pvr_rgb%num_pixel_xy,                     &
     &    pvr_img%ipixel_small, pvr_img%rgba_whole,                     &
     &    pvr_img%rgba_rank0, pvr_rgb%rgba_real_gl, pvr_img%COMM)
      call end_elapsed_time(79)
!
!      if(my_rank .eq. 0) then
!        write(*,*) 'picked points'
!        do j = 244, 246
!          do i = 636, 638
!            ipix = i + (j-1)* pvr_rgb%num_pixels(1)
!            write(*,*) i, j, ipix, pvr_rgb%rgba_real_gl(1:4,ipix)
!          end do
!        end do
!      end if
!
      if(my_rank .eq. pvr_rgb%irank_image_file) then
        call set_pvr_colorbar(pvr_rgb%num_pixel_xy, pvr_rgb%num_pixels, &
     &      color_param, cbar_param, pvr_rgb%rgba_real_gl)
!
        call set_pvr_axislabel(cbar_param%iflag_pvr_axis,               &
     &      pvr_rgb%num_pixel_xy, pvr_rgb%num_pixels,                   &
     &      pvr_screen, pvr_rgb%rgba_real_gl)
      end if
!
      end subroutine rendering_image_4_lic
!
!  ---------------------------------------------------------------------
!
      end module write_LIC_image
