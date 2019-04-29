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
!!     &          pvr_start, pvr_stencil, pvr_img, pvr_rgb)
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
     &          pvr_start, pvr_stencil, pvr_img, pvr_rgb)
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
      use composite_pvr_images
      use draw_pvr_colorbar
      use composite_pvr_images
      use PVR_image_transfer
      use pvr_axis_label
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
      type(pvr_segmented_img), intent(inout) :: pvr_img
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
!
      if(iflag_debug .gt. 0) write(*,*) 'copy_segmented_image'
      call copy_segmented_image(pvr_start%num_pvr_ray,                  &
     &    pvr_start%id_pixel_start, pvr_start%rgba_ray,                 &
     &    pvr_img%num_overlap, pvr_rgb%num_pixel_xy,                    &
     &    pvr_img%npixel_img, pvr_img%iflag_img_pe,                     &
     &    pvr_img%iflag_mapped, pvr_img%rgba_lc)
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+3)
!
!       Outut semented image
!      if(i_debug .gt. 0) then
!        do i = 1, pvr_img%num_overlap
!          j = pvr_img%istack_overlap(my_rank) + i
!          do k = 1, pvr_img%npixel_img
!            ipix = pvr_img%ipixel_small(k)
!            pvr_rgb%rgba_real_lc(1:4,ipix) = pvr_img%rgba_lc(1:4,i,k)
!          end do
!          call sel_write_pvr_local_img(j, istep_pvr, pvr_rgb)
!        end do
!      end if
!
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+4)
      if(iflag_debug .gt. 0) write(*,*) 'distribute_segmented_images'
      call distribute_segmented_images                                  &
     &   (pvr_img%num_overlap, pvr_img%istack_overlap,                  &
     &    pvr_img%ntot_overlap, pvr_img%npixel_img,                     &
     &    pvr_img%istack_pixel, pvr_img%npixel_img_local,               &
     &    pvr_img%rgba_lc, pvr_img%rgba_recv, pvr_img%rgba_part,        &
     &    pvr_img%COMM)
!
!
      if(iflag_debug .gt. 0) write(*,*) 'blend_image_over_segments'
      call blend_image_over_segments                                    &
     &   (pvr_img%ntot_overlap, pvr_img%npixel_img_local,               &
     &    pvr_img%ip_closer, pvr_img%rgba_part, pvr_img%rgba_whole)
!
      if(iflag_debug .gt. 0) write(*,*) 'collect_segmented_images'
      call collect_segmented_images(pvr_rgb%irank_image_file,           &
     &    pvr_img%npixel_img_local, pvr_img%istack_pixel,               &
     &    pvr_img%npixel_img, pvr_rgb%num_pixel_xy,                     &
     &    pvr_img%ipixel_small, pvr_img%rgba_whole,                     &
     &    pvr_img%rgba_rank0, pvr_rgb%rgba_real_gl, pvr_img%COMM)
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+4)
!
!
      if(iflag_debug .gt. 0) write(*,*) 'collect_rendering_image'
      call collect_rendering_image(pvr_start, pvr_stencil)
!
!
      call compare_image_composition(pvr_stencil, pvr_img, pvr_rgb)
!
!       Output semented image
      if(size(pvr_stencil%rgba_gl,2) .gt. 0) then
        do ipix = 1, size(pvr_stencil%rgba_gl,2)
          pvr_rgb%rgba_real_lc(1:4,ipix)                                &
     &              = pvr_stencil%rgba_gl(1:4,ipix)
        end do
        call sel_write_pvr_local_img(my_rank, istep_pvr, pvr_rgb)
      end if
!
      if(my_rank .eq. pvr_rgb%irank_image_file) then
        call set_pvr_colorbar(pvr_rgb%num_pixel_xy, pvr_rgb%num_pixels, &
     &      color_param, cbar_param, pvr_rgb%rgba_real_gl)
        call set_pvr_axislabel(cbar_param%iflag_pvr_axis,               &
     &      pvr_rgb%num_pixel_xy, pvr_rgb%num_pixels,                   &
     &      pvr_screen, pvr_rgb%rgba_real_gl)
      end if
      call calypso_mpi_barrier
      if(iflag_debug .gt. 0) write(*,*) 'rendering_image end'
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
!  ---------------------------------------------------------------------
!
      subroutine compare_image_composition                              &
     &         (pvr_stencil, pvr_img, pvr_rgb)
!
      use t_pvr_image_array
      use t_pvr_stencil_buffer
      use composite_pvr_images
      use composite_pvr_images
!
      type(pvr_stencil_buffer), intent(in) :: pvr_stencil
      type(pvr_segmented_img), intent(in) :: pvr_img
      type(pvr_image_type), intent(in) :: pvr_rgb
!
      integer(kind = kint) :: num_fail
      integer(kind = kint), allocatable :: ilist_fail(:), ilist_tmp(:)
      integer(kind = kint) :: i, iflag
!
!
      write(my_rank+50,*) 'pixel check', my_rank,                       &
     &          size(pvr_stencil%rgba_gl,2),                            &
     &          size(pvr_rgb%rgba_real_gl,2), pvr_rgb%num_pixel_xy
      if(my_rank .eq. pvr_rgb%irank_image_file) then
        num_fail = 0
        allocate(ilist_fail(num_fail))
        do i = 1, pvr_rgb%num_pixel_xy
          iflag = 0
          if(pvr_stencil%rgba_gl(1,i) .ne. pvr_rgb%rgba_real_gl(1,i))   &
     &      iflag = 1
          if(pvr_stencil%rgba_gl(2,i) .ne. pvr_rgb%rgba_real_gl(2,i))   &
     &      iflag = 1
          if(pvr_stencil%rgba_gl(3,i) .ne. pvr_rgb%rgba_real_gl(3,i))   &
     &      iflag = 1
          if(pvr_stencil%rgba_gl(4,i) .ne. pvr_rgb%rgba_real_gl(4,i))   &
     &      iflag = 1
          if(iflag .gt. 0) then
            write(my_rank+50,*) 'rgba_gl', i,                           &
     &                         pvr_stencil%rgba_gl(1:4,i)
            write(my_rank+50,*) 'rgba_real_gl', i,                      &
     &                         pvr_rgb%rgba_real_gl(1:4,i)
!
            allocate(ilist_tmp(num_fail))
            ilist_tmp(1:num_fail) = ilist_fail(1:num_fail)
            deallocate(ilist_fail)
            num_fail = num_fail + 1
            allocate(ilist_fail(num_fail))
            ilist_fail(1:num_fail-1) = ilist_tmp(1:num_fail-1)
            ilist_fail(num_fail) = i
            deallocate(ilist_tmp)
          end if
        end do
      end if
!
      call mpi_bcast(num_fail, 1, CALYPSO_INTEGER,                      &
     &        int(pvr_rgb%irank_image_file), CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .ne. pvr_rgb%irank_image_file)                         &
     &      allocate(ilist_fail(num_fail))
      call mpi_bcast(ilist_fail, num_fail, CALYPSO_INTEGER,             &
     &    int(pvr_rgb%irank_image_file), CALYPSO_COMM, ierr_MPI)
!
      do i = 1, num_fail
        call check_rendering_image                                      &
     &     (50+my_rank, ilist_fail(i), pvr_stencil%img_stack,           &
     &      pvr_stencil%npixel_recved, pvr_stencil%rgba_subdomain,      &
     &      pvr_stencil%npixel_stacked, pvr_stencil%rgba_composit)

        call check_image_over_segments(my_rank+50, ilist_fail(i),       &
     &      pvr_img%ntot_overlap, pvr_img%npixel_img_local,             &
     &      pvr_img%istack_pixel, pvr_rgb%num_pixel_xy,                 &
     &      pvr_img%iflag_img_pe, pvr_img%ip_closer, pvr_img%rgba_part)
      end do
      deallocate(ilist_fail)
!
      end subroutine compare_image_composition
!
!  ---------------------------------------------------------------------
!
      end module write_PVR_image
