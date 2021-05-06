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
!!     &         (istep_pvr, time, mesh, lic_p, color_param,            &
!!     &          cbar_param, field_lic, draw_param, view_param,        &
!!     &          pvr_screen, pvr_start, pvr_stencil, pvr_rgb)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(lic_field_data), intent(in) :: field_lic
!!        type(rendering_parameter), intent(in) :: draw_param
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        type(pvr_colorbar_parameter), intent(in) :: cbar_param
!!        type(pvr_view_parameter), intent(in) :: view_param
!!        type(pvr_projected_position), intent(in) :: pvr_screen
!!        type(pvr_ray_start_type), intent(inout) :: pvr_start
!!        type(pvr_stencil_buffer), intent(inout) :: pvr_stencil
!!        type(pvr_segmented_img), intent(inout) :: pvr_img
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
     &         (istep_pvr, time, mesh, lic_p, color_param,              &
     &          cbar_param, field_lic, draw_param, view_param,          &
     &          pvr_screen, pvr_start, pvr_stencil, pvr_rgb, i_lic)
!
      use m_geometry_constants
      use m_elapsed_labels_4_VIZ
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_control_params_4_pvr
      use t_control_param_LIC
      use t_lic_field_data
      use t_geometries_in_pvr_screen
      use t_pvr_image_array
      use t_pvr_ray_startpoints
      use t_pvr_stencil_buffer
      use ray_trace_LIC_image
      use draw_pvr_colorbar
      use pvr_axis_label
      use write_PVR_image
!      use composit_by_segmentad_image
!
      integer(kind = kint), intent(in) :: i_lic
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
!
      type(mesh_geometry), intent(in) :: mesh
      type(lic_parameters), intent(in) :: lic_p
      type(lic_field_data), intent(in) :: field_lic
      type(rendering_parameter), intent(in) :: draw_param
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
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+3)
      if(iflag_debug .gt. 0) write(*,*) 'ray_trace_each_lic_image'
      call ray_trace_each_lic_image(mesh%node, mesh%ele, mesh%surf,     &
     &    lic_p, pvr_screen, field_lic, draw_param,                     &
     &    color_param, view_param%viewpoint_vec, ray_vec4,              &
     &    pvr_start%num_pvr_ray, pvr_start%id_pixel_check,              &
     &    pvr_start%icount_pvr_trace, pvr_start%isf_pvr_ray_start,      &
     &    pvr_start%xi_pvr_start, pvr_start%xx4_pvr_start,              &
     &    pvr_start%xx4_pvr_ray_start, pvr_start%rgba_ray, i_lic)
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+3)
!
!
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+4)
      if(iflag_debug .gt. 0) write(*,*) 'collect_rendering_image'
      call collect_rendering_image(pvr_start,                           &
     &    pvr_rgb%num_pixel_actual, pvr_rgb%rgba_real_gl, pvr_stencil)
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+4)
!
!      call s_composit_by_segmentad_image                               &
!     &   (istep_pvr, iflag_LIC_time, ist_elapsed_LIC,                  &
!     &    pvr_start, pvr_stencil, pvr_img, pvr_rgb)
!
      if(my_rank .eq. pvr_rgb%irank_image_file) then
        if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+3)
        if(cbar_param%iflag_pvr_colorbar) then
          call set_pvr_colorbar                                         &
     &       (pvr_rgb%num_pixel_xy, pvr_rgb%num_pixels,                 &
     &        color_param, cbar_param, pvr_rgb%rgba_real_gl)
        end if
!
        if(cbar_param%iflag_draw_time) then
          call set_pvr_timelabel                                        &
     &       (time, pvr_rgb%num_pixel_xy, pvr_rgb%num_pixels,           &
     &        cbar_param, pvr_rgb%rgba_real_gl)
        end if
!
        if(cbar_param%iflag_pvr_axis) then
          call set_pvr_axislabel                                        &
     &       (pvr_rgb%num_pixel_xy, pvr_rgb%num_pixels,                 &
     &        pvr_screen, pvr_rgb%rgba_real_gl)
        end if
        if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+3)
      end if
!
      end subroutine rendering_image_4_lic
!
!  ---------------------------------------------------------------------
!
      end module write_LIC_image
