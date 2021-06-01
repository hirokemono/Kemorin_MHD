!>@file  t_rendering_vr_image.f90
!!       module t_rendering_vr_image
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine set_fixed_view_and_image(mesh, group,                &
!!     &          pvr_param, pvr_rgb, pvr_proj)
!!      subroutine rendering_with_fixed_view(istep_pvr, time, mesh,     &
!!     &          field_pvr, pvr_param, pvr_proj, pvr_rgb)
!!      subroutine flush_rendering_4_fixed_view(pvr_proj)
!!
!!      subroutine rendering_at_once(istep_pvr, time, mesh, group,      &
!!     &          field_pvr, pvr_param, pvr_proj, pvr_rgb)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(pvr_field_data), intent(in) :: field_pvr
!!        type(PVR_control_params), intent(in) :: pvr_param
!!        type(PVR_projection_data), intent(inout) :: pvr_proj
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!@endverbatim
!
      module t_rendering_vr_image
!
      use m_precision
      use m_machine_parameter
      use m_constants
      use m_work_time
!
      use calypso_mpi
!
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_control_params_4_pvr
      use t_geometries_in_pvr_screen
      use t_surf_grp_4_pvr_domain
      use t_pvr_ray_startpoints
      use t_pvr_image_array
      use t_pvr_stencil_buffer
      use t_pvr_field_data
      use generate_vr_image
!
      implicit  none
!
!>      Structure of PVR control parameters
      type PVR_control_params
!>        Structure for rendering area by element group
        type(viz_area_parameter) :: area_def
!>        Structure for field parameter for PVR
        type(pvr_field_parameter) :: field_def
!
!>        Parameters for image pixels
        type(pvr_pixel_position_type) :: pixel
!>        Structure for rough serch of subdomains
        type(pvr_domain_outline) :: outline
!>        Field data for volume rendering
        type(rendering_parameter) :: draw_param
!>        Structure for PVR colormap
        type(pvr_colorbar_parameter):: colorbar
!
!>        Viewer coordinate information
        type(pvr_view_parameter) :: view
!>        Color paramter for volume rendering
        type(pvr_colormap_parameter) :: color
!>        Movie parameters
        type(pvr_movie_parameter) :: movie_def
!>        Stereo view parameters
        type(pvr_stereo_parameter) :: stereo_def
      end type PVR_control_params
!
!
!>      Structure for projection data
      type PVR_projection_data
!>        viewpoint
        real(kind = kreal) :: viewpoint_vec(3)
!>        modelview matrix
        real(kind = kreal) :: modelview_mat(4,4)
!>        perspective projection matrix
        real(kind = kreal) :: projection_mat(4,4)
!
!>        Domain boundary information
        type(pvr_bounds_surf_ctl) :: bound
!>        Data on screen coordinate
        type(pvr_projected_position) :: screen
!>        Parallel stencil buffer
        type(pvr_stencil_buffer) :: stencil
!>        Start point structure for volume rendering with fixed view
        type(pvr_ray_start_type) :: start_fix
!
!>        Start point structure for volume rendering with fixed view
        type(pvr_ray_start_type) :: start_save
      end type PVR_projection_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_fixed_view_and_image(mesh, group,                  &
     &          pvr_param, pvr_rgb, pvr_proj)
!
      use cal_pvr_modelview_mat
      use t_pvr_stencil_buffer
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(PVR_control_params), intent(in) :: pvr_param
      type(pvr_image_type), intent(in) :: pvr_rgb
!
      type(PVR_projection_data), intent(inout) :: pvr_proj
!
!
      call transfer_to_screen(mesh%node, mesh%ele, mesh%surf,           &
     &    group%surf_grp, group%surf_grp_norm, pvr_param%draw_param,    &
     &    pvr_param%pixel, pvr_param%view%n_pvr_pixel,                  &
     &    pvr_proj%viewpoint_vec, pvr_proj%modelview_mat,               &
     &    pvr_proj%projection_mat, pvr_proj%bound, pvr_proj%screen,     &
     &    pvr_proj%start_fix)
      call const_pvr_stencil_buffer                                     &
     &   (pvr_rgb, pvr_proj%start_fix, pvr_proj%stencil)
!
      call allocate_item_pvr_ray_start                                  &
     &   (pvr_proj%start_fix%num_pvr_ray, pvr_proj%start_save)
      call copy_item_pvr_ray_start                                      &
     &   (pvr_proj%start_fix, pvr_proj%start_save)
!
      end subroutine set_fixed_view_and_image
!
!  ---------------------------------------------------------------------
!
      subroutine rendering_with_fixed_view(istep_pvr, time, mesh,       &
     &          field_pvr, pvr_param, pvr_proj, pvr_rgb)
!
      use write_PVR_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
      type(mesh_geometry), intent(in) :: mesh
      type(pvr_field_data), intent(in) :: field_pvr
      type(PVR_control_params), intent(in) :: pvr_param
!
      type(PVR_projection_data), intent(inout) :: pvr_proj
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
!
      call copy_item_pvr_ray_start                                      &
     &   (pvr_proj%start_save, pvr_proj%start_fix)
!
      if(iflag_debug .gt. 0) write(*,*) 'rendering_image'
      call rendering_image(istep_pvr, time, mesh,                       &
     &    pvr_param%color, pvr_param%colorbar, field_pvr,               &
     &    pvr_param%draw_param, pvr_proj%screen,                        &
     &    pvr_proj%viewpoint_vec, pvr_proj%start_fix,                   &
     &    pvr_proj%stencil, pvr_rgb)
!
      end subroutine rendering_with_fixed_view
!
!  ---------------------------------------------------------------------
!
      subroutine flush_rendering_4_fixed_view(pvr_proj)
!
      type(PVR_projection_data), intent(inout) :: pvr_proj
!
!
      call dealloc_pvr_surf_domain_item(pvr_proj%bound)
      call dealloc_projected_position(pvr_proj%screen)
      call dealloc_pvr_stencil_buffer(pvr_proj%stencil)
!
      end subroutine flush_rendering_4_fixed_view
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine rendering_at_once(istep_pvr, time, mesh, group,        &
     &          field_pvr, pvr_param, pvr_proj, pvr_rgb)
!
      use cal_pvr_modelview_mat
      use write_PVR_image
      use t_pvr_stencil_buffer
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(pvr_field_data), intent(in) :: field_pvr
      type(PVR_control_params), intent(in) :: pvr_param
!
      type(PVR_projection_data), intent(inout) :: pvr_proj
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
!>      Start point structure for volume rendering with rotation
      type(pvr_ray_start_type) :: start_rot
!>      Parallel stencil buffer
      type(pvr_stencil_buffer) :: stencil_rot
!
!
      call transfer_to_screen(mesh%node, mesh%ele, mesh%surf,           &
     &    group%surf_grp, group%surf_grp_norm, pvr_param%draw_param,    &
     &    pvr_param%pixel, pvr_param%view%n_pvr_pixel,                  &
     &    pvr_proj%viewpoint_vec, pvr_proj%modelview_mat,               &
     &    pvr_proj%projection_mat, pvr_proj%bound, pvr_proj%screen,     &
     &    start_rot)
      call const_pvr_stencil_buffer                                     &
     &   (pvr_rgb, start_rot, stencil_rot)
!
      if(iflag_debug .gt. 0) write(*,*) 'rendering_image'
      call rendering_image(istep_pvr, time, mesh,                       &
     &    pvr_param%color, pvr_param%colorbar, field_pvr,               &
     &    pvr_param%draw_param, pvr_proj%screen,                        &
     &    pvr_proj%viewpoint_vec, start_rot, stencil_rot, pvr_rgb)
      call deallocate_pvr_ray_start(start_rot)
      call dealloc_pvr_stencil_buffer(stencil_rot)
!
      end subroutine rendering_at_once
!
!  ---------------------------------------------------------------------
!
      end module t_rendering_vr_image
