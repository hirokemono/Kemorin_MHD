!>@file  t_rendering_vr_image.f90
!!       module t_rendering_vr_image
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine set_fixed_view_and_image(i_stereo, num_stereo,       &
!!     &          mesh, pvr_param, pvr_rgb, pvr_bound, pvr_proj)
!!      subroutine rendering_with_fixed_view                            &
!!     &         (istep_pvr, time, mesh, group, sf_grp_4_sf,            &
!!     &          field_pvr, pvr_param, pvr_proj, pvr_rgb)
!!      subroutine flush_rendering_4_fixed_view(pvr_proj)
!!
!!      subroutine rendering_at_once(istep_pvr, time, i_stereo, i_rot,  &
!!     &          mesh, group, sf_grp_4_sf, field_pvr, pvr_param,       &
!!     &          pvr_bound, pvr_proj, pvr_rgb)
!!        integer(kind = kint), intent(in) :: i_stereo, i_rot
!!        integer(kind = kint), intent(in) :: istep_pvr
!!        real(kind = kreal), intent(in) :: time
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
!!        type(pvr_field_data), intent(in) :: field_pvr
!!        type(PVR_control_params), intent(in) :: pvr_param
!!        type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
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
      use t_surf_grp_list_each_surf
      use t_control_params_4_pvr
      use t_geometries_in_pvr_screen
      use t_pvr_ray_startpoints
      use t_pvr_image_array
      use t_pvr_stencil_buffer
      use t_pvr_field_data
      use t_control_params_stereo_pvr
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
      subroutine set_fixed_view_and_image(i_stereo, num_stereo,         &
     &          mesh, pvr_param, pvr_rgb, pvr_bound, pvr_proj)
!
      use cal_pvr_projection_mat
      use cal_pvr_modelview_mat
      use t_pvr_stencil_buffer
!
      integer(kind = kint), intent(in) :: i_stereo, num_stereo
      type(mesh_geometry), intent(in) :: mesh
      type(PVR_control_params), intent(in) :: pvr_param
      type(pvr_image_type), intent(in) :: pvr_rgb
!
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
      type(PVR_projection_data), intent(inout) :: pvr_proj
!
!
      if(num_stereo .gt. 1) then
        call set_pvr_step_projection_mat(i_stereo, num_stereo,          &
     &      pvr_param%view, pvr_param%stereo_def,                       &
     &      pvr_proj%screen%projection_mat)
      else
        if(iflag_debug .gt. 0) write(*,*) 'set_pvr_projection_matrix'
        call set_pvr_projection_matrix                                  &
     &     (pvr_param%view, pvr_proj%screen%projection_mat)
!        call set_pvr_orthogonal_params(pvr_param%view)
      end if
!
      call cal_pvr_modelview_matrix(i_stereo, izero, pvr_param%outline, &
     &    pvr_param%movie_def, pvr_param%stereo_def, pvr_param%view,    &
     &    pvr_proj%screen%viewpoint_vec, pvr_proj%screen%modelview_mat)
!
      call transfer_to_screen(mesh%node, mesh%surf,                     &
     &    pvr_param%pixel, pvr_param%view%n_pvr_pixel,                  &
     &    pvr_bound, pvr_proj%screen, pvr_proj%start_fix)
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
      subroutine rendering_with_fixed_view                              &
     &         (istep_pvr, time, mesh, group, sf_grp_4_sf,              &
     &          field_pvr, pvr_param, pvr_proj, pvr_rgb)
!
      use write_PVR_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
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
      call rendering_image(istep_pvr, time, mesh, group, sf_grp_4_sf,   &
     &    pvr_param%color, pvr_param%colorbar, field_pvr,               &
     &    pvr_param%draw_param, pvr_proj%screen, pvr_proj%start_fix,    &
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
      call dealloc_pvr_stencil_buffer(pvr_proj%stencil)
!
      end subroutine flush_rendering_4_fixed_view
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine rendering_at_once(istep_pvr, time, i_stereo, i_rot,    &
     &          mesh, group, sf_grp_4_sf, field_pvr, pvr_param,         &
     &          pvr_bound, pvr_proj, pvr_rgb)
!
      use cal_pvr_projection_mat
      use cal_pvr_modelview_mat
      use write_PVR_image
      use t_pvr_stencil_buffer
!
      integer(kind = kint), intent(in) :: i_stereo, i_rot
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
      type(pvr_field_data), intent(in) :: field_pvr
      type(PVR_control_params), intent(in) :: pvr_param
!
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
      type(PVR_projection_data), intent(inout) :: pvr_proj
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
!
      if(pvr_param%movie_def%iflag_movie_mode .eq. I_LOOKINGLASS) then
        call set_pvr_step_projection_mat                                &
     &     (i_rot, pvr_param%movie_def%num_frame,                       &
     &      pvr_param%view, pvr_param%stereo_def,                       &
     &      pvr_proj%screen%projection_mat)
!      else if(num_stereo .gt. 1) then
!        call set_pvr_step_projection_mat(i_stereo, num_stereo,         &
!     &      pvr_param%view, pvr_param%stereo_def,                      &
!     &      pvr_proj%screen%projection_mat)
      else
        call set_pvr_projection_matrix                                  &
     &     (pvr_param%view, pvr_proj%screen%projection_mat)
      end if
!
      if(pvr_param%movie_def%iflag_movie_mode .eq. I_LOOKINGLASS) then
        call cal_pvr_modelview_matrix(i_rot, izero,                     &
     &      pvr_param%outline, pvr_param%movie_def,                     &
     &      pvr_param%stereo_def, pvr_param%view,                       &
     &      pvr_proj%screen%viewpoint_vec,                              &
     &      pvr_proj%screen%modelview_mat)
      else
        call cal_pvr_modelview_matrix(i_stereo, i_rot,                  &
     &      pvr_param%outline, pvr_param%movie_def,                     &
     &      pvr_param%stereo_def, pvr_param%view,                       &
     &      pvr_proj%screen%viewpoint_vec,                              &
     &      pvr_proj%screen%modelview_mat)
      end if
!
      call transfer_to_screen(mesh%node, mesh%surf,                     &
     &    pvr_param%pixel, pvr_param%view%n_pvr_pixel,                  &
     &    pvr_bound, pvr_proj%screen, pvr_proj%start_fix)
      call const_pvr_stencil_buffer                                     &
     &   (pvr_rgb, pvr_proj%start_fix, pvr_proj%stencil)
!
      if(iflag_debug .gt. 0) write(*,*) 'rendering_image'
      call rendering_image(istep_pvr, time, mesh, group, sf_grp_4_sf,   &
     &    pvr_param%color, pvr_param%colorbar, field_pvr,               &
     &    pvr_param%draw_param, pvr_proj%screen, pvr_proj%start_fix,    &
     &    pvr_proj%stencil, pvr_rgb)
      call deallocate_pvr_ray_start(pvr_proj%start_fix)
      call dealloc_pvr_stencil_buffer(pvr_proj%stencil)
!
      end subroutine rendering_at_once
!
!  ---------------------------------------------------------------------
!
      end module t_rendering_vr_image
