!>@file  rendering_LIC_image.f90
!!       module rendering_LIC_image
!!
!!@author  Y. Liao and H. Matsui
!!@date Programmed in Feb., 2018
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine lic_rendering_with_fixed_view(istep_pvr, time, mesh, &
!!     &         (istep_pvr, time, node, ele, surf,                     &
!!     &          lic_p, field_lic, pvr_param, pvr_proj, pvr_rgb)
!!      subroutine rendering_lic_at_once                                &
!!     &         (istep_pvr, time, i_stereo, i_rot, mesh, group, lic_p, &
!!     &          field_lic, pvr_param, pvr_bound, pvr_proj, pvr_rgb)
!!        integer(kind = kint), intent(in) :: i_stereo, i_rot
!!        integer(kind = kint), intent(in) :: istep_pvr
!!        real(kind = kreal), intent(in) :: time
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(lic_parameters), intent(in) :: lic_p
!!        type(PVR_control_params), intent(in) :: pvr_param
!!        type(lic_field_data), intent(in) :: field_lic
!!        type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
!!        type(PVR_projection_data), intent(inout) :: pvr_proj
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!@endverbatim
!
      module rendering_LIC_image
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
      use t_control_param_LIC
      use t_lic_field_data
      use t_geometries_in_pvr_screen
      use t_surf_grp_4_pvr_domain
      use t_pvr_ray_startpoints
      use t_pvr_image_array
      use t_rendering_vr_image
      use generate_vr_image
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine lic_rendering_with_fixed_view(istep_pvr, time, mesh,   &
     &          lic_p, field_lic, pvr_param, pvr_proj, pvr_rgb)
!
      use write_LIC_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
      type(mesh_geometry), intent(in) :: mesh
      type(lic_parameters), intent(in) :: lic_p
      type(PVR_control_params), intent(in) :: pvr_param
      type(lic_field_data), intent(in) :: field_lic
!
      type(PVR_projection_data), intent(inout) :: pvr_proj
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
!
      call copy_item_pvr_ray_start                                      &
     &   (pvr_proj%start_save, pvr_proj%start_fix)
!
      if(iflag_debug .gt. 0) write(*,*) 'rendering_image_4_lic'
      call rendering_image_4_lic(istep_pvr, time, mesh, lic_p,          &
     &    pvr_param%color, pvr_param%colorbar, field_lic,               &
     &    pvr_param%draw_param, pvr_proj%screen,                        &
     &    pvr_param%view%viewpoint, pvr_proj%start_fix,                 &
     &    pvr_proj%stencil, pvr_rgb)
!
      end subroutine lic_rendering_with_fixed_view
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine rendering_lic_at_once                                  &
     &         (istep_pvr, time, i_stereo, i_rot, mesh, group, lic_p,   &
     &          field_lic, pvr_param, pvr_bound, pvr_proj, pvr_rgb)
!
      use cal_pvr_projection_mat
      use cal_pvr_modelview_mat
      use write_LIC_image
      use t_pvr_stencil_buffer
!
      integer(kind = kint), intent(in) :: i_stereo, i_rot
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(lic_parameters), intent(in) :: lic_p
      type(PVR_control_params), intent(in) :: pvr_param
      type(lic_field_data), intent(in) :: field_lic
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
     &      pvr_proj%projection_mat)
!      else if(num_stereo .gt. 1) then
!        call set_pvr_step_projection_mat(i_stereo, num_stereo,        &
!     &      pvr_param%view, pvr_param%stereo_def,                     &
!     &      pvr_proj%projection_mat)
      else
        call set_pvr_projection_matrix                                  &
     &     (pvr_param%view, pvr_proj%projection_mat)
      end if
!
      if(pvr_param%movie_def%iflag_movie_mode .eq. I_LOOKINGLASS) then
        call cal_pvr_modelview_matrix                                   &
     &     (i_rot, izero, pvr_param%outline,                            &
     &      pvr_param%movie_def, pvr_param%stereo_def, pvr_param%view,  &
     &      pvr_proj%viewpoint_vec, pvr_proj%modelview_mat)
      else
        call cal_pvr_modelview_matrix                                   &
     &     (i_stereo, i_rot, pvr_param%outline,                         &
     &      pvr_param%movie_def, pvr_param%stereo_def, pvr_param%view,  &
     &      pvr_proj%viewpoint_vec, pvr_proj%modelview_mat)
      end if
!
      call alloc_projected_position                                     &
     &   (mesh%node, mesh%surf, pvr_proj%screen)
!
      call transfer_to_screen(mesh%node, mesh%ele, mesh%surf,           &
     &    group%surf_grp, group%surf_grp_norm, pvr_param%draw_param,    &
     &    pvr_param%pixel, pvr_param%view%n_pvr_pixel,                  &
     &    pvr_proj%viewpoint_vec, pvr_proj%modelview_mat,               &
     &    pvr_proj%projection_mat, pvr_bound, pvr_proj%screen,          &
     &    pvr_proj%start_fix)
      call const_pvr_stencil_buffer                                     &
     &   (pvr_rgb, pvr_proj%start_fix, pvr_proj%stencil)
!
      if(iflag_debug .gt. 0) write(*,*) 'rendering_image_4_lic'
      call rendering_image_4_lic(istep_pvr, time, mesh, lic_p,          &
     &    pvr_param%color, pvr_param%colorbar, field_lic,               &
     &    pvr_param%draw_param, pvr_proj%screen,                        &
     &    pvr_proj%viewpoint_vec, pvr_proj%start_fix, pvr_proj%stencil, &
     &    pvr_rgb)
!
      call deallocate_pvr_ray_start(pvr_proj%start_fix)
      call dealloc_pvr_stencil_buffer(pvr_proj%stencil)
      call dealloc_projected_position(pvr_proj%screen)
!
      end subroutine rendering_lic_at_once
!
!  ---------------------------------------------------------------------
!
      end module rendering_LIC_image
