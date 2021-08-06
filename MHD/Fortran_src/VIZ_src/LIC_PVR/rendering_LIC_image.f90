!>@file  rendering_LIC_image.f90
!!       module rendering_LIC_image
!!
!!@author  Y. Liao and H. Matsui
!!@date Programmed in Feb., 2018
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine lic_rendering_with_fixed_view                        &
!!     &         (istep_pvr, time, mesh, group, sf_grp_4_sf, lic_p,     &
!!     &          field_lic, pvr_param, pvr_proj, pvr_rgb,              &
!!     &          SR_sig, SR_r, elapse_ray_trace_out, count_int_nod)
!!      subroutine rendering_lic_at_once(istep_pvr, time,               &
!!     &          i_stereo, i_rot, mesh, group, sf_grp_4_sf, lic_p,     &
!!     &          field_lic, pvr_param, pvr_bound, pvr_proj, pvr_rgb,   &
!!     &          SR_sig, SR_r, SR_i,                                   &
!!     &          elapse_ray_trace_out, count_int_nod)
!!        integer(kind = kint), intent(in) :: i_stereo, i_rot
!!        integer(kind = kint), intent(in) :: istep_pvr
!!        real(kind = kreal), intent(in) :: time
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
!!        type(lic_parameters), intent(in) :: lic_p
!!        type(PVR_control_params), intent(in) :: pvr_param
!!        type(lic_field_data), intent(in) :: field_lic
!!        type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
!!        type(PVR_projection_data), intent(inout) :: pvr_proj
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!        type(send_recv_int_buffer), intent(inout) :: SR_i
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
      use t_surf_grp_list_each_surf
      use t_control_params_4_pvr
      use t_control_param_LIC
      use t_lic_field_data
      use t_geometries_in_pvr_screen
      use t_surf_grp_4_pvr_domain
      use t_pvr_ray_startpoints
      use t_pvr_image_array
      use t_rendering_vr_image
      use t_solver_SR
      use t_solver_SR_int
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
      subroutine lic_rendering_with_fixed_view                          &
     &         (istep_pvr, time, mesh, group, sf_grp_4_sf, lic_p,       &
     &          field_lic, pvr_param, pvr_proj, pvr_rgb,                &
     &          SR_sig, SR_r, elapse_ray_trace_out, count_int_nod)
!
      use write_LIC_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
      type(lic_parameters), intent(in) :: lic_p
      type(PVR_control_params), intent(in) :: pvr_param
      type(lic_field_data), intent(in) :: field_lic
!
      type(PVR_projection_data), intent(inout) :: pvr_proj
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      real(kind = kreal), intent(inout) :: elapse_ray_trace_out(2)
      real(kind = kreal), intent(inout)                                 &
     &                    :: count_int_nod(mesh%node%numnod)
!
!
      call copy_item_pvr_ray_start                                      &
     &   (pvr_proj%start_save, pvr_proj%start_fix)
!
      if(iflag_debug .gt. 0) write(*,*) 'rendering_image_4_lic'
      call rendering_image_4_lic                                        &
     &   (istep_pvr, time, mesh, group, sf_grp_4_sf, lic_p,             &
     &    pvr_param%color, pvr_param%colorbar, field_lic,               &
     &    pvr_param%draw_param, pvr_proj%screen, pvr_proj%start_fix,    &
     &    pvr_proj%stencil, pvr_rgb, SR_sig, SR_r,                      &
     &    elapse_ray_trace_out, count_int_nod)
!
      end subroutine lic_rendering_with_fixed_view
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine rendering_lic_at_once(istep_pvr, time,                 &
     &          i_stereo, i_rot, mesh, group, sf_grp_4_sf, lic_p,       &
     &          field_lic, pvr_param, pvr_bound, pvr_proj, pvr_rgb,     &
     &          SR_sig, SR_r, SR_i,                                     &
     &          elapse_ray_trace_out, count_int_nod)
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
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
      type(lic_parameters), intent(in) :: lic_p
      type(PVR_control_params), intent(in) :: pvr_param
      type(lic_field_data), intent(in) :: field_lic
!
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
      type(PVR_projection_data), intent(inout) :: pvr_proj
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
      type(send_recv_int_buffer), intent(inout) :: SR_i
      real(kind = kreal), intent(inout) :: elapse_ray_trace_out(2)
      real(kind = kreal), intent(inout)                                 &
     &                    :: count_int_nod(mesh%node%numnod)
!
!
      if(pvr_param%movie_def%iflag_movie_mode .eq. I_LOOKINGLASS) then
        call set_pvr_step_projection_mat                                &
     &     (i_rot, pvr_param%movie_def%num_frame,                       &
     &      pvr_param%view, pvr_param%stereo_def,                       &
     &      pvr_proj%screen%projection_mat)
!      else if(num_stereo .gt. 1) then
!        call set_pvr_step_projection_mat(i_stereo, num_stereo,        &
!     &      pvr_param%view, pvr_param%stereo_def,                     &
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
     &   (pvr_rgb, pvr_proj%start_fix, pvr_proj%stencil,                &
     &    SR_sig, SR_r, SR_i)
!
      if(iflag_debug .gt. 0) write(*,*) 'rendering_image_4_lic'
      call rendering_image_4_lic                                        &
     &   (istep_pvr, time, mesh, group, sf_grp_4_sf, lic_p,             &
     &    pvr_param%color, pvr_param%colorbar, field_lic,               &
     &    pvr_param%draw_param, pvr_proj%screen, pvr_proj%start_fix,    &
     &    pvr_proj%stencil, pvr_rgb, SR_sig, SR_r,                      &
     &    elapse_ray_trace_out, count_int_nod)
!
      call deallocate_pvr_ray_start(pvr_proj%start_fix)
      call dealloc_pvr_stencil_buffer(pvr_proj%stencil)
!
      end subroutine rendering_lic_at_once
!
!  ---------------------------------------------------------------------
!
      end module rendering_LIC_image
