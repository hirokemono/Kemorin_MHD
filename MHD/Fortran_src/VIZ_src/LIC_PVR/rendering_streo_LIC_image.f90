!>@file  rendering_streo_LIC_image.f90
!!       module rendering_streo_LIC_image
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine s_each_LIC_anaglyph(istep_pvr, time, elps_LIC,       &
!!     &          viz_fem, field_lic, sf_grp_4_sf, lic_param,           &
!!     &          pvr_param, pvr_proj, pvr_rgb, rep_ref_viz, m_SR)
!!      subroutine anaglyph_lic_rendering_w_rot                         &
!!     &         (istep_pvr, time, elps_PVR, elps_LIC,                  &
!!     &          viz_fem, sf_grp_4_sf, field_lic, lic_p, pvr_param,    &
!!     &          pvr_bound, pvr_proj, pvr_rgb, rep_ref_viz, m_SR)
!!        integer(kind = kint), intent(in) :: istep_pvr
!!        type(elapsed_lables), intent(in) :: elps_PVR
!!        type(elapsed_lables), intent(in) :: elps_LIC
!!        type(mesh_data), intent(in) :: viz_fem
!!        type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
!!        type(lic_parameters), intent(in) :: lic_p
!!        type(lic_field_data), intent(in) :: field_lic
!!        type(PVR_control_params), intent(in) :: pvr_param
!!        type(pvr_image_type), intent(in) :: pvr_rgb
!!        type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
!!        type(PVR_projection_data), intent(inout) :: pvr_proj(2)
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!        type(lic_repart_reference), intent(inout) :: rep_ref_viz
!!        type(mesh_SR), intent(inout) :: m_SR
!!@endverbatim
!
      module rendering_streo_LIC_image
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
      use t_rendering_vr_image
      use t_control_params_4_pvr
      use t_control_param_LIC
      use t_lic_field_data
      use t_geometries_in_pvr_screen
      use t_surf_grp_4_pvr_domain
      use t_pvr_ray_startpoints
      use t_pvr_image_array
      use t_lic_repart_reference
      use t_mesh_SR
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
      subroutine s_each_LIC_anaglyph(istep_pvr, time, elps_LIC,         &
     &          viz_fem, field_lic, sf_grp_4_sf, lic_param,             &
     &          pvr_param, pvr_proj, pvr_rgb, rep_ref_viz, m_SR)
!
      use cal_pvr_modelview_mat
      use rendering_LIC_image
      use set_default_pvr_params
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
!
      type(elapsed_lables), intent(in) :: elps_LIC
      type(mesh_data), intent(in) :: viz_fem
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
      type(lic_field_data), intent(in) :: field_lic
      type(lic_parameters), intent(in) :: lic_param
!
      type(PVR_control_params), intent(inout) :: pvr_param
      type(PVR_projection_data), intent(inout) :: pvr_proj(2)
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(lic_repart_reference), intent(inout) :: rep_ref_viz
      type(mesh_SR), intent(inout) :: m_SR
!
!
      if(iflag_debug .gt. 0) write(*,*) 'set_default_pvr_data_params'
      call set_default_pvr_data_params                                  &
     &   (pvr_param%outline, pvr_param%color)
!
!   Left eye
      call alloc_pvr_left_eye_image(pvr_rgb)
      call lic_rendering_with_fixed_view(istep_pvr, time, elps_LIC,     &
     &    viz_fem%mesh, viz_fem%group, sf_grp_4_sf, lic_param,          &
     &    field_lic, pvr_param, pvr_proj(1), pvr_rgb,                   &
     &    rep_ref_viz, m_SR)
      call store_left_eye_image(pvr_rgb)
!
!   Right eye
      call lic_rendering_with_fixed_view(istep_pvr, time, elps_LIC,     &
     &    viz_fem%mesh, viz_fem%group, sf_grp_4_sf, lic_param,          &
     &    field_lic, pvr_param, pvr_proj(2), pvr_rgb,                   &
     &    rep_ref_viz, m_SR)
      call add_left_eye_image(pvr_rgb)
      call dealloc_pvr_left_eye_image(pvr_rgb)
!
      end subroutine s_each_LIC_anaglyph
!
!  ---------------------------------------------------------------------
!
      subroutine anaglyph_lic_rendering_w_rot                           &
     &         (istep_pvr, time, elps_PVR, elps_LIC,                    &
     &          viz_fem, sf_grp_4_sf, field_lic, lic_p, pvr_param,      &
     &          pvr_bound, pvr_proj, pvr_rgb, rep_ref_viz, m_SR)
!
      use t_rotation_pvr_images
      use set_default_pvr_params
      use set_PVR_view_and_image
      use rendering_LIC_image
      use write_multi_PVR_image
      use output_image_sel_4_png
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
!
      type(elapsed_lables), intent(in) :: elps_PVR, elps_LIC
      type(mesh_data), intent(in) :: viz_fem
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
      type(lic_parameters), intent(in) :: lic_p
      type(lic_field_data), intent(in) :: field_lic
!
      type(PVR_control_params), intent(inout) :: pvr_param
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
      type(PVR_projection_data), intent(inout) :: pvr_proj(2)
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(lic_repart_reference), intent(inout) :: rep_ref_viz
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_rot
      type(rotation_pvr_images) :: rot_imgs1
!
!
      if(iflag_debug .gt. 0) write(*,*) 'set_default_pvr_data_params'
      call set_default_pvr_data_params                                  &
     &   (pvr_param%outline, pvr_param%color)
!
      if(my_rank .eq. 0) write(*,*) 'init_rot_pvr_image_arrays'
      call init_rot_pvr_image_arrays                                    &
     &   (pvr_param%movie_def, pvr_rgb, rot_imgs1)
!
!
      call alloc_pvr_left_eye_image(pvr_rgb)
      do i_rot = 1, pvr_param%movie_def%num_frame
!   Left eye
        call rot_multi_view_projection_mats(ione, i_rot,                &
     &      pvr_param, pvr_proj(1)%screen)
        call rendering_lic_at_once(istep_pvr, time,                     &
     &      elps_PVR, elps_LIC, viz_fem%mesh, viz_fem%group,            &
     &      sf_grp_4_sf, lic_p, field_lic, pvr_param, pvr_bound,        &
     &      pvr_proj(1), pvr_rgb, rep_ref_viz, m_SR)
        call store_left_eye_image(pvr_rgb)
!
!   Right eye
        call rot_multi_view_projection_mats(itwo, i_rot,                &
     &      pvr_param, pvr_proj(2)%screen)
        call rendering_lic_at_once(istep_pvr, time,                     &
     &      elps_PVR, elps_LIC, viz_fem%mesh, viz_fem%group,            &
     &      sf_grp_4_sf, lic_p, field_lic, pvr_param, pvr_bound,        &
     &      pvr_proj(2), pvr_rgb, rep_ref_viz, m_SR)
        call add_left_eye_image(pvr_rgb)
        call copy_pvr_image_data(pvr_rgb, rot_imgs1%rot_pvr_rgb(i_rot))
      end do
      call dealloc_pvr_left_eye_image(pvr_rgb)
      if(elps_LIC%flag_elapsed)                                         &
     &            call end_elapsed_time(elps_LIC%ist_elapsed+1)
!
      if(elps_LIC%flag_elapsed)                                         &
     &            call start_elapsed_time(elps_LIC%ist_elapsed+2)
      call output_rotation_PVR_images(istep_pvr,                        &
     &    pvr_param%movie_def%num_frame, rot_imgs1%rot_pvr_rgb(1))
      call dealloc_rot_pvr_image_arrays(pvr_param%movie_def, rot_imgs1)
      if(elps_LIC%flag_elapsed)                                         &
     &            call end_elapsed_time(elps_LIC%ist_elapsed+2)
      if(elps_LIC%flag_elapsed)                                         &
     &            call start_elapsed_time(elps_LIC%ist_elapsed+1)
!
      end subroutine anaglyph_lic_rendering_w_rot
!
!  ---------------------------------------------------------------------
!
      end module rendering_streo_LIC_image
