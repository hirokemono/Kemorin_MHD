!>@file  rendering_streo_LIC_image.f90
!!       module rendering_streo_LIC_image
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine lic_rendering_with_rotation                          &
!!     &         (istep_pvr, time, mesh, group, sf_grp_4_sf, lic_p,     &
!!     &          field_lic, pvr_rgb, pvr_param, pvr_bound, pvr_proj,   &
!!     &          rep_ref_viz, m_SR)
!!      subroutine anaglyph_lic_rendering_w_rot                         &
!!     &         (istep_pvr, time, viz_fem, field_lic, lic_p,           &
!!     &          lic_p, pvr_rgb, pvr_param, pvr_bound, pvr_proj,       &
!!     &          rep_ref_viz, m_SR)
!!        type(mesh_data), intent(in) :: viz_fem
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
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
      subroutine lic_rendering_with_rotation                            &
     &         (istep_pvr, time, mesh, group, sf_grp_4_sf, lic_p,       &
     &          field_lic, pvr_rgb, pvr_param, pvr_bound, pvr_proj,     &
     &          rep_ref_viz, m_SR)
!
      use t_rotation_pvr_images
      use m_elapsed_labels_4_VIZ
      use rendering_LIC_image
      use write_PVR_image
      use output_image_sel_4_png
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
      type(lic_parameters), intent(in) :: lic_p
      type(lic_field_data), intent(in) :: field_lic
      type(pvr_image_type), intent(in) :: pvr_rgb
!
      type(PVR_control_params), intent(inout) :: pvr_param
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
      type(PVR_projection_data), intent(inout) :: pvr_proj
      type(lic_repart_reference), intent(inout) :: rep_ref_viz
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_rot
      type(rotation_pvr_images) :: rot_imgs1
!
!
      if(my_rank .eq. 0) write(*,*) 'init_rot_pvr_image_arrays'
      call init_rot_pvr_image_arrays                                    &
     &   (pvr_param%movie_def, pvr_rgb, rot_imgs1)
!
      do i_rot = 1, pvr_param%movie_def%num_frame
        call rendering_lic_at_once(istep_pvr, time, izero, i_rot,       &
     &      mesh, group, sf_grp_4_sf, lic_p, field_lic, pvr_param,      &
     &      pvr_bound, pvr_proj, rot_imgs1%rot_pvr_rgb(i_rot),          &
     &      rep_ref_viz, m_SR)
      end do
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+1)
!
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+2)
      do i_rot = 1, pvr_param%movie_def%num_frame
        call sel_write_pvr_image_file(istep_pvr, i_rot,                 &
     &                                rot_imgs1%rot_pvr_rgb(i_rot))
      end do
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+2)
!
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+1)
      call dealloc_rot_pvr_image_arrays(pvr_param%movie_def, rot_imgs1)
!
      end subroutine lic_rendering_with_rotation
!
!  ---------------------------------------------------------------------
!
      subroutine anaglyph_lic_rendering_w_rot                           &
     &         (istep_pvr, time, viz_fem, sf_grp_4_sf, field_lic,       &
     &          lic_p, pvr_rgb, pvr_param, pvr_bound, pvr_proj,         &
     &          rep_ref_viz, m_SR)
!
      use t_rotation_pvr_images
      use m_elapsed_labels_4_VIZ
      use set_default_pvr_params
      use rendering_LIC_image
      use write_PVR_image
      use output_image_sel_4_png
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
!
      type(mesh_data), intent(in) :: viz_fem
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
      type(lic_parameters), intent(in) :: lic_p
      type(lic_field_data), intent(in) :: field_lic
      type(pvr_image_type), intent(in) :: pvr_rgb
!
      type(PVR_control_params), intent(inout) :: pvr_param
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
      type(PVR_projection_data), intent(inout) :: pvr_proj(2)
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
      do i_rot = 1, pvr_param%movie_def%num_frame
!   Left eye
        call rendering_lic_at_once                                      &
     &     (istep_pvr, time, ione, i_rot, viz_fem%mesh, viz_fem%group,  &
     &      sf_grp_4_sf, lic_p, field_lic, pvr_param, pvr_bound,        &
     &      pvr_proj(1), rot_imgs1%rot_pvr_rgb(i_rot),                  &
     &      rep_ref_viz, m_SR)
        call store_left_eye_image(rot_imgs1%rot_pvr_rgb(i_rot))
!
!   Right eye
        call rendering_lic_at_once                                      &
     &     (istep_pvr, time, itwo, i_rot, viz_fem%mesh, viz_fem%group,  &
     &      sf_grp_4_sf, lic_p, field_lic, pvr_param, pvr_bound,        &
     &      pvr_proj(2), rot_imgs1%rot_pvr_rgb(i_rot),                  &
     &      rep_ref_viz, m_SR)
        call add_left_eye_image(rot_imgs1%rot_pvr_rgb(i_rot))
      end do
      if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+1)
!
      do i_rot = 1, pvr_param%movie_def%num_frame
        call sel_write_pvr_image_file(istep_pvr, i_rot,                 &
     &                                rot_imgs1%rot_pvr_rgb(i_rot))
      end do
!
      if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+1)
      call dealloc_rot_pvr_image_arrays(pvr_param%movie_def, rot_imgs1)
!
      end subroutine anaglyph_lic_rendering_w_rot
!
!  ---------------------------------------------------------------------
!
      end module rendering_streo_LIC_image
