!>@file  each_LIC_rendering.f90
!!       module each_LIC_rendering
!!
!!@author  Y. Liao and H. Matsui
!!@date Programmed in Feb., 2018
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine s_each_LIC_rendering(istep_pvr, time, num_img,       &
!!     &          viz_fem, field_lic, sf_grp_4_sf, lic_param,           &
!!     &          pvr_param, pvr_proj, pvr_rgb, rep_ref_viz, m_SR)
!!        integer(kind = kint), intent(in) :: num_img
!!        integer(kind = kint), intent(in) :: istep_pvr
!!        real(kind = kreal), intent(in) :: time
!!        type(mesh_data), intent(in) :: viz_fem
!!        type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
!!        type(lic_field_data), intent(in) :: field_lic
!!        type(lic_parameters), intent(in) :: lic_param
!!        type(PVR_control_params), intent(inout) :: pvr_param
!!        type(PVR_projection_data), intent(inout) :: pvr_proj(num_img)
!!        type(pvr_image_type), intent(inout) :: pvr_rgb(num_img)
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!        type(lic_repart_reference), intent(inout) :: rep_ref_viz
!!        type(mesh_SR), intent(inout) :: m_SR
!!      subroutine s_each_LIC_anaglyph(istep_pvr, time,                 &
!!     &          viz_fem, field_lic, sf_grp_4_sf, lic_param,           &
!!     &          pvr_param, pvr_proj, pvr_rgb, rep_ref_viz, m_SR)
!!        integer(kind = kint), intent(in) :: istep_pvr
!!        real(kind = kreal), intent(in) :: time
!!        type(mesh_data), intent(in) :: viz_fem
!!        type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
!!        type(lic_field_data), intent(in) :: field_lic
!!        type(lic_parameters), intent(in) :: lic_param
!!        type(PVR_control_params), intent(inout) :: pvr_param
!!        type(PVR_projection_data), intent(inout) :: pvr_proj(2)
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!        type(lic_repart_reference), intent(inout) :: rep_ref_viz
!!        type(mesh_SR), intent(inout) :: m_SR
!!      subroutine s_each_LIC_rendering_w_rot                           &
!!     &         (istep_pvr, time, viz_fem, field_lic, sf_grp_4_sf,     &
!!     &          lic_param, pvr_param, pvr_bound, pvr_proj, pvr_rgb,   &
!!     &          rep_ref_viz, m_SR)
!!      subroutine each_LIC_quilt_rendering_w_rot(istep_pvr, time,      &
!!     &          num_img, viz_fem, field_lic, sf_grp_4_sf,             &
!!     &          lic_param, pvr_param, pvr_bound, pvr_proj, pvr_rgb,   &
!!     &          rep_ref_viz, m_SR)
!!        integer(kind = kint), intent(in) :: num_img
!!        integer(kind = kint), intent(in) :: istep_pvr
!!        real(kind = kreal), intent(in) :: time
!!        type(mesh_data), intent(in) :: viz_fem
!!        type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
!!        type(lic_field_data), intent(in) :: field_lic
!!        type(lic_parameters), intent(in) :: lic_param
!!        type(PVR_control_params), intent(inout) :: pvr_param
!!        type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
!!        type(PVR_projection_data), intent(inout) :: pvr_proj(num_img)
!!        type(pvr_image_type), intent(inout) :: pvr_rgb(num_img)
!!        type(lic_repart_reference), intent(inout) :: rep_ref_viz
!!        type(mesh_SR), intent(inout) :: m_SR
!!@endverbatim
!
!
      module each_LIC_rendering
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
!
      use t_mesh_data
      use t_phys_data
!
      use t_surf_grp_list_each_surf
      use t_rendering_vr_image
      use t_control_params_4_pvr
      use t_surf_grp_4_pvr_domain
      use t_pvr_ray_startpoints
      use t_pvr_image_array
      use t_geometries_in_pvr_screen
      use t_lic_field_data
      use t_control_param_LIC
      use t_lic_repart_reference
      use t_mesh_SR
!
      use set_default_pvr_params
      use set_position_pvr_screen
      use mesh_outline_4_pvr
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
      subroutine s_each_LIC_rendering(istep_pvr, time, num_img,         &
     &          viz_fem, field_lic, sf_grp_4_sf, lic_param,             &
     &          pvr_param, pvr_proj, pvr_rgb, rep_ref_viz, m_SR)
!
      use cal_pvr_modelview_mat
      use rendering_LIC_image
      use rendering_streo_LIC_image
!
      integer(kind = kint), intent(in) :: num_img
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
!
      type(mesh_data), intent(in) :: viz_fem
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
      type(lic_field_data), intent(in) :: field_lic
      type(lic_parameters), intent(in) :: lic_param
!
      type(PVR_control_params), intent(inout) :: pvr_param
      type(PVR_projection_data), intent(inout) :: pvr_proj(num_img)
      type(pvr_image_type), intent(inout) :: pvr_rgb(num_img)
      type(lic_repart_reference), intent(inout) :: rep_ref_viz
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_img
!
!
      if(iflag_debug .gt. 0) write(*,*) 'set_default_pvr_data_params'
      call set_default_pvr_data_params                                  &
     &   (pvr_param%outline, pvr_param%color)
!
      do i_img = 1, num_img
        call lic_rendering_with_fixed_view(istep_pvr, time,             &
     &      viz_fem%mesh, viz_fem%group, sf_grp_4_sf, lic_param,        &
     &      field_lic, pvr_param,  pvr_proj(i_img), pvr_rgb(i_img),     &
     &      rep_ref_viz, m_SR)
      end do
!
      end subroutine s_each_LIC_rendering
!
!  ---------------------------------------------------------------------
!
      subroutine s_each_LIC_anaglyph(istep_pvr, time,                   &
     &          viz_fem, field_lic, sf_grp_4_sf, lic_param,             &
     &          pvr_param, pvr_proj, pvr_rgb, rep_ref_viz, m_SR)
!
      use cal_pvr_modelview_mat
      use rendering_LIC_image
      use rendering_streo_LIC_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
!
      type(mesh_data), intent(in) :: viz_fem
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
      type(lic_field_data), intent(in) :: field_lic
      type(lic_parameters), intent(in) :: lic_param
!
      type(PVR_control_params), intent(inout) :: pvr_param
      type(PVR_projection_data), intent(inout) :: pvr_proj(2)
      type(pvr_image_type), intent(inout) :: pvr_rgb(2)
      type(lic_repart_reference), intent(inout) :: rep_ref_viz
      type(mesh_SR), intent(inout) :: m_SR
!
!
      if(iflag_debug .gt. 0) write(*,*) 'set_default_pvr_data_params'
      call set_default_pvr_data_params                                  &
     &   (pvr_param%outline, pvr_param%color)
!
!   Left eye
      call lic_rendering_with_fixed_view(istep_pvr, time,               &
     &    viz_fem%mesh, viz_fem%group, sf_grp_4_sf, lic_param,          &
     &    field_lic, pvr_param, pvr_proj(1), pvr_rgb(1),                &
     &    rep_ref_viz, m_SR)
      call store_left_eye_image(pvr_rgb(1))
!
!   Right eye
      call lic_rendering_with_fixed_view(istep_pvr, time,               &
     &    viz_fem%mesh, viz_fem%group, sf_grp_4_sf, lic_param,          &
     &    field_lic, pvr_param, pvr_proj(2), pvr_rgb(1),                &
     &    rep_ref_viz, m_SR)
      call add_left_eye_image(pvr_rgb(1))
!
      end subroutine s_each_LIC_anaglyph
!
!  ---------------------------------------------------------------------
!
      subroutine s_each_LIC_rendering_w_rot                             &
     &         (istep_pvr, time, viz_fem, field_lic, sf_grp_4_sf,       &
     &          lic_param, pvr_param, pvr_bound, pvr_proj, pvr_rgb,     &
     &          rep_ref_viz, m_SR)
!
      use cal_pvr_modelview_mat
      use rendering_LIC_image
      use rendering_streo_LIC_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
!
      type(mesh_data), intent(in) :: viz_fem
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
      type(lic_field_data), intent(in) :: field_lic
      type(lic_parameters), intent(in) :: lic_param
!
      type(PVR_control_params), intent(inout) :: pvr_param
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
      type(PVR_projection_data), intent(inout) :: pvr_proj
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(lic_repart_reference), intent(inout) :: rep_ref_viz
      type(mesh_SR), intent(inout) :: m_SR
!
!
      if(iflag_debug .gt. 0) write(*,*) 'set_default_pvr_data_params'
      call set_default_pvr_data_params                                  &
     &   (pvr_param%outline, pvr_param%color)
!
      call lic_rendering_with_rotation(istep_pvr, time,                 &
     &    viz_fem%mesh, viz_fem%group, sf_grp_4_sf,                     &
     &    lic_param, field_lic, pvr_rgb, pvr_param, pvr_bound,          &
     &    pvr_proj, rep_ref_viz, m_SR)
!
      end subroutine s_each_LIC_rendering_w_rot
!
!  ---------------------------------------------------------------------
!
      subroutine each_LIC_quilt_rendering_w_rot(istep_pvr, time,        &
     &          num_img, viz_fem, field_lic, sf_grp_4_sf,               &
     &          lic_param, pvr_param, pvr_bound, pvr_proj, pvr_rgb,     &
     &          rep_ref_viz, m_SR)
!
      use m_elapsed_labels_4_VIZ
      use cal_pvr_modelview_mat
      use rendering_LIC_image
      use rendering_streo_LIC_image
      use write_PVR_image
!
      integer(kind = kint), intent(in) :: num_img
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
!
      type(mesh_data), intent(in) :: viz_fem
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
      type(lic_field_data), intent(in) :: field_lic
      type(lic_parameters), intent(in) :: lic_param
!
      type(PVR_control_params), intent(inout) :: pvr_param
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
      type(PVR_projection_data), intent(inout) :: pvr_proj(num_img)
      type(pvr_image_type), intent(inout) :: pvr_rgb(num_img)
      type(lic_repart_reference), intent(inout) :: rep_ref_viz
      type(mesh_SR), intent(inout) :: m_SR
!
      integer(kind = kint) :: i_img, i_rot
!
!
      if(iflag_debug .gt. 0) write(*,*) 'set_default_pvr_data_params'
      call set_default_pvr_data_params                                  &
     &   (pvr_param%outline, pvr_param%color)
!
      do i_rot = 1, pvr_param%movie_def%num_frame
        do i_img = 1, num_img
          call rendering_lic_at_once(istep_pvr, time, izero, i_rot,     &
     &        viz_fem%mesh, viz_fem%group, sf_grp_4_sf,                 &
     &        lic_param, field_lic, pvr_param, pvr_bound,               &
     &        pvr_proj(i_img), pvr_rgb(i_img),  rep_ref_viz, m_SR)
        end do
        if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+1)
!
        if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+2)
        call set_output_rot_sequence_image(istep_pvr,                   &
     &     pvr_rgb(1)%id_pvr_file_type, pvr_rgb(1)%pvr_prefix, num_img, &
     &     pvr_param%stereo_def%n_column_row_view, pvr_rgb(1))
        if(iflag_LIC_time) call end_elapsed_time(ist_elapsed_LIC+2)
        if(iflag_LIC_time) call start_elapsed_time(ist_elapsed_LIC+1)
      end do
!
      end subroutine each_LIC_quilt_rendering_w_rot
!
!  ---------------------------------------------------------------------
!
      end module each_LIC_rendering
