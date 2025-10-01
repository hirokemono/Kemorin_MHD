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
!!     &         (istep_pvr, time, elps_LIC, mesh, group, sf_grp_4_sf,  &
!!     &          lic_p, field_lic, pvr_param, pvr_proj, pvr_rgb,       &
!!     &          rep_ref_viz, m_SR)
!!      subroutine rendering_lic_at_once                                &
!!     &         (istep_pvr, time, elps_PVR, elps_LIC,                  &
!!     &          mesh, group, sf_grp_4_sf, lic_p, field_lic, pvr_param,&
!!     &          pvr_bound, pvr_proj, pvr_rgb, rep_ref_viz, m_SR)
!!        integer(kind = kint), intent(in) :: i_img, i_rot
!!        integer(kind = kint), intent(in) :: istep_pvr
!!        type(elapsed_lables), intent(in) :: elps_LIC
!!        type(elapsed_lables), intent(in) :: elps_PVR
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
!!        type(lic_repart_reference), intent(inout) :: rep_ref_viz
!!        type(mesh_SR), intent(inout) :: m_SR
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
      subroutine lic_rendering_with_fixed_view                          &
     &         (istep_pvr, time, elps_LIC, mesh, group, sf_grp_4_sf,    &
     &          lic_p, field_lic, pvr_param, pvr_proj, pvr_rgb,         &
     &          rep_ref_viz, m_SR)
!
      use write_LIC_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
      type(elapsed_lables), intent(in) :: elps_LIC
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(sf_grp_list_each_surf), intent(in) :: sf_grp_4_sf
      type(lic_parameters), intent(in) :: lic_p
      type(PVR_control_params), intent(in) :: pvr_param
      type(lic_field_data), intent(in) :: field_lic
!
      type(PVR_projection_data), intent(inout) :: pvr_proj
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(lic_repart_reference), intent(inout) :: rep_ref_viz
      type(mesh_SR), intent(inout) :: m_SR
!
!
      call copy_item_pvr_ray_start                                      &
     &   (pvr_proj%start_save, pvr_proj%start_fix)
!
      if(iflag_debug .gt. 0) write(*,*) 'rendering_image_4_lic'
      call rendering_image_4_lic                                        &
     &   (istep_pvr, time, elps_LIC, mesh, group, sf_grp_4_sf, lic_p,   &
     &    pvr_param%color, pvr_param%colorbar, field_lic,               &
     &    pvr_param%draw_param, pvr_proj%screen, pvr_proj%start_fix,    &
     &    pvr_proj%stencil, pvr_rgb, rep_ref_viz, m_SR)
!
      end subroutine lic_rendering_with_fixed_view
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine rendering_lic_at_once                                  &
     &         (istep_pvr, time, elps_PVR, elps_LIC,                    &
     &          mesh, group, sf_grp_4_sf, lic_p, field_lic, pvr_param,  &
     &          pvr_bound, pvr_proj, pvr_rgb, rep_ref_viz, m_SR)
!
      use cal_pvr_projection_mat
      use cal_pvr_modelview_mat
      use write_LIC_image
      use t_pvr_stencil_buffer
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
      type(elapsed_lables), intent(in) :: elps_PVR, elps_LIC
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
      type(lic_repart_reference), intent(inout) :: rep_ref_viz
      type(mesh_SR), intent(inout) :: m_SR
!
!
      call transfer_to_screen(mesh%node, mesh%surf,                     &
     &    pvr_param%pixel, pvr_param%multi_view(1)%n_pvr_pixel,         &
     &    pvr_bound, pvr_proj%screen, pvr_proj%start_fix)
      call const_pvr_stencil_buffer                                     &
     &   (elps_PVR, pvr_rgb, pvr_proj%start_fix, pvr_proj%stencil,      &
     &    m_SR%SR_sig, m_SR%SR_r, m_SR%SR_i)
!
      if(iflag_debug .gt. 0) write(*,*) 'rendering_image_4_lic'
      call rendering_image_4_lic                                        &
     &   (istep_pvr, time, elps_LIC, mesh, group, sf_grp_4_sf, lic_p,   &
     &    pvr_param%color, pvr_param%colorbar, field_lic,               &
     &    pvr_param%draw_param, pvr_proj%screen, pvr_proj%start_fix,    &
     &    pvr_proj%stencil, pvr_rgb, rep_ref_viz, m_SR)
!
      call deallocate_pvr_ray_start(pvr_proj%start_fix)
      call dealloc_pvr_stencil_buffer(pvr_proj%stencil)
!
      end subroutine rendering_lic_at_once
!
!  ---------------------------------------------------------------------
!
      end module rendering_LIC_image
