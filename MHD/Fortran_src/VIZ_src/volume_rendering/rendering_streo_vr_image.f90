!>@file  rendering_streo_vr_image.f90
!!       module rendering_streo_vr_image
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine rendering_with_rotation                              &
!!     &         (istep_pvr, node, ele, surf, group,                    &
!!     &          pvr_param, pvr_proj, pvr_data, pvr_rgb)
!!      subroutine anaglyph_rendering_w_rotation                        &
!!     &         (istep_pvr,  node, ele, surf, group,                   &
!!     &          pvr_param, pvr_proj, pvr_data, pvr_rgb)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(mesh_groups), intent(in) :: group
!!        type(PVR_control_params), intent(in) :: pvr_param
!!        type(PVR_projection_data), intent(inout) :: pvr_proj(2)
!!        type(PVR_image_generator), intent(inout) :: pvr_data
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!@endverbatim
!
      module rendering_streo_vr_image
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
      use t_rendering_vr_image
      use t_control_params_4_pvr
      use t_geometries_in_pvr_screen
      use t_surf_grp_4_pvr_domain
      use t_pvr_ray_startpoints
      use t_pvr_image_array
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
      subroutine rendering_with_rotation                                &
     &         (istep_pvr, node, ele, surf, group,                      &
     &          pvr_param, pvr_proj, pvr_data, pvr_rgb)
!
      use cal_pvr_modelview_mat
      use composite_pvr_images
      use write_PVR_image
!
      integer(kind = kint), intent(in) :: istep_pvr
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(mesh_groups), intent(in) :: group
      type(PVR_control_params), intent(in) :: pvr_param
!
      type(PVR_projection_data), intent(inout) :: pvr_proj
      type(PVR_image_generator), intent(inout) :: pvr_data
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
      integer(kind = kint) :: i_rot, ist_rot, ied_rot
!
!
      ist_rot = pvr_data%view%istart_rot
      ied_rot = pvr_data%view%iend_rot
      do i_rot = ist_rot, ied_rot
        call cal_pvr_modelview_matrix                                   &
     &     (i_rot, pvr_param%outline, pvr_data%view, pvr_data%color)
!
        call rendering_at_once(istep_pvr, node, ele, surf, group,       &
     &      pvr_param, pvr_proj, pvr_data, pvr_rgb)
!
        call end_elapsed_time(71)
        call start_elapsed_time(72)
        call sel_write_pvr_image_file(i_rot, istep_pvr, pvr_rgb)
        call calypso_mpi_barrier
        call end_elapsed_time(72)
        call start_elapsed_time(71)
      end do
!
      end subroutine rendering_with_rotation
!
!  ---------------------------------------------------------------------
!
      subroutine anaglyph_rendering_w_rotation                          &
     &         (istep_pvr, node, ele, surf, group,                      &
     &          pvr_param, pvr_proj, pvr_data, pvr_rgb)
!
      use cal_pvr_modelview_mat
      use write_PVR_image
!
      integer(kind = kint), intent(in) :: istep_pvr
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(mesh_groups), intent(in) :: group
      type(PVR_control_params), intent(in) :: pvr_param
!
      type(PVR_projection_data), intent(inout) :: pvr_proj(2)
      type(PVR_image_generator), intent(inout) :: pvr_data
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
      integer(kind = kint) :: i_rot, ist_rot, ied_rot
!
!
      ist_rot = pvr_data%view%istart_rot
      ied_rot = pvr_data%view%iend_rot
      do i_rot = ist_rot, ied_rot
        call cal_pvr_modelview_matrix                                   &
     &     (i_rot, pvr_param%outline, pvr_data%view, pvr_data%color)
!
!    Left eye
        call rendering_at_once(istep_pvr, node, ele, surf, group,       &
     &      pvr_param, pvr_proj(1), pvr_data, pvr_rgb)
        call store_left_eye_image(pvr_rgb)
!
!    Right eye
        call rendering_at_once(istep_pvr, node, ele, surf, group,       &
     &      pvr_param, pvr_proj(2), pvr_data, pvr_rgb)
        call add_left_eye_image(pvr_rgb)
!
        call end_elapsed_time(71)
        call start_elapsed_time(72)
        call sel_write_pvr_image_file(i_rot, istep_pvr, pvr_rgb)
        call calypso_mpi_barrier
        call end_elapsed_time(72)
        call start_elapsed_time(71)
      end do
!
      end subroutine anaglyph_rendering_w_rotation
!
!  ---------------------------------------------------------------------
!
      end module rendering_streo_vr_image
