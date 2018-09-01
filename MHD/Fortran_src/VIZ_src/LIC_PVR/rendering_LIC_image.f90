!>@file  rendering_LIC_image.f90
!!       module rendering_LIC_image
!!
!!@author  Y. Liao and H. Matsui
!!@date Programmed in Feb., 2018
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine lic_rendering_with_fixed_view(istep_pvr,             &
!!     &          node, ele, surf, lic_p, pvr_param, file_param,        &
!!     &          start_pt, image, pvr_data, pvr_rgb)
!!      subroutine rendering_lic_at_once(istep_pvr,                     &
!!     &          node, ele, surf, group, lic_p, pvr_param, file_param, &
!!     &          start_pt, image, pvr_proj, pvr_data, pvr_rgb)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(mesh_groups), intent(in) :: group
!!        type(lic_parameters), intent(in) :: lic_p
!!        type(pvr_projection_data), intent(inout) :: pvr_proj
!!        type(PVR_control_params), intent(in) :: pvr_param
!!        type(pvr_output_parameter), intent(in) :: file_param
!!        type(PVR_image_generator), intent(inout) :: pvr_data
!!        type(pvr_ray_start_type), intent(inout) :: start_pt
!!        type(pvr_segmented_img), intent(inout)  :: image
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
      subroutine lic_rendering_with_fixed_view(istep_pvr,               &
     &          node, ele, surf, lic_p, pvr_param, file_param,          &
     &          start_pt, image, pvr_data, pvr_rgb)
!
      use composite_pvr_images
      use write_LIC_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(lic_parameters), intent(in) :: lic_p
      type(PVR_control_params), intent(in) :: pvr_param
      type(pvr_output_parameter), intent(in) :: file_param
!
      type(PVR_image_generator), intent(inout) :: pvr_data
      type(pvr_ray_start_type), intent(inout) :: start_pt
      type(pvr_segmented_img), intent(inout)  :: image
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
      type(pvr_ray_start_type) :: start_wk
!
!
      call copy_item_pvr_ray_start                                      &
     &   (pvr_data%start_save, start_pt)
!
      if(iflag_debug .gt. 0) write(*,*) 'rendering_image_4_lic'
      call rendering_image_4_lic(istep_pvr, file_param,                 &
     &    node, ele, surf, lic_p, pvr_data%color, pvr_param%colorbar,   &
     &    pvr_param%field, pvr_data%view, pvr_data%screen,              &
     &    start_wk, image, pvr_rgb)
!
      end subroutine lic_rendering_with_fixed_view
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine rendering_lic_at_once(istep_pvr,                       &
     &          node, ele, surf, group, lic_p, pvr_param, file_param,   &
     &          start_pt, image, pvr_proj, pvr_data, pvr_rgb)
!
      use cal_pvr_modelview_mat
      use composite_pvr_images
      use write_LIC_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(mesh_groups), intent(in) :: group
      type(lic_parameters), intent(in) :: lic_p
      type(PVR_control_params), intent(in) :: pvr_param
      type(pvr_output_parameter), intent(in) :: file_param
!
      type(pvr_projection_data), intent(inout) :: pvr_proj
      type(PVR_image_generator), intent(inout) :: pvr_data
      type(pvr_ray_start_type), intent(inout) :: start_pt
      type(pvr_segmented_img), intent(inout)  :: image
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
!
      call transfer_to_screen                                           &
     &   (node, ele, surf, group%surf_grp, group%surf_grp_geom,         &
     &    pvr_param%field, pvr_data%view, pvr_proj%projection_mat,      &
     &    pvr_param%pixel, pvr_proj%bound, pvr_data%screen, start_pt)
      call set_subimages(pvr_rgb%num_pixel_xy, start_pt, image)
!
      if(iflag_debug .gt. 0) write(*,*) 'rendering_image_4_lic'
      call rendering_image_4_lic(istep_pvr, file_param,                 &
     &    node, ele, surf, lic_p, pvr_data%color, pvr_param%colorbar,   &
     &    pvr_param%field, pvr_data%view, pvr_data%screen,              &
     &    start_pt, image, pvr_rgb)
!
      end subroutine rendering_lic_at_once
!
!  ---------------------------------------------------------------------
!
      end module rendering_LIC_image
