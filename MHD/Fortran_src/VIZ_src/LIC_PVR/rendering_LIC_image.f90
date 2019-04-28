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
!!     &         (istep_pvr, node, ele, surf, lic_p, pvr_param,         &
!!     &          pvr_proj, pvr_rgb)
!!      subroutine rendering_lic_at_once                                &
!!     &         (istep_pvr,  node, ele, surf, group, lic_p, pvr_param, &
!!     &          pvr_proj, pvr_rgb)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(mesh_groups), intent(in) :: group
!!        type(lic_parameters), intent(in) :: lic_p
!!        type(PVR_projection_data), intent(inout) :: pvr_proj
!!        type(PVR_control_params), intent(in) :: pvr_param
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
      subroutine lic_rendering_with_fixed_view                          &
     &         (istep_pvr, node, ele, surf, lic_p, pvr_param,           &
     &          pvr_proj, pvr_rgb)
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
!
      type(PVR_projection_data), intent(inout) :: pvr_proj
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
!
      call copy_item_pvr_ray_start                                      &
     &   (pvr_proj%start_save, pvr_proj%start_pt)
!
      if(iflag_debug .gt. 0) write(*,*) 'rendering_image_4_lic'
      call rendering_image_4_lic(istep_pvr, node, ele, surf, lic_p,     &
     &    pvr_param%color, pvr_param%colorbar, pvr_param%field,         &
     &    pvr_param%view, pvr_proj%screen, pvr_proj%start_pt,           &
     &    pvr_proj%image, pvr_rgb)
!
      end subroutine lic_rendering_with_fixed_view
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine rendering_lic_at_once                                  &
     &         (istep_pvr, node, ele, surf, group, lic_p, pvr_param,    &
     &          pvr_proj, pvr_rgb)
!
      use cal_pvr_modelview_mat
      use composite_pvr_images
      use write_LIC_image
      use t_pvr_stencil_buffer
!
      integer(kind = kint), intent(in) :: istep_pvr
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(mesh_groups), intent(in) :: group
      type(lic_parameters), intent(in) :: lic_p
      type(PVR_control_params), intent(in) :: pvr_param
!
      type(PVR_projection_data), intent(inout) :: pvr_proj
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
!
      call dealloc_pvr_local_subimage(pvr_proj%image)
      call deallocate_pvr_ray_start(pvr_proj%start_pt)
!
      call transfer_to_screen(node, ele, surf,                          &
     &    group%surf_grp, group%surf_grp_geom,  pvr_param%field,        &
     &    pvr_param%view, pvr_proj%projection_mat, pvr_param%pixel,     &
     &    pvr_proj%bound, pvr_proj%screen, pvr_proj%start_pt)
      call const_pvr_stencil_buffer                                     &
     &   (pvr_rgb%num_pixel_xy, pvr_proj%start_pt)
      call set_subimages                                                &
     &   (pvr_rgb%num_pixel_xy, pvr_proj%start_pt, pvr_proj%image)
!
      if(iflag_debug .gt. 0) write(*,*) 'rendering_image_4_lic'
      call rendering_image_4_lic(istep_pvr, node, ele, surf, lic_p,     &
     &    pvr_param%color, pvr_param%colorbar, pvr_param%field,         &
     &    pvr_param%view, pvr_proj%screen, pvr_proj%start_pt,           &
     &    pvr_proj%image, pvr_rgb)
!
      end subroutine rendering_lic_at_once
!
!  ---------------------------------------------------------------------
!
      end module rendering_LIC_image
