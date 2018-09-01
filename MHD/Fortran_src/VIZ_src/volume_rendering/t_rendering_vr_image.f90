!>@file  t_rendering_vr_image.f90
!!       module t_rendering_vr_image
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine set_fixed_view_and_image(node, ele, surf, group,     &
!!     &          pvr_param, pvr_data, pvr_rgb, pvr_proj)
!!      subroutine rendering_with_fixed_view                            &
!!     &         (istep_pvr, node, ele, surf, pvr_param, file_param,    &
!!     &          pvr_proj, pvr_data, pvr_rgb)
!!      subroutine flush_rendering_4_fixed_view(pvr_proj)
!!
!!      subroutine rendering_at_once(istep_pvr, node, ele, surf, group, &
!!     &          pvr_param, file_param, pvr_proj, pvr_data, pvr_rgb)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(mesh_groups), intent(in) :: group
!!        type(PVR_control_params), intent(in) :: pvr_param
!!        type(pvr_projection_data), intent(inout) :: pvr_proj
!!        type(PVR_image_generator), intent(inout) :: pvr_data
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
      use t_control_params_4_pvr
      use t_geometries_in_pvr_screen
      use t_surf_grp_4_pvr_domain
      use t_pvr_ray_startpoints
      use t_pvr_image_array
      use generate_vr_image
!
      implicit  none
!
!>      Structure of PVR field parameters
      type PVR_field_params
!>        Structure for field parameter for PVR
        type(pvr_field_parameter) :: field_def
!>        Structure for rendering area by element group
        type(viz_area_parameter) :: area_def
      end type PVR_field_params
!
!>      Structure of PVR control parameters
      type PVR_control_params
!>        Parameters for image pixels
        type(pvr_pixel_position_type) :: pixel
!>        Structure for rough serch of subdomains
        type(pvr_domain_outline) :: outline
!>        Field data for volume rendering
        type(pvr_projected_field) :: field
!>        Structure for PVR colormap
        type(pvr_colorbar_parameter):: colorbar
      end type PVR_control_params
!
!
!>      Structure of PVR image generation
      type PVR_image_generator
!>        Viewer coordinate information
        type(pvr_view_parameter) :: view
!>        color paramter for volume rendering
        type(pvr_colormap_parameter) :: color
      end type PVR_image_generator
!
!>      Structure for projection data
      type pvr_projection_data
!>        perspective projection matrix
        real(kind = kreal) :: projection_mat(4,4)
!
!>        Domain boundary information
        type(pvr_bounds_surf_ctl) :: bound
!>        Data on screen coordinate
        type(pvr_projected_position) :: screen
!>        Start point structure for volume rendering
        type(pvr_ray_start_type) :: start_pt
!>        Work area of  point structure for volume rendering
        type(pvr_ray_start_type) :: start_save
!>        Pixel data structure for volume rendering
        type(pvr_segmented_img) :: image
      end type pvr_projection_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_fixed_view_and_image(node, ele, surf, group,       &
     &          pvr_param, pvr_data, pvr_rgb, pvr_proj)
!
      use cal_pvr_modelview_mat
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(mesh_groups), intent(in) :: group
      type(PVR_control_params), intent(in) :: pvr_param
      type(pvr_image_type), intent(in) :: pvr_rgb
      type(PVR_image_generator), intent(in) :: pvr_data
!
      type(pvr_projection_data), intent(inout) :: pvr_proj
!
!
      call transfer_to_screen(node, ele, surf,                          &
     &    group%surf_grp, group%surf_grp_geom, pvr_param%field,         &
     &    pvr_data%view, pvr_proj%projection_mat, pvr_param%pixel,      &
     &    pvr_proj%bound, pvr_proj%screen, pvr_proj%start_pt)
      call set_subimages                                                &
     &   (pvr_rgb%num_pixel_xy, pvr_proj%start_pt, pvr_proj%image)
!
      call allocate_item_pvr_ray_start                                  &
     &   (pvr_proj%start_pt%num_pvr_ray, pvr_proj%start_save)
      call copy_item_pvr_ray_start                                      &
     &   (pvr_proj%start_pt, pvr_proj%start_save)
!
      end subroutine set_fixed_view_and_image
!
!  ---------------------------------------------------------------------
!
      subroutine rendering_with_fixed_view                              &
     &         (istep_pvr, node, ele, surf, pvr_param, file_param,      &
     &          pvr_proj, pvr_data, pvr_rgb)
!
      use write_PVR_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(PVR_control_params), intent(in) :: pvr_param
      type(pvr_output_parameter), intent(in) :: file_param
!
      type(pvr_projection_data), intent(inout) :: pvr_proj
      type(PVR_image_generator), intent(inout) :: pvr_data
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
!
      call copy_item_pvr_ray_start                                      &
     &   (pvr_proj%start_save, pvr_proj%start_pt)
!
      if(iflag_debug .gt. 0) write(*,*) 'rendering_image'
      call rendering_image(istep_pvr, file_param,                       &
     &    node, ele, surf, pvr_data%color, pvr_param%colorbar,          &
     &    pvr_param%field, pvr_data%view, pvr_proj%screen,              &
     &    pvr_proj%start_pt, pvr_proj%image, pvr_rgb)
!
      end subroutine rendering_with_fixed_view
!
!  ---------------------------------------------------------------------
!
      subroutine flush_rendering_4_fixed_view(pvr_proj)
!
      type(pvr_projection_data), intent(inout) :: pvr_proj
!
!
      call dealloc_pvr_surf_domain_item(pvr_proj%bound)
      call dealloc_projected_position(pvr_proj%screen)
      call deallocate_pvr_ray_start(pvr_proj%start_pt)
      call deallocate_pvr_ray_start(pvr_proj%start_save)
      call dealloc_pvr_local_subimage(pvr_proj%image)
!
      end subroutine flush_rendering_4_fixed_view
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine rendering_at_once(istep_pvr, node, ele, surf, group,   &
     &          pvr_param, file_param, pvr_proj, pvr_data, pvr_rgb)
!
      use cal_pvr_modelview_mat
      use composite_pvr_images
      use write_PVR_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(mesh_groups), intent(in) :: group
      type(PVR_control_params), intent(in) :: pvr_param
      type(pvr_output_parameter), intent(in) :: file_param
!
      type(pvr_projection_data), intent(inout) :: pvr_proj
      type(PVR_image_generator), intent(inout) :: pvr_data
      type(pvr_image_type), intent(inout) :: pvr_rgb
!
!
      call transfer_to_screen                                           &
     &   (node, ele, surf, group%surf_grp, group%surf_grp_geom,         &
     &    pvr_param%field, pvr_data%view, pvr_proj%projection_mat,      &
     &    pvr_param%pixel,  pvr_proj%bound, pvr_proj%screen,            &
     &    pvr_proj%start_pt)
      call set_subimages                                                &
     &   (pvr_rgb%num_pixel_xy, pvr_proj%start_pt, pvr_proj%image)
!
      if(iflag_debug .gt. 0) write(*,*) 'rendering_image'
      call rendering_image(istep_pvr, file_param,                       &
     &    node, ele, surf, pvr_data%color, pvr_param%colorbar,          &
     &    pvr_param%field, pvr_data%view, pvr_proj%screen,              &
     &    pvr_proj%start_pt, pvr_proj%image, pvr_rgb)
!
      end subroutine rendering_at_once
!
!  ---------------------------------------------------------------------
!
      end module t_rendering_vr_image
