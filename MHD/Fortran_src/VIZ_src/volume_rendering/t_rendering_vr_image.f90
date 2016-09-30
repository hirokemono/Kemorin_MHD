!>@file  t_rendering_vr_image.f90
!!       module t_rendering_vr_image
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine rendering_with_rotation(istep_pvr, node, ele, surf,  &
!!     &          group, pvr_param, pvr_data)
!!      subroutine set_fixed_view_and_image(node, ele, surf, group,     &
!!     &          pvr_param, pvr_data)
!!      subroutine rendering_with_fixed_view(istep_pvr, node, ele, surf,&
!!     &          pvr_param, pvr_data)
!!      subroutine flush_rendering_4_fixed_view(pvr_data)
!!
!!      subroutine rendering_at_once(isel_projection,                   &
!!     &           node, ele, surf, group, pvr_param, pvr_data)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(mesh_groups), intent(in) :: group
!!        type(PVR_control_params), intent(in) :: pvr_param
!!        type(PVR_image_generator), intent(inout) :: pvr_data
!!@endverbatim
!
      module t_rendering_vr_image
!
      use m_precision
      use m_machine_parameter
      use m_constants
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
!>      Structure of PVR control parameters
      type PVR_control_params
!>        Parameters for output files
        type(pvr_output_parameter) :: file
!>        Parameters for image pixels
        type(pvr_pixel_position_type) :: pixel
!>        Structure for field parameter for PVR
        type(pvr_field_parameter) :: field_def
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
!>        Domain boundary information
        type(pvr_bounds_surf_ctl) :: bound
!>        Data on screen oordinate
        type(pvr_projected_data) :: screen
!>        Start point structure for volume rendering
        type(pvr_ray_start_type) :: start_pt
!
!>        Pixel data structure for volume rendering
        type(pvr_image_type) :: rgb
!
!>        Pixel data structure for volume rendering
        type(pvr_segmented_img) :: image
!>        Pixel data structure for volume rendering
        type(pvr_segmented_img) :: image_2
!
!>        Stored start point structure for volume rendering
        type(pvr_ray_start_type) :: start_pt_1
!>        Stored start point structure for volume rendering
        type(pvr_ray_start_type) :: start_pt_2
!
!>        Data on screen oordinate
        type(pvr_projected_data) :: screen_1
!>        Data on screen oordinate
        type(pvr_projected_data) :: screen_2
      end type PVR_image_generator
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine rendering_with_rotation(istep_pvr, node, ele, surf,    &
     &          group, pvr_param, pvr_data)
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
      type(PVR_image_generator), intent(inout) :: pvr_data
!
!
      integer(kind = kint) :: i_rot, ist_rot, ied_rot
!
      ist_rot = pvr_data%view%istart_rot
      ied_rot = pvr_data%view%iend_rot
      do i_rot = ist_rot, ied_rot
        call cal_pvr_modelview_matrix                                   &
     &     (i_rot, pvr_param%outline, pvr_data%view, pvr_data%color,    &
     &      pvr_data%screen)
!
        call rendering_at_once(IFLAG_NORMAL, node, ele, surf, group,    &
     &      pvr_param, pvr_data)
!
        if(iflag_debug .gt. 0) write(*,*) 'sel_write_pvr_image_file'
        call sel_write_pvr_image_file                                   &
     &   (pvr_param%file, i_rot, istep_pvr, IFLAG_NORMAL, pvr_data%rgb)
!
        call dealloc_pvr_local_subimage(pvr_data%image)
        call deallocate_pvr_ray_start(pvr_data%start_pt)
      end do
!
      end subroutine rendering_with_rotation
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_fixed_view_and_image(node, ele, surf, group,       &
     &          pvr_param, pvr_data)
!
      use cal_pvr_modelview_mat
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(mesh_groups), intent(in) :: group
      type(PVR_control_params), intent(in) :: pvr_param
!
      type(PVR_image_generator), intent(inout) :: pvr_data
!
!
      call cal_pvr_modelview_matrix                                     &
     &   (izero, pvr_param%outline, pvr_data%view, pvr_data%color,      &
     &    pvr_data%screen)
!
      call transfer_to_screen(IFLAG_NORMAL,                             &
     &    node, ele, surf, group%surf_grp, group%surf_grp_geom,         &
     &    pvr_param%field, pvr_data%view, pvr_param%pixel,              &
     &    pvr_data%bound, pvr_data%screen, pvr_data%start_pt)
!
      call set_subimages(pvr_data%rgb%num_pixel_xy,                     &
     &    pvr_data%start_pt, pvr_data%image)
!
      call allocate_item_pvr_ray_start                                  &
     &   (pvr_data%start_pt%num_pvr_ray, pvr_data%start_pt_1)
      call copy_item_pvr_ray_start                                      &
     &   (pvr_data%start_pt, pvr_data%start_pt_1)
!
      end subroutine set_fixed_view_and_image
!
!  ---------------------------------------------------------------------
!
      subroutine rendering_with_fixed_view(istep_pvr, node, ele, surf,  &
     &          pvr_param, pvr_data)
!
      use composite_pvr_images
      use set_pvr_ray_start_point
      use write_PVR_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(PVR_control_params), intent(in) :: pvr_param
!
      type(PVR_image_generator), intent(inout) :: pvr_data
!
!
      call copy_item_pvr_ray_start                                      &
     &   (pvr_data%start_pt_1, pvr_data%start_pt)
!
      if(iflag_debug .gt. 0) write(*,*) 'rendering_image'
      call rendering_image                                              &
     &   (node, ele, surf, pvr_data%color, pvr_param%colorbar,          &
     &    pvr_param%field, pvr_data%screen, pvr_data%start_pt,          &
     &    pvr_data%image, pvr_data%rgb)
!
      if(iflag_debug .gt. 0) write(*,*) 'sel_write_pvr_image_file'
      call sel_write_pvr_image_file                                     &
     &   (pvr_param%file, iminus, istep_pvr, IFLAG_NORMAL, pvr_data%rgb)
!
      if(pvr_param%file%iflag_monitoring .gt. 0) then
        call sel_write_pvr_image_file                                   &
     &   (pvr_param%file, iminus, iminus, IFLAG_NORMAL, pvr_data%rgb)
      end if
!
      end subroutine rendering_with_fixed_view
!
!  ---------------------------------------------------------------------
!
      subroutine flush_rendering_4_fixed_view(pvr_data)
!
!
      type(PVR_image_generator), intent(inout) :: pvr_data
!
      call dealloc_pvr_local_subimage(pvr_data%image)
      call deallocate_pvr_ray_start(pvr_data%start_pt)
      call deallocate_item_pvr_ray_start(pvr_data%start_pt_1)
!
      end subroutine flush_rendering_4_fixed_view
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine rendering_at_once(isel_projection,                     &
     &           node, ele, surf, group, pvr_param, pvr_data)
!
      use cal_pvr_modelview_mat
      use composite_pvr_images
      use write_PVR_image
!
      integer(kind = kint) :: isel_projection
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(mesh_groups), intent(in) :: group
      type(PVR_control_params), intent(in) :: pvr_param
!
      type(PVR_image_generator), intent(inout) :: pvr_data
!
!
      call transfer_to_screen(isel_projection,                          &
     &      node, ele, surf, group%surf_grp, group%surf_grp_geom,       &
     &      pvr_param%field, pvr_data%view, pvr_param%pixel,            &
     &      pvr_data%bound, pvr_data%screen, pvr_data%start_pt)
      call set_subimages(pvr_data%rgb%num_pixel_xy,                     &
     &      pvr_data%start_pt, pvr_data%image)
!
      if(iflag_debug .gt. 0) write(*,*) 'rendering_image'
      call rendering_image                                              &
     &     (node, ele, surf, pvr_data%color, pvr_param%colorbar,        &
     &      pvr_param%field, pvr_data%screen, pvr_data%start_pt,        &
     &      pvr_data%image, pvr_data%rgb)
!
      end subroutine rendering_at_once
!
!  ---------------------------------------------------------------------
!
      end module t_rendering_vr_image
