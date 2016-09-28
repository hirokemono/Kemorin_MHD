!>@file  generate_vr_image.f90
!!       module generate_vr_image
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine transfer_to_screen                                   &
!!     &        (node, ele, surf, surf_grp, surf_grp_v,                 &
!!     &         field_pvr, view_param, pvr_bound,  pixel_xy, pvr_start)
!!      subroutine rendering_image(i_rot, istep_pvr, node, ele, surf,   &
!!     &          file_param, color_param, cbar_param, view_param,      &
!!     &          field_pvr, pvr_start, pvr_img)
!!      subroutine set_subimages(pvr_start, pvr_img)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: surf_grp
!!        type (surface_node_grp_data), intent(in)  :: surf_nod_grp
!!        type(pvr_output_parameter), intent(in) :: file_param
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        type(pvr_colorbar_parameter), intent(in) :: cbar_param
!!        type(pvr_view_parameter), intent(inout) :: view_param
!!        type(pvr_projected_field), intent(inout) :: field_pvr
!!        type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
!!        type(pvr_pixel_position_type), intent(inout) :: pixel_xy
!!        type(pvr_ray_start_type), intent(inout) :: pvr_start
!!        type(pvr_image_type), intent(inout) :: pvr_img
!!@endverbatim
!
      module generate_vr_image
!
      use m_precision
      use m_machine_parameter
      use m_constants
!
      use calypso_mpi
!
      use t_control_params_4_pvr
      use t_geometries_in_pvr_screen
      use t_surf_grp_4_pvr_domain
      use t_pvr_ray_startpoints
      use t_pvr_image_array
!
      implicit  none
!
      private :: s_set_pvr_ray_start_point
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine transfer_to_screen                                     &
     &        (node, ele, surf, surf_grp, surf_grp_v,                   &
     &         field_pvr, view_param, pvr_bound,  pixel_xy, pvr_start)
!
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_surface_group_geometry
      use set_position_pvr_screen
      use find_pvr_surf_domain
      use pvr_surface_enhancement
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: surf_grp
      type(surface_group_geometry), intent(in) :: surf_grp_v
      type(pvr_pixel_position_type), intent(in) :: pixel_xy
!
      type(pvr_view_parameter), intent(inout) :: view_param
      type(pvr_projected_field), intent(inout) :: field_pvr
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
      type(pvr_ray_start_type), intent(inout) :: pvr_start
!
!
      call cal_position_pvr_modelview(view_param%modelview_mat,         &
     &    node%numnod, node%xx, pvr_start%x_nod_model)
!
      call norm_on_model_pvr_domains                                    &
     &   (node%numnod, ele%numele, surf%numsurf, surf%nnod_4_surf,      &
     &    surf%ie_surf, surf%isf_4_ele, pvr_start%x_nod_model,          &
     &    pvr_bound%num_pvr_surf, pvr_bound%item_pvr_surf,              &
     &    pvr_bound%screen_norm)
!
!
      call overwte_position_pvr_screen(view_param%projection_mat,       &
     &    node%numnod, pvr_start%x_nod_model)
!
      call set_pvr_domain_surface_data(view_param%n_pvr_pixel,          &
     &    node%numnod, ele%numele, surf%numsurf, surf%nnod_4_surf,      &
     &    surf%ie_surf, surf%isf_4_ele, pvr_start%x_nod_model,          &
     &    pvr_bound)
!
      call set_opacity_for_boundaries                                   &
     &   (surf_grp, surf_grp_v, view_param,                             &
     &    field_pvr%iflag_enhanse, field_pvr%enhansed_opacity,          &
     &    ele%numele, surf%numsurf, surf%isf_4_ele,                     &
     &    field_pvr%arccos_sf)
!
      if(iflag_debug .gt. 0) write(*,*) 's_set_pvr_ray_start_point'
      call s_set_pvr_ray_start_point(node, ele, surf,                   &
     &    view_param%viewpoint_vec, pvr_bound, field_pvr, pixel_xy,     &
     &    pvr_start)
!
      end subroutine transfer_to_screen
!
!  ---------------------------------------------------------------------
!
      subroutine rendering_image(i_rot, istep_pvr, node, ele, surf,     &
     &          file_param, color_param, cbar_param, view_param,        &
     &          field_pvr, pvr_start, pvr_img)
!
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use composite_pvr_images
      use set_pvr_ray_start_point
      use write_PVR_image
!
      integer(kind = kint), intent(in) :: i_rot, istep_pvr
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
!
      type(pvr_output_parameter), intent(in) :: file_param
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(pvr_colorbar_parameter), intent(in) :: cbar_param
!
      type(pvr_view_parameter), intent(in) :: view_param
      type(pvr_projected_field), intent(in) :: field_pvr
!
      type(pvr_ray_start_type), intent(inout) :: pvr_start
      type(pvr_image_type), intent(inout) :: pvr_img
!
!
      if(iflag_debug .gt. 0) write(*,*) 'const_image_over_domains'
      call const_image_over_domains(node, ele, surf,                    &
     &    pvr_start%x_nod_model, view_param%viewpoint_vec,              &
     &    field_pvr, color_param, cbar_param, pvr_start, pvr_img)
!
      if(iflag_debug .gt. 0) write(*,*) 'sel_write_pvr_image_file'
      call sel_write_pvr_image_file                                     &
     &   (file_param, i_rot, istep_pvr, pvr_img)
!
      if(file_param%iflag_monitoring .gt. 0) then
        call sel_write_pvr_image_file                                   &
     &     (file_param, izero, iminus, pvr_img)
      end if
!
      end subroutine rendering_image
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine s_set_pvr_ray_start_point                              &
     &         (node, ele, surf, viewpoint_vec, pvr_bound,              &
     &          field_pvr, pixel_xy, pvr_start)
!
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
      use set_pvr_ray_start_point
      use cal_field_on_surf_viz
!
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
!
      type(pvr_bounds_surf_ctl), intent(in) :: pvr_bound
      type(pvr_projected_field), intent(in) :: field_pvr
      type(pvr_pixel_position_type), intent(in) :: pixel_xy
      type(pvr_ray_start_type), intent(inout) :: pvr_start
!
!
      call allocate_num_pvr_ray_start                                   &
     &     (pvr_bound%num_pvr_surf, pvr_start)
!
      call count_temporal_pvr_ray_start                                 &
     &   (pvr_bound%num_pvr_surf, pvr_bound%screen_norm,                &
     &    pvr_bound%isurf_xrng, pvr_bound%jsurf_yrng, ray_vec,          &
     &    pvr_start%ntot_tmp_pvr_ray, pvr_start%istack_tmp_pvr_ray_st)
!
      call allocate_tmp_pvr_ray_start(pvr_start)
!
      call count_each_pvr_ray_start                                     &
     &   (node%numnod, ele%numele, surf%numsurf, surf%nnod_4_surf,      &
     &    surf%ie_surf, surf%isf_4_ele, pvr_start%x_nod_model,          &
     &    pixel_xy%num_pixel_x, pixel_xy%num_pixel_y,                   &
     &    pixel_xy%pixel_point_x, pixel_xy%pixel_point_y,               &
     &    pvr_bound%num_pvr_surf, pvr_bound%item_pvr_surf,              &
     &    pvr_bound%screen_norm, pvr_bound%isurf_xrng,                  &
     &    pvr_bound%jsurf_yrng, ray_vec,                                &
     &    pvr_start%num_pvr_ray, pvr_start%istack_pvr_ray_sf,           &
     &    pvr_start%ntot_tmp_pvr_ray, pvr_start%istack_tmp_pvr_ray_st,  &
     &    pvr_start%ipix_start_tmp, pvr_start%iflag_start_tmp,          &
     &    pvr_start%xi_start_tmp)
!
      call allocate_item_pvr_ray_start(pvr_start)
      call allocate_item_pvr_ray_pixels(pvr_start)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_each_pvr_ray_start'
      call set_each_pvr_ray_start                                       &
     &   (node%numnod, ele%numele, surf%numsurf, surf%nnod_4_surf,      &
     &    node%xx, surf%ie_surf, surf%isf_4_ele, pvr_start%x_nod_model, &
     &    pixel_xy%num_pixel_x, pixel_xy%num_pixel_y,                   &
     &    pixel_xy%pixel_point_x, pixel_xy%pixel_point_y,               &
     &    pvr_bound%num_pvr_surf, pvr_bound%item_pvr_surf,              &
     &    pvr_bound%screen_norm, viewpoint_vec, ray_vec,                &
     &    pvr_start%ntot_tmp_pvr_ray, pvr_start%istack_tmp_pvr_ray_st,  &
     &    pvr_start%ipix_start_tmp, pvr_start%iflag_start_tmp,          &
     &    pvr_start%xi_start_tmp, pvr_start%istack_pvr_ray_sf,          &
     &    pvr_start%num_pvr_ray, pvr_start%id_pixel_start,              &
     &    pvr_start%icount_pvr_trace, pvr_start%isf_pvr_ray_start,      &
     &    pvr_start%xi_pvr_start, pvr_start%xx_pvr_start,               &
     &    pvr_start%xx_pvr_ray_start, pvr_start%pvr_ray_dir)
!
      end subroutine s_set_pvr_ray_start_point
!
!  ---------------------------------------------------------------------
!
      subroutine set_subimages(pvr_start, pvr_img)
!
      use ray_trace_4_each_image
      use composite_pvr_images
      use PVR_image_transfer
!
      type(pvr_ray_start_type), intent(inout) :: pvr_start
      type(pvr_image_type), intent(inout) :: pvr_img
!
!
      if(iflag_debug .gt. 0) write(*,*) 'cont_overlap_in_each_domain'
      call cont_overlap_in_each_domain(pvr_start%num_pvr_ray,           &
     &    pvr_start%id_pixel_start,  pvr_img%num_pixel_xy,              &
     &    pvr_img%iflag_img_pe, pvr_img%iflag_mapped,                   &
     &    pvr_img%num_overlap)
!
      call count_pixel_with_image                                       &
     &   (pvr_img%num_pixel_xy, pvr_img%npixel_img,                     &
     &    pvr_img%iflag_img_pe, pvr_img%iflag_mapped)
!
      call alloc_pvr_local_subimage(pvr_img)
!
      call share_num_images_to_compose(pvr_img%num_overlap,             &
     &    pvr_img%istack_overlap, pvr_img%ntot_overlap)
!
      call count_pixel_for_composit(pvr_img%num_pixel_xy,               &
     &    pvr_img%npixel_img, pvr_img%npixel_img_local,                 &
     &    pvr_img%istack_pixel, pvr_img%ipixel_small,                   &
     &    pvr_img%iflag_img_pe)
!
      call alloc_pvr_subimage_array(pvr_img)
!
      call cal_image_pixel_depth(pvr_start%num_pvr_ray,                 &
     &    pvr_start%id_pixel_start, pvr_start%xx_pvr_ray_start,         &
     &    pvr_img%num_overlap, pvr_img%num_pixel_xy,                    &
     &    pvr_img%npixel_img, pvr_img%iflag_img_pe,                     &
     &    pvr_img%iflag_mapped, pvr_img%iflag_img_lc, pvr_img%depth_lc)
!
      if(iflag_debug .gt. 0) write(*,*) 'distribute_pixel_depth'
      call distribute_pixel_depth                                       &
     &   (pvr_img%num_overlap, pvr_img%istack_overlap,                  &
     &    pvr_img%ntot_overlap, pvr_img%npixel_img,                     &
     &    pvr_img%istack_pixel, pvr_img%npixel_img_local,               &
     &    pvr_img%depth_lc, pvr_img%depth_recv, pvr_img%depth_part,     &
     &    pvr_img%COMM)
!
      if(iflag_debug .gt. 0) write(*,*) 'sort_subimage_pixel_depth'
      call sort_subimage_pixel_depth                                    &
     &   (pvr_img%ntot_overlap, pvr_img%npixel_img_local,               &
     &    pvr_img%depth_part, pvr_img%ip_closer)
!
      end subroutine set_subimages
!
!  ---------------------------------------------------------------------
!
      end module generate_vr_image
