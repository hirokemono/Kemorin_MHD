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
!!     &         draw_param, view_param, projection_mat, pixel_xy,      &
!!     &         pvr_bound, pvr_screen, pvr_start)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: surf_grp
!!        type (surface_node_grp_data), intent(in)  :: surf_nod_grp
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        type(pvr_colorbar_parameter), intent(in) :: cbar_param
!!        type(rendering_parameter), intent(in) :: draw_param
!!        type(pvr_view_parameter), intent(inout) :: view_param
!!        type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
!!        type(pvr_pixel_position_type), intent(inout) :: pixel_xy
!!        type(pvr_ray_start_type), intent(inout) :: pvr_start
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
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
     &         draw_param, pixel_xy, n_pvr_pixel,                       &
     &         viewpoint_vec, modelview_mat, projection_mat,            &
     &         pvr_bound, pvr_screen, pvr_start)
!
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_surface_group_normals
      use set_position_pvr_screen
      use find_pvr_surf_domain
      use pvr_axis_label
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: surf_grp
      type(surface_group_normals), intent(in) :: surf_grp_v
      type(pvr_pixel_position_type), intent(in) :: pixel_xy
      type(rendering_parameter), intent(in) :: draw_param
      integer(kind = kint), intent(in) :: n_pvr_pixel(2)
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
      real(kind = kreal), intent(in) :: modelview_mat(4,4)
      real(kind = kreal), intent(in) :: projection_mat(4,4)
!
      type(pvr_projected_position), intent(inout) :: pvr_screen
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
      type(pvr_ray_start_type), intent(inout) :: pvr_start
!
!
      call axis_direction_in_screen                                     &
     &   (modelview_mat, projection_mat, pvr_screen)
!
      call cal_position_pvr_modelview(modelview_mat,                    &
     &    node%numnod, node%xx, pvr_screen%x_nod_model)
!
      call norm_on_model_pvr_domains                                    &
     &   (node, surf, modelview_mat, projection_mat,                    &
     &    pvr_bound%num_pvr_surf, pvr_bound%item_pvr_surf,              &
     &    pvr_bound%screen_norm)
!
!
      call overwte_position_pvr_screen(projection_mat,                  &
     &    node%numnod, pvr_screen%x_nod_model)
!
      call set_pvr_domain_surface_data(n_pvr_pixel, node, surf,         &
     &    modelview_mat, projection_mat, pvr_screen%x_nod_model,        &
     &    pvr_bound)
!
      if(iflag_debug .gt. 0) write(*,*) 's_set_pvr_ray_start_point'
      call s_set_pvr_ray_start_point(node, ele, surf, pvr_bound,        &
     &    pixel_xy, pvr_screen, viewpoint_vec, pvr_start)
!
      end subroutine transfer_to_screen
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine s_set_pvr_ray_start_point(node, ele, surf, pvr_bound,  &
     &          pixel_xy, pvr_screen, viewpoint_vec, pvr_start)
!
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
      use count_pvr_ray_start_point
      use set_pvr_ray_start_point
      use cal_field_on_surf_viz
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
!
      type(pvr_bounds_surf_ctl), intent(in) :: pvr_bound
      type(pvr_pixel_position_type), intent(in) :: pixel_xy
      type(pvr_projected_position), intent(in) :: pvr_screen
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
!
      type(pvr_ray_start_type), intent(inout) :: pvr_start
!
      integer(kind = kint) :: num_ray_local
!
!
      call allocate_num_pvr_ray_start                                   &
     &   (pvr_bound%num_pvr_surf, pvr_start)
!
      call count_temporal_pvr_ray_start                                 &
     &   (pvr_bound%num_pvr_surf, pvr_bound%screen_norm,                &
     &    pvr_bound%isurf_xrng, pvr_bound%jsurf_yrng, ray_vec4,         &
     &    pvr_start%ntot_tmp_pvr_ray, pvr_start%istack_tmp_pvr_ray_st)
!
      call allocate_tmp_pvr_ray_start(pvr_start)
!
      call count_each_pvr_ray_start                                     &
     &   (node%numnod, ele%numele, surf%numsurf, surf%nnod_4_surf,      &
     &    surf%ie_surf, surf%isf_4_ele, pvr_screen%x_nod_model,         &
     &    pixel_xy%num_pixel_x, pixel_xy%num_pixel_y,                   &
     &    pixel_xy%pixel_point_x, pixel_xy%pixel_point_y,               &
     &    pvr_bound%num_pvr_surf, pvr_bound%item_pvr_surf,              &
     &    pvr_bound%screen_norm, pvr_bound%isurf_xrng,                  &
     &    pvr_bound%jsurf_yrng, ray_vec4, num_ray_local,                &
     &    pvr_start%istack_pvr_ray_sf, pvr_start%ntot_tmp_pvr_ray,      &
     &    pvr_start%istack_tmp_pvr_ray_st, pvr_start%ipix_start_tmp,    &
     &    pvr_start%iflag_start_tmp, pvr_start%xi_start_tmp)
!
      call allocate_item_pvr_ray_start(num_ray_local, pvr_start)
      call allocate_item_pvr_ray_pixels(pvr_start)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_each_pvr_ray_start'
      call set_each_pvr_ray_start                                       &
     &  (node%numnod, ele%numele, surf%numsurf, surf%nnod_4_surf,       &
     &   node%xx, surf%ie_surf, surf%isf_4_ele, pvr_screen%x_nod_model, &
     &   pixel_xy%num_pixel_x, pixel_xy%num_pixel_y,                    &
     &   pixel_xy%pixel_point_x, pixel_xy%pixel_point_y,                &
     &   pvr_bound%num_pvr_surf, pvr_bound%item_pvr_surf,               &
     &   pvr_bound%screen_norm, viewpoint_vec, ray_vec4,                &
     &   pvr_start%ntot_tmp_pvr_ray, pvr_start%istack_tmp_pvr_ray_st,   &
     &   pvr_start%ipix_start_tmp, pvr_start%iflag_start_tmp,           &
     &   pvr_start%xi_start_tmp, pvr_start%istack_pvr_ray_sf,           &
     &   pvr_start%num_pvr_ray, pvr_start%id_pixel_start,               &
     &   pvr_start%icount_pvr_trace, pvr_start%isf_pvr_ray_start,       &
     &   pvr_start%xi_pvr_start, pvr_start%xx4_pvr_start,               &
     &   pvr_start%xx4_pvr_ray_start)
!
!      if(iflag_debug .gt. 0) then
!        call check_pvr_ray_startpoint                                  &
!     &     (pixel_xy%num_pixel_x, pixel_xy%num_pixel_y,                &
!     &      pvr_start%num_pvr_ray, pvr_start%id_pixel_start)
!      end if
!       call set_pvr_ray_trace_check                                   &
!     &     (pixel_xy%num_pixel_x, pixel_xy%num_pixel_y,               &
!     &      pvr_start%num_pvr_ray, pvr_start%id_pixel_start,          &
!     &      pvr_start%id_pixel_check)
!
      end subroutine s_set_pvr_ray_start_point
!
!  ---------------------------------------------------------------------
!
      end module generate_vr_image
