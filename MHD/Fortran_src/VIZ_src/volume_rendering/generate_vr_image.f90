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
!!     &        (node, ele, surf, surf_grp, surf_grp_v, surf_nod_grp,   &
!!     &         field_pvr, view_param, pvr_bound,  pixel_xy, pvr_start)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: surf_grp
!!        type (surface_node_grp_data), intent(in)  :: surf_nod_grp
!!        type(pvr_view_parameter), intent(inout) :: view_param
!!        type(pvr_projected_field), intent(inout) :: field_pvr
!!        type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
!!        type(pvr_pixel_position_type), intent(inout) :: pixel_xy
!!        type(pvr_ray_start_type), intent(inout) :: pvr_start
!!      subroutine rendering_image(i_rot, istep_pvr, node, ele, surf,   &
!!     &       file_param, color_param, cbar_param, view_param,         &
!!     &       field_pvr, pixel_xy, pvr_start, pvr_img)
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
!
!>      Structure of PVR image generation
      type PVR_image_generator
!>        Viewer coordinate information
        type(pvr_view_parameter) :: view
!>        color paramter for volume rendering
        type(pvr_colormap_parameter) :: color
!>        Field data for volume rendering
        type(pvr_projected_field) :: field
!>        Domain boundary information
        type(pvr_bounds_surf_ctl) :: bound
!>        Start point structure for volume rendering
        type(pvr_ray_start_type) :: start_pt
!>        Stored start point structure for volume rendering
        type(pvr_ray_start_type) :: start_pt_saved
!
!>        Pixel data structure for volume rendering
        type(pvr_image_type) :: image
      end type PVR_image_generator
!
      private :: s_set_pvr_ray_start_point, ray_trace_local
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_fixed_view_and_image(node, ele, surf, group,       &
     &          outline, pixel_xy, pvr_data)
!
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use cal_pvr_modelview_mat
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(mesh_groups), intent(in) :: group
!
      type(pvr_domain_outline), intent(in) :: outline
      type(pvr_pixel_position_type), intent(in) :: pixel_xy
!
      type(PVR_image_generator), intent(inout) :: pvr_data
!
!
      call cal_pvr_modelview_matrix                                     &
     &   (izero, outline, pvr_data%view, pvr_data%color)
      call transfer_to_screen                                           &
     &   (node, ele, surf, group%surf_grp, group%surf_grp_geom,         &
     &   pvr_data%field, pvr_data%view, pvr_data%bound, pixel_xy,       &
     &   pvr_data%start_pt)
!
      end subroutine set_fixed_view_and_image
!
!  ---------------------------------------------------------------------
!
      subroutine rendering_with_fixed_view(istep_pvr, node, ele, surf,  &
     &       file_param, cbar_param, pixel_xy, pvr_data)
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use composite_pvr_images
      use set_pvr_ray_start_point
!
      integer(kind = kint), intent(in) :: istep_pvr
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
!
      type(pvr_output_parameter), intent(in) :: file_param
      type(pvr_colorbar_parameter), intent(in) :: cbar_param
      type(pvr_pixel_position_type), intent(in) :: pixel_xy
!
      type(PVR_image_generator), intent(inout) :: pvr_data
!
      integer(kind = kint), parameter :: i_rot = -1
!
!
      call rendering_image(i_rot, istep_pvr, node, ele, surf,           &
     &    file_param, pvr_data%color, cbar_param, pvr_data%view,        &
     &    pvr_data%bound, pvr_data%field, pixel_xy,                     &
     &    pvr_data%start_pt, pvr_data%image)
!
      call dealloc_pvr_local_subimage(pvr_data%image)
!
      end subroutine rendering_with_fixed_view
!
!  ---------------------------------------------------------------------
!
      subroutine rendering_with_rotation                                &
     &         (istep_pvr, node, ele, surf, group,                      &
     &          file_param, outline, cbar_param, pixel_xy, pvr_data)
!
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use cal_pvr_modelview_mat
!
      integer(kind = kint), intent(in) :: istep_pvr
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(mesh_groups), intent(in) :: group
!
      type(pvr_output_parameter), intent(in) :: file_param
      type(pvr_domain_outline), intent(in) :: outline
      type(pvr_colorbar_parameter), intent(in) :: cbar_param
      type(pvr_pixel_position_type), intent(in) :: pixel_xy
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
     &     (i_rot, outline, pvr_data%view, pvr_data%color)
        call transfer_to_screen                                         &
     &     (node, ele, surf, group%surf_grp, group%surf_grp_geom,       &
     &      pvr_data%field, pvr_data%view, pvr_data%bound, pixel_xy,    &
     &      pvr_data%start_pt)
!
        call rendering_image(i_rot, istep_pvr, node, ele, surf,         &
     &      file_param, pvr_data%color, cbar_param, pvr_data%view,      &
     &      pvr_data%bound, pvr_data%field, pixel_xy,                   &
     &      pvr_data%start_pt, pvr_data%image)
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
     &    node%numnod, node%xx, field_pvr%x_nod_model)
!
      call norm_on_model_pvr_domains                                    &
     &   (node%numnod, ele%numele, surf%numsurf, surf%nnod_4_surf,      &
     &    surf%ie_surf, surf%isf_4_ele, field_pvr%x_nod_model,          &
     &    pvr_bound%num_pvr_surf, pvr_bound%item_pvr_surf,              &
     &    pvr_bound%screen_norm)
!
!
      call overwte_position_pvr_screen(view_param%projection_mat,       &
     &    node%numnod, field_pvr%x_nod_model)
!
      call set_pvr_domain_surface_data(view_param%n_pvr_pixel,          &
     &    node%numnod, ele%numele, surf%numsurf, surf%nnod_4_surf,      &
     &    surf%ie_surf, surf%isf_4_ele, field_pvr%x_nod_model,          &
     &    pvr_bound)
!
      call set_opacity_for_boundaries                                   &
     &   (surf_grp, surf_grp_v, view_param,                             &
     &    field_pvr%iflag_enhanse, field_pvr%enhansed_opacity,          &
     &    ele%numele, surf%numsurf, surf%isf_4_ele,                     &
     &    field_pvr%arccos_sf)
!
      if(iflag_debug .gt. 0) write(*,*) 's_set_pvr_ray_start_point'
      call s_set_pvr_ray_start_point                                    &
     &   (node%numnod, ele%numele, surf%numsurf, surf%nnod_4_surf,      &
     &    surf%ie_surf, surf%isf_4_ele, node%xx,                        &
     &    view_param%viewpoint_vec, pvr_bound, field_pvr, pixel_xy,     &
     &    pvr_start)
!
      end subroutine transfer_to_screen
!
!  ---------------------------------------------------------------------
!
      subroutine rendering_image(i_rot, istep_pvr, node, ele, surf,     &
     &       file_param, color_param, cbar_param, view_param,           &
     &       pvr_bound, field_pvr, pixel_xy, pvr_start, pvr_img)
!
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use composite_pvr_images
      use set_pvr_ray_start_point
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
      type(pvr_bounds_surf_ctl), intent(in) :: pvr_bound
      type(pvr_projected_field), intent(in) :: field_pvr
      type(pvr_pixel_position_type), intent(in) :: pixel_xy
!
      type(pvr_ray_start_type), intent(inout) :: pvr_start
      type(pvr_image_type), intent(inout) :: pvr_img
!
!      integer(kind = kint) :: i, j, k, ipix
!
!
      if(iflag_debug .gt. 0) write(*,*) 'set_each_pvr_ray_start'
      call set_each_pvr_ray_start                                       &
     &   (node%numnod, ele%numele, surf%numsurf, surf%nnod_4_surf,      &
     &    node%xx, surf%ie_surf, surf%isf_4_ele, field_pvr%x_nod_model, &
     &    pixel_xy%num_pixel_x, pixel_xy%num_pixel_y,                   &
     &    pixel_xy%pixel_point_x, pixel_xy%pixel_point_y,               &
     &    pvr_bound%num_pvr_surf, pvr_bound%item_pvr_surf,              &
     &    pvr_bound%screen_norm, view_param%viewpoint_vec, ray_vec,     &
     &    pvr_start%ntot_tmp_pvr_ray, pvr_start%istack_tmp_pvr_ray_st,  &
     &    pvr_start%ipix_start_tmp, pvr_start%iflag_start_tmp,          &
     &    pvr_start%xi_start_tmp, pvr_start%istack_pvr_ray_sf,          &
     &    pvr_start%num_pvr_ray, pvr_start%id_pixel_start,              &
     &    pvr_start%icount_pvr_trace, pvr_start%isf_pvr_ray_start,      &
     &    pvr_start%xi_pvr_start, pvr_start%xx_pvr_start,               &
     &    pvr_start%xx_pvr_ray_start, pvr_start%pvr_ray_dir)
!
      call set_subimages(pvr_start, pvr_img)
!
      if(iflag_debug .gt. 0) write(*,*) 'ray_trace_local'
      call ray_trace_local(node, ele, surf, view_param%viewpoint_vec,   &
     &    field_pvr, color_param, pvr_start, pvr_img)
!
!      do i = 1, pvr_img%num_overlap
!        j = pvr_img%istack_overlap(my_rank) + i
!        do k = 1, pvr_img%npixel_img
!          ipix = pvr_img%ipixel_small(k)
!          pvr_img%old_rgba_lc(1:4,ipix) = pvr_img%rgba_lc(1:4,j,k)
!        end do
!        call sel_write_pvr_local_img(file_param, j, istep_pvr, pvr_img)
!      end do
!
      if(iflag_debug .gt. 0) write(*,*) 'blend_image_over_domains'
      call blend_image_over_domains                                     &
     &   (color_param, cbar_param, pvr_img)
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
      subroutine s_set_pvr_ray_start_point(numnod, numele, numsurf,     &
     &          nnod_4_surf, ie_surf, isf_4_ele, xx, viewpoint_vec,     &
     &          pvr_bound, field_pvr, pixel_xy, pvr_start)
!
      use set_pvr_ray_start_point
      use cal_field_on_surf_viz
!
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
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
      call count_each_pvr_ray_start(numnod, numele, numsurf,            &
     &    nnod_4_surf, ie_surf, isf_4_ele, field_pvr%x_nod_model,       &
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
      end subroutine s_set_pvr_ray_start_point
!
!  ---------------------------------------------------------------------
!
      subroutine ray_trace_local(node, ele, surf, viewpoint_vec,        &
     &          field_pvr, color_param, pvr_start, pvr_img)
!
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
      use ray_trace_4_each_image
      use composite_pvr_images
      use set_pvr_ray_start_point
!
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(pvr_projected_field), intent(in) :: field_pvr
!
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(pvr_ray_start_type), intent(inout) :: pvr_start
      type(pvr_image_type), intent(inout) :: pvr_img
!
!
      if(iflag_debug .gt. 0) write(*,*) 's_ray_trace_4_each_image'
      call s_ray_trace_4_each_image                                     &
     &   (node%numnod, ele%numele, surf%numsurf, surf%nnod_4_surf,      &
     &    surf%ie_surf, surf%isf_4_ele, surf%iele_4_surf,               &
     &    ele%interior_ele, node%xx, surf%vnorm_surf,                   &
     &    field_pvr%x_nod_model, viewpoint_vec,                         &
     &    field_pvr, color_param, ray_vec,                              &
     &    pvr_start%num_pvr_ray, pvr_start%icount_pvr_trace,            &
     &    pvr_start%isf_pvr_ray_start, pvr_start%xi_pvr_start,          &
     &    pvr_start%xx_pvr_start, pvr_start%xx_pvr_ray_start,           &
     &    pvr_start%rgba_ray)
!
      if(iflag_debug .gt. 0) write(*,*) 'copy_segmented_image'
      call copy_segmented_image(pvr_start%num_pvr_ray,                  &
     &    pvr_start%id_pixel_start, pvr_start%rgba_ray,                 &
     &    pvr_img%num_overlap, pvr_img%num_pixel_xy,                    &
     &    pvr_img%npixel_img, pvr_img%iflag_img_pe,                     &
     &    pvr_img%iflag_mapped, pvr_img%rgba_lc)
!
      end subroutine ray_trace_local
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
