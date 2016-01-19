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
!!     &         (numele, numsurf, nnod_4_surf, ie_surf, isf_4_ele,     &
!!     &          proj, field_pvr, view_param, pvr_bound)
!!      subroutine rendering_image                                      &
!!     &       (i_rot, istep_pvr, numnod, numele, numsurf, nnod_4_surf, &
!!     &       interior_ele, xx, ie_surf, isf_4_ele, iele_4_surf,       &
!!     &       file_param, color_param, cbar_param, view_param,         &
!!     &       field_pvr, pixel_xy, pvr_bound, pvr_start, pvr_img)
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
      implicit  none
!
      private :: s_set_pvr_ray_start_point, ray_trace_local
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine transfer_to_screen(numnod, numele, numsurf,            &
     &          nnod_4_surf, xx, ie_surf, isf_4_ele,                    &
     &          field_pvr, view_param, pvr_bound, pixel_xy, pvr_start)
!
      use m_geometry_constants
      use t_control_params_4_pvr
      use t_surf_grp_4_pvr_domain
      use t_geometries_in_pvr_screen
      use t_pvr_ray_startpoints
      use set_position_pvr_screen
      use find_pvr_surf_domain
!
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
!
      type(pvr_view_parameter), intent(inout) :: view_param
      type(pvr_projected_field), intent(inout) :: field_pvr
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
      type(pvr_pixel_position_type), intent(inout) :: pixel_xy
      type(pvr_ray_start_type), intent(inout) :: pvr_start
!
!
      call cal_position_pvr_modelview(view_param%modelview_mat,         &
     &    numnod, xx, field_pvr%x_nod_model)
      call norm_on_model_pvr_domains(numnod, numele, numsurf,           &
     &    nnod_4_surf, ie_surf, isf_4_ele, field_pvr%x_nod_model,       &
     &    pvr_bound%num_pvr_surf, pvr_bound%item_pvr_surf,              &
     &    pvr_bound%screen_norm)
!
!
      call overwte_position_pvr_screen(view_param%projection_mat,       &
     &    numnod, field_pvr%x_nod_model)
!
      call set_pvr_domain_surface_data(view_param%n_pvr_pixel,          &
     &    numnod, numele, numsurf, nnod_4_surf, ie_surf, isf_4_ele,     &
     &    field_pvr%x_nod_model, pvr_bound)
!
      if(iflag_debug .gt. 0) write(*,*) 's_set_pvr_ray_start_point'
      call s_set_pvr_ray_start_point(numnod, numele, numsurf,           &
     &        nnod_4_surf, ie_surf, isf_4_ele,  pvr_bound,              &
     &        field_pvr, pixel_xy, pvr_start)
!      call check_pvr_ray_startpoints(my_rank, pvr_start)
!
      end subroutine transfer_to_screen
!
!  ---------------------------------------------------------------------
!
      subroutine rendering_image                                        &
     &       (i_rot, istep_pvr, numnod, numele, numsurf, nnod_4_surf,   &
     &       interior_ele, xx, ie_surf, isf_4_ele, iele_4_surf,         &
     &       file_param, color_param, cbar_param, view_param,           &
     &       field_pvr, pixel_xy, pvr_bound, pvr_start, pvr_img)
!
      use m_geometry_constants
      use t_control_params_4_pvr
      use t_surf_grp_4_pvr_domain
      use t_pvr_ray_startpoints
      use t_pvr_image_array
      use t_geometries_in_pvr_screen
      use composite_pvr_images
!
      integer(kind = kint), intent(in) :: i_rot, istep_pvr
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: iele_4_surf(numsurf,2,2)
      integer(kind = kint), intent(in) :: interior_ele(numele)
      real(kind = kreal), intent(in)  :: xx(numnod,3)
!
      type(pvr_output_parameter), intent(in) :: file_param
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(pvr_colorbar_parameter), intent(in) :: cbar_param
!
      type(pvr_view_parameter), intent(in) :: view_param
      type(pvr_projected_field), intent(in) :: field_pvr
      type(pvr_pixel_position_type), intent(in) :: pixel_xy
      type(pvr_bounds_surf_ctl), intent(in) :: pvr_bound
!
      type(pvr_ray_start_type), intent(inout) :: pvr_start
      type(pvr_image_type), intent(inout) :: pvr_img
!
!
      if(iflag_debug .gt. 0) write(*,*) 'ray_trace_local'
      call ray_trace_local(numnod, numele, numsurf, nnod_4_surf,        &
     &       ie_surf, isf_4_ele, iele_4_surf, interior_ele, xx,         &
     &       view_param%viewpoint_vec, field_pvr%x_nod_model,           &
     &       field_pvr, color_param, pixel_xy, pvr_bound,               &
     &       pvr_start, pvr_img)
!
!      call sel_write_pvr_local_img(file_param, pvr_img)
!
      if(iflag_debug .gt. 0) write(*,*) 'blend_image_over_domains'
      call blend_image_over_domains                                     &
     &   (color_param, cbar_param, pvr_img)
!
      if(iflag_debug .gt. 0) write(*,*) 'sel_write_pvr_image_file'
      call sel_write_pvr_image_file                                     &
     &   (file_param, i_rot, istep_pvr, pvr_img)
!
      end subroutine rendering_image
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine s_set_pvr_ray_start_point(numnod, numele, numsurf,     &
     &          nnod_4_surf, ie_surf, isf_4_ele,                        &
     &          pvr_bound, field_pvr, pixel_xy, pvr_start)
!
      use t_surf_grp_4_pvr_domain
      use t_pvr_ray_startpoints
      use t_geometries_in_pvr_screen
      use set_pvr_ray_start_point
      use cal_field_on_surf_viz
!
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
!
      type(pvr_bounds_surf_ctl), intent(in) :: pvr_bound
      type(pvr_projected_field), intent(in) :: field_pvr
      type(pvr_ray_start_type), intent(inout) :: pvr_start
      type(pvr_pixel_position_type), intent(inout) :: pixel_xy
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
!
      end subroutine s_set_pvr_ray_start_point
!
!  ---------------------------------------------------------------------
!
      subroutine ray_trace_local(numnod, numele, numsurf, nnod_4_surf,  &
     &    ie_surf, isf_4_ele, iele_4_surf, interior_ele, xx,            &
     &    viewpoint_vec, x_nod_screen, field_pvr, color_param,          &
     &    pixel_xy, pvr_bound, pvr_start, pvr_img)
!
      use t_control_params_4_pvr
      use t_surf_grp_4_pvr_domain
      use t_pvr_image_array
      use t_pvr_ray_startpoints
      use t_geometries_in_pvr_screen
      use ray_trace_4_each_image
      use composite_pvr_images
      use set_pvr_ray_start_point
!
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: iele_4_surf(numsurf,2,2)
      integer(kind = kint), intent(in) :: interior_ele(numele)
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
!
      real(kind = kreal), intent(in) :: x_nod_screen(numnod,4)
      type(pvr_projected_field), intent(in) :: field_pvr
      type(pvr_pixel_position_type), intent(in) :: pixel_xy
      type(pvr_bounds_surf_ctl), intent(in) :: pvr_bound
!
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(pvr_ray_start_type), intent(inout) :: pvr_start
      type(pvr_image_type), intent(inout) :: pvr_img
!
!
      if(iflag_debug .gt. 0) write(*,*) 'set_each_pvr_ray_start'
      call set_each_pvr_ray_start(numnod, numele, numsurf,              &
     &    nnod_4_surf, xx, ie_surf, isf_4_ele, field_pvr%x_nod_model,   &
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
      if(iflag_debug .gt. 0) write(*,*) 's_ray_trace_4_each_image'
      call s_ray_trace_4_each_image                                     &
     &   (numnod, numele, numsurf, nnod_4_surf, ie_surf,                &
     &    isf_4_ele, iele_4_surf, interior_ele, xx,                     &
     &    field_pvr%iflag_used_ele, x_nod_screen, field_pvr%d_pvr,      &
     &    field_pvr%grad_ele, viewpoint_vec, color_param, ray_vec,      &
     &    pvr_start%num_pvr_ray, pvr_start%icount_pvr_trace,            &
     &    pvr_start%isf_pvr_ray_start, pvr_start%xi_pvr_start,          &
     &    pvr_start%xx_pvr_start, pvr_start%xx_pvr_ray_start,           &
     &    pvr_start%rgba_ray)
!
      if(iflag_debug .gt. 0) write(*,*) 'blend_overlapped_area'
      call blend_overlapped_area(pvr_start%num_pvr_ray,                 &
     &    pvr_start%id_pixel_start, pvr_start%xx_pvr_ray_start,         &
     &    pvr_start%rgba_ray, pvr_img%num_pixel_xy,                     &
     &    pvr_img%iflag_mapped, pvr_img%rgba_lc, pvr_img%depth_lc)
!
      end subroutine ray_trace_local
!
!  ---------------------------------------------------------------------
!
      end module generate_vr_image
