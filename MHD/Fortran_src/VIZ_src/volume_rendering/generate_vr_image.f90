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
!!     &      (i_pvr, i_rot, istep_pvr, numnod, numele, numsurf,        &
!!     &      nnod_4_surf, e_multi, xx, ie_surf, isf_4_ele, iele_4_surf,&
!!     &      proj, file_param, color_param, cbar_param, view_param,    &
!!     &      field_pvr, pvr_bound, pixel_xy, pvr_start, pvr_img)
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
      private :: write_pvr_image_file
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine transfer_to_screen                                     &
     &         (numele, numsurf, nnod_4_surf, ie_surf, isf_4_ele,       &
     &          proj, field_pvr, view_param, pvr_bound)
!
      use m_geometry_constants
      use t_control_params_4_pvr
      use t_surf_grp_4_pvr_domain
      use t_geometries_in_pvr_screen
      use set_position_pvr_screen
      use find_pvr_surf_domain
!
      integer(kind = kint), intent(in) :: numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
!
      type(pvr_view_parameter), intent(inout) :: view_param
      type(pvr_projected_type), intent(inout) :: proj
      type(pvr_projected_field), intent(inout) :: field_pvr
      type(pvr_bounds_surf_ctl), intent(inout) :: pvr_bound
!
!
      call cal_position_pvr_screen                                      &
     &      (view_param%modelview_mat, view_param%projection_mat,       &
     &       proj%nnod_pvr, proj%istack_nod_pvr,  proj%x_nod_sim,       &
     &       field_pvr%x_nod_model, field_pvr%x_nod_screen)
      call position_pvr_domain_on_screen(view_param%modelview_mat,      &
     &       view_param%projection_mat, pvr_bound%num_pvr_surf,         &
     &       pvr_bound%xx_nod, pvr_bound%xx_model, pvr_bound%xx_screen)
!
      call set_pvr_domain_surface_data(view_param%n_pvr_pixel,          &
     &       numele, numsurf, nnod_4_surf, ie_surf, isf_4_ele,          &
     &       proj%nnod_pvr, field_pvr%x_nod_model,                      &
     &       field_pvr%x_nod_screen, pvr_bound)
!
      end subroutine transfer_to_screen
!
!  ---------------------------------------------------------------------
!
      subroutine rendering_image                                        &
     &       (i_pvr, i_rot, istep_pvr, numnod, numele, numsurf,         &
     &       nnod_4_surf, e_multi, xx, ie_surf, isf_4_ele, iele_4_surf, &
     &       proj, file_param, color_param, cbar_param, view_param,     &
     &       field_pvr, pvr_bound, pixel_xy, pvr_start, pvr_img)
!
      use m_geometry_constants
      use t_control_params_4_pvr
      use t_surf_grp_4_pvr_domain
      use t_pvr_ray_startpoints
      use t_pvr_image_array
      use t_geometries_in_pvr_screen
!
      integer(kind = kint), intent(in) :: i_pvr, i_rot, istep_pvr
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: iele_4_surf(numsurf,2,2)
      real(kind = kreal), intent(in)  :: xx(numnod,3)
      real(kind = kreal), intent(in) :: e_multi(numele)
!
      type(pvr_projected_type), intent(in) :: proj
!
      type(pvr_output_parameter), intent(in) :: file_param
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(pvr_colorbar_parameter), intent(in) :: cbar_param
!
      type(pvr_view_parameter), intent(in) :: view_param
      type(pvr_projected_field), intent(in) :: field_pvr
      type(pvr_bounds_surf_ctl), intent(in) :: pvr_bound
!
      type(pvr_pixel_position_type), intent(inout) :: pixel_xy
      type(pvr_ray_start_type), intent(inout) :: pvr_start
      type(pvr_image_type), intent(inout) :: pvr_img
!
!
      call alloc_pvr_image_array_type(view_param%n_pvr_pixel, pvr_img)
!
      if(iflag_debug .gt. 0) write(*,*) 's_set_pvr_ray_start_point'
      call s_set_pvr_ray_start_point(numnod, numele, numsurf,           &
     &        nnod_4_surf, xx, ie_surf, isf_4_ele,                      &
     &        view_param%viewpoint_vec, pvr_bound,                      &
     &        proj, field_pvr, pixel_xy, pvr_start)
!      call check_pvr_ray_startpoints(my_rank, pvr_start)
!
      if(iflag_debug .gt. 0) write(*,*) 's_ray_trace_4_each_image'
      call ray_trace_local(numnod, numele, numsurf,                     &
     &       nnod_4_surf, ie_surf, isf_4_ele, iele_4_surf, e_multi, xx, &
     &       view_param%viewpoint_vec, proj%nnod_pvr, proj%nele_pvr,    &
     &       field_pvr%x_nod_screen, field_pvr, color_param,            &
     &       pvr_start, pvr_img)
!
      if(iflag_debug .gt. 0) write(*,*) 'write_pvr_image_file', i_pvr
      call write_pvr_image_file(file_param, color_param, cbar_param,    &
     &    i_rot, istep_pvr, pvr_img)
!
      call deallocate_pvr_ray_start(pvr_start)
      call dealloc_pvr_image_array_type(pvr_img)
!
      end subroutine rendering_image
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine s_set_pvr_ray_start_point(numnod, numele, numsurf,     &
     &          nnod_4_surf, xx, ie_surf, isf_4_ele, viewpoint_vec,     &
     &          pvr_bound, proj, field_pvr, pixel_xy, pvr_start)
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
      real(kind = kreal), intent(in)  :: xx(numnod,3)
!
      real(kind = kreal), intent(in)  :: viewpoint_vec(3)
!
      type(pvr_bounds_surf_ctl), intent(in) :: pvr_bound
      type(pvr_projected_type), intent(in) :: proj
      type(pvr_projected_field), intent(in) :: field_pvr
      type(pvr_ray_start_type), intent(inout) :: pvr_start
      type(pvr_pixel_position_type), intent(inout) :: pixel_xy
!
      integer(kind = kint) :: inum
!
!
      call allocate_num_pvr_ray_start                                   &
     &   (pvr_bound%num_pvr_surf, pvr_start)
!
      call count_each_pvr_ray_start_point(numele, isf_4_ele,            &
     &    pixel_xy%num_pixel_x, pixel_xy%num_pixel_y,                   &
     &    pixel_xy%pixel_point_x, pixel_xy%pixel_point_y,               &
     &    pvr_bound%num_pvr_surf, pvr_bound%item_pvr_surf,              &
     &    pvr_bound%screen_norm, pvr_bound%xx_screen,                   &
     &    pvr_bound%isurf_xrng, pvr_bound%jsurf_yrng,                   &
     &    ray_vec, pvr_start%num_pvr_ray, pvr_start%istack_pvr_ray_sf)
!
      call allocate_item_pvr_ray_start(pvr_start)
!
!$omp parallel do private (inum)
      do inum = 1, pvr_bound%num_pvr_surf
        call set_each_pvr_ray_start(inum, numnod,                       &
     &    numele, numsurf, nnod_4_surf, xx, ie_surf, isf_4_ele,         &
     &    proj%nnod_pvr, field_pvr%x_nod_screen,                        &
     &    pixel_xy%num_pixel_x, pixel_xy%num_pixel_y,                   &
     &    pixel_xy%pixel_point_x, pixel_xy%pixel_point_y,               &
     &    pvr_bound%num_pvr_surf, pvr_bound%item_pvr_surf,              &
     &    pvr_bound%screen_norm, pvr_bound%xx_screen,                   &
     &    pvr_bound%isurf_xrng, pvr_bound%jsurf_yrng,                   &
     &    viewpoint_vec, ray_vec, pvr_start%istack_pvr_ray_sf,          &
     &    pvr_start%num_pvr_ray, pvr_start%id_pixel_start,              &
     &    pvr_start%icount_pvr_trace, pvr_start%isf_pvr_ray_start,      &
     &    pvr_start%xi_pvr_start, pvr_start%xx_pvr_start,               &
     &    pvr_start%xx_pvr_ray_start, pvr_start%pvr_ray_dir)
      end do
!$omp end parallel do
!
      end subroutine s_set_pvr_ray_start_point
!
!  ---------------------------------------------------------------------
!
      subroutine ray_trace_local(numnod, numele, numsurf,               &
     &    nnod_4_surf, ie_surf, isf_4_ele, iele_4_surf, e_multi, xx,    &
     &    viewpoint_vec, nnod_pvr, nele_pvr, x_nod_screen, field_pvr,   &
     &    color_param, pvr_start, pvr_img)
!
      use t_control_params_4_pvr
      use t_pvr_image_array
      use t_pvr_ray_startpoints
      use t_geometries_in_pvr_screen
      use ray_trace_4_each_image
      use composite_pvr_images
!
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: iele_4_surf(numsurf,2,2)
      real(kind = kreal), intent(in) :: e_multi(numele)
      real(kind = kreal), intent(in) :: xx(numnod,3)
      real(kind = kreal), intent(in) :: viewpoint_vec(3)
!
      integer(kind = kint), intent(in) :: nnod_pvr, nele_pvr
      real(kind = kreal), intent(in) :: x_nod_screen(nnod_pvr,4)
      type(pvr_projected_field), intent(in) :: field_pvr
!
!      type(pvr_output_parameter), intent(in) :: file_param
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(pvr_ray_start_type), intent(inout) :: pvr_start
      type(pvr_image_type), intent(inout) :: pvr_img
!
!
      call s_ray_trace_4_each_image                                     &
     &   (numnod, numele, numsurf, nnod_4_surf,                         &
     &    ie_surf, isf_4_ele, iele_4_surf, e_multi, xx,                 &
     &    nnod_pvr, nele_pvr, field_pvr%iflag_used_ele, x_nod_screen,   &
     &    field_pvr%d_pvr, field_pvr%grad_ele,                          &
     &    viewpoint_vec, color_param, ray_vec,                          &
     &    pvr_start%num_pvr_ray, pvr_start%icount_pvr_trace,            &
     &    pvr_start%isf_pvr_ray_start, pvr_start%xi_pvr_start,          &
     &    pvr_start%xx_pvr_start, pvr_start%xx_pvr_ray_start,           &
     &    pvr_start%rgba_ray)
!
      call blend_overlapped_area(pvr_start%num_pvr_ray,                 &
     &    pvr_start%id_pixel_start, pvr_start%xx_pvr_ray_start,         &
     &    pvr_start%rgba_ray, pvr_img%num_pixel_xy,                     &
     &    pvr_img%iflag_mapped, pvr_img%rgba_lc, pvr_img%depth_lc)
!
!
!      call sel_write_pvr_local_img                                     &
!     &   (file_param, pvr_img%num_pixels, pvr_img%num_pixel_xy,        &
!     &    pvr_img%rgba_lc,  pvr_img%rgb_chara_lc)
!
      end subroutine ray_trace_local
!
!  ---------------------------------------------------------------------
!
      subroutine write_pvr_image_file                                   &
     &         (file_param, color_param, cbar_param, i_rot, istep_pvr,  &
     &          pvr_img)
!
      use t_control_params_4_pvr
      use t_pvr_image_array
      use composite_pvr_images
!
      integer(kind = kint), intent(in) :: i_rot, istep_pvr
!
      type(pvr_output_parameter), intent(in) :: file_param
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(pvr_colorbar_parameter), intent(in) :: cbar_param
!
      type(pvr_image_type), intent(inout) :: pvr_img
!
!
      if(iflag_debug .gt. 0) write(*,*) 'blend_image_over_domains'
      call blend_image_over_domains(color_param, cbar_param,            &
     &    pvr_img%istack_image, pvr_img%num_pixels,                     &
     &    pvr_img%num_pixel_xy, pvr_img%iflag_mapped, pvr_img%depth_lc, &
     &    pvr_img%rgba_lc, pvr_img%rgba_real_gl)
!
      if(iflag_debug .gt. 0) write(*,*) 'sel_write_pvr_image_file'
      call sel_write_pvr_image_file                                     &
     &   (file_param, i_rot, istep_pvr, pvr_img%num_pixels,             &
     &    pvr_img%num_pixel_xy, pvr_img%rgba_real_gl,                   &
     &    pvr_img%rgba_chara_gl, pvr_img%rgb_chara_gl)
!
      end subroutine write_pvr_image_file
!
!  ---------------------------------------------------------------------
!
      end module generate_vr_image
