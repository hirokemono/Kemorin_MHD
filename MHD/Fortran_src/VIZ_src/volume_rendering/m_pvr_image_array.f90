!
!      module m_pvr_image_array
!
!       Programmed by H. Matsui
!
!!      subroutine s_set_pvr_ray_start_point(numnod, numele, numsurf,   &
!!     &          nnod_4_surf, xx, ie_surf, isf_4_ele, viewpoint_vec,   &
!!     &          pvr_bound, proj, pixel_xy, pvr_start)
!!      subroutine ray_trace_local(i_pvr, numnod, numele, numsurf,      &
!!     &    nnod_4_surf, ie_surf, isf_4_ele, iele_4_surf, e_multi, xx,  &
!!     &    viewpoint_vec, proj, pvr_start, pvr_img)
!
      module m_pvr_image_array
!
      use m_precision
!
      use calypso_mpi
      use m_constants
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_pvr_ray_start_point(numnod, numele, numsurf,     &
     &          nnod_4_surf, xx, ie_surf, isf_4_ele, viewpoint_vec,     &
     &          pvr_bound, proj, pixel_xy, pvr_start)
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
     &    proj%nnod_pvr, proj%x_nod_screen,                             &
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
      subroutine ray_trace_local(i_pvr, numnod, numele, numsurf,        &
     &    nnod_4_surf, ie_surf, isf_4_ele, iele_4_surf, e_multi, xx,    &
     &    viewpoint_vec, proj, pvr_start, pvr_img)
!
      use t_pvr_image_array
      use t_pvr_ray_startpoints
      use t_geometries_in_pvr_screen
      use m_geometries_in_pvr_screen
      use ray_trace_4_each_image
      use composite_pvr_images
!
      integer(kind = kint), intent(in) :: i_pvr
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
      type(pvr_projected_type), intent(in) :: proj
      type(pvr_ray_start_type), intent(inout) :: pvr_start
      type(pvr_image_type), intent(inout) :: pvr_img
!
!
      call s_ray_trace_4_each_image                                     &
     &   (numnod, numele, numsurf, nnod_4_surf,                         &
     &    ie_surf, isf_4_ele, iele_4_surf, e_multi, xx,                 &
     &    proj%nnod_pvr, proj%nele_pvr,                                 &
     &    proj%field_pvr(i_pvr)%iflag_used_ele, proj%x_nod_screen,      &
     &    proj%field_pvr(i_pvr)%d_pvr, proj%field_pvr(i_pvr)%grad_ele,  &
     &    viewpoint_vec, color_params(i_pvr), ray_vec,                  &
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
      call deallocate_item_pvr_ray_start(pvr_start)
!
!      call sel_write_pvr_local_img                                     &
!     &   (i_pvr, pvr_img%num_pixels, pvr_img%num_pixel_xy,             &
!     &    pvr_img%rgba_lc,  pvr_img%rgb_chara_lc)
!
      end subroutine ray_trace_local
!
!  ---------------------------------------------------------------------
!
      subroutine blend_pvr_over_domains(i_pvr, pvr_img)
!
      use t_pvr_image_array
      use composite_pvr_images
!
      integer(kind = kint), intent(in) :: i_pvr
      type(pvr_image_type), intent(inout) :: pvr_img
!
!
      call blend_image_over_domains(i_pvr, pvr_img%istack_image,        &
     &    pvr_img%num_pixels, pvr_img%num_pixel_xy,                     &
     &    pvr_img%iflag_mapped, pvr_img%depth_lc,                       &
     &    pvr_img%rgba_lc, pvr_img%rgba_real_gl)
!
      end subroutine blend_pvr_over_domains
!
!  ---------------------------------------------------------------------
!
      subroutine write_pvr_image_file(i_pvr, i_rot, istep_pvr, pvr_img)
!
      use t_pvr_image_array
      use composite_pvr_images
!
      integer(kind = kint), intent(in) :: i_pvr, i_rot, istep_pvr
      type(pvr_image_type), intent(inout) :: pvr_img
!
      call sel_write_pvr_image_file                                     &
     &   (i_pvr, i_rot, istep_pvr, pvr_img%num_pixels,                  &
     &    pvr_img%num_pixel_xy, pvr_img%rgba_real_gl,                   &
     &    pvr_img%rgba_chara_gl, pvr_img%rgb_chara_gl)
!
      end subroutine write_pvr_image_file
!
!  ---------------------------------------------------------------------
!
      end module m_pvr_image_array
