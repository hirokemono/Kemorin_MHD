!>@file   set_scalar_on_xyz_plane.f90
!!@brief  module set_scalar_on_xyz_plane
!!
!!@author H. Matsui
!!@date Programmed in June, 2023
!
!>@brief Subroutines for plane projection
!!@verbatim
!!      subroutine sel_scalar_on_xyz_plane(psf_nod, psf_ele, d_scalar,  &
!!     &                                   map_data, pvr_rgb, map_e)
!!        type(node_data), intent(in) :: psf_nod
!!        type(element_data), intent(in) :: psf_ele
!!        real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
!!        type(map_rendering_data), intent(inout) :: map_data
!!        type(pvr_image_type), intent(inout) :: pvr_rgb
!!        type(map_patches_for_1patch), intent(inout) :: map_e
!!@endverbatim
      module set_scalar_on_xyz_plane
!
      use m_precision
      use m_constants
!
      use t_geometry_data
      use t_phys_data
      use t_map_patch_from_1patch
!
      implicit  none
!
      private :: set_scalar_on_xy_plane, set_scalar_on_xz_plane
      private :: set_scalar_on_yz_plane
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine sel_scalar_on_xyz_plane(psf_nod, psf_ele, d_scalar,    &
     &                                   map_data, pvr_rgb, map_e)
!
      use t_pvr_image_array
      use t_map_rendering_data
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
!
      type(map_rendering_data), intent(inout) :: map_data
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(map_patches_for_1patch), intent(inout) :: map_e
!
!
      if(map_data%iflag_2d_projection_mode .eq. iflag_xy_plane)  then
        call set_scalar_on_xy_plane(psf_nod, psf_ele, d_scalar,         &
     &      map_data%xmin_frame, map_data%xmax_frame,                   &
     &      map_data%ymin_frame, map_data%ymax_frame,                   &
     &      pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      map_data%d_map, pvr_rgb%rgba_real_gl, map_e)
      else if(map_data%iflag_2d_projection_mode .eq. iflag_xz_plane)    &
     &                                                            then
        call set_scalar_on_xz_plane(psf_nod, psf_ele, d_scalar,         &
     &      map_data%xmin_frame, map_data%xmax_frame,                   &
     &      map_data%ymin_frame, map_data%ymax_frame,                   &
     &      pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      map_data%d_map, pvr_rgb%rgba_real_gl, map_e)
      else if(map_data%iflag_2d_projection_mode .eq. iflag_yz_plane)    &
     &                                                            then
        call set_scalar_on_yz_plane(psf_nod, psf_ele, d_scalar,         &
     &      map_data%xmin_frame, map_data%xmax_frame,                   &
     &      map_data%ymin_frame, map_data%ymax_frame,                   &
     &      pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      map_data%d_map, pvr_rgb%rgba_real_gl, map_e)
      end if
!
      end subroutine sel_scalar_on_xyz_plane
!
!  ---------------------------------------------------------------------
!
      subroutine sel_draw_isoline_on_xyz_plane                          &
     &         (psf_nod, psf_ele, d_scalar, nwidth, idots,              &
     &          map_data, d_ref, color_ref, pvr_rgb, map_e)
!
      use t_pvr_image_array
      use t_map_rendering_data
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
      integer(kind = kint), intent(in) :: nwidth, idots
      real(kind = kreal), intent(in) :: d_ref
      real(kind = kreal), intent(in) :: color_ref(4)
!
      type(map_rendering_data), intent(inout) :: map_data
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(map_patches_for_1patch), intent(inout) :: map_e
!
!
      if(map_data%iflag_2d_projection_mode .eq. iflag_xy_plane)  then
        call draw_isoline_on_xy_plane                                   &
     &     (psf_nod, psf_ele, d_scalar, nwidth, idots,                  &
     &      map_data%xmin_frame, map_data%xmax_frame,                   &
     &      map_data%ymin_frame, map_data%ymax_frame,                   &
     &      pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      d_ref, color_ref, pvr_rgb%rgba_real_gl, map_e)
      else if(map_data%iflag_2d_projection_mode .eq. iflag_xz_plane)    &
     &                                                            then
        call draw_isoline_on_xz_plane                                   &
     &     (psf_nod, psf_ele, d_scalar, nwidth, idots,                  &
     &      map_data%xmin_frame, map_data%xmax_frame,                   &
     &      map_data%ymin_frame, map_data%ymax_frame,                   &
     &      pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      d_ref, color_ref, pvr_rgb%rgba_real_gl, map_e)
      else if(map_data%iflag_2d_projection_mode .eq. iflag_yz_plane)    &
     &                                                            then
        call draw_isoline_on_yz_plane                                   &
     &     (psf_nod, psf_ele, d_scalar, nwidth, idots,                  &
     &      map_data%xmin_frame, map_data%xmax_frame,                   &
     &      map_data%ymin_frame, map_data%ymax_frame,                   &
     &      pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      d_ref, color_ref, pvr_rgb%rgba_real_gl, map_e)
      end if
!
      end subroutine sel_draw_isoline_on_xyz_plane
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_scalar_on_xy_plane(psf_nod, psf_ele, d_scalar,     &
     &          xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, d_map, rgba, map_e)
!
      use set_xyz_plot_from_1patch
      use map_patch_from_1patch
      use draw_pixels_on_map
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
!
      real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
      real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
      real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
      integer(kind = kint), intent(in) :: nxpixel, nypixel
!
      real(kind = kreal), intent(inout) :: d_map(nxpixel*nypixel)
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint) :: iele, k_ymin, k_ymid, k_ymax
!
!
      do iele = 1, psf_ele%numele
        call set_xy_plot_from_1patch(psf_nod, psf_ele, d_scalar, iele,  &
     &      map_e%xy_map(1,1,1), map_e%d_map_patch(1,1))
        call find_map_path_orientation(map_e%xy_map(1,1,1),             &
     &                                   k_ymin, k_ymid, k_ymax)
        call fill_triangle_data_on_image                                &
     &       (xmin_frame, xmax_frame, ymin_frame, ymax_frame,           &
     &        nxpixel, nypixel, k_ymin, k_ymid, k_ymax,                 &
     &        map_e%xy_map(1,1,1), map_e%d_map_patch(1,1),              &
     &        d_map, rgba)
      end do
!
      end subroutine set_scalar_on_xy_plane
!
!  ---------------------------------------------------------------------
!
      subroutine set_scalar_on_xz_plane(psf_nod, psf_ele, d_scalar,     &
     &          xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, d_map, rgba, map_e)
!
      use t_map_patch_from_1patch
      use set_xyz_plot_from_1patch
      use map_patch_from_1patch
      use draw_pixels_on_map
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
!
      real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
      real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
      real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
      integer(kind = kint), intent(in) :: nxpixel, nypixel
!
      real(kind = kreal), intent(inout) :: d_map(nxpixel*nypixel)
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint) :: iele, k_ymin, k_ymid, k_ymax
!
!
      do iele = 1, psf_ele%numele
        call set_xz_plot_from_1patch(psf_nod, psf_ele, d_scalar, iele,  &
     &      map_e%xy_map(1,1,1), map_e%d_map_patch(1,1))
        call find_map_path_orientation(map_e%xy_map(1,1,1),             &
     &                                   k_ymin, k_ymid, k_ymax)
        call fill_triangle_data_on_image                                &
     &       (xmin_frame, xmax_frame, ymin_frame, ymax_frame,           &
     &        nxpixel, nypixel, k_ymin, k_ymid, k_ymax,                 &
     &        map_e%xy_map(1,1,1), map_e%d_map_patch(1,1),              &
     &        d_map, rgba)
      end do
!
      end subroutine set_scalar_on_xz_plane
!
!  ---------------------------------------------------------------------
!
      subroutine set_scalar_on_yz_plane(psf_nod, psf_ele, d_scalar,     &
     &          xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, d_map, rgba, map_e)
!
      use t_map_patch_from_1patch
      use set_xyz_plot_from_1patch
      use map_patch_from_1patch
      use draw_pixels_on_map
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
!
      real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
      real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
      real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
      integer(kind = kint), intent(in) :: nxpixel, nypixel
!
      real(kind = kreal), intent(inout) :: d_map(nxpixel*nypixel)
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint) :: iele, k_ymin, k_ymid, k_ymax
!
!
      do iele = 1, psf_ele%numele
        call set_yz_plot_from_1patch(psf_nod, psf_ele, d_scalar, iele,  &
     &      map_e%xy_map(1,1,1), map_e%d_map_patch(1,1))
        call find_map_path_orientation(map_e%xy_map(1,1,1),             &
     &                                   k_ymin, k_ymid, k_ymax)
        call fill_triangle_data_on_image                                &
     &       (xmin_frame, xmax_frame, ymin_frame, ymax_frame,           &
     &        nxpixel, nypixel, k_ymin, k_ymid, k_ymax,                 &
     &        map_e%xy_map(1,1,1), map_e%d_map_patch(1,1),              &
     &        d_map, rgba)
      end do
!
      end subroutine set_scalar_on_yz_plane
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine draw_isoline_on_xy_plane                               &
     &         (psf_nod, psf_ele, d_scalar, nwidth, idots,              &
     &          xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, d_ref, color_ref, rgba, map_e)
!
      use set_xyz_plot_from_1patch
      use map_patch_from_1patch
      use draw_pixels_on_map
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
!
      integer(kind = kint), intent(in) :: nwidth, idots
      real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
      real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
      real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
      integer(kind = kint), intent(in) :: nxpixel, nypixel
      real(kind = kreal), intent(in) :: d_ref
      real(kind = kreal), intent(in) :: color_ref(4)
!
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint) :: iele
!
!
      do iele = 1, psf_ele%numele
        call set_xy_plot_from_1patch(psf_nod, psf_ele, d_scalar, iele,  &
     &      map_e%xy_map(1,1,1), map_e%d_map_patch(1,1))
        call s_draw_isoline_in_triangle(nwidth, idots,                  &
     &      xmin_frame, xmax_frame, ymin_frame, ymax_frame,             &
     &      nxpixel, nypixel, map_e%xy_map(1,1,1),                      &
     &      map_e%d_map_patch(1,1), d_ref, color_ref, rgba)
      end do
!
      end subroutine draw_isoline_on_xy_plane
!
!  ---------------------------------------------------------------------
!
      subroutine draw_isoline_on_xz_plane                               &
     &         (psf_nod, psf_ele, d_scalar, nwidth, idots,              &
     &          xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, d_ref, color_ref, rgba, map_e)
!
      use t_map_patch_from_1patch
      use set_xyz_plot_from_1patch
      use map_patch_from_1patch
      use draw_pixels_on_map
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
!
      integer(kind = kint), intent(in) :: nwidth, idots
      real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
      real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
      real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
      integer(kind = kint), intent(in) :: nxpixel, nypixel
      real(kind = kreal), intent(in) :: d_ref
      real(kind = kreal), intent(in) :: color_ref(4)
!
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint) :: iele
!
!
      do iele = 1, psf_ele%numele
        call set_xz_plot_from_1patch(psf_nod, psf_ele, d_scalar, iele,  &
     &      map_e%xy_map(1,1,1), map_e%d_map_patch(1,1))
        call s_draw_isoline_in_triangle(nwidth, idots,                  &
     &      xmin_frame, xmax_frame, ymin_frame, ymax_frame,             &
     &      nxpixel, nypixel, map_e%xy_map(1,1,1),                      &
     &      map_e%d_map_patch(1,1), d_ref, color_ref, rgba)
      end do
!
      end subroutine draw_isoline_on_xz_plane
!
!  ---------------------------------------------------------------------
!
      subroutine draw_isoline_on_yz_plane                               &
     &         (psf_nod, psf_ele, d_scalar, nwidth, idots,              &
     &          xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, d_ref, color_ref, rgba, map_e)
!
      use t_map_patch_from_1patch
      use set_xyz_plot_from_1patch
      use map_patch_from_1patch
      use draw_pixels_on_map
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
!
      integer(kind = kint), intent(in) :: nwidth, idots
      real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
      real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
      real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
      integer(kind = kint), intent(in) :: nxpixel, nypixel
      real(kind = kreal), intent(in) :: d_ref
      real(kind = kreal), intent(in) :: color_ref(4)
!
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint) :: iele
!
!
      do iele = 1, psf_ele%numele
        call set_yz_plot_from_1patch(psf_nod, psf_ele, d_scalar, iele,  &
     &      map_e%xy_map(1,1,1), map_e%d_map_patch(1,1))
        call s_draw_isoline_in_triangle(nwidth, idots,                  &
     &      xmin_frame, xmax_frame, ymin_frame, ymax_frame,             &
     &      nxpixel, nypixel, map_e%xy_map(1,1,1),                      &
     &      map_e%d_map_patch(1,1), d_ref, color_ref, rgba)
      end do
!
      end subroutine draw_isoline_on_yz_plane
!
!  ---------------------------------------------------------------------
!
      end module set_scalar_on_xyz_plane
