!>@file   draw_lines_on_map.f90
!!@brief  module draw_lines_on_map
!!
!!@author H. Matsui
!!@date Programmed in July, 2023
!
!>@brief Subroutines to draw lines on map
!!
!!@verbatim
!!      subroutine draw_isolines(nxpixel, nypixel,                      &
!!     &                         map_data, color_param, rgba)
!!      subroutine draw_zeroline(nxpixel, nypixel, color_param,         &
!!     &                         d_map, rgba)
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel, nline
!!        type(map_rendering_data), intent(in) :: map_data
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        real(kind = kreal), intent(in) :: d_map(nxpixel*nypixel)
!!        real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!!
!!      subroutine draw_radius_grid(nxpixel, nypixel,                   &
!!     &          bg_color, flag_fill, ref_r, r_map, rgba)
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel
!!        real(kind = kreal), intent(in) :: ref_r
!!        real(kind = kreal), intent(in) :: r_map(nxpixel*nypixel)
!!        real(kind = kreal), intent(in) :: bg_color(4)
!!        logical, intent(in) :: flag_fill
!!        real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!!      subroutine draw_mapflame(nxpixel, nypixel, bg_color, flag_fill, &
!!     &                         phi_map, rgba)
!!      subroutine draw_longitude_grid(nxpixel, nypixel,                &
!!     &          bg_color, flag_fill, phi_map, rgba)
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel
!!        real(kind = kreal), intent(in) :: phi_map(nxpixel*nypixel)
!!        real(kind = kreal), intent(in) :: bg_color(4)
!!        logical, intent(in) :: flag_fill
!!        real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel
!!        real(kind = kreal), intent(in) :: phi_map(nxpixel*nypixel)
!!        real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!!        real(kind = kreal), intent(in) :: bg_color(4)
!!        logical, intent(in) :: flag_fill
!!      subroutine draw_latitude_grid(nxpixel, nypixel,                 &
!!     &          bg_color, flag_fill, theta_map, rgba)
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel
!!        real(kind = kreal), intent(in) :: theta_map(nxpixel*nypixel)
!!        real(kind = kreal), intent(in) :: bg_color(4)
!!        logical, intent(in) :: flag_fill
!!        real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!!
!!      subroutine draw_map_tangent_cyl_grid(nxpixel, nypixel,          &
!!     &          bg_color, flag_fill, theta_ref, theta_map, rgba)
!!      subroutine draw_med_tangent_cyl_grid(nxpixel, nypixel,          &
!!     &          bg_color, flag_fill, radius_ICB, x_map, rgba)
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel
!!        real(kind = kreal), intent(in) :: theta_ref(2)
!!        real(kind = kreal), intent(in) :: theta_map(nxpixel*nypixel)
!!        real(kind = kreal), intent(in) :: radius_ICB
!!        real(kind = kreal), intent(in) :: bg_color(4)
!!        logical, intent(in) :: flag_fill
!!        real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!!@endverbatim
      module draw_lines_on_map
!
      use m_precision
      use m_constants
      use t_geometry_data
      use t_map_patch_from_1patch
      use t_map_rendering_data
      use t_pvr_image_array
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine draw_isolines(nxpixel, nypixel,                        &
     &                         map_data, color_param, rgba)
!
      use t_map_rendering_data
      use t_pvr_colormap_parameter
      use set_color_4_pvr
      use draw_pixels_on_map
!
      integer(kind = kint), intent(in) :: nxpixel, nypixel
      type(map_rendering_data), intent(in) :: map_data
      type(pvr_colormap_parameter), intent(in) :: color_param
!
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!
      integer(kind = kint), parameter :: nwidth = 3
      integer(kind = kint) :: idots
      integer(kind = kint) :: iline, imax
      real(kind = kreal) :: color_ref(4)
      real(kind = kreal) :: d_ref, dmin, dmax
!
!
      imax = color_param%num_pvr_datamap_pnt
      do iline = 1, map_data%num_line
        dmin = color_param%pvr_datamap_param(1,1)
        dmax = color_param%pvr_datamap_param(1,imax)
        d_ref = dmin + dble(iline-1) * (dmax - dmin)                    &
     &         / dble(map_data%num_line-1)
        if(d_ref .ge. zero) then
          idots = 0
        else
          idots = 2
        end if
!
        if(map_data%iflag_isoline_color .eq. iflag_white) then
          color_ref(1:4) =   one
        else if(map_data%iflag_isoline_color .eq. iflag_black) then
          color_ref(1:4) =   zero
        else
          call value_to_rgb(color_param%id_pvr_color(2),                &
     &                      color_param%id_pvr_color(1),                &
     &                      color_param%num_pvr_datamap_pnt,            &
     &                      color_param%pvr_datamap_param,              &
     &                      d_ref, color_ref(1))
        end if
!
        color_ref(4) =   one
        call draw_isoline_on_pixel(nxpixel, nypixel, nwidth,            &
     &      idots, d_ref, color_ref, map_data%d_map, rgba)
      end do
!
      end subroutine draw_isolines
!
!  ---------------------------------------------------------------------
!
      subroutine draw_zeroline(nxpixel, nypixel, color_param,           &
     &                         d_map, rgba)
!
      use t_pvr_colormap_parameter
      use set_color_4_pvr
      use draw_pixels_on_map
!
      integer(kind = kint), intent(in) :: nxpixel, nypixel
      type(pvr_colormap_parameter), intent(in) :: color_param
      real(kind = kreal), intent(in) :: d_map(nxpixel*nypixel)
!
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!
      integer(kind = kint), parameter :: idots =  0
      integer(kind = kint), parameter :: nwidth = 4
      real(kind = kreal) :: color_ref(4)
!
!
      call value_to_rgb(color_param%id_pvr_color(2),                    &
     &                  color_param%id_pvr_color(1),                    &
     &                  color_param%num_pvr_datamap_pnt,                &
     &                  color_param%pvr_datamap_param,                  &
     &                  zero, color_ref(1))
      call draw_isoline_on_pixel(nxpixel, nypixel, nwidth,              &
     &    idots, zero, color_ref, d_map, rgba)
!
      end subroutine draw_zeroline
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine draw_radius_grid(psf_nod, psf_ele, map_data,           &
     &          bg_color, flag_fill, ref_r, pvr_rgb, map_e)
!
      use set_scalar_on_xyz_plane
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      type(map_rendering_data), intent(in) :: map_data
      real(kind = kreal), intent(in) :: ref_r
      real(kind = kreal), intent(in) :: bg_color(4)
      logical, intent(in) :: flag_fill
!
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint), parameter :: idots =  0
      integer(kind = kint), parameter :: nwidth = 3
      real(kind = kreal) :: color_ref(4)
!
      call set_flame_color(flag_fill, bg_color, color_ref)
      call sel_draw_isoline_on_xyz_plane                                &
     &   (psf_nod, psf_ele, psf_nod%rr(1), nwidth, idots,               &
     &    map_data, ref_r, color_ref, pvr_rgb, map_e)
!
      end subroutine draw_radius_grid
!
!  ---------------------------------------------------------------------
!
      subroutine draw_mapflame(psf_nod, psf_ele, map_data,              &
     &          bg_color, flag_fill, pvr_rgb, map_e)
!
      use draw_aitoff_map
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      type(map_rendering_data), intent(in) :: map_data
      real(kind = kreal), intent(in) :: bg_color(4)
      logical, intent(in) :: flag_fill
!
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint), parameter :: idots =  0
      integer(kind = kint), parameter :: nwidth = 3
      real(kind = kreal) :: pi
      real(kind = kreal) :: color_ref(4)
      real(kind = kreal), allocatable :: phi_shift(:)
      integer(kind = kint) :: i
!
      call set_flame_color(flag_fill, bg_color, color_ref)
!
      pi = four * atan(one)
      allocate(phi_shift(psf_nod%numnod))
      do i = 1, psf_nod%numnod
        if(psf_nod%xx(i,2) .ge. 0) then
          phi_shift(i) = psf_nod%phi(i)
        else
          phi_shift(i) = two*pi - psf_nod%phi(i)
        end if
      end do
      call draw_isoline_on_map_image                                    &
     &   (psf_nod, psf_ele, phi_shift(1), nwidth, idots,                &
     &    map_data%xmin_frame, map_data%xmax_frame,                     &
     &    map_data%ymin_frame, map_data%ymax_frame,                     &
     &    pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),                 &
     &    (-pi), color_ref, pvr_rgb%rgba_real_gl, map_e)
      call draw_isoline_on_map_image                                    &
     &   (psf_nod, psf_ele, phi_shift(1), nwidth, idots,                &
     &    map_data%xmin_frame, map_data%xmax_frame,                     &
     &    map_data%ymin_frame, map_data%ymax_frame,                     &
     &    pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),                 &
     &    pi, color_ref, pvr_rgb%rgba_real_gl, map_e)
      deallocate(phi_shift)
!
      end subroutine draw_mapflame
!
!  ---------------------------------------------------------------------
!
      subroutine draw_longitude_grid(psf_nod, psf_ele, map_data,        &
     &          bg_color, flag_fill, pvr_rgb, map_e)
!
      use draw_aitoff_map
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      type(map_rendering_data), intent(in) :: map_data
      real(kind = kreal), intent(in) :: bg_color(4)
      logical, intent(in) :: flag_fill
!
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint), parameter :: idots =  3
      integer(kind = kint), parameter :: nwidth = 1
      integer(kind = kint) :: ii, i
      real(kind = kreal) :: phi_ref, pi
      real(kind = kreal) :: color_ref(4)
      real(kind = kreal), allocatable :: phi_shift(:)
!
!
      call set_flame_color(flag_fill, bg_color, color_ref)
!
      pi = four * atan(one)
      allocate(phi_shift(psf_nod%numnod))
      do i = 1, psf_nod%numnod
        if(psf_nod%xx(i,2) .ge. 0) then
          phi_shift(i) = psf_nod%phi(i)
        else
          phi_shift(i) = two*pi - psf_nod%phi(i)
        end if
      end do
!
      do ii = 1, 5
        phi_ref = pi * dble(ii-3) / 3.0d0
        call draw_isoline_on_map_image                                  &
     &     (psf_nod, psf_ele, phi_shift(1), nwidth, idots,              &
     &      map_data%xmin_frame, map_data%xmax_frame,                   &
     &      map_data%ymin_frame, map_data%ymax_frame,                   &
     &      pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      phi_ref, color_ref, pvr_rgb%rgba_real_gl, map_e)
      end do
      deallocate(phi_shift)
!
      end subroutine draw_longitude_grid
!
!  ---------------------------------------------------------------------
!
      subroutine draw_latitude_grid(psf_nod, psf_ele, map_data,         &
     &          bg_color, flag_fill, pvr_rgb, map_e)
!
      use draw_aitoff_map
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      type(map_rendering_data), intent(in) :: map_data
      real(kind = kreal), intent(in) :: bg_color(4)
      logical, intent(in) :: flag_fill
!
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint), parameter :: idots =  3
      integer(kind = kint), parameter :: nwidth = 1
      integer(kind = kint) :: jj
      real(kind = kreal) :: theta_ref, pi
      real(kind = kreal) :: color_ref(4)
!
      call set_flame_color(flag_fill, bg_color, color_ref)
!
      pi = four * atan(one)
      do jj = 1, 5
        theta_ref = pi * dble(jj) / 6.0d0
        call draw_isoline_on_map_image                                  &
     &     (psf_nod, psf_ele, psf_nod%theta(1), nwidth, idots,          &
     &      map_data%xmin_frame, map_data%xmax_frame,                   &
     &      map_data%ymin_frame, map_data%ymax_frame,                   &
     &      pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      theta_ref, color_ref, pvr_rgb%rgba_real_gl, map_e)
      end do
!
      end subroutine draw_latitude_grid
!
!  ---------------------------------------------------------------------
!
      subroutine draw_map_tangent_cyl_grid(psf_nod, psf_ele, map_data,  &
     &          bg_color, flag_fill, theta_ref, pvr_rgb, map_e)
!
      use draw_aitoff_map
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      type(map_rendering_data), intent(in) :: map_data
      real(kind = kreal), intent(in) :: theta_ref(2)
      real(kind = kreal), intent(in) :: bg_color(4)
      logical, intent(in) :: flag_fill
!
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint), parameter :: idots =  4
      integer(kind = kint), parameter :: nwidth = 3
      real(kind = kreal) :: color_ref(4)
!
      call set_flame_color(flag_fill, bg_color, color_ref)
!
        call draw_isoline_on_map_image                                  &
     &     (psf_nod, psf_ele, psf_nod%theta(1), nwidth, idots,          &
     &      map_data%xmin_frame, map_data%xmax_frame,                   &
     &      map_data%ymin_frame, map_data%ymax_frame,                   &
     &      pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      theta_ref(1), color_ref, pvr_rgb%rgba_real_gl, map_e)
        call draw_isoline_on_map_image                                  &
     &     (psf_nod, psf_ele, psf_nod%theta(1), nwidth, idots,          &
     &      map_data%xmin_frame, map_data%xmax_frame,                   &
     &      map_data%ymin_frame, map_data%ymax_frame,                   &
     &      pvr_rgb%num_pixels(1), pvr_rgb%num_pixels(2),               &
     &      theta_ref(2), color_ref, pvr_rgb%rgba_real_gl, map_e)
!
      end subroutine draw_map_tangent_cyl_grid
!
!  ---------------------------------------------------------------------
!
      subroutine draw_med_tangent_cyl_grid(psf_nod, psf_ele, map_data,  &
     &          bg_color, flag_fill, radius_ICB, pvr_rgb, map_e)
!
      use set_scalar_on_xyz_plane
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      type(map_rendering_data), intent(in) :: map_data
      real(kind = kreal), intent(in) :: radius_ICB
      real(kind = kreal), intent(in) :: bg_color(4)
      logical, intent(in) :: flag_fill
!
      type(pvr_image_type), intent(inout) :: pvr_rgb
      type(map_patches_for_1patch), intent(inout) :: map_e
!
!
      integer(kind = kint), parameter :: idots =  6
      integer(kind = kint), parameter :: nwidth = 3
      real(kind = kreal) :: color_ref(4)
!
      call set_flame_color(flag_fill, bg_color, color_ref)
      call sel_draw_isoline_on_xyz_plane                                &
     &   (psf_nod, psf_ele, psf_nod%xx(1,1), nwidth, idots,             &
     &    map_data, radius_ICB, color_ref, pvr_rgb, map_e)
!
      end subroutine draw_med_tangent_cyl_grid
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_flame_color(flag_fill, bg_color, flame_color)
!
      logical, intent(in) :: flag_fill
      real(kind = kreal), intent(in) :: bg_color(4)
      real(kind = kreal), intent(inout) :: flame_color(4)
!
      if(flag_fill) then
        flame_color(1:3) = zero
        flame_color(4) =   one
      else
        flame_color(1:3) = bg_color(1:3) + (one - two*bg_color(1:3))
        flame_color(4) = one
      end if
!
      end subroutine set_flame_color
!
!  ---------------------------------------------------------------------
!
      end module draw_lines_on_map
