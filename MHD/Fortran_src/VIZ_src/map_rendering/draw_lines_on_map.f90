!>@file   draw_lines_on_map.f90
!!@brief  module draw_lines_on_map
!!
!!@author H. Matsui
!!@date Programmed in July, 2023
!
!>@brief Subroutines to draw lines on map
!!
!!@verbatim
!!      subroutine map_value_to_rgb(color_param, nxpixel, nypixel, npix,&
!!     &                            d_map, rgba)
!!        type(pvr_colormap_parameter), intent(in) :: color_param
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel, npix
!!        real(kind = kreal), intent(in) :: d_map(npix)
!!        real(kind = kreal), intent(inout) :: rgba(4,npix)
!!      subroutine draw_aitoff_map_frame                                &
!!     &         (xmin_frame, xmax_frame, ymin_frame, ymax_frame,       &
!!     &          nxpixel, nypixel, npix, rgba)
!!        real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
!!        real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel, npix
!!        real(kind = kreal), intent(inout) :: rgba(4,npix)
!!      subroutine draw_aitoff_lat_line                                 &
!!     &         (xmin_frame, xmax_frame, ymin_frame, ymax_frame,       &
!!     &          theta_ref, rgba_in, nxpixel, nypixel, npix, rgba)
!!        real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
!!        real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel, npix
!!        real(kind = kreal), intent(in) :: theta_ref
!!        real(kind = kreal), intent(in) :: rgba_in(4)
!!        real(kind = kreal), intent(inout) :: rgba(4,npix)
!!@endverbatim
      module draw_lines_on_map
!
      use m_precision
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
      subroutine map_value_to_rgb(color_param, nxpixel, nypixel, npix,  &
     &                            d_map, rgba)
!
      use t_pvr_colormap_parameter
      use set_color_4_pvr
!
      type(pvr_colormap_parameter), intent(in) :: color_param
      integer(kind = kint), intent(in) :: nxpixel, nypixel, npix
      real(kind = kreal), intent(in) :: d_map(npix)
!
      real(kind = kreal), intent(inout) :: rgba(4,npix)
!
      integer(kind = kint) :: i_img, i, j
!
!
!$omp parallel do private(i,j,i_img)
      do j = 1, nypixel
        do i = 1, nxpixel
          i_img = i + (j-1) * nxpixel
          if(rgba(4,i_img) .eq. zero) cycle
!
          call value_to_rgb(color_param%id_pvr_color(2),                &
     &                      color_param%id_pvr_color(1),                &
     &                      color_param%num_pvr_datamap_pnt,            &
     &                      color_param%pvr_datamap_param,              &
     &                      d_map(i_img), rgba(1,i_img))
        end do
      end do
!$omp end parallel do
!
      end subroutine map_value_to_rgb
!
!  ---------------------------------------------------------------------
!
      subroutine fill_background(nxpixel, nypixel, bg_rgba, rgba)
!
      use t_pvr_colormap_parameter
      use set_color_4_pvr
!
      integer(kind = kint), intent(in) :: nxpixel, nypixel
      real(kind = kreal), intent(in) :: bg_rgba(4)
!
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!
      integer(kind = kint) :: i_img, i, j
!
!
!$omp parallel do private(i,j,i_img)
      do j = 1, nypixel
        do i = 1, nxpixel
          i_img = i + (j-1) * nxpixel
          if(rgba(4,i_img) .eq. zero) rgba(1:4,i_img) = bg_rgba(1:4)
        end do
      end do
!$omp end parallel do
!
      end subroutine fill_background
!
!  ---------------------------------------------------------------------
!
      subroutine map_value_to_longitude                                 &
     &         (xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, phi_map)
!
      use aitoff
!
      real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
      real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
      integer(kind = kint), intent(in) :: nxpixel, nypixel
!
      real(kind = kreal), intent(inout) :: phi_map(nxpixel*nypixel)
!
      integer(kind = kint) :: i_img, i, j
      real(kind = kreal) :: x_pix1, y_pix1
      real(kind = kreal) :: theta(1), phi(1)
!
!
!$omp parallel do private(i,j,x_pix1,y_pix1,i_img,theta,phi)
      do j = 1, nypixel-1
        do i = 1, nxpixel-1
          x_pix1 = xmin_frame + (xmax_frame - xmin_frame)               &
     &                         * dble(i-1) / dble(nxpixel-1)
          y_pix1 = ymin_frame + (ymax_frame - ymin_frame)               &
     &                         * dble(j-1) / dble(nypixel-1)
          i_img = i + (j-1) * nxpixel
          call reverse_aitoff(x_pix1, y_pix1, theta(1), phi(1))
          phi_map(i_img) = phi(1)
        end do
      end do
!$omp end parallel do
!
      end subroutine map_value_to_longitude
!
!  ---------------------------------------------------------------------
!
      subroutine map_value_to_colatitude                                &
     &         (xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, phi_map)
!
      use aitoff
!
      real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
      real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
      integer(kind = kint), intent(in) :: nxpixel, nypixel
!
      real(kind = kreal), intent(inout) :: phi_map(nxpixel*nypixel)
!
      integer(kind = kint) :: i_img, i, j
      real(kind = kreal) :: x_pix1, y_pix1
      real(kind = kreal) :: theta(1), phi(1)
!
!
!$omp parallel do private(i,j,x_pix1,y_pix1,i_img,theta,phi)
      do j = 1, nypixel-1
        do i = 1, nxpixel-1
          x_pix1 = xmin_frame + (xmax_frame - xmin_frame)               &
     &                         * dble(i-1) / dble(nxpixel-1)
          y_pix1 = ymin_frame + (ymax_frame - ymin_frame)               &
     &                         * dble(j-1) / dble(nypixel-1)
          i_img = i + (j-1) * nxpixel
          call reverse_aitoff(x_pix1, y_pix1, theta(1), phi(1))
          phi_map(i_img) = theta(1)
        end do
      end do
!$omp end parallel do
!
      end subroutine map_value_to_colatitude
!
!  ---------------------------------------------------------------------
!
      subroutine draw_aitoff_map_frame                                  &
     &         (xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, npix, rgba)
!
      use aitoff
!
      real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
      real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
      integer(kind = kint), intent(in) :: nxpixel, nypixel, npix
!
      real(kind = kreal), intent(inout) :: rgba(4,npix)
!
      integer(kind = kint) :: ii, jj
      integer(kind = kint) :: i_img, i, j
      real(kind = kreal) :: x_pix1, x_pix2, y_pix1, y_pix2
      real(kind = kreal) :: phi_ref, theta_ref, pi
      real(kind = kreal) :: theta(4), phi(4)
!
!
      pi = four * atan(one)
!$omp parallel do private(i,j,x_pix1,x_pix2,y_pix1,y_pix2,ii,jj,i_img,  &
!$omp&                    phi_ref,theta_ref,theta,phi)
      do j = 1, nypixel-1
        do i = 1, nxpixel-1
          if(mod(j,6).ge.3 .and. mod(i,6).lt.3) cycle
          if(mod(j,6).lt.3 .and. mod(i,6).ge.3) cycle
!
!
          x_pix1 = xmin_frame + (xmax_frame - xmin_frame)               &
     &                         * dble(i-1) / dble(nxpixel-1)
          x_pix2 = xmin_frame + (xmax_frame - xmin_frame)               &
     &                         * dble(i) /   dble(nxpixel-1)
          y_pix1 = ymin_frame + (ymax_frame - ymin_frame)               &
     &                         * dble(j-1) / dble(nypixel-1)
          y_pix2 = ymin_frame + (ymax_frame - ymin_frame)               &
     &                         * dble(j) / dble(nypixel-1)
          call reverse_aitoff(x_pix1, y_pix1, theta(1), phi(1))
          call reverse_aitoff(x_pix2, y_pix1, theta(2), phi(2))
          call reverse_aitoff(x_pix1, y_pix2, theta(3), phi(3))
          call reverse_aitoff(x_pix2, y_pix2, theta(4), phi(4))
!
          if(    (theta(1)*theta(2)) .le. 0.0d0                         &
     &      .or. (theta(1)*theta(3)) .le. 0.0d0                         &
     &      .or. (theta(1)*theta(4)) .le. 0.0d0) then
            i_img = i + (j-1) * nxpixel
            rgba(1:4,i_img) = one
            rgba(4,  i_img) = one
          end if
!
          if(theta(1) .le. zero) cycle
          if(theta(2) .le. zero) cycle
          if(theta(3) .le. zero) cycle
          if(theta(4) .le. zero) cycle
          do ii = 1, 5
            phi_ref = pi * dble(ii-3) / 3.0d0
            if(     (phi(1)-phi_ref)*(phi(2)-phi_ref) .le. zero         &
     &         .or. (phi(1)-phi_ref)*(phi(3)-phi_ref) .le. zero         &
     &         .or. (phi(1)-phi_ref)*(phi(4)-phi_ref) .le. zero) then
              i_img = i + (j-1) * nxpixel
              rgba(1:4,i_img) = zero
              rgba(4,  i_img) = one
!              rgba(1:4,i_img+1,j) = zero
!              rgba(4,  i_img+1,j) = one
            end if
          end do
!
          do jj = 1, 5
            theta_ref = pi * dble(jj) / 6.0d0
            if(    (theta(1)-theta_ref)*(theta(2)-theta_ref) .le. zero  &
     &        .or. (theta(1)-theta_ref)*(theta(3)-theta_ref) .le. zero  &
     &        .or. (theta(1)-theta_ref)*(theta(4)-theta_ref) .le. zero) &
     &         then
              i_img = i + (j-1) * nxpixel
              rgba(1:4,i_img) = zero
              rgba(4,  i_img) = one
!              rgba(1:4,i_img+nxpixel) = zero
!              rgba(4,  i_img+nxpixel) = one
            end if
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine draw_aitoff_map_frame
!
!  ---------------------------------------------------------------------
!
      subroutine draw_aitoff_lat_line                                   &
     &         (xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          theta_ref, rgba_in, nxpixel, nypixel, npix, rgba)
!
      use aitoff
!
      real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
      real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
      integer(kind = kint), intent(in) :: nxpixel, nypixel, npix
      real(kind = kreal), intent(in) :: theta_ref
      real(kind = kreal), intent(in) :: rgba_in(4)
!
      real(kind = kreal), intent(inout) :: rgba(4,npix)
!
      integer(kind = kint) :: ii, jj
      integer(kind = kint) :: i_img, i, j
      real(kind = kreal) :: x_pix1, x_pix2, y_pix1, y_pix2
      real(kind = kreal) :: phi_ref, pi
      real(kind = kreal) :: theta(4), phi(4)
!
!
      pi = four * atan(one)
!$omp parallel do private(i,j,x_pix1,x_pix2,y_pix1,y_pix2,ii,jj,i_img,  &
!$omp&                    phi_ref,theta,phi)
      do j = 2, nypixel-1
        do i = 2, nxpixel-1
          if(mod(i,12).ge.6) cycle
!
          x_pix1 = xmin_frame + (xmax_frame - xmin_frame)               &
     &                         * dble(i-1) / dble(nxpixel-1)
          x_pix2 = xmin_frame + (xmax_frame - xmin_frame)               &
     &                         * dble(i) /   dble(nxpixel-1)
          y_pix1 = ymin_frame + (ymax_frame - ymin_frame)               &
     &                         * dble(j-1) / dble(nypixel-1)
          y_pix2 = ymin_frame + (ymax_frame - ymin_frame)               &
     &                         * dble(j) / dble(nypixel-1)
          call reverse_aitoff(x_pix1, y_pix1, theta(1), phi(1))
          call reverse_aitoff(x_pix2, y_pix1, theta(2), phi(2))
          call reverse_aitoff(x_pix1, y_pix2, theta(3), phi(3))
          call reverse_aitoff(x_pix2, y_pix2, theta(4), phi(4))
!
          if(    (theta(1)-theta_ref)*(theta(2)-theta_ref) .le. zero    &
     &      .or. (theta(1)-theta_ref)*(theta(3)-theta_ref) .le. zero    &
     &      .or. (theta(1)-theta_ref)*(theta(4)-theta_ref) .le. zero)   &
     &       then
             i_img = i + (j-1) * nxpixel
             rgba(1:3,i_img) = rgba_in(1:3)
             rgba(4,  i_img) = one
             if(rgba(4,i_img-nxpixel).gt.0) then
               rgba(1:3,i_img-nxpixel) = rgba_in(1:3)
             end if
             if(rgba(4,i_img+nxpixel).gt.0) then
               rgba(1:3,i_img+nxpixel) = rgba_in(1:3)
             end if
          end if
        end do
      end do
!$omp end parallel do
!
      end subroutine draw_aitoff_lat_line
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine draw_isolines(nxpixel, nypixel, color_param,           &
     &                         nline, d_map, rgba)
!
      use t_pvr_colormap_parameter
      use set_color_4_pvr
!
      integer(kind = kint), intent(in) :: nxpixel, nypixel, nline
      type(pvr_colormap_parameter), intent(in) :: color_param
      real(kind = kreal), intent(in) :: d_map(nxpixel*nypixel)
!
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!
      integer(kind = kint), parameter :: nwidth = 2
      integer(kind = kint) :: idots
      integer(kind = kint) :: iline, imax
      real(kind = kreal) :: color_ref(4)
      real(kind = kreal) :: d_ref, dmin, dmax
!
!
      imax = color_param%num_pvr_datamap_pnt
      do iline = 1, nline
        dmin = color_param%pvr_datamap_param(1,1)
        dmax = color_param%pvr_datamap_param(1,imax)
        d_ref = dble(iline-1) * (dmax - dmin) / dble(nline-1)
        if(d_ref .le. zero) then
          idots = 0
        else
          idots = 3
        end if
        call value_to_rgb(color_param%id_pvr_color(2),                  &
     &                    color_param%id_pvr_color(1),                  &
     &                    color_param%num_pvr_datamap_pnt,              &
     &                    color_param%pvr_datamap_param,                &
     &                    d_ref, color_ref(1))
        color_ref(1:3) = half * color_ref(1:3)
        call draw_isoline_on_pixel(nxpixel, nypixel, nwidth,            &
     &      idots, d_ref, color_ref, d_map, rgba)
      end do
!
      end subroutine draw_isolines
!
!  ---------------------------------------------------------------------
!
      subroutine draw_zeroline(nxpixel, nypixel, d_map, rgba)
!
      integer(kind = kint), intent(in) :: nxpixel, nypixel
      real(kind = kreal), intent(in) :: d_map(nxpixel*nypixel)
!
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!
      integer(kind = kint), parameter :: idots =  0
      integer(kind = kint), parameter :: nwidth = 4
      real(kind = kreal), parameter                                     &
     &                   :: color_ref(4) = (/zero,zero,zero,one/)
!
!
      call draw_isoline_on_pixel(nxpixel, nypixel, nwidth,              &
     &    idots, zero, color_ref, d_map, rgba)
!
      end subroutine draw_zeroline
!
!  ---------------------------------------------------------------------
!
      subroutine draw_mapflame(nxpixel, nypixel, phi_map, rgba)
!
      integer(kind = kint), intent(in) :: nxpixel, nypixel
      real(kind = kreal), intent(in) :: phi_map(nxpixel*nypixel)
!
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!
      integer(kind = kint), parameter :: idots =  0
      integer(kind = kint), parameter :: nwidth = 1
      real(kind = kreal), parameter                                     &
     &                   :: color_ref(4) = (/zero,zero,zero,one/)
      real(kind = kreal) :: pi
!
!
      pi = four * atan(one)
      call draw_isoline_on_pixel(nxpixel, nypixel, nwidth,              &
     &    idots, zero, color_ref, phi_map, rgba)
      call draw_isoline_on_pixel(nxpixel, nypixel, nwidth,              &
     &    idots, (two*pi), color_ref, phi_map, rgba)
!
      end subroutine draw_mapflame
!
!  ---------------------------------------------------------------------
!
      subroutine draw_longitude_grid(nxpixel, nypixel, phi_map, rgba)
!
      integer(kind = kint), intent(in) :: nxpixel, nypixel
      real(kind = kreal), intent(in) :: phi_map(nxpixel*nypixel)
!
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!
      integer(kind = kint), parameter :: idots = 3
      integer(kind = kint), parameter :: nwidth = 1
      real(kind = kreal), parameter                                     &
     &                   :: color_ref(4) = (/zero,zero,zero,one/)
      integer(kind = kint) :: ii
      real(kind = kreal) :: phi_ref, pi
!
!
      pi = four * atan(one)
      do ii = 1, 5
        phi_ref = pi * dble(ii-3) / 3.0d0
        call draw_isoline_on_pixel(nxpixel, nypixel, nwidth, idots,     &
     &      phi_ref, color_ref, phi_map, rgba)
      end do
!
      end subroutine draw_longitude_grid
!
!  ---------------------------------------------------------------------
!
      subroutine draw_latitude_grid(nxpixel, nypixel, theta_map, rgba)
!
      integer(kind = kint), intent(in) :: nxpixel, nypixel
      real(kind = kreal), intent(in) :: theta_map(nxpixel*nypixel)
!
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!
      integer(kind = kint), parameter :: idots = 3
      integer(kind = kint), parameter :: nwidth = 1
      real(kind = kreal), parameter                                     &
     &                   :: color_ref(4) = (/zero,zero,zero,one/)
      integer(kind = kint) :: jj
      real(kind = kreal) :: theta_ref, pi
!
!
      pi = four * atan(one)
      do jj = 1, 5
        theta_ref = pi * dble(jj) / 6.0d0
        call draw_isoline_on_pixel(nxpixel, nypixel, nwidth, idots,     &
     &      theta_ref, color_ref, theta_map, rgba)
      end do
!
      end subroutine draw_latitude_grid
!
!  ---------------------------------------------------------------------
!
      subroutine draw_tangent_cyl_grid(nxpixel, nypixel, theta_ref,     &
     &                                 theta_map, rgba)
!
      integer(kind = kint), intent(in) :: nxpixel, nypixel
      real(kind = kreal), intent(in) :: theta_ref(2)
      real(kind = kreal), intent(in) :: theta_map(nxpixel*nypixel)
!
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!
      integer(kind = kint), parameter :: idots = 0
      integer(kind = kint), parameter :: nwidth = 2
      real(kind = kreal), parameter                                     &
     &                   :: color_ref(4) = (/zero,zero,zero,one/)
!
!
      call draw_isoline_on_pixel(nxpixel, nypixel, nwidth, idots,       &
     &    theta_ref(1), color_ref, theta_map, rgba)
      call draw_isoline_on_pixel(nxpixel, nypixel, nwidth, idots,       &
     &    theta_ref(2), color_ref, theta_map, rgba)
!
      end subroutine draw_tangent_cyl_grid
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_aitoff_phi_on_map                                  &
     &         (xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, d_map)
!
      use aitoff
!
      real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
      real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
      integer(kind = kint), intent(in) :: nxpixel, nypixel
!
      real(kind = kreal), intent(inout) :: d_map(nxpixel*nypixel)
!
      integer(kind = kint) :: i, j
      real(kind = kreal) :: x_pix1, y_pix1
      real(kind = kreal) :: theta(1), phi(1)
!
!
!$omp parallel do private(i,j,x_pix1,y_pix1,theta,phi)
      do j = 2, nypixel-1
        do i = 2, nxpixel-1
          x_pix1 = xmin_frame + (xmax_frame - xmin_frame)               &
     &                         * dble(i-1) / dble(nxpixel-1)
          y_pix1 = ymin_frame + (ymax_frame - ymin_frame)               &
     &                         * dble(j-1) / dble(nypixel-1)
          call reverse_aitoff(x_pix1, y_pix1, theta(1), phi(1))
          d_map(nxpixel*nypixel) = phi(1)
        end do
      end do
!$omp end parallel do
!
      end subroutine set_aitoff_phi_on_map
!
!  ---------------------------------------------------------------------
!
      subroutine set_aitoff_lat_on_map                                  &
     &         (xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, d_map)
!
      use aitoff
!
      real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
      real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
      integer(kind = kint), intent(in) :: nxpixel, nypixel
!
      real(kind = kreal), intent(inout) :: d_map(nxpixel*nypixel)
!
      integer(kind = kint) :: i, j
      real(kind = kreal) :: x_pix1, y_pix1
      real(kind = kreal) :: theta(1), phi(1)
!
!
!$omp parallel do private(i,j,x_pix1,y_pix1,theta,phi)
      do j = 2, nypixel-1
        do i = 2, nxpixel-1
          x_pix1 = xmin_frame + (xmax_frame - xmin_frame)               &
     &                         * dble(i-1) / dble(nxpixel-1)
          y_pix1 = ymin_frame + (ymax_frame - ymin_frame)               &
     &                         * dble(j-1) / dble(nypixel-1)
          call reverse_aitoff(x_pix1, y_pix1, theta(1), phi(1))
          d_map(nxpixel*nypixel) = theta(1)
        end do
      end do
!$omp end parallel do
!
      end subroutine set_aitoff_lat_on_map
!
!  ---------------------------------------------------------------------
!
      subroutine draw_isoline_on_pixel(nxpixel, nypixel, nwidth,        &
     &          idots, d_ref, color_ref, d_map, rgba)
!
      integer(kind = kint), intent(in) :: nxpixel, nypixel
      integer(kind = kint), intent(in) :: nwidth, idots
      real(kind = kreal), intent(in) :: d_map(nxpixel*nypixel)
      real(kind = kreal), intent(in) :: color_ref(4)
      real(kind = kreal), intent(in) :: d_ref
!
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!
      integer(kind = kint) :: i_img, i, j, icou, isq, i1, i2, i3, i4
      integer(kind = kint) :: nwidth_xn, nwidth_xp
      integer(kind = kint) :: nwidth_yt, nwidth_yn, nwidth_yp
!
      isq = 2*idots
      nwidth_xn = -(nwidth-1) / 4
      nwidth_xp =  (nwidth+2) / 4
      nwidth_yt = -(nwidth-1) / 2
      nwidth_yn =  (nwidth_yt-1) / 2 + 1
      nwidth_yp =  nwidth / 4
!
!$omp parallel do private(i,j,i_img,icou,i1,i2,i3,i4)
      do j = 1, nypixel
        do i = 1, nxpixel
          i_img = i + (j-1) * nxpixel
          if(mod(j,isq).ge.idots .and. mod(i,isq).lt.idots) cycle
          if(mod(j,isq).lt.idots .and. mod(i,isq).ge.idots) cycle
          if(rgba(4,i_img) .eq. zero) cycle
          do icou = nwidth_xn, nwidth_xp
            i1 = max(i+icou,  1) +       (j-1) * nxpixel
            i2 = min(i+icou+1,nxpixel) + (j-1) * nxpixel
            i3 = i + max(j+icou-1,1) *     nxpixel
            i4 = i + min(j+icou,nypixel) * nxpixel
            if((rgba(4,i1)*rgba(4,i2)*rgba(4,i3)*rgba(4,i4))            &
     &                                                 .eq. zero) cycle
            if(   ((d_map(i1)-d_ref)*(d_map(i2)-d_ref)) .le. zero       &
     &       .or. ((d_map(i1)-d_ref)*(d_map(i3)-d_ref)) .le. zero       &
     &       .or. ((d_map(i1)-d_ref)*(d_map(i4)-d_ref)) .le. zero) then
              rgba(1:4,i_img) = color_ref(1:4)
            end if
          end do
!
          do icou = nwidth_yn, nwidth_yp
            i1 = i + max(j+icou-1,1) *     nxpixel
            i2 = i + min(j+icou,nypixel) * nxpixel
            i3 = i + max(j+icou-1,1) *     nxpixel
            i4 = i + min(j+icou,nypixel) * nxpixel
            if((rgba(4,i1)*rgba(4,i2)*rgba(4,i3)*rgba(4,i4))            &
     &                                                 .eq. zero) cycle
            if(   ((d_map(i1)-d_ref)*(d_map(i2)-d_ref)) .le. zero       &
     &       .or. ((d_map(i1)-d_ref)*(d_map(i3)-d_ref)) .le. zero       &
     &       .or. ((d_map(i1)-d_ref)*(d_map(i4)-d_ref)) .le. zero) then
              rgba(1:4,i_img) = color_ref(1:4)
            end if
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine draw_isoline_on_pixel
!
!  ---------------------------------------------------------------------
!
      end module draw_lines_on_map
