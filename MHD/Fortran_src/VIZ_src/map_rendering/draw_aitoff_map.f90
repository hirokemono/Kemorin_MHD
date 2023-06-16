!>@file   draw_aitoff_map.f90
!!@brief  module draw_aitoff_map
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!@n    modified in July, 2014
!
!>@brief Structure for cross sectioning
!!
!!@verbatim
!!      subroutine set_scalar_on_map_image(psf_nod, psf_ele, d_scalar,  &
!!     &          xmin_frame, xmax_frame, ymin_frame, ymax_frame,       &
!!     &          nxpixel, nypixel, npix, d_map, rgba, map_e)
!!        type(node_data), intent(in) :: psf_nod
!!        type(element_data), intent(in) :: psf_ele
!!        real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
!!        real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
!!        real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel, npix
!!        real(kind = kreal), intent(inout) :: d_map(npix)
!!        real(kind = kreal), intent(inout) :: rgba(4,npix)
!!        type(map_patches_for_1patch), intent(inout) :: map_e
!!      subroutine draw_isoline_on_map_image                            &
!!     &         (psf_nod, psf_ele, d_scalar, nwidth, idots,            &
!!     &          xmin_frame, xmax_frame, ymin_frame, ymax_frame,       &
!!     &          nxpixel, nypixel, d_ref, color_ref, rgba, map_e)
!!      subroutine draw_aitoff_map_isolines                             &
!!     &         (psf_nod, psf_ele, d_scalar, map_data, color_param,    &
!!     &          nxpixel, nypixel, color_ref, rgba, map_e)
!!        type(node_data), intent(in) :: psf_nod
!!        type(element_data), intent(in) :: psf_ele
!!        real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
!!        integer(kind = kint), intent(in) :: nwidth, idots
!!        real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
!!        real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel, npix
!!        real(kind = kreal), intent(in) :: d_ref
!!        real(kind = kreal), intent(in) :: color_ref(4)
!!        real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
!!        type(map_patches_for_1patch), intent(inout) :: map_e
!!@endverbatim
      module draw_aitoff_map
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
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_scalar_on_map_image(psf_nod, psf_ele, d_scalar,    &
     &          xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, npix, d_map, rgba, map_e)
!
      use map_patch_from_1patch
      use draw_pixels_on_map
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
!
      real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
      real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
      real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
      integer(kind = kint), intent(in) :: nxpixel, nypixel, npix
!
      real(kind = kreal), intent(inout) :: d_map(npix)
      real(kind = kreal), intent(inout) :: rgba(4,npix)
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint) :: iele, i
      integer(kind = kint) :: k_ymin, k_ymid, k_ymax
!
!
      do iele = 1, psf_ele%numele
        call s_set_map_patch_from_1patch(iele,                          &
     &      psf_nod%numnod, psf_ele%numele, psf_nod%xx, psf_ele%ie,     &
     &      ione, d_scalar, map_e%n_map_patch,                          &
     &      map_e%x_map_patch, map_e%d_map_patch(1,1))
!
        do i = 1, map_e%n_map_patch
          call set_sph_position_4_map_patch                             &
     &       (map_e%x_map_patch(1,1,i), map_e%rtp_map_patch(1,1,i))
          call patch_to_aitoff(map_e%rtp_map_patch(1,1,i),              &
     &                         map_e%xy_map(1,1,i))
!
          call find_map_path_orientation(map_e%xy_map(1,1,i),           &
     &                                   k_ymin, k_ymid, k_ymax)
          call fill_triangle_data_on_image                              &
     &         (xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, k_ymin, k_ymid, k_ymax,               &
     &          map_e%xy_map(1,1,i), map_e%d_map_patch(1,i),            &
     &          d_map, rgba)
        end do
      end do
!
      end subroutine set_scalar_on_map_image
!
!  ---------------------------------------------------------------------
!
      subroutine draw_isoline_on_map_image                              &
     &         (psf_nod, psf_ele, d_scalar, nwidth, idots,              &
     &          xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, d_ref, color_ref, rgba, map_e)
!
      use map_patch_from_1patch
      use draw_isoline_in_triangle
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
!
      integer(kind = kint), intent(in) :: nwidth, idots
      real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
      real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
      integer(kind = kint), intent(in) :: nxpixel, nypixel
      real(kind = kreal), intent(in) :: d_ref
      real(kind = kreal), intent(in) :: color_ref(4)
!
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint) :: iele, i
!
      real(kind=kreal) :: xy_map(2,3,3)
      real(kind=kreal) :: d_map_patch(3,3)
!
!
      do iele = 1, psf_ele%numele
        call s_set_map_patch_from_1patch(iele,                          &
     &      psf_nod%numnod, psf_ele%numele, psf_nod%xx, psf_ele%ie,     &
     &      ione, d_scalar(1), map_e%n_map_patch,                       &
     &      xy_map(1,1,1), d_map_patch(1,1))
!
        do i = 1, map_e%n_map_patch
          call set_sph_position_4_map_patch                             &
     &       (xy_map(1,1,i), map_e%rtp_map_patch(1,1,i))
          call patch_to_aitoff(map_e%rtp_map_patch(1,1,i),              &
     &                         map_e%xy_map(1,1,i))
!
          call s_draw_isoline_in_triangle(nwidth, idots,                &
     &        xmin_frame, xmax_frame, ymin_frame, ymax_frame,           &
     &        nxpixel, nypixel, map_e%xy_map(1,1,i),                    &
     &        d_map_patch(1,1), d_ref, color_ref, rgba)
        end do
      end do
!
      end subroutine draw_isoline_on_map_image
!
!  ---------------------------------------------------------------------
!
      subroutine draw_aitoff_map_isolines                               &
     &         (psf_nod, psf_ele, d_scalar, map_data, color_param,      &
     &          nxpixel, nypixel, color_ref, rgba, map_e)
!
      use t_map_rendering_data
      use t_pvr_colormap_parameter
      use set_color_4_pvr
      use draw_pixels_on_map
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      real(kind= kreal), intent(in) :: d_scalar(psf_nod%numnod)
!
      integer(kind = kint), intent(in) :: nxpixel, nypixel
      type(map_rendering_data), intent(in) :: map_data
      type(pvr_colormap_parameter), intent(in) :: color_param
!
      real(kind = kreal), intent(inout) :: rgba(4,nxpixel*nypixel)
      type(map_patches_for_1patch), intent(inout) :: map_e
!
      integer(kind = kint), parameter :: nwidth = 2
      integer(kind = kint) :: idots
      integer(kind = kint) :: iline, imax
      real(kind = kreal) :: color_ref(4)
      real(kind = kreal) :: d_min, d_max, d_ref
!
!
      if(map_data%flag_fixed_isoline_range) then
        d_min = map_data%dmin_isoline
        d_max = map_data%dmax_isoline
      else
        d_min = minval(d_scalar)
        d_max = maxval(d_scalar)
      end if
!
      imax = color_param%num_pvr_datamap_pnt
      do iline = 0, map_data%num_line-1
        d_ref = d_min + (d_max - d_min)                                 &
     &                 * dble(iline) / dble(map_data%num_line-1)
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
        call draw_isoline_on_map_image                                  &
     &     (psf_nod, psf_ele, d_scalar, nwidth, idots,                  &
     &      map_data%xmin_frame, map_data%xmax_frame,                   &
     &      map_data%ymin_frame, map_data%ymax_frame,                   &
     &      nxpixel, nypixel, d_ref, color_ref, rgba, map_e)
      end do
!
      end subroutine draw_aitoff_map_isolines
!
!  ---------------------------------------------------------------------
!
      end module draw_aitoff_map
