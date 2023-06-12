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
!!      subroutine set_scalar_on_map_image(psf_nod, psf_ele, psf_phys,  &
!!     &          xmin_frame, xmax_frame, ymin_frame, ymax_frame,       &
!!     &          nxpixel, nypixel, npix, d_map, rgba, map_e)
!!        type(node_data), intent(in) :: psf_nod
!!        type(element_data), intent(in) :: psf_ele
!!        type(phys_data), intent(in) :: psf_phys
!!        real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
!!        real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel, npix
!!        real(kind = kreal), intent(inout) :: d_map(npix)
!!        real(kind = kreal), intent(inout) :: rgba(4,npix)
!!        type(map_patches_for_1patch), intent(inout) :: map_e
!!      subroutine draw_aitoff_map_zeroline                             &
!!     &         (nxpixel, nypixel, npix, d_map, rgba)
!!        integer(kind = kint), intent(in) :: nxpixel, nypixel, npix
!!        real(kind = kreal), intent(in) :: d_map(npix)
!!        real(kind = kreal), intent(inout) :: rgba(4,npix)
!!@endverbatim
      module draw_aitoff_map
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
      subroutine set_scalar_on_map_image(psf_nod, psf_ele, psf_phys,    &
     &          xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, npix, d_map, rgba, map_e)
!
      use t_geometry_data
      use t_phys_data
      use t_map_patch_from_1patch
      use map_patch_from_1patch
!
      type(node_data), intent(in) :: psf_nod
      type(element_data), intent(in) :: psf_ele
      type(phys_data), intent(in) :: psf_phys
!
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
     &      ione, psf_phys%d_fld(1,1), map_e%n_map_patch,               &
     &      map_e%x_map_patch, map_e%d_map_patch)
!
        do i = 1, map_e%n_map_patch
          call set_sph_position_4_map_patch                             &
     &       (map_e%x_map_patch(1,1,i), map_e%rtp_map_patch(1,1,i))
          call patch_to_aitoff(map_e%rtp_map_patch(1,1,i),              &
     &                         map_e%xy_map(1,1,i))
!
          call find_map_path_orientation(map_e%xy_map(1,1,i),           &
     &                                   k_ymin, k_ymid, k_ymax)
          call fill_triangle_patch_on_image                             &
     &         (xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, npix, k_ymin, k_ymid, k_ymax,         &
     &          map_e%xy_map(1,1,i), map_e%d_map_patch(1,1,i),          &
     &          d_map, rgba)
        end do
      end do
!
      end subroutine set_scalar_on_map_image
!
!  ---------------------------------------------------------------------
!
      subroutine draw_aitoff_map_zeroline                               &
     &         (nxpixel, nypixel, npix, d_map, rgba)
!
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
        do i = 2, nxpixel-1
          i_img = i + (j-1) * nxpixel
          if(rgba(4,i_img) .eq. zero) cycle
!
          if((d_map(i_img-1)*d_map(i_img+1)) .le. 0) then
            rgba(1:4,i_img) = zero
            rgba(4,  i_img) = one
          end if
        end do
      end do
!$omp end parallel do
!
!$omp parallel do private(i,j,i_img)
      do j = 2, nypixel-2
        do i = 1, nxpixel
          i_img = i + (j-1) * nxpixel
          if(rgba(4,i_img) .eq. zero) cycle
!
          if((d_map(i_img-nxpixel)*d_map(i_img+nxpixel)) .le. 0) then
            rgba(1:4,i_img) = zero
            rgba(4,  i_img) = one
          end if
        end do
      end do
!$omp end parallel do
!
      end subroutine draw_aitoff_map_zeroline
!
!  ---------------------------------------------------------------------
!
      subroutine fill_triangle_patch_on_image                           &
     &         (xmin_frame, xmax_frame, ymin_frame, ymax_frame,         &
     &          nxpixel, nypixel, npix, k_ymin, k_ymid, k_ymax,         &
     &          xy_patch, d_patch, d_map, rgba)
!
      real(kind= kreal), intent(in) :: xmin_frame, xmax_frame
      real(kind= kreal), intent(in) :: ymin_frame, ymax_frame
      integer(kind = kint), intent(in) :: nxpixel, nypixel, npix
      integer(kind = kint), intent(in) :: k_ymin, k_ymid, k_ymax
      real(kind = kreal), intent(in) :: xy_patch(2,3)
      real(kind = kreal), intent(in) :: d_patch(3)
!
      real(kind = kreal), intent(inout) :: d_map(npix)
      real(kind = kreal), intent(inout) :: rgba(4,npix)
!
      integer(kind = kint) :: ix, iy, i_img
      integer(kind = kint) :: ix_min, ix_max
      integer(kind = kint) :: iy_min, iy_mid, iy_max
      integer(kind = kint) :: kmin, kmax
      real(kind = kreal) :: x(2), d(2)
      real(kind = kreal) :: ratio_ymid, ratio_ymax, ratio_x
!
!
      iy_min = int(1 + dble(nypixel-1)                                  &
     &                * (xy_patch(2,k_ymin) - ymin_frame)               &
     &                  / (ymax_frame - ymin_frame))
      iy_mid = int(1 + dble(nypixel-1)                                  &
     &                * (xy_patch(2,k_ymid) - ymin_frame)               &
     &                  / (ymax_frame - ymin_frame))
      iy_max = int(1 + dble(nypixel-1)                                  &
     &                * (xy_patch(2,k_ymax) - ymin_frame)               &
     &                  / (ymax_frame - ymin_frame))
      do iy = iy_min, iy_mid
        if(iy_max.eq.iy_min .or. iy_mid.eq.iy_min) then
          x(1) = xy_patch(1,k_ymin)
          x(2) = xy_patch(1,k_ymid)
          d(1) = d_patch(k_ymin)
          d(2) = d_patch(k_ymid)
        else
          ratio_ymid = dble(iy-iy_min) / dble(iy_mid-iy_min)
          ratio_ymax = dble(iy-iy_min) / dble(iy_max-iy_min)
          x(1) = (one-ratio_ymid) * xy_patch(1,k_ymin)                  &
     &              + ratio_ymid *  xy_patch(1,k_ymid)
          x(2) = (one-ratio_ymax) * xy_patch(1,k_ymin)                  &
     &              + ratio_ymax *  xy_patch(1,k_ymax)
          d(1) = (one-ratio_ymid) * d_patch(k_ymin)                     &
     &              + ratio_ymid *  d_patch(k_ymid)
          d(2) = (one-ratio_ymax) * d_patch(k_ymin)                     &
     &              + ratio_ymax *  d_patch(k_ymax)
        end if
        if(x(1) .le. x(2)) then
          kmin = 1
          kmax = 2
        else
          kmin = 2
          kmax = 1
        end if
        ix_min = int(1 + dble(nxpixel-1)*(x(kmin) - xmin_frame)         &
     &                  / (xmax_frame - xmin_frame))
        ix_max = int(1 + dble(nxpixel-1)*(x(kmax) - xmin_frame)         &
     &                  / (xmax_frame - xmin_frame))
!
        i_img = ix_min + (iy-1) * nxpixel
        d_map(i_img) =  d(kmin)
        rgba(4,i_img) = one
!
        do ix = ix_min+1, ix_max
          i_img = ix + (iy-1) * nxpixel
          ratio_x = dble(ix-ix_min) / dble(ix_max-ix_min)
          d_map(i_img) = (one - ratio_x) * d(kmin) + ratio_x *  d(kmax)
          rgba(4,i_img) = one
        end do
      end do
!
      do iy = iy_mid+1, iy_max
        if(iy_max.eq.iy_min) then
          x(1) = xy_patch(1,k_ymid)
          x(2) = xy_patch(1,k_ymax)
          d(1) = d_patch(k_ymid)
          d(2) = d_patch(k_ymax)
        else
          ratio_ymid = dble(iy-iy_mid) / dble(iy_max-iy_mid)
          ratio_ymax = dble(iy-iy_min) / dble(iy_max-iy_min)
          x(1) = (one-ratio_ymid) * xy_patch(1,k_ymid)                  &
     &              + ratio_ymid *  xy_patch(1,k_ymax)
          x(2) = (one-ratio_ymax) * xy_patch(1,k_ymin)                  &
     &              + ratio_ymax *  xy_patch(1,k_ymax)
          d(1) = (one-ratio_ymid) * d_patch(k_ymid)                     &
     &              + ratio_ymid *  d_patch(k_ymax)
          d(2) = (one-ratio_ymax) * d_patch(k_ymin)                     &
     &              + ratio_ymax *  d_patch(k_ymax)
        end if
        if(x(1) .le. x(2)) then
          kmin = 1
          kmax = 2
        else
          kmin = 2
          kmax = 1
        end if
        ix_min = int(1 + dble(nxpixel-1)*(x(kmin) - xmin_frame)         &
     &                  / (xmax_frame - xmin_frame))
        ix_max = int(1 + dble(nxpixel-1)*(x(kmax) - xmin_frame)         &
     &                  / (xmax_frame - xmin_frame))
!
        i_img = ix_min + (iy-1) * nxpixel
        d_map(i_img) =  d(kmin)
        rgba(4,i_img) = one
!
        do ix = ix_min+1, ix_max
          i_img = ix + (iy-1) * nxpixel
          ratio_x = dble(ix-ix_min) / dble(ix_max-ix_min)
          d_map(i_img) = (one - ratio_x) * d(kmin) + ratio_x *  d(kmax)
          rgba(4,i_img) = one
        end do
      end do
!
      end subroutine fill_triangle_patch_on_image
!
!  ---------------------------------------------------------------------
!
      end module draw_aitoff_map
