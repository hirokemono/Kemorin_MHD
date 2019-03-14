!>@file  composite_pvr_images.f90
!!       module composite_pvr_images
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine count_overlap_in_each_domain                         &
!!     &         (num_pvr_ray, id_pixel_start,                          &
!!     &          num_pixel_xy, iflag_img_pe, iflag_mapped, num_overlap)
!!      subroutine count_pixel_for_composit(num_pixel_xy,               &
!!     &          npixel_img, npixel_img_local, istack_pixel,           &
!!     &          ipixel_small, iflag_img_pe)
!!
!!      subroutine cal_image_pixel_depth(num_pvr_ray,                   &
!!     &         id_pixel_start, xx_pvr_ray_start, num_overlap,         &
!!     &         num_pixel_xy, iflag_mapped, iflag_img_pe, iflag_img_lc,&
!!     &         depth_lc)
!!      subroutine copy_segmented_image                                 &
!!     &        (num_pvr_ray, id_pixel_start, rgba_ray,                 &
!!     &         num_overlap, num_pixel_xy, iflag_mapped, rgba_lc)
!!
!!      subroutine sort_subimage_pixel_depth(ntot_overlap,              &
!!     &          npixel_img_local, depth_part, ip_closer)
!!      subroutine blend_image_over_segments                            &
!!     &         (ntot_overlap, npixel_img_local, ip_closer,            &
!!     &          rgba_part, rgba_whole)
!!@endverbatim
!
      module composite_pvr_images
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_overlap_in_each_domain                           &
     &         (num_pvr_ray, id_pixel_start,                            &
     &          num_pixel_xy, iflag_img_pe, iflag_mapped, num_overlap)
!
      integer(kind = kint), intent(in) :: num_pvr_ray
      integer(kind = kint), intent(in) :: id_pixel_start(num_pvr_ray)
!
      integer(kind = kint), intent(in) :: num_pixel_xy
      integer(kind = kint), intent(inout) :: iflag_mapped(num_pixel_xy)
      integer(kind = kint), intent(inout) :: iflag_img_pe(num_pixel_xy)
      integer(kind = kint), intent(inout) :: num_overlap
!
      integer(kind = kint) :: inum, ipix
!
!
!$omp parallel workshare
      iflag_mapped = 0
      iflag_img_pe = 0
!$omp end parallel workshare
      do inum = 1, num_pvr_ray
        ipix = id_pixel_start(inum)
        iflag_mapped(ipix) = iflag_mapped(ipix) + 1
        iflag_img_pe(ipix) = 1
      end do
      num_overlap = maxval(iflag_mapped,1)
!
      end subroutine count_overlap_in_each_domain
!
!  ---------------------------------------------------------------------
!
      subroutine count_pixel_for_composit(num_pixel_xy,                 &
     &          npixel_img, npixel_img_local, istack_pixel,             &
     &          ipixel_small, iflag_img_pe)
!
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: num_pixel_xy, npixel_img
!
      integer(kind = kint), intent(inout) :: npixel_img_local
      integer(kind = kint), intent(inout) :: istack_pixel(0:nprocs)
      integer(kind = kint), intent(inout) :: ipixel_small(npixel_img)
      integer(kind = kint), intent(inout) :: iflag_img_pe(num_pixel_xy)
!
      integer(kind = kint) :: ipix, icou, max_smp
      integer(kind = kint) :: np
!
!
      np = int(nprocs,KIND(np))
      call count_number_4_smp(np, ione, npixel_img,                     &
     &    istack_pixel, max_smp)
      npixel_img_local = istack_pixel(my_rank+1)                        &
     &                  - istack_pixel(my_rank)
!
      icou = 0
      do ipix = 1, num_pixel_xy
        if(iflag_img_pe(ipix) .gt. 0) then
          icou = icou + 1
          ipixel_small(icou) =  ipix
          iflag_img_pe(ipix) =  icou
        end if
      end do
!
      end subroutine count_pixel_for_composit
!
!  ---------------------------------------------------------------------
!
      subroutine cal_image_pixel_depth(num_pvr_ray,                     &
     &         id_pixel_start, xx_pvr_ray_start, num_overlap,           &
     &         num_pixel_xy, npixel_img, iflag_img_pe, iflag_mapped,    &
     &         iflag_img_lc, depth_lc)
!
      integer(kind = kint), intent(in) :: num_pvr_ray
      integer(kind = kint), intent(in) :: id_pixel_start(num_pvr_ray)
      real(kind = kreal), intent(in)                                    &
     &                    ::  xx_pvr_ray_start(3,num_pvr_ray)
!
      integer(kind = kint), intent(in) :: num_overlap, num_pixel_xy
      integer(kind = kint), intent(in) :: npixel_img
      integer(kind = kint), intent(in) :: iflag_img_pe(num_pixel_xy)
      integer(kind = kint), intent(inout) :: iflag_mapped(num_pixel_xy)
      integer(kind = kint), intent(inout)                               &
     &                   :: iflag_img_lc(num_overlap,npixel_img)
      real(kind = kreal), intent(inout)                                 &
     &                   :: depth_lc(num_overlap,npixel_img)
!
      integer(kind = kint) :: inum, ipix, icou, inod
!
!
!$omp parallel workshare
      iflag_mapped(1:num_pixel_xy) = 0
!$omp end parallel workshare
!$omp parallel workshare
      iflag_img_lc(1:num_overlap,1:npixel_img) = 0
      depth_lc(1:num_overlap,1:npixel_img) = -1000.0
!$omp end parallel workshare
!
      do inum = 1, num_pvr_ray
        ipix = id_pixel_start(inum)
        inod = iflag_img_pe(ipix)
        iflag_mapped(ipix) = iflag_mapped(ipix) + 1
        icou = iflag_mapped(ipix)
        iflag_img_lc(icou,inod) = 1
        depth_lc(icou,inod) =  xx_pvr_ray_start(3,inum)
      end do
!
      end subroutine cal_image_pixel_depth
!
!  ---------------------------------------------------------------------
!
      subroutine copy_segmented_image                                   &
     &        (num_pvr_ray, id_pixel_start, rgba_ray,                   &
     &         num_overlap, num_pixel_xy, npixel_img,                   &
     &         iflag_img_pe, iflag_mapped, rgba_lc)
!
      integer(kind = kint), intent(in) :: num_pvr_ray
      integer(kind = kint), intent(in) :: id_pixel_start(num_pvr_ray)
      real(kind = kreal), intent(in) ::  rgba_ray(4,num_pvr_ray)
!
      integer(kind = kint), intent(in) :: num_overlap, num_pixel_xy
      integer(kind = kint), intent(in) :: npixel_img
      integer(kind = kint), intent(in) :: iflag_img_pe(num_pixel_xy)
      integer(kind = kint), intent(inout) :: iflag_mapped(num_pixel_xy)
      real(kind = kreal), intent(inout)                                 &
     &                    :: rgba_lc(4,num_overlap,npixel_img)
!
      integer(kind = kint) :: inum, ipix, icou, inod
!
!
!$omp parallel workshare
      iflag_mapped(1:num_pixel_xy) = 0
!$omp end parallel workshare
!$omp parallel workshare
      rgba_lc(1:4,1:num_overlap,1:npixel_img) = 0.0d0
!$omp end parallel workshare
!
      do inum = 1, num_pvr_ray
        ipix = id_pixel_start(inum)
        inod = iflag_img_pe(ipix)
        iflag_mapped(ipix) = iflag_mapped(ipix) + 1
        icou = iflag_mapped(ipix)
!
        rgba_lc(1,icou,inod) = rgba_ray(1,inum)
        rgba_lc(2,icou,inod) = rgba_ray(2,inum)
        rgba_lc(3,icou,inod) = rgba_ray(3,inum)
        rgba_lc(4,icou,inod) = rgba_ray(4,inum)
      end do
!
      end subroutine copy_segmented_image
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine sort_subimage_pixel_depth(ntot_overlap,                &
     &          npixel_img_local, depth_part, ip_closer)
!
      use quicksort
!
      integer(kind = kint), intent(in) :: ntot_overlap
      integer(kind = kint), intent(in) :: npixel_img_local
!
      real(kind = kreal), intent(inout)                                 &
     &             :: depth_part(ntot_overlap,npixel_img_local)
      integer(kind = kint), intent(inout)                               &
     &             :: ip_closer(ntot_overlap,npixel_img_local)
!
      integer(kind = kint) :: inum, ipix, iflag
      integer(kind = kint) :: ip_tmp(ntot_overlap)
      real(kind = kreal) :: depth_tmp(ntot_overlap)
!
!
!!$omp parallel do private(ipix,inum,ip_tmp,depth_tmp)
      do ipix = 1, npixel_img_local
        iflag = 0
        do inum = 1, ntot_overlap
          depth_tmp(inum) = depth_part(inum,ipix)
          if(depth_tmp(inum) .gt. -100.0) then
            iflag = 1
            ip_tmp(inum) = inum
          else
            ip_tmp(inum) = 0
          end if
        end do
!
        if(iflag .gt. 0) then
          call quicksort_real_w_index(ntot_overlap, depth_tmp,          &
     &        ione, ntot_overlap, ip_tmp)
        end if
!
        do inum = 1, ntot_overlap
          ip_closer(inum,ipix) = ip_tmp(inum)
        end do
      end do
!!$omp end parallel do
!
      end subroutine sort_subimage_pixel_depth
!
!  ---------------------------------------------------------------------
!
      subroutine blend_image_over_segments                              &
     &         (ntot_overlap, npixel_img_local, ip_closer,              &
     &          rgba_part, rgba_whole)
!
      use set_rgba_4_each_pixel
!
      integer(kind = kint), intent(in) :: ntot_overlap
      integer(kind = kint), intent(in) :: npixel_img_local
!
      integer(kind = kint), intent(in)                                  &
     &             :: ip_closer(ntot_overlap,npixel_img_local)
      real(kind = kreal), intent(in)                                    &
     &             :: rgba_part(4,ntot_overlap,npixel_img_local)
      real(kind = kreal), intent(inout)                                 &
     &             :: rgba_whole(4,npixel_img_local)
!
      integer(kind = kint) :: ip, ipix, inum
!
!
!$omp parallel do private(ipix,inum,ip)
      do ipix = 1, npixel_img_local
        rgba_whole(1:4,ipix) = 0.0d0
        do inum = ntot_overlap, 1, -1
          ip = ip_closer(inum,ipix)
          if(ip .le. 0) exit
!
          call composite_alpha_blending(rgba_part(1:4,ip,ipix),         &
     &        rgba_whole(1:4,ipix))
        end do
      end do
!$omp end parallel do
!
      end subroutine blend_image_over_segments
!
!  ---------------------------------------------------------------------
!
      end module composite_pvr_images
