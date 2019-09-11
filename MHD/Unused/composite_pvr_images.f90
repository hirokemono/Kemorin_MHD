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
!!      subroutine check_image_over_segments(id_file, iref,             &
!!     &           ntot_overlap, npixel_img_local, istack_pixel,        &
!!     &          num_pixel_xy, iflag_img_pe, rip_closer, gba_part)
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
      subroutine set_subimages(num_pixel_xy, pvr_start, pvr_img)
!
      use ray_trace_4_each_image
      use composite_pvr_images
      use PVR_image_transfer
!
      integer(kind = kint), intent(in) :: num_pixel_xy
      type(pvr_ray_start_type), intent(inout) :: pvr_start
      type(pvr_segmented_img), intent(inout) :: pvr_img
!
!
      call alloc_pvr_subimage_flags(num_pixel_xy, pvr_img)
!
      if(iflag_debug .gt. 0) write(*,*) 'count_overlap_in_each_domain'
      call count_overlap_in_each_domain(pvr_start%num_pvr_ray,          &
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
      subroutine alloc_pvr_subimage_array(pvr_img)
!
      type(pvr_segmented_img), intent(inout) :: pvr_img
!
      integer(kind = kint) :: npix, nolp
!
!
      nolp = pvr_img%ntot_overlap
      npix = pvr_img%npixel_img_local
!
      allocate(pvr_img%ip_closer(nolp,npix))
!
      allocate(pvr_img%depth_part(nolp,npix))
      allocate(pvr_img%depth_recv(nolp*npix))
!
      allocate(pvr_img%rgba_recv(4,nolp*npix))
      allocate(pvr_img%rgba_part(4,nolp,npix))
      allocate(pvr_img%rgba_whole(4,npix))
!
      pvr_img%ip_closer =  -1
      pvr_img%depth_part = 0.0d0
      pvr_img%rgba_part =  0.0d0
      pvr_img%rgba_whole =  0.0d0
!
      pvr_img%depth_recv =  0.0d0
      pvr_img%rgba_recv =   0.0d0
!
      end subroutine alloc_pvr_subimage_array
!
!  ---------------------------------------------------------------------
!
      subroutine dedealloc_pvr_subimage_array(pvr_img)
!
      type(pvr_segmented_img), intent(inout) :: pvr_img
!
!
      allocate(pvr_img%ip_closer)
!
      allocate(pvr_img%depth_part)
      allocate(pvr_img%depth_recv)
!
      allocate(pvr_img%rgba_recv)
      allocate(pvr_img%rgba_part)
      allocate(pvr_img%rgba_whole)
!
      end subroutine dealloc_pvr_subimage_array
!
!  ---------------------------------------------------------------------
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
      real(kind = kreal), intent(in)                                   &
     &             :: depth_part(ntot_overlap,npixel_img_local)
!
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
      subroutine check_image_over_segments(id_file, iref,               &
     &           ntot_overlap, npixel_img_local, istack_pixel,          &
     &           num_pixel_xy, iflag_img_pe, ip_closer, rgba_part)
!
      use set_rgba_4_each_pixel
!
      integer(kind = kint), intent(in) :: id_file, iref
      integer(kind = kint), intent(in) :: ntot_overlap
      integer(kind = kint), intent(in) :: npixel_img_local
      integer(kind = kint), intent(in) :: istack_pixel(0:nprocs)
      integer(kind = kint), intent(in) :: num_pixel_xy
      integer(kind = kint), intent(in) :: iflag_img_pe(num_pixel_xy)
!
      integer(kind = kint), intent(in)                                  &
     &             :: ip_closer(ntot_overlap,npixel_img_local)
      real(kind = kreal), intent(in)                                    &
     &             :: rgba_part(4,ntot_overlap,npixel_img_local)
!
      integer(kind = kint) :: ip, ipix, inum, itmp, iwrite
      real(kind = kreal) :: rgb_test(4)
!
!
      if(iflag_img_pe(iref) .le. 0) return

      iwrite = (iflag_img_pe(iref) - istack_pixel(my_rank))
      if(iwrite .le. 0 .or. iwrite .gt. npixel_img_local) return
!
      rgb_test(1:4) = 0.0d0
!
!$omp parallel do private(ipix,inum,ip)
      do ipix = 1, npixel_img_local
        do inum = ntot_overlap, 1, -1
          ip = ip_closer(inum,ipix)
          if(ip .le. 0) exit
!
           if(ipix .eq. iwrite) then
!
              call composite_alpha_blending(rgba_part(1:4,ip,ipix),     &
     &            rgb_test(1:4))
             write(id_file,*) 'blend', iref, inum, ip,                  &
     &                        rgba_part(1:4,ip,ipix)
           end if
        end do
      end do
!$omp end parallel do
      write(id_file,*) 'tested RGB', rgb_test
!
      end subroutine check_image_over_segments
!
!  ---------------------------------------------------------------------
!
      end module composite_pvr_images
