!
!      module composite_pvr_images
!
!       Programmed by H. Matsui
!
!!      subroutine sort_subimage_pixel_depth(ntot_overlap,              &
!!     &          npixel_img_local, depth_part, ip_closer)
!!
!!      subroutine old_blend_image_over_domains                         &
!!     &          (color_param, cbar_param, pvr_img)
!!      subroutine cvt_double_rgba_to_char_rgb(num_pixel, rgba, crgb)
!!      subroutine cvt_double_rgba_to_char_rgba(num_pixel, rgba, crgba)
!!      subroutine sel_write_pvr_image_file                             &
!!     &         (file_param, i_rot, istep_pvr, pvr_img)
!!      subroutine sel_write_pvr_local_img(file_param, index, pvr_img)
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
!
!
      call count_number_4_smp(nprocs, ione, npixel_img,                 &
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
      subroutine blend_image_over_domains                               &
     &          (color_param, cbar_param, pvr_img)
!
      use t_control_params_4_pvr
      use t_pvr_image_array
      use set_rgba_4_each_pixel
      use draw_pvr_colorbar
      use PVR_image_transfer
!
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(pvr_colorbar_parameter), intent(in) :: cbar_param
      type(pvr_image_type), intent(inout) :: pvr_img
!
!>       MPI rank for image output
      integer(kind = kint), parameter :: irank_tgt = 0
      integer(kind = kint) :: ip, ipix, inum
!
!
      call distribute_segmented_images                                  &
     &   (pvr_img%num_overlap, pvr_img%istack_overlap,                  &
     &    pvr_img%ntot_overlap, pvr_img%npixel_img,                     &
     &    pvr_img%istack_pixel, pvr_img%npixel_img_local,               &
     &    pvr_img%rgba_lc, pvr_img%rgba_recv, pvr_img%rgba_part,        &
     &    pvr_img%COMM)
!
!$omp parallel do private(ipix,inum,ip)
      do ipix = 1, pvr_img%npixel_img_local
        pvr_img%rgba_whole(1:4,ipix) = 0.0d0
        do inum = pvr_img%ntot_overlap, 1, -1
          ip = pvr_img%ip_closer(inum,ipix)
          if(ip .le. 0) exit
!
          call composite_alpha_blending(pvr_img%rgba_part(1:4,ip,ipix), &
     &        pvr_img%rgba_whole(1:4,ipix))
        end do
      end do
!$omp end parallel do
!
      call collect_segmented_images                                     &
     &   (irank_tgt, pvr_img%npixel_img_local, pvr_img%istack_pixel,    &
     &    pvr_img%npixel_img, pvr_img%num_pixel_xy,                     &
     &    pvr_img%ipixel_small, pvr_img%rgba_whole,                     &
     &    pvr_img%rgba_rank0, pvr_img%rgba_real_gl, pvr_img%COMM)
!
      if(my_rank .eq. irank_tgt) then
        call set_pvr_colorbar(pvr_img%num_pixel_xy, pvr_img%num_pixels, &
     &      color_param, cbar_param, pvr_img%rgba_real_gl)
      end if
!
      end subroutine blend_image_over_domains
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine sel_write_pvr_image_file                               &
     &         (file_param, i_rot, istep_pvr, pvr_img)
!
      use t_pvr_image_array
      use t_control_params_4_pvr
      use output_image_sel_4_png
      use set_parallel_file_name
      use convert_real_rgb_2_bite
!
      type(pvr_output_parameter), intent(in) :: file_param
      integer(kind = kint), intent(in) :: i_rot, istep_pvr
!
      type(pvr_image_type), intent(inout) :: pvr_img
!
      character(len=kchara) :: tmpchara, img_head
!
      if(my_rank .ne. 0) return
!
      if(istep_pvr .ge. 0) then
        call add_int_suffix(istep_pvr, file_param%pvr_prefix, tmpchara)
      else
        tmpchara = file_param%pvr_prefix
      end if
!
      if(i_rot .gt. 0) then
        call add_int_suffix(i_rot, tmpchara, img_head)
      else
        img_head = tmpchara
      end if
!
      if(file_param%id_pvr_transparent .eq. 1) then
          call cvt_double_rgba_to_char_rgba(pvr_img%num_pixel_xy,       &
     &        pvr_img%rgba_real_gl,  pvr_img%rgba_chara_gl)
          call sel_rgba_image_file(file_param%id_pvr_file_type,         &
     &        img_head, pvr_img%num_pixels(1), pvr_img%num_pixels(2),   &
     &        pvr_img%rgba_chara_gl)
      else
          call cvt_double_rgba_to_char_rgb(pvr_img%num_pixel_xy,        &
     &        pvr_img%rgba_real_gl,  pvr_img%rgb_chara_gl)
          call sel_output_image_file(file_param%id_pvr_file_type,       &
     &        img_head, pvr_img%num_pixels(1), pvr_img%num_pixels(2),   &
     &        pvr_img%rgb_chara_gl)
      end if
!
      end subroutine sel_write_pvr_image_file
!
!  ---------------------------------------------------------------------
!
      subroutine sel_write_pvr_local_img                                &
     &         (file_param, index, istep_pvr, pvr_img)
!
      use t_pvr_image_array
      use t_control_params_4_pvr
      use output_image_sel_4_png
      use set_parallel_file_name
      use convert_real_rgb_2_bite
!
      type(pvr_output_parameter), intent(in) :: file_param
      integer(kind = kint), intent(in) :: index, istep_pvr
!
      type(pvr_image_type), intent(inout) :: pvr_img
!
      character(len=kchara) :: tmpchara, img_head
!
!
      if(istep_pvr .ge. 0) then
        call add_int_suffix(istep_pvr, file_param%pvr_prefix, tmpchara)
      else
        tmpchara = file_param%pvr_prefix
      end if
      call add_int_suffix(index, tmpchara, img_head)
!
      call cvt_double_rgba_to_char_rgb(pvr_img%num_pixel_xy,            &
     &    pvr_img%old_rgba_lc, pvr_img%rgb_chara_lc)
!
      call sel_output_image_file(file_param%id_pvr_file_type,           &
     &    img_head, pvr_img%num_pixels(1), pvr_img%num_pixels(2),       &
     &    pvr_img%rgb_chara_lc)
!
      end subroutine sel_write_pvr_local_img
!
!  ---------------------------------------------------------------------
!
      end module composite_pvr_images
