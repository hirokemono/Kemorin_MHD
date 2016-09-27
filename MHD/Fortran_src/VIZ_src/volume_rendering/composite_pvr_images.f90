!
!      module composite_pvr_images
!
!       Programmed by H. Matsui
!
!      subroutine allocate_pvr_image_array
!      subroutine deallocate_pvr_image_array
!
!!      subroutine share_num_images_to_compose                          &
!!     &         (num_overlap, istack_overlap, ntot_overlap)
!!      subroutine count_pixel_with_image(num_pixel_xy,                 &
!!     &          npixel_img, npixel_img_local, istack_pixel,           &
!!     &          ipixel_small, iflag_img_pe, iflag_mapped)
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
!
!>       status flag for sending
      integer, save, allocatable :: sta1(:,:)
!>       status flag for recieving
      integer, save, allocatable :: sta2(:,:)
!>       status flag for sending
      integer, save, allocatable :: req1(:  )
!>       status flag for recieving
      integer, save, allocatable :: req2(:  )
!
      private :: sta1, sta2, req1, req2
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_image_comm_status
!
!
      if(allocated(sta1)) return
!
      allocate(sta1(MPI_STATUS_SIZE,nprocs))
      allocate(req1(nprocs))
      allocate(sta2(MPI_STATUS_SIZE,nprocs))
      allocate(req2(nprocs))
!
      end subroutine alloc_pvr_image_comm_status
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_image_comm_status
!
!
      if(allocated(sta1))  deallocate (sta1, req1, sta2, req2)
!
      end subroutine dealloc_pvr_image_comm_status
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_pixel_with_image(num_pixel_xy, npixel_img,       &
     &          iflag_img_pe, iflag_mapped)
!
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: num_pixel_xy
!
      integer(kind = kint), intent(inout) :: npixel_img
      integer(kind = kint), intent(inout) :: iflag_img_pe(num_pixel_xy)
      integer(kind = kint), intent(inout) :: iflag_mapped(num_pixel_xy)
!
      integer(kind = kint) :: ipix, icou, max_smp
!
!
      call MPI_allREDUCE(iflag_img_pe, iflag_mapped, num_pixel_xy,      &
     &    CALYPSO_INTEGER, MPI_MAX, CALYPSO_COMM, ierr_MPI)
!
      npixel_img = 0
      do ipix = 1, num_pixel_xy
        iflag_img_pe(ipix) = iflag_mapped(ipix)
        npixel_img = npixel_img + iflag_img_pe(ipix)
      end do
!
      end subroutine count_pixel_with_image
!
!  ---------------------------------------------------------------------
!
      subroutine share_num_images_to_compose                            &
     &         (num_overlap, istack_overlap, ntot_overlap)
!
      integer(kind = kint), intent(in) :: num_overlap
      integer(kind = kint), intent(inout) :: ntot_overlap
      integer(kind = kint), intent(inout) :: istack_overlap(0:nprocs)
!
      integer(kind = kint) :: ip
!
!
      call MPI_Allgather(num_overlap, ione, CALYPSO_INTEGER,            &
     &                   istack_overlap(1), ione, CALYPSO_INTEGER,      &
     &                   CALYPSO_COMM, ierr_MPI)
      istack_overlap(0) = 0
      do ip = 1, nprocs
        istack_overlap(ip) =  istack_overlap(ip-1) + istack_overlap(ip)
      end do
      ntot_overlap = istack_overlap(nprocs)
!
      end subroutine share_num_images_to_compose
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
      call alloc_pvr_image_comm_status
!
      call distribute_segmented_images                                  &
     &   (pvr_img%num_overlap, pvr_img%istack_overlap,                  &
     &    pvr_img%ntot_overlap, pvr_img%npixel_img,                     &
     &    pvr_img%istack_pixel, pvr_img%npixel_img_local,               &
     &    pvr_img%rgba_lc, pvr_img%rgba_recv, pvr_img%rgba_part)
!
!$omp parallel do private(ipix,inum,ip)
      do ipix = 1, pvr_img%npixel_img_local
        pvr_img%rgba_whole(1:4,ipix) = 0.0d0
        do inum = pvr_img%ntot_overlap, 1, -1
          ip = pvr_img%ip_closer(inum,ipix)
!          if(ip .eq. 0) exit
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
     &    pvr_img%rgba_rank0, pvr_img%rgba_real_gl)
      call dealloc_pvr_image_comm_status
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
      subroutine share_image_depth_to_compose                           &
     &         (num_overlap, istack_images, ntot_overlap)
!
      integer(kind = kint), intent(in) :: num_overlap
      integer(kind = kint), intent(inout) :: ntot_overlap
      integer(kind = kint), intent(inout) :: istack_images(0:nprocs)
!
      integer(kind = kint) :: ip
!
!
      call MPI_Allgather(num_overlap, ione, CALYPSO_INTEGER,            &
     &                   istack_images(1), ione, CALYPSO_INTEGER,       &
     &                   CALYPSO_COMM, ierr_MPI)
      istack_images(0) = 0
      do ip = 1, nprocs
        istack_images(ip) =  istack_images(ip-1) + istack_images(ip)
      end do
      ntot_overlap = istack_images(nprocs)
!
      end subroutine share_image_depth_to_compose
!
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
      subroutine sel_write_pvr_local_img(file_param, index, pvr_img)
!
      use t_pvr_image_array
      use t_control_params_4_pvr
      use output_image_sel_4_png
      use set_parallel_file_name
      use convert_real_rgb_2_bite
!
      type(pvr_output_parameter), intent(in) :: file_param
      integer(kind = kint), intent(in) :: index
!
      type(pvr_image_type), intent(inout) :: pvr_img
!
      character(len=kchara) :: img_head
!
!
      call add_int_suffix(index, file_param%pvr_prefix, img_head)
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
!  ---------------------------------------------------------------------
!
      subroutine distribute_pixel_depth                                 &
     &         (num_overlap, istack_overlap, ntot_overlap,              &
     &          npixel_img, istack_pixel, npixel_img_local,             &
     &          depth_lc, depth_recv, depth_part)
!
      integer(kind = kint), intent(in) :: num_overlap, npixel_img
      real(kind = kreal), intent(in)                                    &
     &                    :: depth_lc(num_overlap,npixel_img)
!
      integer(kind = kint), intent(in) :: istack_pixel(0:nprocs)
      integer(kind = kint), intent(in) :: npixel_img_local
!
      integer(kind = kint), intent(in) :: istack_overlap(0:nprocs)
      integer(kind = kint), intent(in) :: ntot_overlap
!
      real(kind = kreal), intent(inout)                                 &
     &             :: depth_recv(ntot_overlap*npixel_img_local)
      real(kind = kreal), intent(inout)                                 &
     &             :: depth_part(ntot_overlap,npixel_img_local)
!
      integer(kind = kint) :: num, i_rank, ist, i, ipix, icou, jst
      integer(kind = kint) :: nneib_send, nneib_recv
!
!
!$omp workshare
      depth_recv(1:ntot_overlap*npixel_img_local) = -1000.0d0
!$omp end workshare
!$omp workshare
      depth_part(1:ntot_overlap,1:npixel_img_local) = -1000.0d0
!$omp end workshare
!
      call calypso_mpi_barrier
!
      nneib_send = 0
      do i_rank = 0, nprocs-1
          nneib_send = nneib_send + 1
          ist =  istack_pixel(i_rank)
          num = (istack_pixel(i_rank+1) - istack_pixel(i_rank))         &
     &          * num_overlap
          call MPI_ISEND(depth_lc(1,ist+1), num, CALYPSO_REAL,          &
     &        i_rank, 0, CALYPSO_COMM, req1(nneib_send), ierr_MPI)
      end do
!
      nneib_recv = 0
      do i_rank = 0, nprocs-1
          nneib_recv = nneib_recv + 1
          jst = istack_overlap(i_rank) * npixel_img_local
          num =  npixel_img_local                                       &
     &         * (istack_overlap(i_rank+1) - istack_overlap(i_rank))
          call MPI_IRECV(depth_recv(jst+1), num, CALYPSO_REAL,          &
     &        i_rank, 0, CALYPSO_COMM, req2(nneib_recv), ierr_MPI)
      end do
!
      call MPI_WAITALL (nneib_recv, req2, sta2, ierr_MPI)
      call MPI_WAITALL (nneib_send, req1, sta1, ierr_MPI)
!
!
!$omp parallel do private(ipix,i_rank,ist,num,icou,jst)
      do ipix = 1, npixel_img_local
        do i_rank = 0, nprocs-1
          ist = istack_overlap(i_rank)
          num = istack_overlap(i_rank+1) - istack_overlap(i_rank)
          jst = ist * npixel_img_local + (ipix-1) * num
          do icou = 1, num
            depth_part(ist+icou,ipix) = depth_recv(jst+icou)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine distribute_pixel_depth
!
!  ---------------------------------------------------------------------
!
      subroutine distribute_segmented_images                            &
     &         (num_overlap, istack_overlap, ntot_overlap,              &
     &          npixel_img, istack_pixel, npixel_img_local,             &
     &          rgba_lc, rgba_recv, rgba_part)
!
      integer(kind = kint), intent(in) :: num_overlap, npixel_img
      real(kind = kreal), intent(in)                                    &
     &                    :: rgba_lc(4,num_overlap,npixel_img)
!
      integer(kind = kint), intent(in) :: istack_pixel(0:nprocs)
      integer(kind = kint), intent(in) :: npixel_img_local
!
      integer(kind = kint), intent(in) :: istack_overlap(0:nprocs)
      integer(kind = kint), intent(in) :: ntot_overlap
!
      real(kind = kreal), intent(inout)                                 &
     &             :: rgba_recv(4,ntot_overlap*npixel_img_local)
      real(kind = kreal), intent(inout)                                 &
     &             :: rgba_part(4,ntot_overlap,npixel_img_local)
!
      integer(kind = kint) :: num, i_rank, ist, i, ipix, icou, jst
      integer(kind = kint) :: nneib_send, nneib_recv
!
!
!$omp workshare
      rgba_recv(1:4,ntot_overlap*npixel_img_local) = zero
!$omp end workshare
!$omp workshare
      rgba_part(1:4,1:ntot_overlap,1:npixel_img_local) = zero
!$omp end workshare
!
      call calypso_mpi_barrier
!
      nneib_send = 0
      do i_rank = 0, nprocs-1
!        if(i_rank .eq. my_rank) cycle
          nneib_send = nneib_send + 1
          ist =  istack_pixel(i_rank)
          num = (istack_pixel(i_rank+1) - istack_pixel(i_rank))         &
     &          * ifour * num_overlap
          call MPI_ISEND(rgba_lc(1,1,ist+1), num, CALYPSO_REAL,         &
     &        i_rank, 0, CALYPSO_COMM, req1(nneib_send), ierr_MPI)
      end do
!
      nneib_recv = 0
      do i_rank = 0, nprocs-1
!        if(i_rank .eq. my_rank) cycle
          nneib_recv = nneib_recv + 1
          jst = istack_overlap(i_rank) * npixel_img_local
          num = ifour * npixel_img_local                                &
     &         * (istack_overlap(i_rank+1) - istack_overlap(i_rank))
          call MPI_IRECV(rgba_recv(1,jst+1), num, CALYPSO_REAL,         &
     &        i_rank, 0, CALYPSO_COMM, req2(nneib_recv), ierr_MPI)
      end do
!
      call MPI_WAITALL (nneib_recv, req2, sta2, ierr_MPI)
      call MPI_WAITALL (nneib_send, req1, sta1, ierr_MPI)
!
!
!$omp parallel do private(ipix,i_rank,ist,num,icou,jst)
      do ipix = 1, npixel_img_local
        do i_rank = 0, nprocs-1
          ist = istack_overlap(i_rank)
          num = istack_overlap(i_rank+1) - istack_overlap(i_rank)
          jst = ist * npixel_img_local + (ipix-1) * num
          do icou = 1, num
            rgba_part(1,ist+icou,ipix) = rgba_recv(1,jst+icou)
            rgba_part(2,ist+icou,ipix) = rgba_recv(2,jst+icou)
            rgba_part(3,ist+icou,ipix) = rgba_recv(3,jst+icou)
            rgba_part(4,ist+icou,ipix) = rgba_recv(4,jst+icou)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine distribute_segmented_images
!
!  ---------------------------------------------------------------------
!
      subroutine collect_segmented_images                               &
     &         (irank_tgt, npixel_img_local, istack_pixel,              &
     &          npixel_img, num_pixel_xy, ipixel_small, rgba_whole,     &
     &          rgba_rank0, rgba_real_gl)
!
      integer(kind = kint), intent(in) :: irank_tgt
      integer(kind = kint), intent(in) :: istack_pixel(0:nprocs)
      integer(kind = kint), intent(in) :: npixel_img_local
      real(kind = kreal), intent(in) :: rgba_whole(4,npixel_img_local)
!
      integer(kind = kint), intent(in) :: npixel_img, num_pixel_xy
      integer(kind = kint), intent(inout) :: ipixel_small(npixel_img)
!
      real(kind = kreal), intent(inout) :: rgba_rank0(4,npixel_img)
      real(kind = kreal), intent(inout) :: rgba_real_gl(4,num_pixel_xy)
!
      integer(kind = kint) :: num, i_rank, ist, i, ipix
      integer(kind = kint) :: nneib_send, nneib_recv
!
!
      nneib_send = 0
      nneib_recv = 0
      num = ifour * npixel_img_local
      if(my_rank .ne. irank_tgt) then
        nneib_send = 1
        call MPI_ISEND(rgba_whole(1,1), num, CALYPSO_REAL,              &
     &      irank_tgt, 0, CALYPSO_COMM, req1(1), ierr_MPI)
      end if
!
      if(my_rank .eq. irank_tgt) then
        do i_rank = 0, nprocs-1
          if(i_rank .eq. irank_tgt) cycle
!
          nneib_recv = nneib_recv + 1
          ist =          istack_pixel(i_rank)
          num = ifour * (istack_pixel(i_rank+1) - istack_pixel(i_rank))
          call MPI_IRECV(rgba_rank0(1,ist+1), num, CALYPSO_REAL,        &
     &        i_rank, 0, CALYPSO_COMM, req2(nneib_recv), ierr_MPI)
        end do
      end if
!
      call MPI_WAITALL(nneib_recv, req2(1), sta2, ierr_MPI)
!
      if(my_rank .eq. irank_tgt) then
        ist = istack_pixel(irank_tgt)
        num = istack_pixel(irank_tgt+1) - ist
!$omp parallel do
        do i = 1, num
          rgba_rank0(1,ist+i) = rgba_whole(1,i)
          rgba_rank0(2,ist+i) = rgba_whole(2,i)
          rgba_rank0(3,ist+i) = rgba_whole(3,i)
          rgba_rank0(4,ist+i) = rgba_whole(4,i)
        end do
!$omp end parallel do
!
        rgba_real_gl(1:4,1:num_pixel_xy) = 0.0d0
!$omp parallel do private(i,ipix)
        do i = 1, npixel_img
          ipix = ipixel_small(i)
          rgba_real_gl(1,ipix) = rgba_rank0(1,i)
          rgba_real_gl(2,ipix) = rgba_rank0(2,i)
          rgba_real_gl(3,ipix) = rgba_rank0(3,i)
          rgba_real_gl(4,ipix) = rgba_rank0(4,i)
        end do
!$omp end parallel do
      end if
!
      call MPI_WAITALL(nneib_send, req1(1), sta1, ierr_MPI)
!
      end subroutine collect_segmented_images
!
!  ---------------------------------------------------------------------
!
      end module composite_pvr_images
