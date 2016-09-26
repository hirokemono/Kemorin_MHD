!
!      module composite_pvr_images
!
!       Programmed by H. Matsui
!
!      subroutine allocate_pvr_image_array
!      subroutine deallocate_pvr_image_array
!
!!      subroutine share_num_images_to_compose                          &
!!     &         (num_overlap, istack_images, ntot_overlap)
!!      subroutine share_subimage_depth(num_overlap, num_pixel_xy,      &
!!     &          iflag_mapped, depth_lc, istack_images, ntot_overlap,  &
!!     &          ave_depth_lc, ave_depth_gl, ip_closer)
!!
!!      subroutine blend_image_over_domains                             &
!!     &          (color_param, cbar_param, pvr_img)
!!      subroutine cvt_double_rgba_to_char_rgb(num_pixel, rgba, crgb)
!!      subroutine cvt_double_rgba_to_char_rgba(num_pixel, rgba, crgba)
!!      subroutine sel_write_pvr_image_file                             &
!!     &         (file_param, i_rot, istep_pvr, pvr_img)
!!      subroutine sel_write_pvr_local_img(file_param, pvr_img)
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
      private :: blend_image_from_subdomains, distribute_average_depth
      private :: distribute_segmented_images, collect_segmented_images
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
      subroutine share_num_images_to_compose                            &
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
      end subroutine share_num_images_to_compose
!
!  ---------------------------------------------------------------------
!
      subroutine share_subimage_depth(num_overlap, num_pixel_xy,        &
     &          iflag_mapped, depth_lc, istack_images, ntot_overlap,    &
     &          ave_depth_lc, ave_depth_gl, ip_closer)
!
      use quicksort
!
      integer(kind = kint), intent(in) :: ntot_overlap
      integer(kind = kint), intent(in) :: num_overlap, num_pixel_xy
      integer(kind = kint), intent(in) :: istack_images(0:nprocs)
      integer(kind = kint), intent(in) :: iflag_mapped(num_pixel_xy)
      real(kind = kreal), intent(inout)                                 &
     &                   :: depth_lc(num_pixel_xy,num_overlap)
!
      real(kind = kreal), intent(inout) :: ave_depth_lc(ntot_overlap)
      real(kind = kreal), intent(inout) :: ave_depth_gl(ntot_overlap)
      integer(kind = kint), intent(inout) :: ip_closer(ntot_overlap)
!
      integer(kind = kint) :: inum, ipix, icou
      real(kind = kreal) :: sum_depth, covered_area
!
!
!$omp parallel do
      do inum = 1, ntot_overlap
        ip_closer(inum) = inum
      end do
!$omp end parallel do
!
!$omp parallel workshare
      ave_depth_lc(1:ntot_overlap) = 0.0d0
      ave_depth_gl(1:ntot_overlap) = 0.0d0
!$omp end parallel workshare
!
!$omp parallel do private(inum,icou,ipix,covered_area,sum_depth)
      do inum = 1, num_overlap
        covered_area = 0.0d0
        sum_depth =    0.0d0
        do ipix = 1, num_pixel_xy
          if(iflag_mapped(ipix) .le. inum) then
            covered_area = covered_area + 1.0d0
            sum_depth = sum_depth + depth_lc(ipix,inum)
          end if
        end do
        icou = inum + istack_images(my_rank)
        ave_depth_lc(icou) = sum_depth / covered_area
      end do
!$omp end parallel do
!
      call MPI_allREDUCE(ave_depth_lc, ave_depth_gl, ntot_overlap,      &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
!
!      if(my_rank .eq. 0) then
!        do inum = 1, ntot_overlap
!          write(*,*) inum,  ave_depth_gl(inum)
!        end do
!      end if
!
      call quicksort_real_w_index(ntot_overlap, ave_depth_gl,           &
     &    ione, ntot_overlap, ip_closer)
!
      end subroutine share_subimage_depth
!
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
      subroutine blend_image_over_domains                               &
     &          (color_param, cbar_param, pvr_img)
!
      use t_control_params_4_pvr
      use t_pvr_image_array
      use draw_pvr_colorbar
!
      type(pvr_colormap_parameter), intent(in) :: color_param
      type(pvr_colorbar_parameter), intent(in) :: cbar_param
      type(pvr_image_type), intent(inout) :: pvr_img
!
!>       MPI rank for image outp
      integer(kind = kint), parameter :: irank_tgt = 0
!
!
      call alloc_pvr_image_comm_status
!
! -- Set Average depth for each subdomain
      if(iflag_debug .gt. 0) write(*,*) 'distribute_average_depth'
      call distribute_average_depth                                     &
     &   (pvr_img%num_pixel_xy, pvr_img%iflag_mapped, pvr_img%old_depth_lc, &
     &    pvr_img%ip_closer_old, pvr_img%old_ave_depth_gl)
!
! Distribute image
      if(iflag_debug .gt. 0) write(*,*) 'distribute_segmented_images'
      call distribute_segmented_images                                  &
     &   (pvr_img%num_pixel_xy, pvr_img%old_rgba_lc,                    &
     &    pvr_img%istack_image, pvr_img%npixel_local, pvr_img%old_rgba_part)
!
!  Alpha blending
      if(iflag_debug .gt. 0) write(*,*) 'blend_image_from_subdomains'
      call blend_image_from_subdomains                                  &
     &   (pvr_img%ip_closer_old, pvr_img%npixel_local,                      &
     &    pvr_img%old_rgba_part, pvr_img%rgba_real_part)
!
!  Collect image to rank 0
      if(iflag_debug .gt. 0) write(*,*) 'collect_segmented_images'
      call collect_segmented_images                                     &
     &   (irank_tgt, pvr_img%npixel_local, pvr_img%istack_image,        &
     &    pvr_img%num_pixel_xy, pvr_img%rgba_real_part,                 &
     &    pvr_img%rgba_real_gl)
!
      if(my_rank .eq. irank_tgt) then
        call set_pvr_colorbar(pvr_img%num_pixel_xy, pvr_img%num_pixels, &
     &      color_param, cbar_param, pvr_img%rgba_real_gl)
      end if
!
      end subroutine blend_image_over_domains
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
      subroutine sel_write_pvr_local_img(file_param, pvr_img)
!
      use t_pvr_image_array
      use t_control_params_4_pvr
      use output_image_sel_4_png
      use set_parallel_file_name
      use convert_real_rgb_2_bite
!
      type(pvr_output_parameter), intent(in) :: file_param
!
      type(pvr_image_type), intent(inout) :: pvr_img
!
      character(len=kchara) :: img_head
!
!
      call add_int_suffix(my_rank, file_param%pvr_prefix, img_head)
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
      subroutine blend_image_from_subdomains(ip_closer, npixel_local,   &
     &          rgba_part, rgba_real_part)
!
      use set_rgba_4_each_pixel
!
      integer(kind = kint), intent(in) :: ip_closer(nprocs)
      integer(kind = kint), intent(in) :: npixel_local
      real(kind = kreal), intent(inout)                                 &
     &             :: rgba_part(4,npixel_local,nprocs)
      real(kind = kreal), intent(inout)                                 &
     &             :: rgba_real_part(4,npixel_local)
!
      integer(kind = kint) :: ip, inum, ipix
!
!
!$omp workshare
      rgba_real_part(1:4,1:npixel_local) = zero
!$omp end workshare
!
!$omp parallel do private(ipix,inum,ip)
      do ipix = 1, npixel_local
        do inum = nprocs, 1, -1
          ip = ip_closer(inum)
          call composite_alpha_blending(rgba_part(1,ipix,ip),           &
     &        rgba_real_part(1,ipix))
        end do
      end do
!$omp end parallel do
!
      end subroutine blend_image_from_subdomains
!
!  ---------------------------------------------------------------------
!
      subroutine distribute_average_depth(num_pixel_xy,                 &
     &          iflag_mapped, depth_lc, ip_closer, ave_depth_gl)
!
      use quicksort
!
      integer(kind = kint), intent(in) :: num_pixel_xy
      integer(kind = kint), intent(in) :: iflag_mapped(num_pixel_xy)
      real(kind = kreal), intent(in) :: depth_lc(num_pixel_xy)
!
      integer(kind = kint), intent(inout) :: ip_closer(nprocs)
      real(kind = kreal), intent(inout) :: ave_depth_gl(nprocs)
!
      real(kind = kreal) :: old_ave_depth_lc, covered_area
      integer(kind = kint) :: ip, ipix
!
      old_ave_depth_lc = 0.0d0
      covered_area = 0.0d0
      do ipix = 1, num_pixel_xy
        covered_area = covered_area + dble(iflag_mapped(ipix))
        if(iflag_mapped(ipix) .gt. 0) then
          old_ave_depth_lc = old_ave_depth_lc + depth_lc(ipix)
        end if
      end do
      old_ave_depth_lc = old_ave_depth_lc / covered_area
!
      call MPI_Allgather(old_ave_depth_lc, ione, CALYPSO_REAL,          &
     &                   ave_depth_gl, ione, CALYPSO_REAL,              &
     &                   CALYPSO_COMM, ierr_MPI)
!
      do ip = 1, nprocs
        ip_closer(ip) = ip
      end do
!
      call quicksort_real_w_index(nprocs, ave_depth_gl, ione, nprocs,   &
     &    ip_closer)
!
      end subroutine distribute_average_depth
!
!  ---------------------------------------------------------------------
!
      subroutine distribute_segmented_images(num_pixel_xy, rgba_lc,     &
     &          istack_image, npixel_local, rgba_part)
!
      integer(kind = kint), intent(in) :: num_pixel_xy
      real(kind = kreal), intent(in) :: rgba_lc(4,num_pixel_xy)
!
      integer(kind = kint), intent(in) :: istack_image(0:nprocs)
      integer(kind = kint), intent(in) :: npixel_local
!
      real(kind = kreal), intent(inout)                                 &
     &             :: rgba_part(4,npixel_local,nprocs)
!
      integer(kind = kint) :: num, i_rank, ist, i
      integer(kind = kint) :: nneib_send, nneib_recv
!
!
!$omp workshare
      rgba_part(1:4,1:npixel_local,1:nprocs) = zero
!$omp end workshare
!
      nneib_send = 0
      do i_rank = 0, nprocs-1
        if(i_rank .eq. my_rank) cycle
          nneib_send = nneib_send + 1
          ist =          istack_image(i_rank)
          num = ifour * (istack_image(i_rank+1) - istack_image(i_rank))
          call MPI_ISEND(rgba_lc(1,ist+1), num, CALYPSO_REAL,           &
     &        i_rank, 0, CALYPSO_COMM, req1(nneib_send), ierr_MPI)
      end do
!
      nneib_recv = 0
      do i_rank = 0, nprocs-1
        if(i_rank .eq. my_rank) cycle
          nneib_recv = nneib_recv + 1
          num = ifour * npixel_local
          call MPI_IRECV(rgba_part(1,1,i_rank+1), num, CALYPSO_REAL,    &
     &        i_rank, 0, CALYPSO_COMM, req2(nneib_recv), ierr_MPI)
      end do
!
      call MPI_WAITALL (nneib_recv, req2, sta2, ierr_MPI)
      call MPI_WAITALL (nneib_send, req1, sta1, ierr_MPI)
!
      num = npixel_local
      ist = istack_image(my_rank)
!$omp parallel do
      do i = 1, num
        rgba_part(1,i,my_rank+1) = rgba_lc(1,ist+i)
        rgba_part(2,i,my_rank+1) = rgba_lc(2,ist+i)
        rgba_part(3,i,my_rank+1) = rgba_lc(3,ist+i)
        rgba_part(4,i,my_rank+1) = rgba_lc(4,ist+i)
      end do
!$omp end parallel do
!
      end subroutine distribute_segmented_images
!
!  ---------------------------------------------------------------------
!
      subroutine collect_segmented_images                               &
     &         (irank_tgt, npixel_local, istack_image,                  &
     &          num_pixel_xy, rgba_real_part, rgba_real_gl)
!
      integer(kind = kint), intent(in) :: irank_tgt
      integer(kind = kint), intent(in) :: istack_image(0:nprocs)
      integer(kind = kint), intent(in) :: npixel_local
      real(kind = kreal), intent(in) :: rgba_real_part(4,npixel_local)
      integer(kind = kint), intent(in) :: num_pixel_xy
!
      real(kind = kreal), intent(inout) :: rgba_real_gl(4,num_pixel_xy)
!
      integer(kind = kint) :: num, i_rank, ist, i
      integer(kind = kint) :: nneib_send, nneib_recv
!
!
      nneib_send = 0
      nneib_recv = 0
      num = ifour * npixel_local
      if(my_rank .ne. irank_tgt) then
        nneib_send = 1
        call MPI_ISEND(rgba_real_part(1,1), num, CALYPSO_REAL,          &
     &      irank_tgt, 0, CALYPSO_COMM, req1(1), ierr_MPI)
      end if
!
      if(my_rank .eq. irank_tgt) then
        do i_rank = 0, nprocs-1
          if(i_rank .eq. irank_tgt) cycle
!
          nneib_recv = nneib_recv + 1
          ist =          istack_image(i_rank)
          num = ifour * (istack_image(i_rank+1) - istack_image(i_rank))
          call MPI_IRECV(rgba_real_gl(1,ist+1), num, CALYPSO_REAL,      &
     &        i_rank, 0, CALYPSO_COMM, req2(nneib_recv), ierr_MPI)
        end do
      end if
!
      call MPI_WAITALL(nneib_recv, req2(1), sta2, ierr_MPI)
!
      if(my_rank .eq. irank_tgt) then
        ist = istack_image(irank_tgt)
        num = istack_image(irank_tgt+1) - ist
!$omp parallel do
        do i = 1, num
          rgba_real_gl(1,ist+i) = rgba_real_part(1,i)
          rgba_real_gl(2,ist+i) = rgba_real_part(2,i)
          rgba_real_gl(3,ist+i) = rgba_real_part(3,i)
          rgba_real_gl(4,ist+i) = rgba_real_part(4,i)
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
