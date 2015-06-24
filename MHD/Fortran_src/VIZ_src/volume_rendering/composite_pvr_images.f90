!
!      module composite_pvr_images
!
!       Programmed by H. Matsui
!
!      subroutine allocate_pvr_image_array
!      subroutine deallocate_pvr_image_array
!
!      subroutine cvt_double_rgba_to_char_rgb(num_pixel, rgba, crgb)
!      subroutine cvt_double_rgba_to_char_rgba(num_pixel, rgba, crgba)
!!      subroutine sel_write_pvr_image_file                             &
!!     &         (i_pvr, i_rot, istep_pvr,  n_pvr_pixel, num_pixel_xy,  &
!!     &          rgba_real_gl, rgba_chara_gl, rgb_chara_gl)
!!      subroutine sel_write_pvr_local_img                              &
!!     &         (i_pvr, n_pvr_pixel, num_pixel_xy, rgba_lc, rgb_chara)
!
      module composite_pvr_images
!
      use m_precision
!
      use calypso_mpi
      use m_constants
!
      implicit  none
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
      subroutine blend_image_over_domains(i_pvr, istack_image,          &
     &          n_pvr_pixel, num_pixel_xy, iflag_mapped,                &
     &          depth_lc, rgba_lc, rgba_real_gl)
!
      use m_control_params_4_pvr
      use quicksort
      use set_rgba_4_each_pixel
      use draw_pvr_colorbar
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: i_pvr
!
      integer(kind = kint), intent(in) :: n_pvr_pixel(2)
!
      integer(kind = kint), intent(in) :: iflag_mapped(num_pixel_xy)
      real(kind = kreal), intent(in) :: rgba_lc(4,num_pixel_xy)
      real(kind = kreal), intent(in) :: depth_lc(num_pixel_xy)
      integer(kind = kint), intent(in) :: num_pixel_xy
!
      integer(kind = kint), intent(inout) :: istack_image(0:nprocs)
      real(kind = kreal), intent(inout) :: rgba_real_gl(4,num_pixel_xy)
      integer(kind = kint) :: npixel_local
!
      integer(kind = kint), allocatable :: ip_farther(:)
      real(kind = kreal), allocatable :: ave_depth_gl(:)
      real(kind = kreal) :: ave_depth_lc, covered_area
      real(kind = kreal), allocatable :: rgba_part(:,:,:)
      real(kind = kreal), allocatable :: rgba_real_part(:,:)
!
      integer(kind = kint) :: num, ip, inum, ipix, max_smp, ist
      integer(kind = kint) :: nneib_recv
!
!
      call alloc_pvr_image_comm_status
!
      allocate(ave_depth_gl(nprocs))
      allocate(ip_farther(nprocs))
      ave_depth_gl = 0.0d0
      ip_farther = -1
!
      call count_number_4_smp(nprocs, ione, num_pixel_xy,               &
     &    istack_image, max_smp)
      npixel_local = istack_image(my_rank+1) - istack_image(my_rank)
!
      allocate(rgba_part(4,npixel_local,nprocs))
      allocate(rgba_real_part(4,npixel_local))
!
!$omp workshare
      rgba_part(1:4,1:npixel_local,1:nprocs) = zero
!$omp end workshare
!
! -- Set Average depth for each subdomain
      ave_depth_lc = 0.0d0
      covered_area = 0.0d0
      do ipix = 1, num_pixel_xy
        covered_area = covered_area + dble(iflag_mapped(ipix))
        if(iflag_mapped(ipix) .gt. 0) then
          ave_depth_lc = ave_depth_lc + depth_lc(ipix)
        end if
      end do
      ave_depth_lc = ave_depth_lc / covered_area
!
      call MPI_Allgather(ave_depth_lc, ione, CALYPSO_REAL,              &
     &                   ave_depth_gl, ione, CALYPSO_REAL,              &
     &                   CALYPSO_COMM, ierr_MPI)
!
      do ip = 1, nprocs
        ip_farther(ip) = ip
      end do
!
      call quicksort_real_w_index(nprocs, ave_depth_gl, ione, nprocs,   &
     &    ip_farther)
!
! Distribute image
!
      do ip = 1, nprocs
        ist =          istack_image(ip-1)
        num = ifour * (istack_image(ip) - istack_image(ip-1))
        call MPI_ISEND(rgba_lc(1,ist+1), num, CALYPSO_REAL,             &
     &      (ip-1), 0, CALYPSO_COMM, req1(ip), ierr_MPI)
      end do
!
      do ip = 1, nprocs
        num = ifour * npixel_local
        call MPI_IRECV(rgba_part(1,1,ip), num, CALYPSO_REAL,            &
     &      (ip-1), 0, CALYPSO_COMM, req2(ip), ierr_MPI)
      end do
!
      call MPI_WAITALL (nprocs, req2, sta2, ierr_MPI)
      call MPI_WAITALL (nprocs, req1, sta1, ierr_MPI)
!
!  Alpha blending
!
!$omp workshare
      rgba_real_part(1:4,1:npixel_local) = zero
!$omp end workshare
!
!$omp parallel do private(ipix,inum,ip)
      do ipix = 1, npixel_local
        do inum = 1, nprocs
          ip = ip_farther(inum)
          call composite_alpha_blending(rgba_part(1,ipix,ip),           &
     &        rgba_real_part(1,ipix))
        end do
      end do
!$omp end parallel do
!
!  Collect image to rank 0
!
      nneib_recv = 0
      num = ifour * npixel_local
      call MPI_ISEND(rgba_real_part(1,1), num, CALYPSO_REAL,            &
     &    izero, 0, CALYPSO_COMM, req1(1), ierr_MPI)
!
      if(my_rank .eq. 0) then
        nneib_recv = nprocs
        do ip = 1, nprocs
          ist =          istack_image(ip-1)
          num = ifour * (istack_image(ip) - istack_image(ip-1))
          call MPI_IRECV(rgba_real_gl(1,ist+1), num, CALYPSO_REAL,      &
     &       (ip-1), 0, CALYPSO_COMM, req2(ip), ierr_MPI)
        end do
      end if
!
      call MPI_WAITALL(nneib_recv, req2, sta2, ierr_MPI)
!
      if(my_rank .eq. 0) then
        call set_pvr_colorbar(i_pvr, num_pixel_xy,                      &
     &      n_pvr_pixel, color_params(i_pvr), rgba_real_gl(1,1))
      end if
      call MPI_WAITALL (ione, req1(1), sta1, ierr_MPI)
!
      deallocate(rgba_real_part, rgba_part)
      deallocate(ave_depth_gl, ip_farther)
!
      end subroutine blend_image_over_domains
!
!  ---------------------------------------------------------------------
!
      subroutine sel_write_pvr_image_file                               &
     &         (i_pvr, i_rot, istep_pvr, n_pvr_pixel, num_pixel_xy,     &
     &          rgba_real_gl, rgba_chara_gl, rgb_chara_gl)
!
      use m_control_params_4_pvr
      use output_image_sel_4_png
      use set_parallel_file_name
      use convert_real_rgb_2_bite
!
      integer(kind = kint), intent(in) :: i_pvr, i_rot, istep_pvr
      integer(kind = kint), intent(in) :: num_pixel_xy
      integer(kind = kint), intent(in) :: n_pvr_pixel(2)
      real(kind = kreal), intent(in) :: rgba_real_gl(4,num_pixel_xy)
      character(len = 1), intent(inout) :: rgba_chara_gl(4,num_pixel_xy)
      character(len = 1), intent(inout) :: rgb_chara_gl(3,num_pixel_xy)
!
      character(len=kchara) :: tmpchara, img_head_tmp
!
      if(my_rank .ne. 0) return
      if(i_rot .gt. 0) then
          call add_int_suffix(istep_pvr, pvr_header(i_pvr),             &
     &            tmpchara)
          call add_int_suffix(i_rot, tmpchara, img_head_tmp)
      else
          call add_int_suffix(istep_pvr, pvr_header(i_pvr),             &
     &            img_head_tmp)
      end if
!
      if(id_pvr_transparent(i_pvr) .eq. 1) then
          call cvt_double_rgba_to_char_rgba(num_pixel_xy,               &
     &             rgba_real_gl(1,1),  rgba_chara_gl(1,1) )
          call sel_rgba_image_file(id_pvr_file_type(i_pvr),             &
     &        img_head_tmp, n_pvr_pixel(1), n_pvr_pixel(2),             &
     &        rgba_chara_gl(1,1) )
      else
          call cvt_double_rgba_to_char_rgb(num_pixel_xy,                &
     &             rgba_real_gl(1,1),  rgb_chara_gl(1,1) )
          call sel_output_image_file(id_pvr_file_type(i_pvr),           &
     &        img_head_tmp, n_pvr_pixel(1), n_pvr_pixel(2),             &
     &        rgb_chara_gl(1,1) )
      end if
!
      end subroutine sel_write_pvr_image_file
!
!  ---------------------------------------------------------------------
!
      subroutine sel_write_pvr_local_img                                &
     &         (i_pvr, n_pvr_pixel, num_pixel_xy, rgba_lc, rgb_chara)
!
      use m_control_params_4_pvr
      use output_image_sel_4_png
      use set_parallel_file_name
      use convert_real_rgb_2_bite
!
      integer(kind = kint), intent(in) :: i_pvr
      integer(kind = kint), intent(in) :: num_pixel_xy
      integer(kind = kint), intent(in) :: n_pvr_pixel(2)
      real(kind = kreal), intent(inout) :: rgba_lc(4,num_pixel_xy)
      character(len = 1), intent(inout) :: rgb_chara(3,num_pixel_xy)
!
      character(len=kchara) :: img_head_tmp
!
      call cvt_double_rgba_to_char_rgb(num_pixel_xy, rgba_lc(1,1),      &
     &    rgb_chara(1,1))
!
      write(img_head_tmp,'(a,i1)')  'img_tmp.', my_rank
      call sel_output_image_file(id_pvr_file_type(i_pvr),              &
     &    img_head_tmp, n_pvr_pixel(1), n_pvr_pixel(2), rgb_chara)
!
      end subroutine sel_write_pvr_local_img
!
!  ---------------------------------------------------------------------
!
      end module composite_pvr_images
