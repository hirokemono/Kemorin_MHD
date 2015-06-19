!
!      module m_pvr_image_array
!
!       Programmed by H. Matsui
!
!      subroutine allocate_pvr_image_array
!      subroutine deallocate_pvr_image_array
!
!      subroutine blend_pvr_over_domains(i_pvr, n_pvr_pixel)
!      subroutine cvt_double_rgba_to_char_rgb(num_pixel, rgba, crgb)
!      subroutine cvt_double_rgba_to_char_rgba(num_pixel, rgba, crgba)
!
      module m_pvr_image_array
!
      use m_precision
!
      use calypso_mpi
      use m_constants
!
      implicit  none
!
      integer(kind = kint) :: nmax_pixel, num_pixel_xy
!
      integer(kind = kint), allocatable :: istack_image(:)
!
      real(kind = kreal), allocatable :: rgba_real_gl(:,:)
      real(kind = kreal), allocatable :: rgba_left_gl(:,:)
      real(kind = kreal), allocatable :: rgba_right_gl(:,:)
!
      character(len = 1), allocatable :: rgb_chara_gl(:,:)
      character(len = 1), allocatable :: rgba_chara_gl(:,:)
!
      real(kind = kreal), allocatable :: rgba_recv(:,:)
      real(kind = kreal), allocatable :: rgba_lc(:,:)
      character(len = 1), allocatable :: rgb_chara_lc(:,:)
      integer(kind = kint), allocatable :: ip_farther(:)
!
      integer(kind = kint), allocatable :: iflag_mapped(:)
      real(kind = kreal), allocatable :: depth_lc(:)
      real(kind = kreal), allocatable :: ave_depth_gl(:)
      real(kind = kreal), allocatable :: ave_depth_recv(:)
      real(kind = kreal) :: ave_depth_lc, covered_area
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
      subroutine allocate_pvr_image_array
!
      use m_control_params_4_pvr
!
      integer(kind = kint) :: num, i_pvr
!
!
      nmax_pixel = n_pvr_pixel(1,1)*n_pvr_pixel(2,1)
      nmax_pixel = n_pvr_pixel(1,1)*n_pvr_pixel(2,1)
      do i_pvr = 2, num_pvr
        num = n_pvr_pixel(1,i_pvr)*n_pvr_pixel(2,i_pvr)
        nmax_pixel = max(nmax_pixel,num)
      end do
!
      allocate(ave_depth_gl(nprocs))
      allocate(ip_farther(nprocs))
      ave_depth_gl = 0.0d0
      ip_farther = -1
!
      if(my_rank .eq. 0) then
        allocate(rgb_chara_gl(3,nmax_pixel))
        allocate(rgba_chara_gl(4,nmax_pixel))
!
        allocate(rgba_left_gl(4,nmax_pixel))
        allocate(rgba_right_gl(4,nmax_pixel))
!
        allocate(rgba_real_gl(4,nmax_pixel))
!
!
        rgba_real_gl =  0.0d0
        rgba_left_gl =  0.0d0
        rgba_right_gl = 0.0d0
!
!
        allocate(rgba_recv(4*nmax_pixel,nprocs))
        allocate(ave_depth_recv(nprocs))
        rgba_recv =      0.0d0
        ave_depth_recv = 0.0d0
      end if
!
      allocate(istack_image(0:nprocs))
!
      allocate(iflag_mapped(nmax_pixel))
      allocate(rgba_lc(4,nmax_pixel))
      allocate(rgb_chara_lc(3,nmax_pixel))
      allocate(depth_lc(nmax_pixel))
      iflag_mapped = 0
      rgba_lc = 0.0d0
      depth_lc = 0.0d0
!
      allocate (sta1(MPI_STATUS_SIZE,nprocs))
      allocate (req1(nprocs))
      allocate (sta2(MPI_STATUS_SIZE,nprocs))
      allocate (req2(nprocs))
!
      end subroutine allocate_pvr_image_array
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_pvr_image_array
!
!
      if(my_rank .eq. 0) then
        deallocate(rgb_chara_gl, rgba_chara_gl)
        deallocate(rgba_left_gl, rgba_right_gl)
        deallocate(rgba_real_gl, rgba_recv)
        deallocate(ave_depth_recv)
      end if
!
      deallocate (sta1, req1, sta2, req2)
!
      deallocate(ave_depth_gl, ip_farther)
      deallocate(istack_image)
      deallocate(rgba_lc,rgb_chara_lc)
      deallocate(iflag_mapped,depth_lc)
!
      end subroutine deallocate_pvr_image_array
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine blend_pvr_over_domains(i_pvr)
!
      use m_control_params_4_pvr
      use quicksort
      use set_rgba_4_each_pixel
      use draw_pvr_colorbar
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: i_pvr
!
      integer(kind = kint) :: npixel_local
      real(kind = kreal), allocatable :: rgba_part(:,:,:)
      real(kind = kreal), allocatable :: rgba_real_part(:,:)
!
      integer(kind = kint) :: num, ip, inum, ipix, max_smp, ist
      integer(kind = kint) :: nneib_recv
!
!
      num_pixel_xy = n_pvr_pixel(1,i_pvr)*n_pvr_pixel(2,i_pvr)
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
        call set_pvr_colorbar(i_pvr, num_pixel_xy, rgba_real_gl(1,1))
      end if
      call MPI_WAITALL (ione, req1(1), sta1, ierr_MPI)
!
      deallocate(rgba_real_part, rgba_part)
!
      end subroutine blend_pvr_over_domains
!
!  ---------------------------------------------------------------------
!
      subroutine write_pvr_image_file(i_pvr, i_rot, istep_pvr)
!
      use m_control_params_4_pvr
      use output_image_sel_4_png
      use set_parallel_file_name
      use convert_real_rgb_2_bite
!
      character(len=kchara) :: tmpchara, img_head_tmp
      integer(kind = kint), intent(in) :: i_pvr, i_rot, istep_pvr
!
      if(my_rank .eq. 0) then
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
     &        img_head_tmp, n_pvr_pixel(1,i_pvr), n_pvr_pixel(2,1),     &
     &        rgba_chara_gl(1,1) )
        else
          call cvt_double_rgba_to_char_rgb(num_pixel_xy,                &
     &             rgba_real_gl(1,1),  rgb_chara_gl(1,1) )
          call sel_output_image_file(id_pvr_file_type(i_pvr),           &
     &        img_head_tmp, n_pvr_pixel(1,i_pvr), n_pvr_pixel(2,1),     &
     &             rgb_chara_gl(1,1) )
        end if
!
      end if
!
      end subroutine write_pvr_image_file
!
!  ---------------------------------------------------------------------
!
      end module m_pvr_image_array
