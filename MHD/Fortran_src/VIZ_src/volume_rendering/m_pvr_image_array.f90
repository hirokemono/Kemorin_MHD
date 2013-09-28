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
      use m_parallel_var_dof
!
      implicit  none
!
      integer(kind = kint) :: nmax_pixel, num_pixel_xy
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
      if(my_rank .eq. 0) then
        allocate(rgb_chara_gl(3,nmax_pixel))
        allocate(rgba_chara_gl(4,nmax_pixel))
!
        allocate(rgba_left_gl(4,nmax_pixel))
        allocate(rgba_right_gl(4,nmax_pixel))
!
        allocate(rgba_real_gl(4,nmax_pixel))
!
        allocate(ave_depth_gl(nprocs))
        allocate(ip_farther(nprocs))
!
        rgba_real_gl =  0.0d0
        rgba_left_gl =  0.0d0
        rgba_right_gl = 0.0d0
!
        ave_depth_gl = 0.0d0
        ip_farther = -1
!
        allocate(rgba_recv(4*nmax_pixel,nprocs))
        allocate(ave_depth_recv(nprocs))
        rgba_recv =      0.0d0
        ave_depth_recv = 0.0d0
      end if
!
      allocate(iflag_mapped(nmax_pixel))
      allocate(rgba_lc(4,nmax_pixel))
      allocate(rgb_chara_lc(3,nmax_pixel))
      allocate(depth_lc(nmax_pixel))
      iflag_mapped = 0
      rgba_lc = 0.0d0
      depth_lc = 0.0d0
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
        deallocate(ave_depth_gl, ave_depth_recv)
      end if
!
      deallocate(rgba_lc,rgb_chara_lc)
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
!
      integer(kind = kint), intent(in) :: i_pvr
!
      integer(kind = kint) :: num, ip, inum, ipix
!
!
      num_pixel_xy = n_pvr_pixel(1,i_pvr)*n_pvr_pixel(2,i_pvr)
!
      covered_area = 0.0d0
      do ipix = 1, num_pixel_xy
        covered_area = covered_area + dble(iflag_mapped(ipix))
        if(iflag_mapped(ipix) .gt. 0) then
          ave_depth_lc = ave_depth_lc + depth_lc(ipix)
        end if
      end do
      ave_depth_lc = ave_depth_lc / covered_area
!
      num = 4*num_pixel_xy
      call MPI_Gather(rgba_lc(1,1), num, MPI_DOUBLE_PRECISION,          &
     &                rgba_recv(1,1), num, MPI_DOUBLE_PRECISION,        &
     &                izero, SOLVER_COMM, ierr)
!
      call MPI_Gather(ave_depth_lc, ione, MPI_DOUBLE_PRECISION,         &
     &                ave_depth_gl(1), ione, MPI_DOUBLE_PRECISION,      &
     &                izero, SOLVER_COMM, ierr)
!
      if(my_rank .eq. 0) then
        do ip = 1, nprocs
          ip_farther(ip) = ip
        end do
!
        call quicksort_real_w_index(nprocs, ave_depth_gl, ione, nprocs, &
     &      ip_farther)
!
!
        rgba_real_gl = 0.0d0
!$omp parallel do private(ipix,inum,ip)
        do ipix = 1, num_pixel_xy
          do inum = 1, nprocs
            ip = ip_farther(inum)
            call alpha_blending(rgba_recv(4*ipix-3,ip),                 &
     &          rgba_real_gl(1,ipix))
          end do
        end do
!$omp end parallel do
!
        call set_pvr_colorbar(i_pvr, num_pixel_xy, rgba_real_gl(1,1))
      end if
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
