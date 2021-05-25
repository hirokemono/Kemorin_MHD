!>@file   t_MPI_quilt_bitmap_IO.f90
!!@brief  module t_MPI_quilt_bitmap_IO
!!
!!@author H. Matsui
!!@date Programmed on May., 2021
!!
!>@brief Quilt format bitmap data IO with MPI-IO
!!
!!@verbatim
!!      subroutine init_quilt_rgb_images                                &
!!     &         (nimage_x, nimage_y, npixel_x, npixel_y, quilt_d)
!!        integer(kind = kint), intent(in) :: nimage_x, nimage_y
!!        integer(kind = kint), intent(in) :: npixel_x, npixel_y
!!        type(MPI_quilt_bitmap_IO), intent(inout) :: quilt_d
!!      subroutine sel_write_pvr_image_files                            &
!!     &         (id_file_type, file_prefix, quilt_d)
!!        integer(kind = kint), intent(in) :: id_file_type
!!        character(len=kchara), intent(in) :: file_prefix
!!        type(MPI_quilt_bitmap_IO), intent(in) :: quilt_d
!!      subroutine alloc_quilt_rgb_images(npixel_x, npixel_y, quilt_d)
!!      subroutine dealloc_quilt_rgb_images(quilt_d)
!!        integer(kind = kint), intent(in) :: npixel_x, npixel_y
!!        type(MPI_quilt_bitmap_IO), intent(inout) :: quilt_d
!!
!!      subroutine mpi_write_quilt_BMP_file(file_prefix, n_column,      &
!!     &                                    num_image_lc, icou_each_pe  &
!!     &                                    npixel_x, npixel_y, images)
!!        character(len=kchara), intent(in) :: file_prefix
!!        integer(kind = kint), intent(in) :: n_column(2)
!!        integer(kind = kint), intent(in) :: num_image_lc
!!        integer(kind = kint), intent(in) :: icou_each_pe(num_image_lc)
!!        integer(kind = kint), intent(in) :: npixel_x, npixel_y
!!        type(each_rgb_image), intent(in) :: images(num_image_lc)
!!@endverbatim
!
      module t_MPI_quilt_bitmap_IO
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      implicit none
!
      type each_rgb_image
        character(len=1), allocatable :: rgb(:,:,:)
      end type each_rgb_image
!
      type MPI_quilt_bitmap_IO
!>        Number of images
        integer(kind = kint) :: n_image
!>        Number of row and columns of images
        integer(kind = kint) :: n_column(2)
!
!>        Number of images in each process
        integer(kind = kint) :: num_image_lc
!>        Horizontal number of pixel
        integer(kind = kint) :: npixel_x
!>        Vertical number of pixel
        integer(kind = kint) :: npixel_y
!>        RGB images
        type(each_rgb_image), allocatable :: images(:)
!>        Image index in each process
        integer(kind = kint), allocatable :: icou_each_pe(:)
      end type MPI_quilt_bitmap_IO
!
     private :: count_local_image_pe_quilt, set_local_image_pe_quilt
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_quilt_rgb_images                                  &
     &         (nimage_x, nimage_y, npixel_x, npixel_y, quilt_d)
!
      integer(kind = kint), intent(in) :: nimage_x, nimage_y
      integer(kind = kint), intent(in) :: npixel_x, npixel_y
!
      type(MPI_quilt_bitmap_IO), intent(inout) :: quilt_d
!
!
      quilt_d%n_column(1) = nimage_x
      quilt_d%n_column(2) = nimage_y
      quilt_d%n_image = nimage_x * nimage_y
      call count_local_image_pe_quilt                                   &
     &   (quilt_d%n_image, quilt_d%num_image_lc)
!
      call alloc_quilt_rgb_images(npixel_x, npixel_y, quilt_d)
      call set_local_image_pe_quilt(quilt_d%n_image,                    &
     &          quilt_d%num_image_lc, quilt_d%icou_each_pe)
!
      end subroutine init_quilt_rgb_images
!
! ----------------------------------------------------------------------
!
      subroutine sel_write_pvr_image_files                              &
     &         (id_file_type, file_prefix, quilt_d)
!
      use output_image_sel_4_png
!
      integer(kind = kint), intent(in) :: id_file_type
      character(len=kchara), intent(in) :: file_prefix
      type(MPI_quilt_bitmap_IO), intent(in) :: quilt_d
!
!
      if(id_file_type .eq. iflag_QUILT_BMP) then
        call mpi_write_quilt_BMP_file(file_prefix, quilt_d%n_column,    &
     &      quilt_d%num_image_lc, quilt_d%icou_each_pe,                 &
     &      quilt_d%npixel_x, quilt_d%npixel_y, quilt_d%images)
      else
        call sel_write_seq_image_files(id_file_type, file_prefix,       &
     &      quilt_d%num_image_lc, quilt_d%icou_each_pe,                 &
     &      quilt_d%npixel_x, quilt_d%npixel_y, quilt_d%images)
      end if
!
      end subroutine sel_write_pvr_image_files
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_quilt_rgb_images(quilt_d)
!
      type(MPI_quilt_bitmap_IO), intent(inout) :: quilt_d
      integer(kind = kint) :: i
!
      do i = 1, quilt_d%num_image_lc
        deallocate(quilt_d%images(i)%rgb)
      end do
      deallocate(quilt_d%images, quilt_d%icou_each_pe)
!
      end subroutine dealloc_quilt_rgb_images
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_quilt_rgb_images(npixel_x, npixel_y, quilt_d)
!
      integer(kind = kint), intent(in) :: npixel_x, npixel_y
      type(MPI_quilt_bitmap_IO), intent(inout) :: quilt_d
      integer(kind = kint) :: i
!
      quilt_d%npixel_x = npixel_x
      quilt_d%npixel_y = npixel_y
!
      allocate(quilt_d%icou_each_pe(quilt_d%num_image_lc))
      allocate(quilt_d%images(quilt_d%num_image_lc))
!
      do i = 1, quilt_d%num_image_lc
        allocate(quilt_d%images(i)%rgb(3,npixel_x,npixel_y))
      end do
!
      end subroutine alloc_quilt_rgb_images
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine count_local_image_pe_quilt(num_image, num_image_lc)
!
      integer(kind = kint), intent(in) :: num_image
      integer(kind = kint), intent(inout) :: num_image_lc
!
      integer(kind = kint) :: icou, ip
!
      icou = 0
      do ip = 0, num_image-1
        if(mod(ip,nprocs) .eq. my_rank) icou = icou + 1
      end do
      num_image_lc = icou
!
      end subroutine count_local_image_pe_quilt
!
! ----------------------------------------------------------------------
!
      subroutine set_local_image_pe_quilt                               &
     &         (num_image, num_image_lc, icou_each_pe)
!
      integer(kind = kint), intent(in) :: num_image
      integer(kind = kint), intent(in) :: num_image_lc
      integer(kind = kint), intent(inout)                               &
     &                     :: icou_each_pe(num_image_lc)
!
      integer(kind = kint) :: icou, ip
!
      icou = 0
      do ip = 0, num_image-1
        if(mod(ip,nprocs) .eq. my_rank) then
          icou = icou + 1
          icou_each_pe(icou) = ip
        end if
      end do
!
      end subroutine set_local_image_pe_quilt
!
! ----------------------------------------------------------------------
!
      subroutine mpi_write_quilt_BMP_file(file_prefix, n_column,        &
     &                                    num_image_lc, icou_each_pe,   &
     &                                    npixel_x, npixel_y, images)
!
      use m_calypso_mpi_IO
      use MPI_ascii_data_IO
      use t_calypso_mpi_IO_param
      use write_bmp_image
!
      character(len=kchara), intent(in) :: file_prefix
      integer(kind = kint), intent(in) :: n_column(2)
      integer(kind = kint), intent(in) :: num_image_lc
      integer(kind = kint), intent(in)  :: icou_each_pe(num_image_lc)
!
      integer(kind = kint), intent(in) :: npixel_x, npixel_y
      type(each_rgb_image), intent(in) :: images(num_image_lc)
!
      character(len=1), allocatable :: bgr_line(:,:)
!
      type(calypso_MPI_IO_params), save :: IO_param
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer :: ilength
!
      integer(kind = kint) :: icou, ix, iy, ip, j
      character(len=kchara) :: file_name
      integer :: ntot_pixel_x, ntot_pixel_y
!
      ntot_pixel_x = int(n_column(1)*npixel_x)
      ntot_pixel_y = int(n_column(2)*npixel_y)
      allocate(bgr_line(3,npixel_x))
!
      file_name = add_bmp_suffix(file_prefix)
      if(my_rank .eq. 0) write(*,*) 'Write Quilt Bitmap: ',             &
     &                  trim(file_name)
      call open_write_mpi_file(file_name, IO_param)
      call mpi_write_charahead(IO_param, 54,                            &
     &    BMP_header(ntot_pixel_x, ntot_pixel_y))
!
      do icou = 1, num_image_lc
        ip = icou_each_pe(icou)
        ix = mod(ip,n_column(1))
        iy = ip / n_column(1)
        ilength = 3*int(npixel_x)
        do j = 1, npixel_y
          bgr_line(1,1:npixel_x) = images(icou)%rgb(3,1:npixel_x,j)
          bgr_line(2,1:npixel_x) = images(icou)%rgb(2,1:npixel_x,j)
          bgr_line(3,1:npixel_x) = images(icou)%rgb(1,1:npixel_x,j)
!
          ioffset = IO_param%ioff_gl                                   &
     &          + ilength * (ix + n_column(1) * ((j-1) + iy*npixel_y))
          call mpi_write_one_chara_b                                   &
     &       (IO_param%id_file, ioffset, ilength, bgr_line(1,1))
        end do
      end do
      call close_mpi_file(IO_param)
      call calypso_MPI_barrier
!
      deallocate(bgr_line)
!
      end subroutine mpi_write_quilt_BMP_file
!
! ----------------------------------------------------------------------
!
      subroutine sel_write_seq_image_files(id_file_type, file_prefix,   &
     &                                     num_image_lc, icou_each_pe,  &
     &                                     npixel_x, npixel_y, images)
!
      use set_parallel_file_name
      use output_image_sel_4_png
!
      integer(kind = kint), intent(in) :: id_file_type
      character(len=kchara), intent(in) :: file_prefix
      integer(kind = kint), intent(in) :: num_image_lc
      integer(kind = kint), intent(in)  :: icou_each_pe(num_image_lc)
!
      integer(kind = kint), intent(in) :: npixel_x, npixel_y
      type(each_rgb_image), intent(in) :: images(num_image_lc)
!
      integer(kind = kint) :: icou, ip
      character(len=kchara) :: fname_tmp
!
      do icou = 1, num_image_lc
        ip = icou_each_pe(icou) + 1
        fname_tmp = add_int_suffix(ip, file_prefix)
        write(*,*) ip, '-th output file from process', my_rank
        call sel_output_image_file(id_file_type, fname_tmp,             &
     &      npixel_x, npixel_y, images(icou)%rgb)
      end do
!
      end subroutine sel_write_seq_image_files
!
! ----------------------------------------------------------------------
!
      end module t_MPI_quilt_bitmap_IO
