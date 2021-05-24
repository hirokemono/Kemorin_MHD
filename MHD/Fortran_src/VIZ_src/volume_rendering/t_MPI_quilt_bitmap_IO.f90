!>@file   t_MPI_quilt_bitmap_IO.f90
!!@brief  module t_MPI_quilt_bitmap_IO
!!
!!@author H. Matsui
!!@date Programmed on May., 2021
!!
!>@brief Quilt format bitmap data IO with MPI-IO
!!
!!@verbatim
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
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine count_local_image_4_quilt(num_image,                   &
     &          nmax_image_lc, num_image_lc, icou_image_pe)
!
      integer(kind = kint), intent(in) :: num_image
      integer(kind = kint), intent(inout) :: nmax_image_lc
      integer(kind = kint), intent(inout) :: num_image_lc
      integer(kind = kint), intent(inout)                               &
     &                     :: icou_image_pe(nmax_image_lc)
!
      integer(kind = kint) :: icou, ip
!
      icou = 0
      do ip = 0, num_image-1
        if(mod(ip,nprocs) .eq. my_rank) then
          icou = icou + 1
          icou_image_pe(icou) = ip
        end if
      end do
      num_image_lc = icou
!
      end subroutine count_local_image_4_quilt
!
! ----------------------------------------------------------------------
!
      subroutine mpi_write_quilt_bitmap(file_prefix, nrow, ncolumn,     &
     &          nmax_image_lc, num_image_lc, icou_image_pe,             &
     &          npixel_x, npixel_y, rgb)
!
      use m_calypso_mpi_IO
      use MPI_ascii_data_IO
      use t_calypso_mpi_IO_param
      use write_bmp_image
!
      implicit none
!
      character(len=kchara), intent(in) :: file_prefix
      integer(kind = kint), intent(in) :: nrow, ncolumn
      integer(kind = kint), intent(in) :: nmax_image_lc, num_image_lc
      integer(kind = kint), intent(in)  :: icou_image_pe(nmax_image_lc)
!
      integer(kind = kint), intent(in) :: npixel_x
      integer(kind = kint), intent(in) :: npixel_y
      character(len=1), intent(in)                                      &
     &                 :: rgb(3,npixel_x,npixel_y,nmax_image_lc)
!
      character(len=1), allocatable :: bgr_line(:,:)
!
      type(calypso_MPI_IO_params), save :: IO_param
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer :: ilength
!
      integer(kind = kint) :: icou, ix, iy, ip, j
      character(len=kchara) :: file_name
!
!
      allocate(bgr_line(3,npixel_x))
!
      file_name = add_bmp_suffix(file_prefix)
      if(my_rank .eq. 0) write(*,*) 'Write Quilt Bitmap: ',             &
     &                  trim(file_name)
      call open_write_mpi_file(file_name, IO_param)
      call mpi_write_charahead(IO_param, 54,                            &
     &    BMP_header(int(nrow*npixel_x), int(ncolumn*npixel_y)))
!
      do icou = 1, num_image_lc
        ip = icou_image_pe(icou)
        ix = mod(ip,nrow)
        iy = ip / nrow
        ilength = 3*int(npixel_x)
        do j = 1, npixel_y
          bgr_line(1,1:npixel_x) = rgb(3,1:npixel_x,j,icou)
          bgr_line(2,1:npixel_x) = rgb(2,1:npixel_x,j,icou)
          bgr_line(3,1:npixel_x) = rgb(1,1:npixel_x,j,icou)
!
          ioffset = IO_param%ioff_gl                                   &
     &             + ilength * (ix + nrow * ((j-1) + iy*npixel_y))
          call mpi_write_one_chara_b                                   &
     &       (IO_param%id_file, ioffset, ilength, bgr_line(1,1))
        end do
      end do
      call close_mpi_file(IO_param)
      call calypso_MPI_barrier
!
      deallocate(bgr_line)
!
      end subroutine mpi_write_quilt_bitmap
!
! ----------------------------------------------------------------------
!
      end module t_MPI_quilt_bitmap_IO
