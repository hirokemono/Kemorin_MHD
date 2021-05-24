!>@file   parallel_png_to_quilt.f90
!!@brief  module parallel_png_to_quilt
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in 2004
!
!>@brief  file convert from PNG to BMP
!
      program parallel_png_to_quilt
!
      use calypso_mpi
      use calypso_mpi_char
      use calypso_mpi_int
      use MPI_ascii_data_IO
      use m_calypso_mpi_IO
!
      use t_calypso_mpi_IO_param
      use t_png_file_access
      use write_bmp_image
!
      use transfer_to_long_integers
      use set_parallel_file_name
!
      character(len=kchara) :: file_prefix
!
      integer(kind = kint), parameter :: num_image =  6
      integer(kind = kint), parameter :: num_row =    2
      integer(kind = kint), parameter :: num_column = 3
      integer(kind = kint) :: nmax_image_pe, num_image_pe
      integer(kind = kint), allocatable :: icou_image_pe(:)
!
      integer(kind = kint) :: npixel_x
      integer(kind = kint) :: npixel_y
      character(len=1), allocatable :: rgb(:,:,:,:)
      character(len=1), allocatable :: bgr_line(:,:)
      character(len=1), allocatable :: gray(:,:)
!
      type(calypso_MPI_IO_params), save :: IO_param
      integer(kind = MPI_OFFSET_KIND) :: ioffset
      integer :: ilength
!
      type(buffer_4_png) :: pbuf_t
!
      integer(kind = kint) :: ntmp_x, ntmp_y
      integer(kind = kint) :: i, icou
      character(len=kchara) :: file_name
!
!
      call calypso_MPI_init
!
      if(my_rank .eq. 0) then
        write(*,*) 'Input image file prefix'
        read(*,*) file_prefix
!
!#ifdef PNG_OUTPUT
        file_name = add_int_suffix(0, file_prefix)
        call read_png_file_f(file_name, npixel_x, npixel_y, pbuf_t)
!
        if(pbuf_t%iflag_cmode .eq. iflag_rgba)                          &
     &                     write(*,*) 'RGBA image file'
        if(pbuf_t%iflag_cmode .eq. iflag_rgb)                           &
     &                     write(*,*) 'RGB image'
        if(pbuf_t%iflag_cmode .eq. iflag_ba)                            &
     &                     write(*,*) 'grayscale with alpha image'
        if(pbuf_t%iflag_cmode .eq. iflag_bw)                            &
     &                     write(*,*) 'grayscale image'
        nullify(pbuf_t%cimage_p)
      end if
!
      call calypso_mpi_bcast_character                                  &
     &   (file_prefix, cast_long(kchara), 0)
      call calypso_mpi_bcast_one_int(npixel_x, 0)
      call calypso_mpi_bcast_one_int(npixel_y, 0)
!
      nmax_image_pe = 1 + (num_image-1) / nprocs
      allocate(rgb(3,npixel_x,npixel_y,nmax_image_pe))
      allocate(bgr_line(3,npixel_x))
      allocate(icou_image_pe(nmax_image_pe))
      icou_image_pe(1:nmax_image_pe) = -1
!
      icou = 0
      do ip = 0, num_image-1
        if(mod(ip,nprocs) .eq. my_rank) then
          icou = icou + 1
          icou_image_pe(icou) = ip
          file_name = add_int_suffix(ip, file_prefix)
          call read_png_file_f(file_name, ntmp_x, ntmp_y, pbuf_t)
!
          if(pbuf_t%iflag_cmode .ge. iflag_bw) then
            allocate(gray(npixel_x,npixel_y))
!
            call copy_grayscale_from_png_f                              &
     &         (npixel_x, npixel_y, gray, pbuf_t)
!
            write(*,*) 'pixel data along with x', npixel_x, npixel_y
            do i = 1, npixel_x
              write(*,*) i, iachar(gray(i,1))
            end do
!
!$omp workshare
            rgb(1,1:ntmp_x,1:ntmp_y,icou) = gray(1:ntmp_x,1:ntmp_y)
            rgb(2,1:ntmp_x,1:ntmp_y,icou) = gray(1:ntmp_x,1:ntmp_y)
            rgb(3,1:ntmp_x,1:ntmp_y,icou) = gray(1:ntmp_x,1:ntmp_y)
!$omp end workshare
            deallocate(gray)
!   For color image
          else
            call copy_rgb_from_png_f(ntmp_x, ntmp_y,                    &
     &                               rgb(1,1,1,icou), pbuf_t)
          end if
!
          nullify(pbuf_t%cimage_p)
        end if
      end do
      num_image_pe = icou
!
      do icou = 1, num_image_pe
        ip = icou_image_pe(icou)
        write(*,*) 'Bitmap output for image: ', ip, ' from ', my_rank
        file_name = add_int_suffix(ip, file_prefix)
        call pixout_BMP(file_name, npixel_x, npixel_y, rgb(1,1,1,icou))
!
        file_name = add_int_suffix(ip, file_prefix)
        file_name = add_int_suffix(ip, file_name)
        call write_png_rgb_f                                            &
     &     (file_name, npixel_x, npixel_y, rgb(1,1,1,icou), pbuf_t)
      end do
!#endif
!
      file_name = add_bmp_suffix(file_prefix)
      if(my_rank .eq. 0) write(*,*) 'Write Quilt Bitmap: ',             &
     &                  trim(file_name)
      call open_write_mpi_file(file_name, IO_param)
      call mpi_write_charahead(IO_param, 54,                            &
     &    BMP_header(int(num_row*npixel_x), int(num_column*npixel_y)))
!
      do icou = 1, num_image_pe
        ip = icou_image_pe(icou)
        ix = mod(ip,num_row)
        iy = ip / num_row
        ilength = 3*int(npixel_x)
        do j = 1, npixel_y
          bgr_line(1,1:npixel_x) = rgb(3,1:npixel_x,j,icou)
          bgr_line(2,1:npixel_x) = rgb(2,1:npixel_x,j,icou)
          bgr_line(3,1:npixel_x) = rgb(1,1:npixel_x,j,icou)
!
          ioffset = IO_param%ioff_gl                                   &
     &             + ilength * (ix + num_row * ((j-1) + iy*npixel_y))
          call mpi_write_one_chara_b                                   &
     &       (IO_param%id_file, ioffset, ilength, bgr_line(1,1))
        end do
      end do
      call close_mpi_file(IO_param)
      call calypso_MPI_barrier
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program parallel_png_to_quilt
