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
      use m_precision
      use calypso_mpi
      use calypso_mpi_char
      use calypso_mpi_int
      use MPI_ascii_data_IO
      use m_calypso_mpi_IO
!
      use t_calypso_mpi_IO_param
      use t_png_file_access
      use t_MPI_quilt_bitmap_IO
      use write_bmp_image
      use output_image_sel_4_png
!
      use transfer_to_long_integers
      use set_parallel_file_name
      use mpi_write_quilt_BMP_file
!
      implicit none
!
      character(len=kchara) :: file_prefix
      integer(kind = kint) :: iflag_gz = 0
!
      integer(kind = kint), parameter :: num_image =  6
      integer(kind = kint), parameter :: nimage_xy(2) = (/2,3/)
      type(MPI_quilt_bitmap_IO) :: quilt_d1
!
      integer(kind = kint) :: npixel_xy(2)
      character(len=1), allocatable :: gray(:,:)
!
      type(buffer_4_png) :: pbuf_t
!
      integer(kind = kint) :: ntmp_x, ntmp_y
      integer(kind = kint) :: icou, ix, iy, ip, i
      character(len=kchara) :: file_name, file_tmp
!
!
      call calypso_MPI_init
!
      if(my_rank .eq. 0) then
        write(*,*) 'Input image file prefix'
        read(*,*) file_prefix
        write(*,*) 'Select compress (1:On, 0: Off)'
        read(*,*) iflag_gz
!
!#ifdef PNG_OUTPUT
        file_name = add_int_suffix(0, file_prefix)
        call read_png_file_f                                            &
     &     (file_name, npixel_xy(1), npixel_xy(2), pbuf_t)
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
      call calypso_mpi_bcast_one_int(npixel_xy(1), 0)
      call calypso_mpi_bcast_one_int(npixel_xy(2), 0)
!
      write(file_tmp,'(2a)') trim(file_prefix), '_quilt'
      call init_quilt_rgb_images                                        &
     &   (file_tmp, iflag_gz, nimage_xy, npixel_xy, quilt_d1)
!
      icou = 0
      do ip = 0, num_image-1
        if(mod(ip,nprocs) .eq. my_rank) then
          icou = icou + 1
          file_name = add_int_suffix(ip, file_prefix)
          call read_png_file_f(file_name, ntmp_x, ntmp_y, pbuf_t)
!
          if(pbuf_t%iflag_cmode .ge. iflag_bw) then
            allocate(gray(npixel_xy(1),npixel_xy(2)))
!
            call copy_grayscale_from_png_f                              &
     &         (npixel_xy(1), npixel_xy(2), gray, pbuf_t)
!
            write(*,*) 'pixel data along with x', npixel_xy(1:2)
            do i = 1, npixel_xy(1)
              write(*,*) i, iachar(gray(i,1))
            end do
!
!$omp workshare
            quilt_d1%images(icou)%rgb(1,1:ntmp_x,1:ntmp_y)              &
     &           = gray(1:ntmp_x,1:ntmp_y)
            quilt_d1%images(icou)%rgb(2,1:ntmp_x,1:ntmp_y)              &
     &           = gray(1:ntmp_x,1:ntmp_y)
            quilt_d1%images(icou)%rgb(3,1:ntmp_x,1:ntmp_y)              &
     &           = gray(1:ntmp_x,1:ntmp_y)
!$omp end workshare
            deallocate(gray)
!   For color image
          else
            call copy_rgb_from_png_f(ntmp_x, ntmp_y,                    &
     &          quilt_d1%images(icou)%rgb, pbuf_t)
          end if
!
          nullify(pbuf_t%cimage_p)
        end if
      end do
!
      call sel_write_pvr_image_files(quilt_d1)
      call dealloc_quilt_rgb_images(quilt_d1)
!
      call calypso_MPI_finalize
!
      write(*,*) '***** program finished *****'
      stop
!
      end program parallel_png_to_quilt
!
!
!
!
