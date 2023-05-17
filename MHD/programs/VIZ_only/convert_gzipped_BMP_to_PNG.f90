!>@file   convert_gzipped_BMP_to_PNG.f90
!!@brief  program convert_gzipped_BMP_to_PNG
!!
!!@author H. Matsui
!!@date Programmed on May., 2021
!!
!>@brief Program to convert from gziiped BMP image to PNG image
      program convert_gzipped_BMP_to_PNG
!
      use output_image_sel_4_png
      use set_parallel_file_name
!
      implicit none
!
      type image_data
        character(len=kchara) :: img_prefix
        integer(kind = kint) :: npix_x
        integer(kind = kint) :: npix_y
        character(len = 1), allocatable :: cimage(:,:)
      end type image_data
!
      type(image_data), save :: img
!
      character(len=kchara) :: file_prefix
      integer(kind = kint) :: inum, ist, ied, increment
!
      write(*,*) 'Input file prefix without step count'
      read(*,*) file_prefix
      write(*,*) 'Input start, end, and increment of counts'
      read(*,*) ist, ied, increment
!
      do inum = ist, ied, increment
        img%img_prefix = add_int_suffix(inum, file_prefix)
        call read_gziped_BMP_image(img)
        call sel_output_image_file(iflag_PNG, img%img_prefix,           &
     &                             img%npix_x, img%npix_y, img%cimage)
        deallocate(img%cimage)
      end do
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine read_gziped_BMP_image(image)
!
      use t_buffer_4_gzip
      use write_bmp_image
      use select_gz_stream_file_IO
      use gz_binary_IO
      use number_to_bit
      use transfer_to_long_integers
!
      type(image_data), intent(inout) :: image
!
      integer(kind = kint) :: num
      character(len = 1), allocatable :: imagebuf(:)
      character(len = 1) :: BMP_head(54)
!
      character(len=kchara) :: bmp_name
      integer(kind = kint) :: j
!
      integer(kind = kint), parameter :: id_bmp = 8
      character, pointer :: FPz_f
      logical :: flag_gzip
      type(buffer_4_gzip) :: zbuf_b
!
      bmp_name =  add_bmp_suffix(image%img_prefix)
!
      call sel_open_read_gz_stream_file(FPz_f, id_bmp, bmp_name,        &
     &                                  flag_gzip, zbuf_b)
      if(flag_gzip) then
        call gz_read_mul_one_character_b(FPz_f, zbuf_b, cast_long(54), &
     &                                   BMP_head)
      else
        read(id_bmp) BMP_head(1:54)
      end if
!
      num = bit4_to_int_little(BMP_head(3))                             &
     &     - bit4_to_int_little(BMP_head(11))
      image%npix_x = bit4_to_int_little(BMP_head(19))
      image%npix_y = bit4_to_int_little(BMP_head(23))

      allocate(imagebuf(num))
      if(flag_gzip) then
        call gz_read_mul_one_character_b(FPz_f, zbuf_b, cast_long(num), &
     &                                   imagebuf)
      else
        read(id_bmp) BMP_head(1:num)
      end if
      call sel_close_read_gz_stream_file(FPz_f, id_bmp,                 &
     &                                   flag_gzip, zbuf_b)
!
!
      allocate(image%cimage(3,image%npix_x*image%npix_y))
!$omp parallel do
      do j = 1, image%npix_x*image%npix_y
        image%cimage(1,j) = imagebuf(3*j  )
        image%cimage(2,j) = imagebuf(3*j-1)
        image%cimage(3,j) = imagebuf(3*j-2)
      end do
!$omp end parallel do
      deallocate(imagebuf)
!
      end subroutine read_gziped_BMP_image
!
!-----------------------------------------------------------------------
!
      end program convert_gzipped_BMP_to_PNG
