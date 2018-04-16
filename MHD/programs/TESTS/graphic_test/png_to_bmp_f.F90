!>@file   png_to_bmp_f.f90
!!@brief  module png_to_bmp_f
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in 2004
!
!>@brief  file convert from PNG to BMP
!
      program png_to_bmp_f
!
      use write_bmp_image
!
      character(len=kchara) :: file_prefix
      character(len=kchara) :: file_tmp
!
      integer(kind = kint) :: iflag_rgba
      integer(kind = kint) :: npixel_x
      integer(kind = kint) :: npixel_y
      character(len=1), allocatable :: rgb(:,:,:)
!
!
      write(*,*) 'Input image file prefix'
      read(*,*) file_prefix
      write(file_tmp,'(a,a1)') trim(file_prefix), char(0)
!#ifdef PNG_OUTPUT
      call read_png_file_c(file_tmp, npixel_x, npixel_y, iflag_rgba)
!
      allocate(rgb(3,npixel_x,npixel_y))
!
      call copy_rgb_from_png_c(npixel_x, npixel_y, iflag_rgba, rgb)
!
      call pixout_BMP(file_prefix, npixel_x, npixel_y, rgb)
!#endif
!
      end program png_to_bmp_f
