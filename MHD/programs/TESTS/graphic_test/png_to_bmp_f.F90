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
      character(len=1), allocatable :: gray(:,:)
!
      integer(kind = kint) :: i
!
!
      write(*,*) 'Input image file prefix'
      read(*,*) file_prefix
      write(file_tmp,'(a,a1)') trim(file_prefix), char(0)
!#ifdef PNG_OUTPUT
      call read_png_file_c(file_tmp, npixel_x, npixel_y, iflag_rgba)
!
      if(iflag_rgba .eq.  1)  write(*,*) 'RGBA image file'
      if(iflag_rgba .eq.  0)  write(*,*) 'RGB image'
      if(iflag_rgba .eq. 11)  write(*,*) 'grayscale with alpha image'
      if(iflag_rgba .eq. 10)  write(*,*) 'grayscale image'
!
      allocate(rgb(3,npixel_x,npixel_y))
!
!   For grayscale image
      if(iflag_rgba .ge. 10) then
        allocate(gray(npixel_x,npixel_y))
!
        call copy_grayscale_from_png_c                                  &
     &     (npixel_x, npixel_y, iflag_rgba, gray)
!
        write(*,*) 'pixel data along with x', npixel_x, npixel_y
        do i = 1, npixel_x
          write(*,*) i, iachar(gray(i,1))
        end do
!
!$omp workshare
        rgb(1,1:npixel_x,1:npixel_y) = gray(1:npixel_x,1:npixel_y)
        rgb(2,1:npixel_x,1:npixel_y) = gray(1:npixel_x,1:npixel_y)
        rgb(3,1:npixel_x,1:npixel_y) = gray(1:npixel_x,1:npixel_y)
!$omp end workshare
!
!   For color image
      else
        call copy_rgb_from_png_c(npixel_x, npixel_y, iflag_rgba, rgb)
      end if
!
      call pixout_BMP(file_prefix, npixel_x, npixel_y, rgb)
!
      if(iflag_rgba .ge. 10) then
      end if
!#endif
!
      end program png_to_bmp_f
