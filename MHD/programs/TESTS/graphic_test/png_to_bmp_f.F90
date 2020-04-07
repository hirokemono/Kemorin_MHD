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
      use t_png_file_access
      use write_bmp_image
!
      character(len=kchara) :: file_prefix
!
      integer(kind = kint) :: npixel_x
      integer(kind = kint) :: npixel_y
      character(len=1), allocatable :: rgb(:,:,:)
      character(len=1), allocatable :: gray(:,:)
!
      type(buffer_4_png) :: pbuf_t
!
      integer(kind = kint) :: i
!
!
      write(*,*) 'Input image file prefix'
      read(*,*) file_prefix
!#ifdef PNG_OUTPUT
      call read_png_file_f(file_prefix, npixel_x, npixel_y, pbuf_t)
!
      if(pbuf_t%iflag_cmode .eq. iflag_rgba)                            &
     &                     write(*,*) 'RGBA image file'
      if(pbuf_t%iflag_cmode .eq. iflag_rgb)                             &
     &                     write(*,*) 'RGB image'
      if(pbuf_t%iflag_cmode .eq. iflag_ba)                              &
     &                     write(*,*) 'grayscale with alpha image'
      if(pbuf_t%iflag_cmode .eq. iflag_bw)                              &
     &                     write(*,*) 'grayscale image'
!
      allocate(rgb(3,npixel_x,npixel_y))
!
!   For grayscale image
      if(pbuf_t%iflag_cmode .ge. iflag_bw) then
        allocate(gray(npixel_x,npixel_y))
!
        call copy_grayscale_from_png_f                                  &
     &     (npixel_x, npixel_y, gray, pbuf_t)
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
        call copy_rgb_from_png_f(npixel_x, npixel_y, rgb, pbuf_t)
      end if
!
      call pixout_BMP(file_prefix, npixel_x, npixel_y, rgb)
!#endif
!
      end program png_to_bmp_f
