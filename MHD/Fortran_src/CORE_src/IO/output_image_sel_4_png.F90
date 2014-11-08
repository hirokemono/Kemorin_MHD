!output_image_sel_4_png.F90
      module output_image_sel_4_png
!
      use m_precision
!
      use write_bmp_image
!
      implicit none
!
      character(len=1024), private ::  fhead_img_v
!
!      subroutine sel_output_image_file(id_file_type, img_head,         &
!     &          npix_x, npix_y, cimage)
!      subroutine sel_rgba_image_file(id_file_type, img_head,           &
!     &          npix_x, npix_y, cimage)
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine sel_output_image_file(id_file_type, img_head,          &
     &          npix_x, npix_y, cimage)
!
      integer(kind = kint), intent(in) :: id_file_type
      integer(kind = kint), intent(in) :: npix_x, npix_y
      character(len=kchara), intent(in) :: img_head
      character(len = 1), intent(in) :: cimage(3,npix_x*npix_y)
      integer(kind = 4) :: npix4_x, npix4_y
!
!
#ifdef PNG_OUTPUT
      if(id_file_type .eq. 12) then
        npix4_x = int(npix_x)
        npix4_y = int(npix_y)
        write(fhead_img_v, '(a,a1)') trim(img_head), CHAR(0)
        call write_png_rgb_c(fhead_img_v, npix4_x, npix4_y,             &
     &      cimage(1,1))
        return
      end if
#endif
!
      call pixout_BMP(img_head, npix_x, npix_y, cimage(1,1))
!
      end subroutine sel_output_image_file
!
!------------------------------------------------------------------
!
      subroutine sel_rgba_image_file(id_file_type, img_head,            &
     &          npix_x, npix_y, cimage)
!
      integer(kind = kint), intent(in) :: id_file_type
      integer(kind = kint), intent(in) :: npix_x, npix_y
      character(len=kchara), intent(in) :: img_head
      character(len = 1), intent(in) :: cimage(4,npix_x*npix_y)
      integer(kind = 4) :: npix4_x, npix4_y
!
!
#ifdef PNG_OUTPUT
      if(id_file_type .eq. 12) then
        write(fhead_img_v, '(a,a1)') trim(img_head), CHAR(0)
        npix4_x = int(npix_x)
        npix4_y = int(npix_y)
        call write_png_rgba_c(fhead_img_v, npix4_x, npix4_y,            &
     &      cimage(1,1))
        return
      end if
#endif
!
      write(*,*) 'BitMap does not support transparent image'
!
      end subroutine sel_rgba_image_file
!
!------------------------------------------------------------------
!
      end module output_image_sel_4_png
