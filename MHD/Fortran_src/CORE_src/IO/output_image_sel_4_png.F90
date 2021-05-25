!> @file  output_image_sel_4_png.f90
!!      module output_image_sel_4_png
!!
!! @author  H. Matsui
!! @date Written in June, 2009
!
!> @brief selector for image data output
!!
!!@verbatim
!!      subroutine sel_output_image_file(id_file_type, img_head,        &
!!     &          npix_x, npix_y, cimage)
!!      subroutine sel_rgba_image_file(id_file_type, img_head,          &
!!     &          npix_x, npix_y, cimage)
!!@endverbatim
!
      module output_image_sel_4_png
!
      use m_precision
!
      use t_png_file_access
      use write_bmp_image
!
      implicit none
!
      character(len = kchara), parameter :: hd_BMP =       'BMP'
      character(len = kchara), parameter :: hd_PNG =       'PNG'
      character(len = kchara), parameter :: hd_QUILT_BMP = 'QUILT'

      integer(kind = kint), parameter :: iflag_UNDEFINED =  -1
      integer(kind = kint), parameter :: iflag_BMP = 11
      integer(kind = kint), parameter :: iflag_PNG = 12
      integer(kind = kint), parameter :: iflag_QUILT_BMP = 111

      type(buffer_4_png), private :: pbuf
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
!
!
#ifdef PNG_OUTPUT
      if(id_file_type .eq. iflag_PNG) then
        call write_png_rgb_f                                            &
     &     (img_head, npix_x, npix_y, cimage(1,1), pbuf)
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
!
!
#ifdef PNG_OUTPUT
      if(id_file_type .eq. iflag_PNG) then
        call write_png_rgba_f                                           &
     &     (img_head, npix_x, npix_y, cimage(1,1), pbuf)
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
