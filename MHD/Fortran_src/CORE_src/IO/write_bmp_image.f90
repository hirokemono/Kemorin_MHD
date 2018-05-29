!>@file   write_bmp_image.f90
!!@brief  module write_bmp_image
!
!> @brief FORTRAN 77 program to make PPM / BMP
!>                                by  K. Hayashi
!!  Modified by H. Matsui on June, 2009
!!
!!@verbatim
!!      subroutine pixout_ppm_p3(fhead, npixel_x, npixel_y, rgb)
!!      subroutine pixout_ppm_p6(fhead, npixel_x, npixel_y, rgb)
!!      subroutine pixout_BMP(fhead, npixel_x, npixel_y, rgb)
!!
!!      character(len=kchara), intent(in) :: fhead
!!      integer, intent(in) :: ihpixf, jvpixf
!! RGB data array
!!      character(len=1), intent(in) :: rgb(3,ihpixf,jvpixf)
!!
!!      subroutine cvt_8bit_cl_int_2_chara(ihpixf, jvpixf, icl_tbl, rgb)
!!      integer, intent(in) :: icl_tbl(3,ihpixf,jvpixf)
!!
!!* --------------------------------------------
!!*
!!* Notes
!!* o With a parameter ipixout set at 1, 2 or others,
!!*   this subroutine will generate PPM-P6(binary), PPM-P3(text),
!!*   or BMP(24bit depth without color table).
!!*
!!* o Some parts follow DEC-FORTRAN that had been defacto-standard long ago.
!!*   Some compilers today may not accept if "ipixout" is not 2.
!!*
!!* o g77 (ver. 3.3.3) works for all three choices.
!!* o Recent intel compiler (ver. 9 or so) works for all three choices.
!!*
!!* --------------------------------------------
!!@endverbatim
!
      module write_bmp_image
!
      use m_precision
!
      implicit none
!
      private :: num2bit4, num2bit2, add_ppm_suffix
!
!------------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------------
!
      subroutine pixout_ppm_p3(fhead, npixel_x, npixel_y, rgb)
!
!* interface arg.
      character(len=kchara), intent(in) :: fhead
      integer(kind = kint), intent(in) :: npixel_x, npixel_y
! RGB data array
      character(len=1), intent(in) :: rgb(3,npixel_x,npixel_y)
!* local
      integer, parameter :: id_img = 16
      character(len=kchara) :: fname
      integer i, j, k, ihpixf, jvpixf
      integer itmp, icnt
!
!

      ihpixf = int(npixel_x)
      jvpixf = int(npixel_y)
!
!* PPM P3 ! rather "safer" choice for many Fortran compiler(s).

      call add_ppm_suffix(fhead, fname)
      open(unit=id_img,file=fname,status='unknown')
      write(*,*) 'Now writing PPM (P3) file : ', fname
!* header
      write(id_img,'(A)') 'P3'
      write(id_img,'(2(1x,i4),'' 255 '')')  ihpixf, jvpixf
      icnt = 0
! here, j (vertical address) runs from top to bottom.
!* image data
      do j = jvpixf, 1, -1
        do i = 1, ihpixf, 1
          do k = 1, 3
            itmp = ichar(rgb(k,i,j))
            icnt = icnt + 4
            if (icnt .LT. 60) then
! mind "$" is not standard.
              write(id_img,fmt='(1x,i3,$)') itmp
            else
              write(id_img,fmt='(1x,i3)') itmp
              icnt = 0
            endif
          end do
        end do
      end do
      write(id_img,'(A)') ' '
      close(id_img)
!
      end subroutine pixout_ppm_p3
!
!------------------------------------------------------------------------
!
      subroutine pixout_ppm_p6(fhead, npixel_x, npixel_y, rgb)
!
!* interface arg.
      character(len=kchara), intent(in) :: fhead
      integer(kind = kint), intent(in) :: npixel_x, npixel_y
! RGB data array
      character(len=1), intent(in) :: rgb(3,npixel_x,npixel_y)
!
!* local
      integer, parameter :: id_img = 16
      character(len=kchara) :: fname
      integer i, j, ihpixf, jvpixf
      integer itmp
      character(len=14) :: frmtstr
!
!
      ihpixf = int(npixel_x)
      jvpixf = int(npixel_y)
!
!* PPM P6
      call add_ppm_suffix(fhead, fname)
      open(unit=id_img,file=fname,status='unknown')
      write(*,*) 'Now writing PPM (P6) file : ', fname
!* header
      write(id_img,'(''P6'', 2(1x,i4),'' 255 '',$)') ihpixf, jvpixf
!* image data
      itmp = ihpixf * jvpixf * 3
! make output "format"
      write(frmtstr,'(''('',i8.8,''A,$)'')') itmp
      write(id_img,fmt=frmtstr)                                         &
     &                ((rgb(1:3,i,j),i=1,ihpixf),j=jvpixf,1,-1)
! some compiler may not accept this line.
! here, j (vertical address) runs from top to bottom.
     close(id_img)
!
      end subroutine pixout_ppm_p6
!
!------------------------------------------------------------------------
!
       subroutine pixout_BMP(fhead, npixel_x, npixel_y, rgb)
!* interface arg.
       character(len=kchara), intent(in) :: fhead
       integer(kind = kint), intent(in) :: npixel_x, npixel_y
! RGB data array
       character(len=1), intent(in) :: rgb(3,npixel_x,npixel_y)
!* local
       integer, parameter :: id_img = 16
       character(len=kchara) :: fname
       integer :: i, j, ihpixf, jvpixf
       integer :: itmp
       character(len=14) :: frmtstr
       character(len=54) :: headmsw
       character(len=4) ::  byt4
       character(len=2) ::  byt2
!
       ihpixf = int(npixel_x)
       jvpixf = int(npixel_y)
!
!* BMP (24bit depth)... this part works only when width is multiple of 4.

      itmp = mod(ihpixf, 4)
      if (itmp .NE. 0) then
        write(*,*) 'width must be multiple of 4'
        stop
      endif
!
      call add_bmp_suffix(fhead, fname)
!
!      open(unit=id_img,file=fname,status='unknown')
!      write(id_img,'(a)')
!      close(id_img)
!
      open(unit=id_img,file=fname,status='unknown')
      write(*,*) 'Now writing BMP(24bit) file : ', trim(fname)
!* header 1 (file header ; 1--14 byte)
      headmsw( 1: 2) = 'BM'             ! declaring this is BMP file
      itmp = 54 + ihpixf * jvpixf * 3 ! total file size = header + data
      call num2bit4(itmp,byt4)
      headmsw( 3: 6) = byt4(1:4)
      itmp = 0                        ! may be 0
      call num2bit2(itmp,byt2)
      headmsw( 7: 8) = byt2(1:2)
      itmp = 0                        ! may be 0
      call num2bit2(itmp,byt2)
      headmsw( 9:10) = byt2(1:2)
      itmp = 54                       ! must be 54 : total length of header
      call num2bit4(itmp,byt4)
      headmsw(11:14) = byt4(1:4)
!* header 2 (bit-map header ; 13--54 byte)
      itmp = 40                       ! must be 40 : length of bit-map header
      call num2bit4(itmp,byt4)
      headmsw(15:18) = byt4(1:4)
      itmp = ihpixf                   ! width
      call num2bit4(itmp,byt4)
      headmsw(19:22) = byt4(1:4)
      itmp = jvpixf                   ! height
      call num2bit4(itmp,byt4)
      headmsw(23:26) = byt4(1:4)
      itmp = 1                        ! must be 1
      call num2bit2(itmp,byt2)
      headmsw(27:28) = byt2(1:2)
      itmp = 24                       ! must be 24 : color depth in bit.
      call num2bit2(itmp,byt2)
      headmsw(29:30) = byt2(1:2)
      itmp = 0                        ! may be 0 : compression method index
      call num2bit4(itmp,byt4)
      headmsw(31:34) = byt4(1:4)
      itmp = 0                        ! may be 0 : file size if compressed
      call num2bit4(itmp,byt4)
      headmsw(35:38) = byt4(1:4)
      itmp = 0                        ! arbit. : pixel per meter, horizontal
      call num2bit4(itmp,byt4)
      headmsw(39:42) = byt4(1:4)
      itmp = 0                        ! arbit. : pixel per meter, vertical
      call num2bit4(itmp,byt4)
      headmsw(43:46) = byt4(1:4)
      itmp = 0                        ! may be 0 here : num. of color used
      call num2bit4(itmp,byt4)
      headmsw(47:50) = byt4(1:4)
      itmp = 0                        ! may be 0 here : num. of important color
      call num2bit4(itmp,byt4)
      headmsw(51:54) = byt4(1:4)

!* writing header part
      write(id_img,'(a54,$)') headmsw(1:54)
!* image data
      itmp = ihpixf * jvpixf * 3
      write(frmtstr,'(''('',i8.8,''A,$)'')') itmp
      write(id_img,fmt=frmtstr) ((rgb(3:1:-1,i,j),i=1,ihpixf),j=1,jvpixf)
! writing in BGR order, not RGB.
      close(id_img)
!
      end subroutine pixout_BMP
!
!------------------------------------------------------------------------
!------------------------------------------------------------------------
!* --------------------------------------
!* convert number to 8-bit characters
!* --------------------------------------

       subroutine num2bit4(inum,byt4)
!
       integer, intent(in) :: inum
       character(len=4), intent(inout) :: byt4
!
       integer :: itmp1, itmp2
!
       itmp1 = inum
       itmp2 = itmp1 / 256**3
       byt4(4:4) = char(itmp2)
       itmp1 =-itmp2 * 256**3 +itmp1
       itmp2 = itmp1 / 256**2
       byt4(3:3) = char(itmp2)
       itmp1 =-itmp2 * 256**2 +itmp1
       itmp2 = itmp1 / 256
       byt4(2:2) = char(itmp2)
       itmp1 =-itmp2 * 256    +itmp1
       byt4(1:1) = char(itmp1)
!
       end subroutine num2bit4
!
!------------------------------------------------------------------------
!
       subroutine num2bit2(inum,byt2)
!
       integer, intent(in) :: inum
       character(len=2), intent(inout) :: byt2
!
       integer itmp1, itmp2
!
       itmp1 = inum
       itmp2 = itmp1 / 256
       byt2(2:2) = char(itmp2)
       itmp1 =-itmp2 * 256 + itmp1
       byt2(1:1) = char(itmp1)
!
       end subroutine num2bit2
!
!------------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_ppm_suffix(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
 1011 format (a,".ppm")
!
      end subroutine add_ppm_suffix
!
!-----------------------------------------------------------------------
!
      subroutine add_bmp_suffix(file_header, file_name)
!
      character(len=kchara), intent(in) :: file_header
      character(len=kchara), intent(inout) :: file_name
!
       write(file_name,1011) trim(file_header)
 1011 format (a,".bmp")
!
      end subroutine add_bmp_suffix
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cvt_8bit_cl_int_2_chara(ihpixf, jvpixf, icl_tbl, rgb)
!
      integer, intent(in) :: ihpixf, jvpixf
      integer, intent(in) :: icl_tbl(3,ihpixf,jvpixf)
! RGB data array
      character(len=1), intent(inout) :: rgb(3,ihpixf,jvpixf)
!
      integer :: j
!
!
      do j = 1, jvpixf
        rgb(1,1:ihpixf,j) = char( icl_tbl(1,1:ihpixf,j) )
        rgb(2,1:ihpixf,j) = char( icl_tbl(2,1:ihpixf,j) )
        rgb(3,1:ihpixf,j) = char( icl_tbl(3,1:ihpixf,j) )
      end do
!
      end subroutine cvt_8bit_cl_int_2_chara
!
!-----------------------------------------------------------------------
!
      end module write_bmp_image
