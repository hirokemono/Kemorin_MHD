!> @file  read_bmp_image.f90
!!      module read_bmp_image
!!
!! @author  H. Matsui
!! @date Written in June, 2009
!
!> @brief FORTRAN 77 program to make BMP
!!
!!@verbatim
!!       subroutine read_BMP_file(fhead, ihpixf, jvpixf)
!!       subroutine copy_rgba_from_BMP(ihpixf, jvpixf, rgba)
!!       subroutine copy_rgb_from_BMP(ihpixf, jvpixf, rgb)
!!
!!      character(len=kchara), intent(in) :: fhead
!!      integer, intent(in) :: ihpixf, jvpixf
!! RGB data array
!!      character(len=1), intent(in) :: rgb(3,ihpixf,jvpixf)
!!@endverbatim
!
      module read_bmp_image
!
      use m_precision
!
      implicit none
!
      character(len=1), allocatable, private :: bgr(:,:,:)
      private :: bit4_to_num
!
!------------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------------
!
       subroutine read_BMP_file(fhead, ihpixf, jvpixf)
!
       use write_bmp_image
!
!* interface arg.
       character(len=kchara), intent(in) :: fhead
       integer, intent(inout) :: ihpixf, jvpixf
!* local
       integer, parameter :: id_img = 16
       integer :: i, j
       integer :: itmp
       character(len=kchara) :: fname
       character(len=14) :: frmtstr
       character(len=1) :: headmsw(54)
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
      open(unit=id_img,file=fname,status='unknown')
      write(*,*) 'Now reading BMP(24bit) file : ', fname
      read(id_img,'(54a1,$)') headmsw(1:54)
!
      call bit4_to_num(headmsw(19), ihpixf)
      call bit4_to_num(headmsw(23), jvpixf)
      allocate( bgr(3,ihpixf,jvpixf) )
!
!* read image data

      read(frmtstr,'(''('',i8.8,''A,$)'')') itmp
      read(id_img,fmt=frmtstr) ((bgr(1:3,i,j),i=1,ihpixf),j=1,jvpixf)
! reading in BGR order, not RGB.
      close(id_img)
!
      end subroutine read_BMP_file
!
!------------------------------------------------------------------------
!
      subroutine copy_rgba_from_BMP(ihpixf,jvpixf, rgba)
! RGB data array
       integer, intent(in) :: ihpixf, jvpixf
       character(len=1), intent(inout) :: rgba(4,ihpixf,jvpixf)
!
       integer :: i, j
!
!
      do j = 1, jvpixf
        do i = 1, ihpixf
          rgba(1,i,j) = bgr(3,i,j)
          rgba(2,i,j) = bgr(2,i,j)
          rgba(3,i,j) = bgr(1,i,j)
          rgba(4,i,j) = char(255)
        end do
      end do
!
      deallocate(bgr)
!
      end subroutine copy_rgba_from_BMP
!
!------------------------------------------------------------------------
!
      subroutine copy_rgb_from_BMP(ihpixf, jvpixf, rgb)
!
! RGB data array
       integer, intent(in) :: ihpixf, jvpixf
       character(len=1), intent(inout) :: rgb(3,ihpixf,jvpixf)
!
       integer :: i, j
!
!
      do j = 1, jvpixf
        do i = 1, ihpixf
          rgb(1,i,j) = bgr(3,i,j)
          rgb(2,i,j) = bgr(2,i,j)
          rgb(3,i,j) = bgr(1,i,j)
        end do
      end do
!
      deallocate(bgr)
!
      end subroutine copy_rgb_from_BMP
!
!------------------------------------------------------------------------
!------------------------------------------------------------------------
!
       subroutine bit4_to_num(byt4, inum)
!
       integer, intent(inout) :: inum
       character(len=1), intent(in) :: byt4(4)
       integer :: itmp
!
!
       read (byt4(1), *) inum
       read (byt4(2), *) itmp
       inum = 256 * inum + itmp
       read (byt4(3), *) itmp
       inum = 256 * inum + itmp
       read (byt4(4), *) itmp
       inum = 256 * inum + itmp
!
       end subroutine bit4_to_num
!
!------------------------------------------------------------------------
!
      end module read_bmp_image
