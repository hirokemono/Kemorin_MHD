!
!      module skip_gz_comment
!
!     Written by H. Matsui on July, 2007
!
!!      subroutine gz_write_textbuf_no_lf
!!      subroutine gz_write_textbuf_w_lf
!!
!!      subroutine get_one_line_from_gz_f
!!      subroutine skip_gz_comment_int(int_input)
!!      subroutine skip_gz_comment_int2(int_input, int_input2)
!!      subroutine skip_gz_comment_int8_int(i8_input, int_input2)
!!      subroutine skip_gz_comment_real(real_input)
!!      subroutine skip_gz_comment_real2(real_input, real_input2)
!!      subroutine skip_gz_comment_chara(chara_input)
!!      subroutine skip_gz_comment_chara_int(chara_input, int_input)
!!      subroutine skip_gz_comment_chara_lint(chara_input, int8_input)
!!
!!      subroutine read_gz_multi_real(num, real_input)
!!      subroutine read_gz_integer_stack(num, istack, ntot)
!!      subroutine read_gz_multi_int(num, int_input)
!!      subroutine read_gz_surf_group(is1, ntot, istack, item_sf)
!!      subroutine read_gz_multi_int8(num, int8_input)
!!      subroutine write_gz_surf_group(is1, ntot, istack, item_sf)
!!      subroutine write_gz_multi_int_8i16(num, int_data)
!!      subroutine write_gz_multi_int_10i8(num, int_data)
!!      subroutine write_gz_multi_int_10i12(num, int_data)
!
!!      subroutine write_gz_comment_string(comment)
!!      subroutine gz_write_chara_nolf(chara_output)
!
      module skip_gz_comment
!
      use m_precision
      use m_constants
      use m_file_format_switch
      use t_buffer_4_gzip
!
      implicit none
!
      integer(kind = 4), parameter, private :: nbuf = 65535
      character(len=nbuf) :: textbuf
!
      type(buffer_4_gzip) :: zbuf1
!
      private :: skip_gz_comment_get_nword
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_write_textbuf_no_lf
!
      use calypso_c_binding
!
!      write(*,*) 'get_one_line_from_gz_f03', nbuf, len(textbuf)
      call gz_write_textbuf_no_lf_f(nbuf, textbuf, zbuf1)
!
      end subroutine gz_write_textbuf_no_lf
!
! ----------------------------------------------------------------------
!
      subroutine gz_write_textbuf_w_lf
!
      use calypso_c_binding
!
!
!      write(*,*) 'get_one_line_from_gz_f03', nbuf, len(textbuf)
      call gz_write_textbuf_w_lf_f(nbuf, textbuf, zbuf1)
!
      end subroutine gz_write_textbuf_w_lf
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer function length_of_c_text(text)
      character(len=*), intent(in) ::  text
      integer :: i
!
      length_of_c_text = -1
      do i = 1, len(textbuf)
        if(textbuf(i:i) .eq. char(0)) then
          length_of_c_text = i
          exit
        end if
      end do
      end function length_of_c_text
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine get_one_line_from_gz_f
!
      use calypso_c_binding
!
!
      write(*,*) 'get_one_line_from_gz_f03', nbuf, len(textbuf)
      call get_one_line_from_gz_f03(nbuf, textbuf, zbuf1)
!
      end subroutine get_one_line_from_gz_f
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_int(int_input)
!
      integer(kind = kint), intent(inout) :: int_input
!
!
      call skip_gz_comment_get_nword
      read(textbuf,*) int_input
!
      end subroutine skip_gz_comment_int
!
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_int2(int_input, int_input2)
!
      integer(kind = kint), intent(inout) :: int_input, int_input2
!
!
      call skip_gz_comment_get_nword
      read(textbuf,*) int_input, int_input2
!
      end subroutine skip_gz_comment_int2
!
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_int8_int(i8_input, int_input2)
!
      integer(kind = kint_gl), intent(inout) :: i8_input
      integer(kind = kint), intent(inout) :: int_input2
!
!
      call skip_gz_comment_get_nword
      read(textbuf,*) i8_input, int_input2
!
      end subroutine skip_gz_comment_int8_int
!
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_real(real_input)
!
      real(kind = kreal), intent(inout) :: real_input
!
      call skip_gz_comment_get_nword
      read(textbuf,*) real_input
!
      end subroutine skip_gz_comment_real
!
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_real2(real_input, real_input2)
!
      real(kind = kreal), intent(inout) :: real_input, real_input2
!
      call skip_gz_comment_get_nword
      read(textbuf,*) real_input, real_input2
!
      end subroutine skip_gz_comment_real2
!
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_chara(chara_input)
!
      character(len = kchara), intent(inout) :: chara_input
      character(len=kchara) :: charaint, fmtchara, tmpchara
!
!
      call skip_gz_comment_get_nword
!
      write(charaint,'(i8)') min(int(zbuf1%len_used - 1), int(kchara))
      write(fmtchara,'(a2,a,a1)')                                       &
     &          '(a', trim(ADJUSTL(charaint)),')'

      write(tmpchara,fmtchara) textbuf
      write(chara_input,'(a)') ADJUSTL(tmpchara)
!
      end subroutine skip_gz_comment_chara
!
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_chara_int(chara_input, int_input)
!
      character(len = kchara), intent(inout) :: chara_input
      integer(kind = kint), intent(inout) :: int_input
!
!
      call skip_gz_comment_get_nword
      read(textbuf,*) chara_input, int_input
!
      end subroutine skip_gz_comment_chara_int
!
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_chara_lint(chara_input, int8_input)
!
      character(len = kchara), intent(inout) :: chara_input
      integer(kind = kint_gl), intent(inout) :: int8_input
!
!
      call skip_gz_comment_get_nword
      read(textbuf,*) chara_input, int8_input
!
      end subroutine skip_gz_comment_chara_lint
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_get_nword
!
      character(len=1) :: chara_flag
!      character(len=nbuf) :: tbuf2
!
      do
        call get_one_line_from_gz_f
        if(zbuf1%len_used .le. 1) cycle
!
        write(chara_flag,'(a1)',err=1) adjustl(textbuf)
        if(chara_flag.eq.char(10) .or. chara_flag.eq.char(13)) cycle
        if(chara_flag.ne.'#' .and. chara_flag.ne.'!') exit
   1    continue
      end do
!
!      write(charaint,'(i8)') zbuf1%len_used - 1
!      write(fmtchara,'(a2,a,a4)')  '(a', trim(ADJUSTL(charaint)),',a1)'
!      write(tbuf2,fmtchara) textbuf, char(32)
!      do i = 1, zbuf1%len_used + 1
!        write(*,*) i, ichar(textbuf(i:i)), ichar(tbuf2(i:i)),       &
!     &              textbuf(i:i), tbuf2(i:i)
!      end do
!
      end subroutine skip_gz_comment_get_nword
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_gz_multi_real(num, real_input)
!
      integer(kind = kint), intent(in) :: num
      real(kind = kreal), intent(inout) :: real_input(num)
!
      integer(kind = kint) :: ist, ist2, ied2
!
!
      if(num .le. 0) return
!
      call skip_gz_comment_get_nword
      read(textbuf,*) real_input(1:zbuf1%num_word)
!
      if(num .gt. zbuf1%num_word) then
        ist = zbuf1%num_word
        do
          call get_one_line_from_gz_f
          ist2 = ist + 1
          ied2 = ist + zbuf1%num_word
          ist = ied2
          read(textbuf,*) real_input(:ied2)
          if(ist .ge. num) exit
        end do
      end if
!
      end subroutine read_gz_multi_real
!
!------------------------------------------------------------------
!
      subroutine read_gz_integer_stack(num, istack, ntot)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: istack(0:num)
      integer(kind = kint), intent(inout) :: ntot
!
!
      istack(0) = 0
      call read_gz_multi_int(num, istack(1))
      ntot = istack(num)
!
      end subroutine read_gz_integer_stack
!
!------------------------------------------------------------------
!
      subroutine read_gz_multi_int(num, int_input)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: int_input(num)
!
      integer(kind = kint) :: ist, ist2, ied2
!
!
      if(num .le. 0) return
!
      call skip_gz_comment_get_nword
      read(textbuf,*) int_input(1:zbuf1%num_word)
!
      if(num .gt. zbuf1%num_word) then
        ist = zbuf1%num_word
        do
          call get_one_line_from_gz_f
          ist2 = ist + 1
          ied2 = ist + zbuf1%num_word
          ist = ied2
          read(textbuf,*) int_input(ist2:ied2)
          if(ist .ge. num) exit
        end do
      end if
!
      end subroutine read_gz_multi_int
!
!------------------------------------------------------------------
!
      subroutine read_gz_surf_group(is1, ntot, istack, item_sf)
!
      integer(kind = kint), intent(in) :: is1, ntot
      integer(kind = kint), intent(in) :: istack(0:1)
      integer(kind = kint), intent(inout) :: item_sf(2,ntot)
!
      integer(kind = kint) :: ist, ist2, ied2
!
!
      if((istack(1) - istack(0)) .le. 0) return
!
      call skip_gz_comment_get_nword
      ist2 = istack(0) + 1
      ied2 = istack(0) + zbuf1%num_word
      read(textbuf,*) item_sf(is1,ist2:ied2)
!
      if((istack(1) - istack(0)) .gt. zbuf1%num_word) then
        ist = istack(0) + zbuf1%num_word
        do
          call get_one_line_from_gz_f
          ist2 = ist + 1
          ied2 = ist + zbuf1%num_word
          ist = ied2
          read(textbuf,*) item_sf(is1,ist2:ied2)
          if(ist .ge. istack(1)) exit
        end do
      end if
!
      end subroutine read_gz_surf_group
!
!------------------------------------------------------------------
!
      subroutine read_gz_multi_int8(num, int8_input)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: int8_input(num)
!
      integer(kind = kint) :: ist, ist2, ied2
!
!
      if(num .le. 0) return
!
      call skip_gz_comment_get_nword
      read(textbuf,*) int8_input(1:zbuf1%num_word)
!
      if(num .gt. zbuf1%num_word) then
        ist = zbuf1%num_word
        do
          call get_one_line_from_gz_f
          ist2 = ist + 1
          ied2 = ist + zbuf1%num_word
          ist = ied2
          read(textbuf,*) int8_input(ist2:ied2)
          if(ist .ge. num) exit
        end do
      end if
!
      end subroutine read_gz_multi_int8
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_gz_surf_group(is1, ntot, istack, item_sf)
!
      integer(kind = kint), intent(in) :: is1, ntot
      integer(kind = kint), intent(in) :: istack(0:1)
      integer(kind = kint), intent(in) :: item_sf(2,ntot)
!
      integer(kind = kint) :: ist, n
      character(len=kchara) :: fmt_txt
!
!
      ist = istack(0)
      do
        n = min(istack(1)-ist-ione,iseven) + 1
        write(fmt_txt,'(a1,i2,a7)') '(', n, 'i16,a1)'
        write(textbuf,fmt_txt) item_sf(is1,ist+1:ist+n), char(0)
        call gz_write_textbuf_w_lf
        ist = ist + n
        if(ist .ge. istack(1)) exit
      end do
!
      end subroutine write_gz_surf_group
!
!------------------------------------------------------------------
!
      subroutine write_gz_multi_int_8i16(num, int_data)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: int_data(num)
!
      integer(kind = kint) :: ist, n
      character(len=kchara) :: fmt_txt
!
!
      ist = 0
      do
        n = min(num-ist-ione,iseven) + 1
        write(fmt_txt,'(a1,i2,a8)') '(', n, 'i16,2a1)'
        write(textbuf,fmt_txt) int_data(ist+1:ist+n), char(10), char(0)
        call gz_write_textbuf_no_lf_f((n*16+2), textbuf, zbuf1)
        ist = ist + n
        if(ist .ge. num) exit
      end do
!
      end subroutine write_gz_multi_int_8i16
!
!------------------------------------------------------------------
!
      subroutine write_gz_multi_int_10i8(num, int_data)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: int_data(num)
!
      integer(kind = kint) :: ist, n
      character(len=kchara) :: fmt_txt
!
!
      ist = 0
      do
        n = min(num-ist-ione,inine) + 1
        write(fmt_txt,'(a1,i3,a6)') '(', n, 'i8,a1)'
        write(textbuf,fmt_txt) int_data(ist+1:ist+n), char(0)
        write(*,*) 'sel_gz_write_textbuf', length_of_c_text(textbuf), (n*8+2)
        call gz_write_textbuf_w_lf
        ist = ist + n
        if(ist .ge. num) exit
      end do
!
      end subroutine write_gz_multi_int_10i8
!
!------------------------------------------------------------------
!
      subroutine write_gz_multi_int_10i12(num, int_data)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: int_data(num)
!
      integer(kind = kint) :: ist, n
      character(len=kchara) :: fmt_txt
!
!
      ist = 0
      do
        n = min(num-ist-ione,inine) + 1
        write(fmt_txt,'(a1,i3,a7)') '(', n, 'i12,a1)'
        write(textbuf,fmt_txt) int_data(ist+1:ist+n), char(0)
        call gz_write_textbuf_w_lf
        ist = ist + n
        if(ist .ge. num) exit
      end do
!
      end subroutine write_gz_multi_int_10i12
!
!------------------------------------------------------------------
!
      subroutine write_gz_comment_string(comment)
!
      character(len=*), intent(in)  ::  comment
!
!
      write(textbuf,'(a,a1)') comment, char(0)
      call gz_write_textbuf_w_lf
!
      end subroutine write_gz_comment_string
!
!------------------------------------------------------------------
!
      subroutine gz_write_chara_nolf(chara_output)
!
!
      character(len=kchara), intent(in) :: chara_output
!
      write(textbuf,'(2a,a1)') trim(chara_output), '    ', CHAR(0)
      call gz_write_textbuf_no_lf
!
      end subroutine gz_write_chara_nolf
!
! ----------------------------------------------------------------------
!
      end module skip_gz_comment
