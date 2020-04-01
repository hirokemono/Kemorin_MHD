!
!      module skip_gz_comment
!
!     Written by H. Matsui on July, 2007
!
!!      subroutine gz_write_textbuf_no_lf(zbuf)
!!      subroutine get_one_line_from_gz_f
!!        type(buffer_4_gzip), intent(inout):: zbuf
!!
!!      subroutine skip_gz_comment_int(int_input, zbuf)
!!      subroutine skip_gz_comment_int(int_input)
!!      subroutine skip_gz_comment_int2(int_input, int_input2, zbuf)
!!      subroutine skip_gz_comment_int8_int(i8_input, int_input2, zbuf)
!!      subroutine skip_gz_comment_real(real_input, zbuf)
!!      subroutine skip_gz_comment_real2(real_input, real_input2, zbuf)
!!      subroutine skip_gz_comment_chara(chara_input, zbuf)
!!      subroutine skip_gz_comment_chara_int                            &
!!     &         (chara_input, int_input, zbuf)
!!      subroutine skip_gz_comment_chara_lint                           &
!!     &         (chara_input, int8_input, zbuf)
!!        type(buffer_4_gzip), intent(inout):: zbuf
!!
!!      subroutine read_gz_multi_real(num, real_input, zbuf)
!!      subroutine read_gz_integer_stack(num, istack, ntot, zbuf)
!!      subroutine read_gz_multi_int(num, int_input, zbuf)
!!      subroutine read_gz_surf_group(is1, ntot, istack, item_sf, zbuf)
!!      subroutine read_gz_multi_int8(num, int8_input, zbuf)
!!        type(buffer_4_gzip), intent(inout):: zbuf
!!
!!      subroutine write_gz_surf_group(is1, ntot, istack, item_sf, zbuf)
!!      subroutine write_gz_multi_int_8i16(num, int_data, zbuf)
!!      subroutine write_gz_multi_int_10i8(num, int_data, zbuf)
!!      subroutine write_gz_multi_int_10i12(num, int_data, zbuf)
!!        type(buffer_4_gzip), intent(inout):: zbuf
!!
!!      subroutine write_gz_comment_string(comment, zbuf)
!!      subroutine gz_write_chara_nolf(chara_output, zbuf)
!!        type(buffer_4_gzip), intent(inout):: zbuf
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
!      character(len=nbuf) :: textbuf
!
!      type(buffer_4_gzip) :: zbuf1
!
      private :: skip_gz_comment_get_nword
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      integer function length_of_c_text(text)
      character(len=*), intent(in) ::  text
      integer :: i
!
      length_of_c_text = -1
      do i = 1, len(text)
        if(text(i:i) .eq. char(0)) then
          length_of_c_text = i
          exit
        end if
      end do
      end function length_of_c_text
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine open_wt_gzfile_a(gzip_name, zbuf)
!
      use calypso_c_binding
!
      character(len = kchara), intent(in) :: gzip_name
      type(buffer_4_gzip) , intent(inout):: zbuf
!
!
      call alloc_fixbuffer_for_zlib(zbuf)
      call open_wt_gzfile_f(gzip_name, zbuf)
!
      end subroutine open_wt_gzfile_a
!
! ----------------------------------------------------------------------
!
      subroutine open_ad_gzfile_a(gzip_name, zbuf)
!
      use calypso_c_binding
!
      character(len = kchara), intent(in) :: gzip_name
      type(buffer_4_gzip) , intent(inout):: zbuf
!
!
      call alloc_fixbuffer_for_zlib(zbuf)
      call open_ad_gzfile_f(gzip_name, zbuf)
!
      end subroutine open_ad_gzfile_a
!
! ----------------------------------------------------------------------
!
      subroutine open_rd_gzfile_a(gzip_name, zbuf)
!
      use calypso_c_binding
!
      character(len = kchara), intent(in) :: gzip_name
      type(buffer_4_gzip) , intent(inout):: zbuf
!
!
      call alloc_fixbuffer_for_zlib(zbuf)
      call open_rd_gzfile_f(gzip_name, zbuf)
!
      end subroutine open_rd_gzfile_a
!
! ----------------------------------------------------------------------
!
      subroutine close_gzfile_a(zbuf)
!
      use calypso_c_binding
!
      type(buffer_4_gzip) , intent(inout):: zbuf
!
      call close_gzfile_b()
      call dealloc_fixbuffer_for_zlib(zbuf)
!
      end subroutine close_gzfile_a
!
!------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine gz_write_textbuf_no_lf(zbuf)
!
      use calypso_c_binding
!
      type(buffer_4_gzip) , intent(inout):: zbuf
!
      call gz_write_textbuf_no_lf_f(nbuf, zbuf%fixbuf(1), zbuf)
!
      end subroutine gz_write_textbuf_no_lf
!
! ----------------------------------------------------------------------
!
      subroutine get_one_line_from_gz_f(zbuf)
!
      use calypso_c_binding
!
      type(buffer_4_gzip) , intent(inout):: zbuf
!
      write(*,*) 'get_one_line_from_gz_f03', nbuf, len(zbuf%fixbuf(1))
      call get_one_line_from_gz_f03(nbuf, zbuf%fixbuf(1), zbuf)
!
      end subroutine get_one_line_from_gz_f
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_int(int_input, zbuf)
!
      integer(kind = kint), intent(inout) :: int_input
      type(buffer_4_gzip) , intent(inout):: zbuf
!
!
      call skip_gz_comment_get_nword(zbuf)
      read(zbuf%fixbuf(1),*) int_input
!
      end subroutine skip_gz_comment_int
!
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_int2(int_input, int_input2, zbuf)
!
      integer(kind = kint), intent(inout) :: int_input, int_input2
      type(buffer_4_gzip), intent(inout):: zbuf
!
!
      call skip_gz_comment_get_nword(zbuf)
      read(zbuf%fixbuf(1),*) int_input, int_input2
!
      end subroutine skip_gz_comment_int2
!
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_int8_int(i8_input, int_input2, zbuf)
!
      integer(kind = kint_gl), intent(inout) :: i8_input
      integer(kind = kint), intent(inout) :: int_input2
      type(buffer_4_gzip), intent(inout):: zbuf
!
!
      call skip_gz_comment_get_nword(zbuf)
      read(zbuf%fixbuf(1),*) i8_input, int_input2
!
      end subroutine skip_gz_comment_int8_int
!
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_real(real_input, zbuf)
!
      real(kind = kreal), intent(inout) :: real_input
      type(buffer_4_gzip), intent(inout):: zbuf
!
      call skip_gz_comment_get_nword(zbuf)
      read(zbuf%fixbuf(1),*) real_input
!
      end subroutine skip_gz_comment_real
!
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_real2(real_input, real_input2, zbuf)
!
      real(kind = kreal), intent(inout) :: real_input, real_input2
      type(buffer_4_gzip), intent(inout):: zbuf
!
      call skip_gz_comment_get_nword(zbuf)
      read(zbuf%fixbuf(1),*) real_input, real_input2
!
      end subroutine skip_gz_comment_real2
!
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_chara(chara_input, zbuf)
!
      character(len = kchara), intent(inout) :: chara_input
      type(buffer_4_gzip), intent(inout):: zbuf
!
      character(len=kchara) :: charaint, fmtchara, tmpchara
!
!
      call skip_gz_comment_get_nword(zbuf)
!
      write(charaint,'(i8)') min(int(zbuf%len_used - 1), int(kchara))
      write(fmtchara,'(a2,a,a1)')                                       &
     &          '(a', trim(ADJUSTL(charaint)),')'

      write(tmpchara,fmtchara) zbuf%fixbuf(1)
      write(chara_input,'(a)') ADJUSTL(tmpchara)
!
      end subroutine skip_gz_comment_chara
!
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_chara_int                              &
     &         (chara_input, int_input, zbuf)
!
      character(len = kchara), intent(inout) :: chara_input
      integer(kind = kint), intent(inout) :: int_input
      type(buffer_4_gzip), intent(inout):: zbuf
!
!
      call skip_gz_comment_get_nword(zbuf)
      read(zbuf%fixbuf(1),*) chara_input, int_input
!
      end subroutine skip_gz_comment_chara_int
!
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_chara_lint                             &
     &         (chara_input, int8_input, zbuf)
!
      character(len = kchara), intent(inout) :: chara_input
      integer(kind = kint_gl), intent(inout) :: int8_input
      type(buffer_4_gzip), intent(inout):: zbuf
!
!
      call skip_gz_comment_get_nword(zbuf)
      read(zbuf%fixbuf(1),*) chara_input, int8_input
!
      end subroutine skip_gz_comment_chara_lint
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine skip_gz_comment_get_nword(zbuf)
!
      type(buffer_4_gzip), intent(inout):: zbuf
      character(len=1) :: chara_flag
!      character(len=65535) :: tbuf2
!
      do
        call get_one_line_from_gz_f(zbuf)
        if(zbuf%len_used .le. 1) cycle
!
        write(chara_flag,'(a1)',err=1) adjustl(zbuf%fixbuf(1))
        if(chara_flag.eq.char(10) .or. chara_flag.eq.char(13)) cycle
        if(chara_flag.ne.'#' .and. chara_flag.ne.'!') exit
   1    continue
      end do
!
!      write(charaint,'(i8)') zbuf%len_used - 1
!      write(fmtchara,'(a2,a,a4)')  '(a', trim(ADJUSTL(charaint)),',a1)'
!      write(tbuf2,fmtchara) zbuf%fixbuf(1), char(32)
!      do i = 1, zbuf%len_used + 1
!        write(*,*) i, ichar(zbuf%fixbuf(1)(i:i)), ichar(tbuf2(i:i)),   &
!     &              zbuf%fixbuf(1)(i:i), tbuf2(i:i)
!      end do
!
      end subroutine skip_gz_comment_get_nword
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_gz_multi_real(num, real_input, zbuf)
!
      integer(kind = kint), intent(in) :: num
      real(kind = kreal), intent(inout) :: real_input(num)
      type(buffer_4_gzip), intent(inout):: zbuf
!
      integer(kind = kint) :: ist, ist2, ied2
!
!
      if(num .le. 0) return
!
      call skip_gz_comment_get_nword(zbuf)
      read(zbuf%fixbuf(1),*) real_input(1:zbuf%num_word)
!
      if(num .gt. zbuf%num_word) then
        ist = zbuf%num_word
        do
          call get_one_line_from_gz_f(zbuf)
          ist2 = ist + 1
          ied2 = ist + zbuf%num_word
          ist = ied2
          read(zbuf%fixbuf(1),*) real_input(:ied2)
          if(ist .ge. num) exit
        end do
      end if
!
      end subroutine read_gz_multi_real
!
!------------------------------------------------------------------
!
      subroutine read_gz_integer_stack(num, istack, ntot, zbuf)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: istack(0:num)
      integer(kind = kint), intent(inout) :: ntot
      type(buffer_4_gzip), intent(inout):: zbuf
!
!
      istack(0) = 0
      call read_gz_multi_int(num, istack(1), zbuf)
      ntot = istack(num)
!
      end subroutine read_gz_integer_stack
!
!------------------------------------------------------------------
!
      subroutine read_gz_multi_int(num, int_input, zbuf)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(inout) :: int_input(num)
      type(buffer_4_gzip), intent(inout):: zbuf
!
      integer(kind = kint) :: ist, ist2, ied2
!
!
      if(num .le. 0) return
!
      call skip_gz_comment_get_nword(zbuf)
      read(zbuf%fixbuf(1),*) int_input(1:zbuf%num_word)
!
      if(num .gt. zbuf%num_word) then
        ist = zbuf%num_word
        do
          call get_one_line_from_gz_f(zbuf)
          ist2 = ist + 1
          ied2 = ist + zbuf%num_word
          ist = ied2
          read(zbuf%fixbuf(1),*) int_input(ist2:ied2)
          if(ist .ge. num) exit
        end do
      end if
!
      end subroutine read_gz_multi_int
!
!------------------------------------------------------------------
!
      subroutine read_gz_surf_group(is1, ntot, istack, item_sf, zbuf)
!
      integer(kind = kint), intent(in) :: is1, ntot
      integer(kind = kint), intent(in) :: istack(0:1)
      integer(kind = kint), intent(inout) :: item_sf(2,ntot)
      type(buffer_4_gzip), intent(inout):: zbuf
!
      integer(kind = kint) :: ist, ist2, ied2
!
!
      if((istack(1) - istack(0)) .le. 0) return
!
      call skip_gz_comment_get_nword(zbuf)
      ist2 = istack(0) + 1
      ied2 = istack(0) + zbuf%num_word
      read(zbuf%fixbuf(1),*) item_sf(is1,ist2:ied2)
!
      if((istack(1) - istack(0)) .gt. zbuf%num_word) then
        ist = istack(0) + zbuf%num_word
        do
          call get_one_line_from_gz_f(zbuf)
          ist2 = ist + 1
          ied2 = ist + zbuf%num_word
          ist = ied2
          read(zbuf%fixbuf(1),*) item_sf(is1,ist2:ied2)
          if(ist .ge. istack(1)) exit
        end do
      end if
!
      end subroutine read_gz_surf_group
!
!------------------------------------------------------------------
!
      subroutine read_gz_multi_int8(num, int8_input, zbuf)
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: int8_input(num)
      type(buffer_4_gzip), intent(inout):: zbuf
!
      integer(kind = kint) :: ist, ist2, ied2
!
!
      if(num .le. 0) return
!
      call skip_gz_comment_get_nword(zbuf)
      read(zbuf%fixbuf(1),*) int8_input(1:zbuf%num_word)
!
      if(num .gt. zbuf%num_word) then
        ist = zbuf%num_word
        do
          call get_one_line_from_gz_f(zbuf)
          ist2 = ist + 1
          ied2 = ist + zbuf%num_word
          ist = ied2
          read(zbuf%fixbuf(1),*) int8_input(ist2:ied2)
          if(ist .ge. num) exit
        end do
      end if
!
      end subroutine read_gz_multi_int8
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_gz_surf_group(is1, ntot, istack, item_sf, zbuf)
!
      use calypso_c_binding
!
      integer(kind = kint), intent(in) :: is1, ntot
      integer(kind = kint), intent(in) :: istack(0:1)
      integer(kind = kint), intent(in) :: item_sf(2,ntot)
      type(buffer_4_gzip), intent(inout):: zbuf
!
      integer(kind = kint) :: ist, n
      character(len=kchara) :: fmt_txt
!
!
      ist = istack(0)
      do
        n = min(istack(1)-ist-ione,iseven) + 1
        write(fmt_txt,'(a1,i2,a8)') '(', n, 'i16,2a1)'
        write(zbuf%fixbuf(1),fmt_txt) item_sf(is1,ist+1:ist+n),         &
     &                               char(10), char(0)
        call gz_write_textbuf_no_lf_f((n*16+2), zbuf%fixbuf(1), zbuf)
        ist = ist + n
        if(ist .ge. istack(1)) exit
      end do
!
      end subroutine write_gz_surf_group
!
!------------------------------------------------------------------
!
      subroutine write_gz_multi_int_8i16(num, int_data, zbuf)
!
      use calypso_c_binding
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: int_data(num)
      type(buffer_4_gzip), intent(inout):: zbuf
!
      integer(kind = kint) :: ist, n
      character(len=kchara) :: fmt_txt
!
!
      ist = 0
      do
        n = min(num-ist-ione,iseven) + 1
        write(fmt_txt,'(a1,i2,a8)') '(', n, 'i16,2a1)'
        write(zbuf%fixbuf(1),fmt_txt) int_data(ist+1:ist+n),            &
     &                               char(10), char(0)
        call gz_write_textbuf_no_lf_f((n*16+2), zbuf%fixbuf(1), zbuf)
        ist = ist + n
        if(ist .ge. num) exit
      end do
!
      end subroutine write_gz_multi_int_8i16
!
!------------------------------------------------------------------
!
      subroutine write_gz_multi_int_10i8(num, int_data, zbuf)
!
      use calypso_c_binding
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: int_data(num)
      type(buffer_4_gzip), intent(inout):: zbuf
!
      integer(kind = kint) :: ist, n
      character(len=kchara) :: fmt_txt
!
!
      ist = 0
      do
        n = min(num-ist-ione,inine) + 1
        write(fmt_txt,'(a1,i3,a7)') '(', n, 'i8,2a1)'
        write(zbuf%fixbuf(1),fmt_txt) int_data(ist+1:ist+n),            &
     &                               char(10), char(0)
        call gz_write_textbuf_no_lf_f((n*8+2), zbuf%fixbuf(1), zbuf)
        ist = ist + n
        if(ist .ge. num) exit
      end do
!
      end subroutine write_gz_multi_int_10i8
!
!------------------------------------------------------------------
!
      subroutine write_gz_multi_int_10i12(num, int_data, zbuf)
!
      use calypso_c_binding
!
      integer(kind = kint), intent(in) :: num
      integer(kind = kint), intent(in) :: int_data(num)
      type(buffer_4_gzip), intent(inout):: zbuf
!
      integer(kind = kint) :: ist, n
      character(len=kchara) :: fmt_txt
!
!
      ist = 0
      do
        n = min(num-ist-ione,inine) + 1
        write(fmt_txt,'(a1,i3,a8)') '(', n, 'i12,2a1)'
        write(zbuf%fixbuf(1),fmt_txt) int_data(ist+1:ist+n),            &
     &                               char(10), char(0)
        call gz_write_textbuf_no_lf_f((n*12+2), zbuf%fixbuf(1), zbuf)
        ist = ist + n
        if(ist .ge. num) exit
      end do
!
      end subroutine write_gz_multi_int_10i12
!
!------------------------------------------------------------------
!
      subroutine write_gz_comment_string(comment, zbuf)
!
      use calypso_c_binding
!
      character(len=*), intent(in)  ::  comment
      type(buffer_4_gzip), intent(inout):: zbuf
!
!
      write(zbuf%fixbuf(1),'(a,2a1)') comment, char(10), char(0)
      call gz_write_textbuf_no_lf_f(length_of_c_text(zbuf%fixbuf(1)),   &
     &    zbuf%fixbuf(1), zbuf)
!
      end subroutine write_gz_comment_string
!
!------------------------------------------------------------------
!
      subroutine gz_write_chara_nolf(chara_output, zbuf)
!
      use calypso_c_binding
!
      character(len=kchara), intent(in) :: chara_output
      type(buffer_4_gzip), intent(inout):: zbuf
!
      integer(kind = kint) :: n
!
      write(zbuf%fixbuf(1),'(2a,a1)') trim(chara_output), '    ',       &
     &                               CHAR(0)
      n = len_trim(chara_output) + 4 + 1
      call gz_write_textbuf_no_lf_f(n, zbuf%fixbuf(1), zbuf)
!
      end subroutine gz_write_chara_nolf
!
! ----------------------------------------------------------------------
!
      end module skip_gz_comment
