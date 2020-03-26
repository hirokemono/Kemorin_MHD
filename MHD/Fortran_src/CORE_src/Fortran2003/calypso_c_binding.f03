!>@file   calypso_c_binding.f90
!!        module calypso_c_binding
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Wrapper for decompression routines by zlib
!!
!!@verbatim
!!@endverbatim
!
      module calypso_c_binding
!
      use ISO_C_BINDING
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      interface
!
!  ---------------------------------------------------------------------
!
        subroutine get_one_line_from_gz                                 &
     &           (num_buffer, num_word, nchara, line_buf)               &
     &            BIND(C, name = 'get_one_line_from_gz')
!
        use ISO_C_BINDING
!
        integer(C_int), intent(in) :: num_buffer
        integer(C_int), intent(inout) :: num_word
        integer(C_int), intent(inout) :: nchara
        character(C_char), intent(inout) :: line_buf(*)
!
        end subroutine get_one_line_from_gz
!
!  ---------------------------------------------------------------------
!
        subroutine write_compress_txt(nchara, line_buf)                 &
     &            BIND(C, name = 'write_compress_txt')
!
        use ISO_C_BINDING
!
        integer(C_int), intent(in) :: nchara
        character(C_char), intent(in) :: line_buf(*)
!
        end subroutine write_compress_txt
!
!  ---------------------------------------------------------------------
!
        subroutine write_compress_txt_nolf(nchara, line_buf)            &
     &            BIND(C, name = 'write_compress_txt_nolf')
!
        use ISO_C_BINDING
!
        integer(C_int), intent(in) :: nchara
        character(C_char), intent(in) :: line_buf(*)
!
        end subroutine write_compress_txt_nolf
!
!  ---------------------------------------------------------------------
!
      end interface
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine get_one_line_from_gz_f03(len_buf, textbuf, zbuf)
!
      use t_buffer_4_gzip
!
      integer, intent(in) :: len_buf
      character(len=1), intent(inout) :: textbuf(len_buf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_text_buffer_for_zlib(len_buf, textbuf, zbuf)
      call get_one_line_from_gz(zbuf%len_buf, zbuf%num_word,            &
     &    zbuf%len_used, zbuf%buf_p)
      call unlink_text_buffer_for_zlib(zbuf)
!
      end subroutine get_one_line_from_gz_f03
!
!  ---------------------------------------------------------------------
!
      subroutine gz_write_textbuf_w_lf_f(len_buf, textbuf, zbuf)
!
      use t_buffer_4_gzip
!
      integer, intent(in) :: len_buf
      character(len=1), intent(in) :: textbuf(len_buf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      write(*,*) 'write_compress_txt'
      call link_text_buffer_for_zlib(len_buf, textbuf, zbuf)
      call write_compress_txt(zbuf%len_buf, zbuf%buf_p)
      call unlink_text_buffer_for_zlib(zbuf)
!
      end subroutine gz_write_textbuf_w_lf_f
!
! ----------------------------------------------------------------------
!
      subroutine gz_write_textbuf_no_lf_f(len_buf, textbuf, zbuf)
!
      use t_buffer_4_gzip
!
      integer, intent(in) :: len_buf
      character(len=1), intent(in) :: textbuf(len_buf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      write(*,*) 'write_compress_txt_nolf'
      call link_text_buffer_for_zlib(len_buf, textbuf, zbuf)
      call write_compress_txt_nolf(zbuf%len_buf, zbuf%buf_p)
      call unlink_text_buffer_for_zlib(zbuf)
!
      end subroutine gz_write_textbuf_no_lf_f
!
! ----------------------------------------------------------------------
!
      end module calypso_c_binding
