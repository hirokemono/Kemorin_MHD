!>@file   gzip_defleate.f90
!!        module gzip_defleate
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Wrapper for compression routines by zlib
!!
!!@verbatim
!!      subroutine gzip_defleat_real_once                               &
!!     &         (len_gzipbuf, gzipbuf, num, data, zbuf)
!!      subroutine gzip_defleat_int8_once                               &
!!     &         (len_gzipbuf, gzipbuf, num, int8_dat, zbuf)
!!      subroutine gzip_defleat_int4_once                               &
!!     &         (len_gzipbuf, gzipbuf, num, int4_dat, zbuf)
!!
!!      subroutine zlib_defleat_char_once                               &
!!     &         (len_buf, textbuf, len_gzipbuf, zbuf, gzipbuf)
!!      subroutine gzip_defleat_char_once                               &
!!     &         (len_gzipbuf, gzipbuf, len_buf, buf, zbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine gzip_defleat_char_begin(stream_ptr, len_buf, textbuf,&
!!     &                                   len_gzipbuf, zbuf, gzipbuf)
!!      subroutine gzip_defleat_char_cont(stream_ptr, len_buf,          &
!!     &                                  textbuf, zbuf)
!!      subroutine gzip_defleat_char_last(stream_ptr, len_buf,          &
!!     &                                  textbuf, zbuf)
!!        character, pointer, intent(inout) :: stream_ptr
!!        integer, intent(in) :: len_buf
!!        character(len=1), target, intent(in) :: textbuf(len_buf)
!!        integer, intent(in) :: len_gzipbuf
!!        character(len=1), target, intent(inout) :: gzipbuf(len_gzipbuf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!
      module gzip_defleate
!
      use ISO_C_BINDING
      use m_precision
      use t_buffer_4_gzip
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      interface
!
!  ---------------------------------------------------------------------
        subroutine calypso_gzip_defleat_once                            &
     &           (len_buf, buf, len_gzipbuf, len_gzipped, gzipbuf)      &
     &            BIND(C, name = 'calypso_gzip_defleat_once')
!
        use ISO_C_BINDING
!
        integer(C_int), intent(in) :: len_buf
        type(C_ptr), value, intent(in) :: buf
!
        integer(C_int), intent(in) :: len_gzipbuf
        character(C_char), intent(inout) :: gzipbuf(*)
        integer(C_int), intent(inout) :: len_gzipped
!
        end subroutine calypso_gzip_defleat_once
!  -----------------
        type(C_ptr) function calypso_gzip_defleat_begin                 &
     &           (len_buf, buf, len_gzipbuf, len_gzipped, gzipbuf)      &
     &            BIND(C, name = 'calypso_gzip_defleat_begin')
!
        use ISO_C_BINDING
!
        integer(C_int), intent(in) :: len_buf
        type(C_ptr), value, intent(in) :: buf
!
        integer(C_int), intent(in) :: len_gzipbuf
        character(C_char), intent(inout) :: gzipbuf(*)
        integer(C_int), intent(inout) :: len_gzipped
!
        end function calypso_gzip_defleat_begin
!  -----------------
        subroutine calypso_gzip_defleat_cont                            &
     &           (stream_ptr, len_buf, buf, len_gzipbuf, len_gzipped)   &
     &            BIND(C, name = 'calypso_gzip_defleat_cont')
!
        use ISO_C_BINDING
!
        type(C_ptr), value :: stream_ptr
        integer(C_int), intent(in) :: len_buf
        type(C_ptr), value, intent(in) :: buf
!
        integer(C_int), intent(in) :: len_gzipbuf
        integer(C_int), intent(inout) :: len_gzipped
!
        end subroutine calypso_gzip_defleat_cont
!  -----------------
        subroutine calypso_gzip_defleat_last                            &
     &           (stream_ptr, len_buf, buf, len_gzipbuf, len_gzipped)   &
     &            BIND(C, name = 'calypso_gzip_defleat_last')
!
        use ISO_C_BINDING
!
        type(C_ptr), value :: stream_ptr
        integer(C_int), intent(in) :: len_buf
        type(C_ptr), value, intent(in) :: buf
!
        integer(C_int), intent(in) :: len_gzipbuf
        integer(C_int), intent(inout) :: len_gzipped
!
        end subroutine calypso_gzip_defleat_last
!  -----------------
        subroutine calypso_zlib_defleat_once                            &
     &           (len_buf, buf, len_gzipbuf, len_gzipped, gzipbuf)      &
     &            BIND(C, name = 'calypso_zlib_defleat_once')
!
        use ISO_C_BINDING
!
        integer(C_int), intent(in) :: len_buf
        type(C_ptr), value, intent(in) :: buf
!
        integer(C_int), intent(in) :: len_gzipbuf
        character(C_char), intent(inout) :: gzipbuf(*)
        integer(C_int), intent(inout) :: len_gzipped
!
        end subroutine calypso_zlib_defleat_once
!  -----------------
        type(C_ptr) function calypso_zlib_defleat_begin                 &
     &           (len_buf, buf, len_gzipbuf, len_gzipped, gzipbuf)      &
     &            BIND(C, name = 'calypso_zlib_defleat_begin')
!
        use ISO_C_BINDING
!
        integer(C_int), intent(in) :: len_buf
        type(C_ptr), value, intent(in) :: buf
!
        integer(C_int), intent(in) :: len_gzipbuf
        character(C_char), intent(inout) :: gzipbuf(*)
        integer(C_int), intent(inout) :: len_gzipped
!
        end function calypso_zlib_defleat_begin
!  -----------------
!
      end interface
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_defleat_real_once                                 &
     &         (num, data, len_gzipbuf, zbuf, gzipbuf)
!
      integer, intent(in) :: num
      real(kind = kreal), target, intent(in) :: data(num)
      integer, intent(in) :: len_gzipbuf
!
      character(len=1), target, intent(inout) :: gzipbuf(len_gzipbuf)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_compressed_buffer(len_gzipbuf, gzipbuf, zbuf)
      call link_real_buffer_for_zlib(num, data, zbuf)
      call calypso_gzip_defleat_once                                    &
     &   (zbuf%len_buf, C_LOC(zbuf%dat_p(1)),                           &
     &    zbuf%len_gzipbuf, zbuf%len_used, zbuf%gzipbuf_p)
      zbuf%ilen_gzipped = zbuf%ilen_gzipped                             &
     &                   + int(zbuf%len_used,KIND(zbuf%ilen_gzipped))
!
      end subroutine gzip_defleat_real_once
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_defleat_int8_once                                 &
     &         (num, int8_dat, len_gzipbuf, zbuf, gzipbuf)
!
      integer, intent(in) :: num
      integer(kind = kint_gl), target, intent(in) :: int8_dat(num)
      integer, intent(in) :: len_gzipbuf
!
      character(len=1), target, intent(inout) :: gzipbuf(len_gzipbuf)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_compressed_buffer(len_gzipbuf, gzipbuf, zbuf)
      call link_int8_buffer_for_zlib(num, int8_dat, zbuf)
      call calypso_gzip_defleat_once                                    &
     &   (zbuf%len_buf, C_LOC(zbuf%idat8_p(1)),                         &
     &    zbuf%len_gzipbuf, zbuf%len_used, zbuf%gzipbuf_p)
      zbuf%ilen_gzipped = zbuf%ilen_gzipped                             &
     &                   + int(zbuf%len_used,KIND(zbuf%ilen_gzipped))
!
      end subroutine gzip_defleat_int8_once
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_defleat_int4_once                                 &
     &         (num, int4_dat, len_gzipbuf, zbuf, gzipbuf)
!
      integer, intent(in) :: num
      integer(kind = 4), target, intent(in) :: int4_dat(num)
      integer, intent(in) :: len_gzipbuf
!
      character(len=1), target, intent(inout) :: gzipbuf(len_gzipbuf)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_compressed_buffer(len_gzipbuf, gzipbuf, zbuf)
      call link_int4_buffer_for_zlib(num, int4_dat, zbuf)
      call calypso_gzip_defleat_once                                    &
     &   (zbuf%len_buf, C_LOC(zbuf%idat4_p(1)),                         &
     &    zbuf%len_gzipbuf, zbuf%len_used, zbuf%gzipbuf_p)
      zbuf%ilen_gzipped = zbuf%ilen_gzipped                             &
     &                   + int(zbuf%len_used,KIND(zbuf%ilen_gzipped))
!
      end subroutine gzip_defleat_int4_once
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine zlib_defleat_char_once                                 &
     &         (len_buf, textbuf, len_gzipbuf, zbuf, gzipbuf)
!
      integer, intent(in) :: len_buf
      character(len=1), target, intent(in) :: textbuf(len_buf)
      integer, intent(in) :: len_gzipbuf
!
      character(len=1), target, intent(inout) :: gzipbuf(len_gzipbuf)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_compressed_buffer(len_gzipbuf, gzipbuf, zbuf)
      call link_text_buffer_for_zlib(len_buf, textbuf, zbuf)
      call calypso_zlib_defleat_once                                    &
     &   (zbuf%len_buf, C_LOC(zbuf%buf_p(1)),                           &
     &    zbuf%len_gzipbuf, zbuf%len_used, zbuf%gzipbuf_p)
      zbuf%ilen_gzipped = zbuf%ilen_gzipped                             &
     &                   + int(zbuf%len_used,KIND(zbuf%ilen_gzipped))
!
      end subroutine zlib_defleat_char_once
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_defleat_char_once                                 &
     &         (len_buf, textbuf, len_gzipbuf, zbuf, gzipbuf)
!
      integer, intent(in) :: len_buf
      character(len=1), target, intent(in) :: textbuf(len_buf)
      integer, intent(in) :: len_gzipbuf
!
      character(len=1), target, intent(inout) :: gzipbuf(len_gzipbuf)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_compressed_buffer(len_gzipbuf, gzipbuf, zbuf)
      call link_text_buffer_for_zlib(len_buf, textbuf, zbuf)
      call calypso_gzip_defleat_once                                    &
     &   (zbuf%len_buf, C_LOC(zbuf%buf_p(1)),                           &
     &    zbuf%len_gzipbuf, zbuf%len_used, zbuf%gzipbuf_p)
      zbuf%ilen_gzipped = zbuf%ilen_gzipped                             &
     &                   + int(zbuf%len_used,KIND(zbuf%ilen_gzipped))
!
      end subroutine gzip_defleat_char_once
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine gzip_defleat_char_begin(stream_ptr, len_buf, textbuf,  &
     &                                   len_gzipbuf, zbuf, gzipbuf)
!
      character, pointer, intent(inout) :: stream_ptr
      integer, intent(in) :: len_buf
      character(len=1), target, intent(in) :: textbuf(len_buf)
      integer, intent(in) :: len_gzipbuf
      character(len=1), target, intent(inout) :: gzipbuf(len_gzipbuf)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      type(C_ptr) :: strm_p
!
      call link_compressed_buffer(len_gzipbuf, gzipbuf, zbuf)
      call link_text_buffer_for_zlib(len_buf, textbuf, zbuf)
      strm_p = calypso_gzip_defleat_begin                               &
     &               (zbuf%len_buf, C_LOC(zbuf%buf_p(1)),               &
     &                zbuf%len_gzipbuf, zbuf%len_used, zbuf%gzipbuf_p)
      call c_f_pointer(strm_p, stream_ptr)
!
      end subroutine gzip_defleat_char_begin
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_defleat_char_cont(stream_ptr, len_buf,            &
     &                                  textbuf, zbuf)
!
      character, pointer, intent(in) :: stream_ptr
      integer, intent(in) :: len_buf
      character(len=1), target, intent(in) :: textbuf(len_buf)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_text_buffer_for_zlib(len_buf, textbuf, zbuf)
      call calypso_gzip_defleat_cont                                    &
     &   (C_LOC(stream_ptr), zbuf%len_buf, C_LOC(zbuf%buf_p(1)),        &
     &    zbuf%len_gzipbuf, zbuf%len_used)
!
      end subroutine gzip_defleat_char_cont
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_defleat_char_last(stream_ptr, len_buf,            &
     &                                  textbuf, zbuf)
!
      character, pointer, intent(inout) :: stream_ptr
      integer, intent(in) :: len_buf
      character(len=1), target, intent(in) :: textbuf(len_buf)
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_text_buffer_for_zlib(len_buf, textbuf, zbuf)
      call calypso_gzip_defleat_last                                    &
     &   (C_LOC(stream_ptr), zbuf%len_buf, C_LOC(zbuf%buf_p(1)),        &
     &    zbuf%len_gzipbuf, zbuf%len_used)
      call unlink_pointer_for_zlib_buffer(zbuf)
      zbuf%ilen_gzipped = zbuf%ilen_gzipped                             &
     &                   + int(zbuf%len_used,KIND(zbuf%ilen_gzipped))
      nullify(stream_ptr)
!
      end subroutine gzip_defleat_char_last
!
!  ---------------------------------------------------------------------
!
      end module gzip_defleate
