!>@file   gzip_infleate.f03
!!        module gzip_infleate
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Wrapper for decompression routines by zlib
!!
!!@verbatim
!!      subroutine gzip_infleat_real_once                               &
!!     &         (len_gzipbuf, gzipbuf, num, data, z_buf)
!!      subroutine gzip_infleat_int8_once                               &
!!     &         (len_gzipbuf, gzipbuf, num, int8_dat, z_buf)
!!      subroutine gzip_infleat_int4_once                               &
!!     &         (len_gzipbuf, gzipbuf, num, int4_dat, z_buf)
!!      subroutine gzip_infleat_char_once                               &
!!     &         (len_gzipbuf, gzipbuf, len_buf, buf, z_buf)
!!        type(zlib_transfer), intent(inout) :: z_buf
!!
!!      subroutine gzip_infleat_char_begin(z_buf)
!!      subroutine gzip_infleat_char_cont(z_buf)
!!      subroutine gzip_infleat_char_last(z_buf)
!!        type(zlib_transfer), intent(inout) :: z_buf
!!@endverbatim
!
      module gzip_infleate
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
!
        subroutine gzip_infleat_once                                    &
     &           (len_gzipbuf, gzipbuf, len_buf, buf, len_gzipped)      &
     &            BIND(C, name = 'gzip_infleat_once')
!
        use ISO_C_BINDING
!
        integer(C_int), intent(in) :: len_gzipbuf
        character(C_char), intent(in) :: gzipbuf(*)
        integer(C_int), intent(in) :: len_buf
!
        type(C_ptr), value :: buf
        integer(C_int), intent(inout) :: len_gzipped
!
        end subroutine gzip_infleat_once
!
!  ---------------------------------------------------------------------
!
        subroutine gzip_infleat_begin                                   &
     &           (len_gzipbuf, gzipbuf, len_buf, buf, len_gzipped)      &
     &            BIND(C, name = 'gzip_infleat_begin')
!
        use ISO_C_BINDING
!
        integer(C_int), intent(in) :: len_gzipbuf
        character(C_char), intent(in) :: gzipbuf(*)
        integer(C_int), intent(in) :: len_buf
!
        type(C_ptr), value :: buf
        integer(C_int), intent(inout) :: len_gzipped
!
        end subroutine gzip_infleat_begin
!
!  ---------------------------------------------------------------------
!
        subroutine gzip_infleat_cont                                    &
     &           (len_gzipbuf, len_buf, buf, len_gzipped)               &
     &            BIND(C, name = 'gzip_infleat_cont')
!
        use ISO_C_BINDING
!
        integer(C_int), intent(in) :: len_gzipbuf
        integer(C_int), intent(in) :: len_buf
!
        type(C_ptr), value :: buf
        integer(C_int), intent(inout) :: len_gzipped
!
        end subroutine gzip_infleat_cont
!
!  ---------------------------------------------------------------------
!
        subroutine gzip_infleat_last                                    &
     &           (len_gzipbuf, len_buf, buf, len_gzipped)               &
     &            BIND(C, name = 'gzip_infleat_last')
!
        use ISO_C_BINDING
!
        integer(C_int), intent(in) :: len_gzipbuf
        integer(C_int), intent(in) :: len_buf
!
        type(C_ptr), value :: buf
        integer(C_int), intent(inout) :: len_gzipped
!
        end subroutine gzip_infleat_last
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
      subroutine gzip_infleat_real_once                                 &
     &         (len_gzipbuf, gzipbuf, num, data, z_buf)
!
      integer, intent(in) :: len_gzipbuf
      integer, intent(in) :: num
      character(len=1), target, intent(in) :: gzipbuf(len_gzipbuf)
!
      real(kind = kreal), target, intent(inout) :: data(num)
      type(zlib_transfer), intent(inout) :: z_buf
!
!
      call link_compressed_buffer(len_gzipbuf, gzipbuf, z_buf)
      call link_real_buffer_for_zlib(num, data, z_buf)
      call gzip_infleat_once(z_buf%len_gzipbuf, z_buf%gzipbuf_p,        &
     &    z_buf%len_buf, C_LOC(z_buf%dat_p), z_buf%len_used)
!
      end subroutine gzip_infleat_real_once
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_infleat_int8_once                                 &
     &         (len_gzipbuf, gzipbuf, num, int8_dat, z_buf)
!
      integer, intent(in) :: len_gzipbuf
      integer, intent(in) :: num
      character(len=1), target, intent(in) :: gzipbuf(len_gzipbuf)
!
      integer(kind = kint_gl), target, intent(inout) :: int8_dat(num)
      type(zlib_transfer), intent(inout) :: z_buf
!
!
      call link_compressed_buffer(len_gzipbuf, gzipbuf, z_buf)
      call link_int8_buffer_for_zlib(num, int8_dat, z_buf)
      call gzip_infleat_once(z_buf%len_gzipbuf, z_buf%gzipbuf_p,        &
     &    z_buf%len_buf, C_LOC(z_buf%idat8_p), z_buf%len_used)
!
      end subroutine gzip_infleat_int8_once
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_infleat_int4_once                                 &
     &         (len_gzipbuf, gzipbuf, num, int4_dat, z_buf)
!
      integer, intent(in) :: len_gzipbuf
      integer, intent(in) :: num
      character(len=1), target, intent(in) :: gzipbuf(len_gzipbuf)
!
      integer(kind = 4), target, intent(inout) :: int4_dat(num)
      type(zlib_transfer), intent(inout) :: z_buf
!
!
      call link_compressed_buffer(len_gzipbuf, gzipbuf, z_buf)
      call link_int4_buffer_for_zlib(num, int4_dat, z_buf)
      call gzip_infleat_once(z_buf%len_gzipbuf, z_buf%gzipbuf_p,        &
     &    z_buf%len_buf, C_LOC(z_buf%idat4_p), z_buf%len_used)
!
      end subroutine gzip_infleat_int4_once
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine gzip_infleat_char_once                                 &
     &         (len_gzipbuf, gzipbuf, len_buf, textbuf, z_buf)
!
      integer, intent(in) :: len_gzipbuf
      character(len=1), target, intent(in) :: gzipbuf(len_gzipbuf)
      integer, intent(in) :: len_buf
      character(len=1), target, intent(in) :: textbuf(len_buf)
!
      type(zlib_transfer), intent(inout) :: z_buf
!
!
      call link_pointer_for_zlib_buffer                                 &
     &   (len_gzipbuf, gzipbuf, len_buf, textbuf, z_buf)
      call gzip_infleat_once(z_buf%len_gzipbuf, z_buf%gzipbuf_p,        &
     &    z_buf%len_buf, C_LOC(z_buf%buf_p), z_buf%len_used)
!
      end subroutine gzip_infleat_char_once
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine gzip_infleat_char_begin(z_buf)
!
      type(zlib_transfer), intent(inout) :: z_buf
!
      call gzip_infleat_begin(z_buf%len_gzipbuf, z_buf%gzipbuf_p,       &
     &    z_buf%len_buf, C_LOC(z_buf%buf_p), z_buf%len_used)
!
      end subroutine gzip_infleat_char_begin
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_infleat_char_cont(z_buf)
!
      type(zlib_transfer), intent(inout) :: z_buf
!
      call gzip_infleat_cont(z_buf%len_gzipbuf, z_buf%len_buf,          &
     &    C_LOC(z_buf%buf_p), z_buf%len_used)
!
      end subroutine gzip_infleat_char_cont
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_infleat_char_last(z_buf)
!
      type(zlib_transfer), intent(inout) :: z_buf
!
      call gzip_infleat_last(z_buf%len_gzipbuf, z_buf%len_buf,          &
     &    C_LOC(z_buf%buf_p), z_buf%len_used)
!
      end subroutine gzip_infleat_char_last
!
!  ---------------------------------------------------------------------
!
      end module gzip_infleate
