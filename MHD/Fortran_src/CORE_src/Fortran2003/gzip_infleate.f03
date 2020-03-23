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
!!     &         (len_gzipbuf, gzipbuf, num, data, len_gzipped)
!!      subroutine gzip_infleat_int8_once                               &
!!     &         (len_gzipbuf, gzipbuf, num, int8_dat, len_gzipped)
!!      subroutine gzip_infleat_int4_once                               &
!!     &         (len_gzipbuf, gzipbuf, num, int4_dat, len_gzipped)
!!      subroutine gzip_infleat_char_once                               &
!!     &         (len_gzipbuf, gzipbuf, len_buf, buf, len_gzipped)
!!
!!      subroutine gzip_infleat_char_begin                              &
!!     &         (len_gzipbuf, gzipbuf, len_buf, buf, len_gzipped)
!!      subroutine gzip_infleat_char_cont                               &
!!     &         (len_gzipbuf, len_buf, buf, len_gzipped)
!!      subroutine gzip_infleat_char_last                               &
!!     &         (len_gzipbuf, len_buf, buf, len_gzipped)
!!@endverbatim
!
      module gzip_infleate
!
      use ISO_C_BINDING
      use m_precision
!
      implicit none
!
      type zlib_transfer
        integer(C_int) :: len_gzipbuf_c
        integer(C_int) :: len_buf_c
        integer(C_int) :: len_gzipped_c
!
        character(C_char), pointer :: gzipbuf_p(:)
        character(C_char), pointer :: buf_p(:)
      end type zlib_transfer
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
     &            BIND(C, name = 'gzip_infleat_begin')
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
     &         (len_gzipbuf, gzipbuf, num, data, len_gzipped)
!
      integer, intent(in) :: len_gzipbuf
      integer, intent(in) :: num
      character(len=1), target, intent(in) :: gzipbuf(len_gzipbuf)
!
      real(kind = kreal), target, intent(inout) :: data(num)
      integer, intent(inout) :: len_gzipped
!
      integer(C_int) :: len_gzipbuf_c, len_buf_c, len_gzipped_c
      character(C_char), pointer :: gzipbuf_p(:)
      real(C_double), pointer :: dat_p(:)
!
!
      len_gzipbuf_c = int(len_gzipbuf,KIND(len_gzipbuf_c))
      len_buf_c =     int((num*kreal),KIND(len_buf_c))
      len_gzipped_c = int(len_gzipped,KIND(len_gzipped_c))
      gzipbuf_p => gzipbuf
      dat_p => data
!
      call gzip_infleat_once(len_gzipbuf_c, gzipbuf_p, len_buf_c,       &
     &    C_LOC(dat_p), len_gzipped_c)
      len_gzipped = int(len_gzipped_c,KIND(len_gzipped))
!
      end subroutine gzip_infleat_real_once
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_infleat_int8_once                                 &
     &         (len_gzipbuf, gzipbuf, num, int8_dat, len_gzipped)
!
      integer, intent(in) :: len_gzipbuf
      integer, intent(in) :: num
      character(len=1), target, intent(in) :: gzipbuf(len_gzipbuf)
!
      integer(kind = kint_gl), target, intent(inout) :: int8_dat(num)
      integer, intent(inout) :: len_gzipped
!
      integer(C_int) :: len_gzipbuf_c, len_buf_c, len_gzipped_c
      character(C_char), pointer :: gzipbuf_p(:)
      integer(C_long), pointer :: idat8_p(:)
!
!
      len_gzipbuf_c = int(len_gzipbuf,KIND(len_gzipbuf_c))
      len_buf_c =     int((num*kint_gl),KIND(len_buf_c))
      len_gzipped_c = int(len_gzipped,KIND(len_gzipped_c))
      gzipbuf_p => gzipbuf
      idat8_p => int8_dat
!
      call gzip_infleat_once(len_gzipbuf_c, gzipbuf_p, len_buf_c,       &
     &    C_LOC(idat8_p), len_gzipped_c)
      len_gzipped = int(len_gzipped_c,KIND(len_gzipped))
!
      end subroutine gzip_infleat_int8_once
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_infleat_int4_once                                 &
     &         (len_gzipbuf, gzipbuf, num, int4_dat, len_gzipped)
!
      integer, intent(in) :: len_gzipbuf
      integer, intent(in) :: num
      character(len=1), target, intent(in) :: gzipbuf(len_gzipbuf)
!
      integer(kind = 4), target, intent(inout) :: int4_dat(num)
      integer, intent(inout) :: len_gzipped
!
      integer(C_int) :: len_gzipbuf_c, len_buf_c, len_gzipped_c
      character(C_char), pointer :: gzipbuf_p(:)
      integer(C_int), pointer :: idat4_p(:)
!
!
      len_gzipbuf_c = int(len_gzipbuf,KIND(len_gzipbuf_c))
      len_buf_c =     int((num*4),KIND(len_buf_c))
      len_gzipped_c = int(len_gzipped,KIND(len_gzipped_c))
      gzipbuf_p => gzipbuf
      idat4_p => int4_dat
!
      call gzip_infleat_once(len_gzipbuf_c, gzipbuf_p, len_buf_c,       &
     &    C_LOC(idat4_p), len_gzipped_c)
      len_gzipped = int(len_gzipped_c,KIND(len_gzipped))
!
      end subroutine gzip_infleat_int4_once
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_infleat_char_once                                 &
     &         (len_gzipbuf, gzipbuf, len_buf, buf, len_gzipped)
!
      integer, intent(in) :: len_gzipbuf
      integer, intent(in) :: len_buf
      character(len=1), target, intent(in) :: gzipbuf(len_gzipbuf)
!
      character(len=1), target, intent(inout) :: buf(len_buf)
      integer, intent(inout) :: len_gzipped
!
      integer(C_int) :: len_gzipbuf_c, len_buf_c, len_gzipped_c
      character(C_char), pointer :: gzipbuf_p(:), buf_p(:)
!
!
      len_gzipbuf_c = int(len_gzipbuf,KIND(len_gzipbuf_c))
      len_buf_c =     int(len_buf,KIND(len_buf_c))
      len_gzipped_c = int(len_gzipped,KIND(len_gzipped_c))
      gzipbuf_p => gzipbuf
      buf_p => buf
!
      call gzip_infleat_once(len_gzipbuf_c, gzipbuf_p, len_buf_c,       &
     &    C_LOC(buf_p), len_gzipped_c)
      len_gzipped = int(len_gzipped_c,KIND(len_gzipped))
!
      end subroutine gzip_infleat_char_once
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine gzip_infleat_char_begin                                &
     &         (len_gzipbuf, gzipbuf, len_buf, buf, z_buf)
!
      integer, intent(in) :: len_gzipbuf
      integer, intent(in) :: len_buf
      character(len=1), target, intent(in) :: gzipbuf(len_gzipbuf)
!
      character(len=1), target, intent(inout) :: buf(len_buf)
      type(zlib_transfer), intent(inout) :: z_buf
!
      integer(C_int) :: len_gzipbuf_c
      character(C_char), pointer :: buf_p(:)
!
!
      len_gzipbuf_c = int(len_gzipbuf,KIND(len_gzipbuf_c))
      z_buf%len_buf_c =     int(len_buf,KIND(z_buf%len_buf_c))
      z_buf%gzipbuf_p => gzipbuf
      buf_p => buf
!
      write(*,*) 'gzip_infleat_begin'
      call gzip_infleat_begin                                           &
     &   (len_gzipbuf_c, z_buf%gzipbuf_p, z_buf%len_buf_c,              &
     &    C_LOC(buf_p), z_buf%len_gzipped_c)
!
      end subroutine gzip_infleat_char_begin
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_infleat_char_cont                                 &
     &         (len_gzipbuf, len_buf, buf, z_buf)
!
      integer, intent(in) :: len_gzipbuf
      integer, intent(in) :: len_buf
!
      character(len=1), target, intent(inout) :: buf(len_buf)
!
      integer(C_int) :: len_gzipbuf_c
      character(C_char), pointer :: buf_p(:)
      type(zlib_transfer), intent(inout) :: z_buf
!
!
      len_gzipbuf_c = int(len_gzipbuf,KIND(len_gzipbuf_c))
      buf_p => buf
!
      write(*,*) 'gzip_infleat_cont'
      call gzip_infleat_cont(len_gzipbuf_c, z_buf%len_buf_c,            &
     &    C_LOC(buf_p), z_buf%len_gzipped_c)
!
      end subroutine gzip_infleat_char_cont
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_infleat_char_last                                 &
     &         (len_gzipbuf, len_buf, buf, len_gzipped, z_buf)
!
      integer, intent(in) :: len_gzipbuf
      integer, intent(in) :: len_buf
!
      character(len=1), target, intent(inout) :: buf(len_buf)
      integer, intent(inout) :: len_gzipped
      type(zlib_transfer), intent(inout) :: z_buf
!
      integer(C_int) :: len_gzipbuf_c
      character(C_char), pointer :: buf_p(:)
!
!
      len_gzipbuf_c = int(len_gzipbuf,KIND(len_gzipbuf_c))
      buf_p => buf
!
      write(*,*) 'gzip_infleat_last'
      call gzip_infleat_last(len_gzipbuf_c, z_buf%len_buf_c,            &
     &    C_LOC(buf_p), z_buf%len_gzipped_c)
      len_gzipped = int(z_buf%len_gzipped_c,KIND(len_gzipped))
!
      end subroutine gzip_infleat_char_last
!
!  ---------------------------------------------------------------------
!
      end module gzip_infleate
