!>@file   gzip_defleate.f03
!>@file   gzip_defleate.f03
!!        module gzip_defleate
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Wrapper for compression routines by zlib
!!
!!@verbatim
!!      subroutine alloc_textbuffer_for_zlib(len_buf, z_buf)
!!      subroutine dealloc_textbuffer_for_zlib(z_buf)
!!        type(zlib_transfer), intent(inout) :: z_buf
!!      subroutine link_pointer_for_zlib_buffer                         &
!!     &         (len_gzipbuf, gzipbuf, len_buf, textbuf, z_buf)
!!      subroutine unlink_pointer_for_zlib_buffer(z_buf)
!!        type(zlib_transfer), intent(inout) :: z_buf
!!
!!      subroutine gzip_defleat_real_once                               &
!!     &         (len_gzipbuf, gzipbuf, num, data, z_buf)
!!      subroutine gzip_defleat_int8_once                               &
!!     &         (len_gzipbuf, gzipbuf, num, int8_dat, z_buf)
!!      subroutine gzip_defleat_int4_once                               &
!!     &         (len_gzipbuf, gzipbuf, num, int4_dat, z_buf)
!!      subroutine gzip_defleat_char_once                               &
!!     &         (len_gzipbuf, gzipbuf, len_buf, buf, z_buf)
!!        type(zlib_transfer), intent(inout) :: z_buf
!!
!!      subroutine gzip_defleat_char_begin(z_buf)
!!      subroutine gzip_defleat_char_cont(z_buf)
!!      subroutine gzip_defleat_char_last(z_buf)
!!        type(zlib_transfer), intent(inout) :: z_buf
!!@endverbatim
!
      module gzip_defleate
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
!        subroutine gzip_defleat_once                                   &
!     &           (len_buf, buf, len_gzipbuf, len_gzipped, gzipbuf)     &
!     &            BIND(C, name = 'gzip_defleat_once')
!
!        use ISO_C_BINDING
!
!        integer(C_int), intent(in) :: len_gzipbuf
!        character(C_char), intent(in) :: gzipbuf(*)
!        integer(C_int), intent(in) :: len_buf
!
!        type(C_ptr), value :: buf
!        integer(C_int), intent(inout) :: len_gzipped
!
!        end subroutine gzip_defleat_once
!
!  ---------------------------------------------------------------------
!
!        subroutine gzip_defleat_begin                                  &
!     &           (len_buf, buf, len_gzipbuf, len_gzipped, gzipbuf)     &
!     &            BIND(C, name = 'gzip_defleat_begin')
!
!        use ISO_C_BINDING
!
!        integer(C_int), intent(in) :: len_gzipbuf
!        character(C_char), intent(in) :: gzipbuf(*)
!        integer(C_int), intent(in) :: len_buf
!
!        type(C_ptr), value :: buf
!        integer(C_int), intent(inout) :: len_gzipped
!
!        end subroutine gzip_defleat_begin
!
!  ---------------------------------------------------------------------
!
!        subroutine gzip_defleat_cont                                   &
!     &           (len_buf, buf, len_gzipbuf, len_gzipped)              &
!     &            BIND(C, name = 'gzip_defleat_cont')
!
!        use ISO_C_BINDING
!
!        integer(C_int), intent(in) :: len_gzipbuf
!        integer(C_int), intent(in) :: len_buf
!
!        type(C_ptr), value :: buf
!        integer(C_int), intent(inout) :: len_gzipped
!
!        end subroutine gzip_defleat_cont
!
!  ---------------------------------------------------------------------
!
!        subroutine gzip_defleat_last                                   &
!     &           (len_buf, buf, len_gzipbuf, len_gzipped)              &
!     &            BIND(C, name = 'gzip_defleat_last')
!
!        use ISO_C_BINDING
!
!        integer(C_int), intent(in) :: len_gzipbuf
!        integer(C_int), intent(in) :: len_buf
!
!        type(C_ptr), value :: buf
!        integer(C_int), intent(inout) :: len_gzipped
!
!        end subroutine gzip_defleat_last
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
      subroutine gzip_defleat_real_once                                 &
     &         (num, data, len_gzipbuf, len_used, gzipbuf)
!
      integer, intent(in) :: len_gzipbuf
      integer, intent(in) :: num
      character(len=1), target, intent(inout) :: gzipbuf(len_gzipbuf)
!
      real(kind = kreal), target, intent(in) :: data(num)
      integer, intent(inout) :: len_used
!      type(zlib_transfer), intent(inout) :: z_buf
!
      real(C_double), pointer :: dat_p(:)
!
!
!      z_buf%len_gzipbuf = int(len_gzipbuf,KIND(z_buf%len_gzipbuf))
!      z_buf%len_buf =     int((num*kreal),KIND(z_buf%len_buf))
!      z_buf%gzipbuf_p => gzipbuf
      dat_p => data
!
      call gzip_defleat_once(z_buf%len_buf, C_LOC(dat_p),               &
     &    z_buf%len_gzipbuf, z_buf%len_used, z_buf%gzipbuf_p)
!
      end subroutine gzip_defleat_real_once
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_defleat_int8_once                                 &
     &         (num, int8_dat, len_gzipbuf, len_used, gzipbuf)
!
      integer, intent(in) :: len_gzipbuf
      integer, intent(in) :: num
      character(len=1), target, intent(inout) :: gzipbuf(len_gzipbuf)
!
      integer(kind = kint_gl), target, intent(in) :: int8_dat(num)
      integer, intent(inout) :: len_used
!      type(zlib_transfer), intent(inout) :: z_buf
!
      integer(C_long), pointer :: idat8_p(:)
!
!
!      z_buf%len_gzipbuf = int(len_gzipbuf,KIND(z_buf%len_gzipbuf))
!      z_buf%len_buf =     int((num*kint_gl),KIND(z_buf%len_buf))
!      z_buf%gzipbuf_p => gzipbuf
      idat8_p => int8_dat
!
      call gzip_defleat_once( z_buf%len_buf, C_LOC(idat8_p),            &
     &    z_buf%len_gzipbuf, z_buf%len_used, z_buf%gzipbuf_p)
!
      end subroutine gzip_defleat_int8_once
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_defleat_int4_once                                 &
     &         (num, int4_dat, len_gzipbuf, len_used, gzipbuf)
!
      integer, intent(in) :: len_gzipbuf
      integer, intent(in) :: num
      character(len=1), target, intent(inout) :: gzipbuf(len_gzipbuf)
!
      integer(kind = 4), target, intent(in) :: int4_dat(num)
      integer, intent(inout) :: len_used
!      type(zlib_transfer), intent(inout) :: z_buf
!
      integer(C_int), pointer :: idat4_p(:)
!
!
!      z_buf%len_gzipbuf = int(len_gzipbuf,KIND(z_buf%len_gzipbuf))
!      z_buf%len_buf =     int((num*4),KIND(z_buf%len_buf))
!      z_buf%gzipbuf_p => gzipbuf
      idat4_p => int4_dat
!
      call gzip_defleat_once(z_buf%len_buf, C_LOC(idat4_p),        &
     &    z_buf%len_gzipbuf, z_buf%len_used, z_buf%gzipbuf_p)
!
      end subroutine gzip_defleat_int4_once
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine gzip_defleat_char_once                                 &
     &         (len_buf, textbuf, len_gzipbuf, len_used, gzipbuf)
!
      integer, intent(in) :: len_gzipbuf
      character(len=1), target, intent(inout) :: gzipbuf(len_gzipbuf)
      integer, intent(in) :: len_buf
      character(len=1), target, intent(in) :: textbuf(len_buf)
!
!      type(zlib_transfer), intent(inout) :: z_buf
      integer, intent(inout) :: len_used
!
!
      call gzip_defleat_once(z_buf%len_buf, C_LOC(z_buf%buf_p),       &
     &    z_buf%len_gzipbuf, z_buf%len_used, z_buf%gzipbuf_p)
!
      end subroutine gzip_defleat_char_once
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine gzip_defleat_char_begin  &
     &         (len_buf, textbuf, len_gzipbuf, len_used, gzipbuf)
!
      integer, intent(in) :: len_gzipbuf
      character(len=1), target, intent(inout) :: gzipbuf(len_gzipbuf)
      integer, intent(in) :: len_buf
      character(len=1), target, intent(in) :: textbuf(len_buf)
      integer, intent(inout) :: len_used
!      type(zlib_transfer), intent(inout) :: z_buf
!
!
      call gzip_defleat_begin(z_buf%len_buf, C_LOC(z_buf%buf_p),       &
     &    z_buf%len_gzipbuf, z_buf%len_used, z_buf%gzipbuf_p)
!
      end subroutine gzip_defleat_char_begin
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_defleat_char_cont   &
     &         (len_buf, textbuf, len_gzipbuf, len_used)
!
      integer, intent(in) :: len_gzipbuf
      integer, intent(in) :: len_buf
      character(len=1), target, intent(in) :: textbuf(len_buf)
      integer, intent(inout) :: len_used
!      type(zlib_transfer), intent(inout) :: z_buf
!
!
      call gzip_defleat_cont(z_buf%len_buf, C_LOC(z_buf%buf_p),         &
     &     z_buf%len_gzipbuf, z_buf%len_used)
!
      end subroutine gzip_defleat_char_cont
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_defleat_char_last  &
     &         (len_buf, textbuf, len_gzipbuf, len_used)
!
      integer, intent(in) :: len_gzipbuf
      integer, intent(in) :: len_buf
      character(len=1), target, intent(in) :: textbuf(len_buf)
      integer, intent(inout) :: len_used
!      type(zlib_transfer), intent(inout) :: z_buf
!
!
      write(*,*) 'gzip_defleat_last'
      call gzip_defleat_last(z_buf%len_buf, C_LOC(z_buf%buf_p),  &
     &    z_buf%len_gzipbuf, z_buf%len_used)
!
      end subroutine gzip_defleat_char_last
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      end module gzip_defleate
