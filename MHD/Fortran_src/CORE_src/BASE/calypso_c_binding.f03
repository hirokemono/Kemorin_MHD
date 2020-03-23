!
!!      subroutine gzip_infleat_real_once                               &
!!     &         (len_gzipbuf, gzipbuf, num, data, len_gzipped)
!!      subroutine gzip_infleat_int8_once                               &
!!     &         (len_gzipbuf, gzipbuf, num, int8_dat, len_gzipped)
!!      subroutine gzip_infleat_int4_once                               &
!!     &         (len_gzipbuf, gzipbuf, num, int4_dat, len_gzipped)
!!      subroutine gzip_infleat_char_once                               &
!!     &         (len_gzipbuf, gzipbuf, len_buf, buf, len_gzipped)
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
        type(C_ptr), value, intent(in) :: buf
        integer(C_int), intent(inout) :: len_gzipped
!
        end subroutine gzip_infleat_once
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
      len_buf_c = int((num*kreal),KIND(len_buf_c))
      gzipbuf_p => gzipbuf
      dat_p => data
!
      call gzip_infleat_once(len_gzipbuf_c, gzipbuf(1), len_buf_c,      &
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
      len_buf_c = int((num*kint_gl),KIND(len_buf_c))
      gzipbuf_p => gzipbuf
      idat8_p => int8_dat
!
      call gzip_infleat_once(len_gzipbuf_c, gzipbuf(1), len_buf_c,      &
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
      len_buf_c = int((num*4),KIND(len_buf_c))
      gzipbuf_p => gzipbuf
      idat4_p => int4_dat
!
      call gzip_infleat_once(len_gzipbuf_c, gzipbuf(1), len_buf_c,      &
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
      len_buf_c = int(len_buf,KIND(len_buf_c))
      gzipbuf_p => gzipbuf
      buf_p => buf
!
      call gzip_infleat_once(len_gzipbuf_c, gzipbuf(1), len_buf_c,      &
     &    C_LOC(buf_p), len_gzipped_c)
      len_gzipped = int(len_gzipped_c,KIND(len_gzipped))
!
      end subroutine gzip_infleat_char_once
!
!  ---------------------------------------------------------------------
!
      end module calypso_c_binding
