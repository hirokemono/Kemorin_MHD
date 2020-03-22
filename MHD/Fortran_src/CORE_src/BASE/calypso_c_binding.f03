!
      module calypso_c_binding
!
      use ISO_C_BINDING
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      interface
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_infleat_once                                      &
     &         (len_gzipbuf, gzipbuf, len_buf, buf, len_gzipped)        &
     &          BIND(C, name = 'gzip_infleat_once')
!
        use ISO_C_BINDING
        integer(C_int), intent(in) :: len_gzipbuf
        character(C_char), intent(in) :: gzipbuf(*)
        integer(C_int), intent(in) :: len_buf
!
        character(C_char), intent(in) :: buf(*)
        integer(C_int), intent(inout) :: len_gzipped
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
      end module calypso_c_binding
!
!  ---------------------------------------------------------------------
!
      subroutine gzip_infleat_once_f                                 &
     &         (len_gzipbuf, gzipbuf, len_buf, buf, len_gzipped)
!
      use ISO_C_BINDING
      use calypso_c_binding
!
      integer, intent(in) :: len_gzipbuf
      integer, intent(in) :: len_buf
      character(len=1), target, intent(in) :: gzipbuf(len_gzipbuf)
!
      character(len=1), target, intent(inout) :: buf(len_buf)
      integer, intent(inout) :: len_gzipped
!
      type(C_ptr) :: buf_c
      integer(C_int) :: len_gzipbuf_c, len_buf_c, len_gzipped_c
      character(C_char), pointer :: gzipbuf_p(:), buf_p(:)
!
!
      len_gzipbuf_c = int(len_gzipbuf,KIND(len_gzipbuf_c))
      len_buf_c = int(len_buf,KIND(len_buf_c))
      gzipbuf_p => gzipbuf
      buf_p => buf
!
      write(*,*) 'gzip_infleat_once from gzip_infleat_once_f'
      call gzip_infleat_once                                            &
     &  (len_gzipbuf_c, gzipbuf(1), len_buf_c, buf_p(1), len_gzipped_c)
      len_gzipped = int(len_gzipped_c,KIND(len_gzipped))
!      call C_F_POINTER(buf_c,buf_p,[len_buf])
!
      end subroutine gzip_infleat_once_f
!
!  ---------------------------------------------------------------------

