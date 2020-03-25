!>@file  t_buffer_4_gzip.f03
!!       module t_buffer_4_gzip
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2016
!!        Modified in March, 2020
!
!> @brief Buffer and pointers for zlib data IO
!!
!!@verbatim
!!      subroutine alloc_zip_buffer(zbuf)
!!      subroutine alloc_textbuffer_for_zlib(len_buf, z_buf)
!!      subroutine dealloc_zip_buffer(zbuf)
!!      subroutine dealloc_textbuffer_for_zlib(z_buf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!        type(zlib_transfer), intent(inout) :: z_buf
!!
!!      subroutine link_pointer_for_zlib_buffer                         &
!!     &         (len_gzipbuf, gzipbuf, len_buf, textbuf, z_buf)
!!      subroutine unlink_pointer_for_zlib_buffer(z_buf)
!!        type(zlib_transfer), intent(inout) :: z_buf
!!
!!      subroutine link_compressed_buffer(len_gzipbuf, gzipbuf, z_buf)
!!      subroutine link_real_buffer_for_zlib(num, data, z_buf)
!!      subroutine link_int8_buffer_for_zlib(num, int8_dat, z_buf)
!!      subroutine link_int4_buffer_for_zlib(num, int4_dat, z_buf)
!!      subroutine link_text_buffer_for_zlib(len_buf, textbuf, z_buf)
!!        type(zlib_transfer), intent(inout) :: z_buf
!!
!!      subroutine unlink_compressed_buffer(z_buf)
!!      subroutine unlink_real_buffer_for_zlib(z_buf)
!!      subroutine unlink_int8_buffer_for_zlib(z_buf)
!!      subroutine unlink_int4_buffer_for_zlib(z_buf)
!!      subroutine unlink_text_buffer_for_zlib(z_buf)
!!        type(zlib_transfer), intent(inout) :: z_buf
!!@endverbatim
!
      module t_buffer_4_gzip
!
      use ISO_C_BINDING
      use m_precision
!
      implicit none
!
!>      Structure of buffer for zlib
      type buffer_4_gzip
!>        Actual size of compressed data buffer
        integer(kind = kint_gl) :: ilen_gzipped
!>        Reserved size of compressed data buffer
        integer(kind = kint_gl) :: ilen_gz
!>        Compressed data buffer
        character(len=1), allocatable :: gzip_buf(:)
      end type buffer_4_gzip
!
      type zlib_transfer
!>        Size of compressed buffer to zlib
        integer(C_int) :: len_gzipbuf
!>        Size of decompressed buffer to zlib
        integer(C_int) :: len_buf
!>        Actual size of compressed buffer to zlib
        integer(C_int) :: len_used
!
!>        Pointer of compressed data buffer
        character(C_char), pointer :: gzipbuf_p(:)
!>        Pointer of decompressed text buffer
        character(C_char), pointer :: buf_p(:)
!>        Pointer of decompressed real data
        real(C_double), pointer :: dat_p(:)
!>        Pointer of decompressed 8-byte integer data
        integer(C_long), pointer :: idat8_p(:)
!>        Pointer of decompressed 84byte integer data
        integer(C_int), pointer :: idat4_p(:)
!
!>        decompressed text buffer
        character(len=1), allocatable :: textbuf(:)
      end type zlib_transfer
!
!  ---------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_zip_buffer(zbuf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      allocate(zbuf%gzip_buf(zbuf%ilen_gz))
!
      end subroutine alloc_zip_buffer
!
! -----------------------------------------------------------------------
!
      subroutine alloc_textbuffer_for_zlib(len_buf, z_buf)
!
      integer, intent(in) :: len_buf
      type(zlib_transfer), intent(inout) :: z_buf
!
      allocate(z_buf%textbuf(len_buf))
!
      end subroutine alloc_textbuffer_for_zlib
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_zip_buffer(zbuf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      deallocate(zbuf%gzip_buf)
!
      end subroutine dealloc_zip_buffer
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_textbuffer_for_zlib(z_buf)
!
      type(zlib_transfer), intent(inout) :: z_buf
!
      deallocate(z_buf%textbuf)
!
      end subroutine dealloc_textbuffer_for_zlib
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine link_pointer_for_zlib_buffer                           &
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
      call link_compressed_buffer(len_gzipbuf, gzipbuf, z_buf)
      call link_text_buffer_for_zlib(len_buf, textbuf, z_buf)
!
      end subroutine link_pointer_for_zlib_buffer
!
!  ---------------------------------------------------------------------
!
      subroutine unlink_pointer_for_zlib_buffer(z_buf)
!
      type(zlib_transfer), intent(inout) :: z_buf
!
      call unlink_compressed_buffer(z_buf)
      call unlink_text_buffer_for_zlib(z_buf)
!
      end subroutine unlink_pointer_for_zlib_buffer
!
!  ---------------------------------------------------------------------
!
      subroutine link_compressed_buffer(len_gzipbuf, gzipbuf, z_buf)
!
      integer, intent(in) :: len_gzipbuf
      character(len=1), target, intent(in) :: gzipbuf(len_gzipbuf)
!
      type(zlib_transfer), intent(inout) :: z_buf
!
!
      z_buf%len_gzipbuf = int(len_gzipbuf,KIND(z_buf%len_gzipbuf))
      z_buf%gzipbuf_p => gzipbuf
!
      end subroutine link_compressed_buffer
!
!  ---------------------------------------------------------------------
!
      subroutine link_real_buffer_for_zlib(num, data, z_buf)
!
      integer, intent(in) :: num
      real(kind = kreal), target, intent(in) :: data(num)
!
      type(zlib_transfer), intent(inout) :: z_buf
!
!
      z_buf%len_buf = int((num*kreal),KIND(z_buf%len_buf))
      z_buf%dat_p => data
!
      end subroutine link_real_buffer_for_zlib
!
!  ---------------------------------------------------------------------
!
      subroutine link_int8_buffer_for_zlib(num, int8_dat, z_buf)
!
      integer, intent(in) :: num
      integer(kind = kint_gl), target, intent(in) :: int8_dat(num)
!
      type(zlib_transfer), intent(inout) :: z_buf
!
!
      z_buf%len_buf = int((num*kint_gl),KIND(z_buf%len_buf))
      z_buf%idat8_p => int8_dat
!
      end subroutine link_int8_buffer_for_zlib
!
!  ---------------------------------------------------------------------
!
      subroutine link_int4_buffer_for_zlib(num, int4_dat, z_buf)
!
      integer, intent(in) :: num
      integer(kind = 4), target, intent(in) :: int4_dat(num)
!
      type(zlib_transfer), intent(inout) :: z_buf
!
!
      z_buf%len_buf = int((num*4),KIND(z_buf%len_buf))
      z_buf%idat4_p => int4_dat
!
      end subroutine link_int4_buffer_for_zlib
!
!  ---------------------------------------------------------------------
!
      subroutine link_text_buffer_for_zlib(len_buf, textbuf, z_buf)
!
      integer, intent(in) :: len_buf
      character(len=1), target, intent(in) :: textbuf(len_buf)
!
      type(zlib_transfer), intent(inout) :: z_buf
!
!
      z_buf%len_buf = int(len_buf,KIND(z_buf%len_buf))
      z_buf%buf_p => textbuf
!
      end subroutine link_text_buffer_for_zlib
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine unlink_compressed_buffer(z_buf)
!
      type(zlib_transfer), intent(inout) :: z_buf
!
      nullify(z_buf%gzipbuf_p)
!
      end subroutine unlink_compressed_buffer
!
!  ---------------------------------------------------------------------
!
      subroutine unlink_real_buffer_for_zlib(z_buf)
!
      type(zlib_transfer), intent(inout) :: z_buf
!
      nullify(z_buf%dat_p)
!
      end subroutine unlink_real_buffer_for_zlib
!
!  ---------------------------------------------------------------------
!
      subroutine unlink_int8_buffer_for_zlib(z_buf)
!
      type(zlib_transfer), intent(inout) :: z_buf
!
      nullify(z_buf%idat8_p)
!
      end subroutine unlink_int8_buffer_for_zlib
!
!  ---------------------------------------------------------------------
!
      subroutine unlink_int4_buffer_for_zlib(z_buf)
!
      type(zlib_transfer), intent(inout) :: z_buf
!
      nullify(z_buf%idat4_p)
!
      end subroutine unlink_int4_buffer_for_zlib
!
!  ---------------------------------------------------------------------
!
      subroutine unlink_text_buffer_for_zlib(z_buf)
!
      type(zlib_transfer), intent(inout) :: z_buf
!
      nullify(z_buf%buf_p)
!
      end subroutine unlink_text_buffer_for_zlib
!
!  ---------------------------------------------------------------------
!
      end module t_buffer_4_gzip
