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
        type(C_ptr), value :: line_buf
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
        type(C_ptr), value :: line_buf
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
        type(C_ptr), value :: line_buf
!
        end subroutine write_compress_txt_nolf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
        subroutine gzread_32bit_f(iflag_swap, ilength, buf, ierr)       &
     &            BIND(C, name = 'gzread_32bit_f')
!
        use ISO_C_BINDING
!
        integer(C_int), intent(in) :: iflag_swap, ilength
        type(C_ptr), value, intent(in) :: buf
        integer(C_int), intent(inout) :: ierr
!
        end subroutine gzread_32bit_f
!
!  ---------------------------------------------------------------------
!
        subroutine gzread_64bit_f(iflag_swap, ilength, buf, ierr)       &
     &            BIND(C, name = 'gzread_64bit_f')
!
        use ISO_C_BINDING
!
        integer(C_int), intent(in) :: iflag_swap, ilength
        type(C_ptr), value, intent(in) :: buf
        integer(C_int), intent(inout) :: ierr
!
        end subroutine gzread_64bit_f
!
!  ---------------------------------------------------------------------
!
        subroutine gzwrite_f(ilength, buf, ierr)                        &
     &            BIND(C, name = 'gzwrite_f')
!
        use ISO_C_BINDING
!
        integer(C_int), intent(in) :: ilength
        type(C_ptr), value, intent(in) :: buf
        integer(C_int), intent(inout) :: ierr
!
        end subroutine gzwrite_f
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
     &    zbuf%len_used, C_LOC(zbuf%buf_p))
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
      call link_text_buffer_for_zlib(len_buf, textbuf, zbuf)
      call write_compress_txt(zbuf%len_buf, C_LOC(zbuf%buf_p))
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
      call link_text_buffer_for_zlib(len_buf, textbuf, zbuf)
      call write_compress_txt_nolf(zbuf%len_buf, C_LOC(zbuf%buf_p))
      call unlink_text_buffer_for_zlib(zbuf)
!
      end subroutine gz_write_textbuf_no_lf_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine gzread_real_f(iflag_swap, num, data, zbuf, ierr)
!
      use t_buffer_4_gzip
!
      integer, intent(in) :: iflag_swap, num
      real(kind = kreal), target, intent(inout) :: data(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer, intent(inout) :: ierr
!
      integer(C_int) :: ierr_c, iswap_c
!
!
      write(*,*) 'gzwrite_real_f'
      iswap_c = iflag_swap
      call link_real_buffer_for_zlib(num, data, zbuf)
      call gzread_64bit_f                                               &
     &   (iswap_c, zbuf%len_buf , C_LOC(zbuf%dat_p), ierr_c)
      call unlink_real_buffer_for_zlib(zbuf)
      ierr = ierr_c
!
      end subroutine gzread_real_f
!
!  ---------------------------------------------------------------------
!
      subroutine gzread_int8_f(iflag_swap, num, int8_dat, zbuf, ierr)
!
      use t_buffer_4_gzip
!
      integer, intent(in) :: iflag_swap, num
      integer(kind = kint_gl), intent(inout) :: int8_dat(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer, intent(inout) :: ierr
!
      integer(C_int) :: ierr_c, iswap_c
!
!
      write(*,*) 'gzread_int8_f'
      iswap_c = iflag_swap
      call link_int8_buffer_for_zlib(num, int8_dat, zbuf)
      call gzread_64bit_f                                               &
     &   (iswap_c, zbuf%len_buf , C_LOC(zbuf%idat8_p), ierr_c)
      call unlink_int8_buffer_for_zlib(zbuf)
      ierr = ierr_c
!
      end subroutine gzread_int8_f
!
!  ---------------------------------------------------------------------
!
      subroutine gzread_int4_f(iflag_swap, num, int4_dat, zbuf, ierr)
!
      use t_buffer_4_gzip
!
      integer, intent(in) :: iflag_swap, num
      integer, target, intent(inout) :: int4_dat(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer, intent(inout) :: ierr
!
      integer(C_int) :: ierr_c, iswap_c
!
!
      write(*,*) 'gzread_int4_f'
      iswap_c = iflag_swap
      call link_int4_buffer_for_zlib(num, int4_dat, zbuf)
      call gzread_32bit_f                                               &
     &   (iswap_c, zbuf%len_buf, C_LOC(zbuf%idat4_p), ierr_c)
      call unlink_int4_buffer_for_zlib(zbuf)
      ierr = ierr_c
!
      end subroutine gzread_int4_f
!
!  ---------------------------------------------------------------------
!
      subroutine gzread_chara_f                                         &
     &         (iflag_swap, len_buf, textbuf, zbuf, ierr)
!
      use t_buffer_4_gzip
!
      integer, intent(in) :: iflag_swap, len_buf
      character(len=1), target, intent(in) :: textbuf(len_buf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer, intent(inout) :: ierr
!
      integer(C_int) :: ierr_c, iswap_c
!
!
      write(*,*) 'gzread_chara_f'
      iswap_c = iflag_swap
      call link_text_buffer_for_zlib(len_buf, textbuf, zbuf)
      call gzread_32bit_f                                               &
     &   (iswap_c, zbuf%len_buf , C_LOC(zbuf%buf_p), ierr_c)
      call unlink_text_buffer_for_zlib(zbuf)
      ierr = ierr_c
!
      end subroutine gzread_chara_f
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine gzwrite_real_f(num, data, zbuf, ierr)
!
      use t_buffer_4_gzip
!
      integer, intent(in) :: num
      real(kind = kreal), target, intent(in) :: data(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer, intent(inout) :: ierr
!
      integer(C_int) :: ierr_c
!
!
      write(*,*) 'gzwrite_real_f'
      call link_real_buffer_for_zlib(num, data, zbuf)
      call gzwrite_f(zbuf%len_buf , C_LOC(zbuf%dat_p), ierr_c)
      call unlink_real_buffer_for_zlib(zbuf)
      ierr = ierr_c
!
      end subroutine gzwrite_real_f
!
!  ---------------------------------------------------------------------
!
      subroutine gzwrite_int8_f(num, int8_dat, zbuf, ierr)
!
      use t_buffer_4_gzip
!
      integer, intent(in) :: num
      integer(kind = kint_gl), intent(in) :: int8_dat(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer, intent(inout) :: ierr
!
      integer(C_int) :: ierr_c
!
!
      write(*,*) 'gzwrite_int8_f'
      call link_int8_buffer_for_zlib(num, int8_dat, zbuf)
      call gzwrite_f(zbuf%len_buf , C_LOC(zbuf%idat8_p), ierr_c)
      call unlink_int8_buffer_for_zlib(zbuf)
      ierr = ierr_c
!
      end subroutine gzwrite_int8_f
!
!  ---------------------------------------------------------------------
!
      subroutine gzwrite_int4_f(num, int4_dat, zbuf, ierr)
!
      use t_buffer_4_gzip
!
      integer, intent(in) :: num
      integer(kind = 4), target, intent(in) :: int4_dat(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer, intent(inout) :: ierr
!
      integer(C_int) :: ierr_c
!
!
      write(*,*) 'gzwrite_int4_f'
      call link_int4_buffer_for_zlib(num, int4_dat, zbuf)
      call gzwrite_f(zbuf%len_buf , C_LOC(zbuf%idat4_p), ierr_c)
      call unlink_int4_buffer_for_zlib(zbuf)
      ierr = ierr_c
!
      end subroutine gzwrite_int4_f
!
!  ---------------------------------------------------------------------
!
      subroutine gzwrite_chara_f(len_buf, textbuf, zbuf, ierr)
!
      use t_buffer_4_gzip
!
      integer, intent(in) :: len_buf
      character(len=1), target, intent(in) :: textbuf(len_buf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
      integer, intent(inout) :: ierr
!
      integer(C_int) :: ierr_c
!
!
      write(*,*) 'gzwrite_chara_f'
      call link_text_buffer_for_zlib(len_buf, textbuf, zbuf)
      call gzwrite_f(zbuf%len_buf , C_LOC(zbuf%buf_p), ierr_c)
      call unlink_text_buffer_for_zlib(zbuf)
      ierr = ierr_c
!
      end subroutine gzwrite_chara_f
!
!  ---------------------------------------------------------------------
!
      end module calypso_c_binding
