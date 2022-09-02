!>@file   gzip_file_access.f90
!!        module gzip_file_access
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Wrapper for decompression routines by zlib
!!
!!@verbatim
!!      subroutine open_wt_gzfile_f(ptr_s, gzip_name, zbuf)
!!      subroutine open_ad_gzfile_f(ptr_s, gzip_name, zbuf)
!!      subroutine open_rd_gzfile_f(ptr_s, gzip_name, zbuf)
!!      subroutine close_gzfile_b(ptr_s)
!!      integer(kind = kint) function check_gzfile_eof(ptr_s)
!!        character, pointer, intent(in) :: ptr_s
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine get_one_line_text_from_gz(ptr_s, zbuf)
!!      subroutine gz_write_textbuf_no_lf(ptr_s, zbuf)
!!        character, pointer, intent(in) :: ptr_s 
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine gzread_real_f(ptr_s, num, data, zbuf)
!!      subroutine gzread_int8_f(ptr_s, num, int8_dat, zbuf)
!!      subroutine gzread_int4_f(ptr_s, num, int4_dat, zbuf)
!!      subroutine gzread_chara_f(ptr_s, len_buf, textbuf, zbuf)
!!        character, pointer, intent(in) :: ptr_s 
!!        integer, intent(in) :: num
!!        real(kind = kreal), target, intent(inout) :: data(num)
!!        integer(kind = kint_gl), intent(inout) :: int8_dat(num)
!!        integer, target, intent(inout) :: int4_dat(num)
!!        character(len=1), target, intent(in) :: textbuf(len_buf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!
!!      subroutine gzwrite_real_f(ptr_s, num, data, zbuf)
!!      subroutine gzwrite_int8_f(ptr_s, num, int8_dat, zbuf)
!!      subroutine gzwrite_int4_f(ptr_s, num, int4_dat, zbuf)
!!      subroutine gzwrite_chara_f(ptr_s, len_buf, textbuf, zbuf)
!!        character, pointer, intent(in) :: ptr_s 
!!        integer, intent(in) :: len_buf
!!        character(len=1), target, intent(in) :: textbuf(len_buf)
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!@endverbatim
!
      module gzip_file_access
!
      use ISO_C_BINDING
      use m_precision
      use t_buffer_4_gzip
!
      implicit none
!
!  -----------------
!
      interface
!
!  -----------------
        type(C_ptr) function open_wt_gzfile_c(gz_file_name)             &
     &           BIND(C, name = 'open_wt_gzfile_c')
          use ISO_C_BINDING
!
          character(C_char), intent(in) :: gz_file_name(*)
        end function open_wt_gzfile_c
!  -----------------
        type(C_ptr) function open_ad_gzfile_c(gz_file_name)             &
     &           BIND(C, name = 'open_ad_gzfile_c')
        use ISO_C_BINDING
!
          character(C_char), intent(in) :: gz_file_name(*)
        end function open_ad_gzfile_c
!  -----------------
        type(C_ptr) function open_rd_gzfile_c(gz_file_name)             &
     &           BIND(C, name = 'open_rd_gzfile_c')
          use ISO_C_BINDING
          character(C_char), intent(in) :: gz_file_name(*)
        end function open_rd_gzfile_c
!  -----------------
        subroutine close_gzfile_c(FP_gzip)                              &
     &            BIND(C, name = 'close_gzfile_c')
          use ISO_C_BINDING
          type(C_ptr), value :: FP_gzip
        end subroutine close_gzfile_c
!  -----------------
        integer(C_int) function check_gzfile_eof_c(FP_gzip)             &
     &                BIND(C, name = 'check_gzfile_eof_c')
          use ISO_C_BINDING
          type(C_ptr), value :: FP_gzip
        end function check_gzfile_eof_c
!  -----------------
        subroutine get_one_line_from_gz_c                               &
     &           (FP_gzip, num_buffer, num_word, nchara, line_buf)      &
     &            BIND(C, name = 'get_one_line_from_gz_c')
          use ISO_C_BINDING
!
          type(C_ptr), value :: FP_gzip
          integer(C_int), intent(in) :: num_buffer
          integer(C_int), intent(inout) :: num_word
          integer(C_int), intent(inout) :: nchara
          type(C_ptr), value :: line_buf
        end subroutine get_one_line_from_gz_c
!  -----------------
        subroutine write_compress_txt_c(FP_gzip, nchara, line_buf)      &
     &            BIND(C, name = 'write_compress_txt_c')
          use ISO_C_BINDING
!
          type(C_ptr), value :: FP_gzip
          integer(C_int), intent(in) :: nchara
          type(C_ptr), value :: line_buf
        end subroutine write_compress_txt_c
!  -----------------
        subroutine write_compress_txt_nolf_c(FP_gzip, nchara, line_buf) &
     &            BIND(C, name = 'write_compress_txt_nolf_c')
          use ISO_C_BINDING
!
          type(C_ptr), value :: FP_gzip
          integer(C_int), intent(in) :: nchara
          type(C_ptr), value :: line_buf
        end subroutine write_compress_txt_nolf_c
!  -----------------
        subroutine gzread_32bit_c                                       &
     &           (FP_gzip, iflag_swap, ilength, buf, ierr)              &
     &            BIND(C, name = 'gzread_32bit_c')
          use ISO_C_BINDING
!
          type(C_ptr), value :: FP_gzip
          integer(C_int), intent(in) :: iflag_swap, ilength
          type(C_ptr), value, intent(in) :: buf
          integer(C_int), intent(inout) :: ierr
        end subroutine gzread_32bit_c
!  -----------------
        subroutine gzread_64bit_c                                       &
     &           (FP_gzip, iflag_swap, ilength, buf, ierr)              &
     &            BIND(C, name = 'gzread_64bit_c')
          use ISO_C_BINDING
!
          type(C_ptr), value :: FP_gzip
          integer(C_int), intent(in) :: iflag_swap, ilength
          type(C_ptr), value, intent(in) :: buf
          integer(C_int), intent(inout) :: ierr
        end subroutine gzread_64bit_c
!  -----------------
        subroutine gzwrite_c(FP_gzip, ilength, buf, ierr)               &
     &            BIND(C, name = 'gzwrite_c')
          use ISO_C_BINDING
!
          type(C_ptr), value :: FP_gzip
          integer(C_int), intent(in) :: ilength
          type(C_ptr), value, intent(in) :: buf
          integer(C_int), intent(inout) :: ierr
        end subroutine gzwrite_c
!  -----------------
      end interface
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine open_wt_gzfile_f(ptr_s, gzip_name, zbuf)
!
      use set_parallel_file_name
!
      character, pointer, intent(inout) :: ptr_s
      character(len = kchara), intent(in) :: gzip_name
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      type(C_ptr) :: FP_z
!
      call link_text_buffer_for_zlib                                    &
     &   (kchara, add_null_character(gzip_name), zbuf)
      FP_z = open_wt_gzfile_c(zbuf%buf_p)
      call c_f_pointer(FP_z, ptr_s)
      call unlink_text_buffer_for_zlib(zbuf)
!
      end subroutine open_wt_gzfile_f
!
!------------------------------------------------------------------
!
      subroutine open_ad_gzfile_f(ptr_s, gzip_name, zbuf)
!
      use set_parallel_file_name
!
      character, pointer, intent(inout) :: ptr_s
      character(len = kchara), intent(in) :: gzip_name
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      type(C_ptr) :: FP_z
!
      call link_text_buffer_for_zlib                                    &
     &   (kchara, add_null_character(gzip_name), zbuf)
      FP_z = open_ad_gzfile_c(zbuf%buf_p)
      call c_f_pointer(FP_z, ptr_s)
      call unlink_text_buffer_for_zlib(zbuf)
!
      end subroutine open_ad_gzfile_f
!
!------------------------------------------------------------------
!
      subroutine open_rd_gzfile_f(ptr_s, gzip_name, zbuf)
!
      use set_parallel_file_name
!
      character, pointer, intent(inout) :: ptr_s
      character(len = kchara), intent(in) :: gzip_name
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      type(C_ptr) :: FP_z
!
      call link_text_buffer_for_zlib                                    &
     &   (kchara, add_null_character(gzip_name), zbuf)
      FP_z = open_rd_gzfile_c(zbuf%buf_p)
      call c_f_pointer(FP_z, ptr_s)
      call unlink_text_buffer_for_zlib(zbuf)
!
      end subroutine open_rd_gzfile_f
!
!------------------------------------------------------------------
!
      subroutine close_gzfile_b(ptr_s)
!
      character, pointer, intent(inout) :: ptr_s
!
      call close_gzfile_c(C_LOC(ptr_s))
      nullify(ptr_s)
!
      end subroutine close_gzfile_b
!
!------------------------------------------------------------------
!
      integer(kind = kint) function check_gzfile_eof(ptr_s)
!
      character, pointer, intent(in) :: ptr_s 
!
      check_gzfile_eof =  check_gzfile_eof_c(C_LOC(ptr_s))
!
      end function check_gzfile_eof
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine get_one_line_text_from_gz(ptr_s, zbuf)
!
      character, pointer, intent(in) :: ptr_s 
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_text_buffer_for_zlib                                    &
     &   (len(zbuf%fixbuf(1)), zbuf%fixbuf(1), zbuf)
      call get_one_line_from_gz_c(C_LOC(ptr_s), zbuf%len_buf,           &
     &    zbuf%num_word, zbuf%len_used, C_LOC(zbuf%buf_p(1)))
      call unlink_text_buffer_for_zlib(zbuf)
!
      end subroutine get_one_line_text_from_gz
!
!  ---------------------------------------------------------------------
!
      subroutine gz_write_textbuf_no_lf(ptr_s, zbuf)
!
      character, pointer, intent(in) :: ptr_s 
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_text_buffer_for_zlib                                    &
     &   (len(zbuf%fixbuf(1)), zbuf%fixbuf(1), zbuf)
      call write_compress_txt_nolf_c(C_LOC(ptr_s), zbuf%len_buf,        &
     &                               C_LOC(zbuf%buf_p(1)))
      call unlink_text_buffer_for_zlib(zbuf)
!
      end subroutine gz_write_textbuf_no_lf
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine gzread_real_f(ptr_s, num, data, zbuf)
!
      character, pointer, intent(in) :: ptr_s 
      integer, intent(in) :: num
      real(kind = kreal), target, intent(inout) :: data(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_real_buffer_for_zlib(num, data, zbuf)
      call gzread_64bit_c(C_LOC(ptr_s), zbuf%iflag_swap,                &
     &    zbuf%len_buf, C_LOC(zbuf%dat_p(1)), zbuf%ierr_zlib)
      call unlink_real_buffer_for_zlib(zbuf)
!
      end subroutine gzread_real_f
!
!  ---------------------------------------------------------------------
!
      subroutine gzread_int8_f(ptr_s, num, int8_dat, zbuf)
!
      character, pointer, intent(in) :: ptr_s 
      integer, intent(in) :: num
      integer(kind = kint_gl), intent(inout) :: int8_dat(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_int8_buffer_for_zlib(num, int8_dat, zbuf)
      call gzread_64bit_c(C_LOC(ptr_s), zbuf%iflag_swap,                &
     &    zbuf%len_buf , C_LOC(zbuf%idat8_p(1)), zbuf%ierr_zlib)
      call unlink_int8_buffer_for_zlib(zbuf)
!
      end subroutine gzread_int8_f
!
!  ---------------------------------------------------------------------
!
      subroutine gzread_int4_f(ptr_s, num, int4_dat, zbuf)
!
      character, pointer, intent(in) :: ptr_s 
      integer, intent(in) :: num
      integer, target, intent(inout) :: int4_dat(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_int4_buffer_for_zlib(num, int4_dat, zbuf)
      call gzread_32bit_c(C_LOC(ptr_s), zbuf%iflag_swap, zbuf%len_buf,  &
     &                    C_LOC(zbuf%idat4_p(1)), zbuf%ierr_zlib)
      call unlink_int4_buffer_for_zlib(zbuf)
!
      end subroutine gzread_int4_f
!
!  ---------------------------------------------------------------------
!
      subroutine gzread_chara_f(ptr_s, len_buf, textbuf, zbuf)
!
      use m_machine_parameter
!
      character, pointer, intent(in) :: ptr_s 
      integer, intent(in) :: len_buf
      character(len=1), target, intent(in) :: textbuf(len_buf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(C_int), parameter :: iflag_noswap = iendian_KEEP
!
      call link_text_buffer_for_zlib(len_buf, textbuf, zbuf)
      call gzread_32bit_c(C_LOC(ptr_s), iflag_noswap, zbuf%len_buf,     &
     &                    C_LOC(zbuf%buf_p(1)), zbuf%ierr_zlib)
      call unlink_text_buffer_for_zlib(zbuf)
!
      end subroutine gzread_chara_f
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine gzwrite_real_f(ptr_s, num, data, zbuf)
!
      character, pointer, intent(in) :: ptr_s 
      integer, intent(in) :: num
      real(kind = kreal), target, intent(in) :: data(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_real_buffer_for_zlib(num, data, zbuf)
      call gzwrite_c(C_LOC(ptr_s), zbuf%len_buf,                        &
     &               C_LOC(zbuf%dat_p(1)), zbuf%ierr_zlib)
      call unlink_real_buffer_for_zlib(zbuf)
!
      end subroutine gzwrite_real_f
!
!  ---------------------------------------------------------------------
!
      subroutine gzwrite_int8_f(ptr_s, num, int8_dat, zbuf)
!
      character, pointer, intent(in) :: ptr_s 
      integer, intent(in) :: num
      integer(kind = kint_gl), intent(in) :: int8_dat(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_int8_buffer_for_zlib(num, int8_dat, zbuf)
      call gzwrite_c(C_LOC(ptr_s), zbuf%len_buf,                        &
     &               C_LOC(zbuf%idat8_p(1)), zbuf%ierr_zlib)
      call unlink_int8_buffer_for_zlib(zbuf)
!
      end subroutine gzwrite_int8_f
!
!  ---------------------------------------------------------------------
!
      subroutine gzwrite_int4_f(ptr_s, num, int4_dat, zbuf)
!
      character, pointer, intent(in) :: ptr_s 
      integer, intent(in) :: num
      integer(kind = 4), target, intent(in) :: int4_dat(num)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_int4_buffer_for_zlib(num, int4_dat, zbuf)
      call gzwrite_c(C_LOC(ptr_s), zbuf%len_buf,                        &
     &               C_LOC(zbuf%idat4_p(1)), zbuf%ierr_zlib)
      call unlink_int4_buffer_for_zlib(zbuf)
!
      end subroutine gzwrite_int4_f
!
!  ---------------------------------------------------------------------
!
      subroutine gzwrite_chara_f(ptr_s, len_buf, textbuf, zbuf)
!
      character, pointer, intent(in) :: ptr_s 
      integer, intent(in) :: len_buf
      character(len=1), target, intent(in) :: textbuf(len_buf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_text_buffer_for_zlib(len_buf, textbuf, zbuf)
      call gzwrite_c(C_LOC(ptr_s), zbuf%len_buf,                        &
     &               C_LOC(zbuf%buf_p(1)), zbuf%ierr_zlib)
      call unlink_text_buffer_for_zlib(zbuf)
!
      end subroutine gzwrite_chara_f
!
!  ---------------------------------------------------------------------
!
      end module gzip_file_access
