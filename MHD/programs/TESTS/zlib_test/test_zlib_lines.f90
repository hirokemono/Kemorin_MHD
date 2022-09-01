!
      program test_zlib_lines
!
      use ISO_C_BINDING
!
      use m_precision
      use t_buffer_4_gzip
      use skip_gz_comment
      use gzip_file_access
!
      implicit none
!
!  -----------------
      interface
!  -----------------
        type(c_ptr) function open_wt_gzfile_c(gz_file_name)             &
     &            BIND(C, name = 'open_wt_gzfile_c')
          use ISO_C_BINDING
!
          character(C_char), intent(in) :: gz_file_name(*)
        end function open_wt_gzfile_c
!  -----------------
        type(c_ptr) function open_rd_gzfile_c(gz_file_name)             &
     &            BIND(C, name = 'open_rd_gzfile_c')
          use ISO_C_BINDING
!
          character(C_char), intent(in) :: gz_file_name(*)
        end function open_rd_gzfile_c
!  -----------------
        subroutine close_gzfile_c(FP_z)                 &
     &            BIND(C, name = 'close_gzfile_c')
          use ISO_C_BINDING
!
          type(C_ptr), value :: FP_z
        end subroutine close_gzfile_c
!  -----------------
      end interface
!  -----------------
!
      integer(kind = kint) :: i,j, k(10)
      character(len=kchara) :: fname
      integer(kind = 4), parameter :: nbuffer = 2000
      type(buffer_4_gzip) :: zbuf_test
!
      type(C_ptr), pointer :: FP_z1
      character, pointer, save :: ptr_s1, ptr_s2
!
!
      write(fname,'(a)') 'test_a.txt.gz'
      call alloc_fixbuffer_for_zlib(zbuf_test)
      call open_wt_gzfile_ff(fname, zbuf_test, ptr_s1)

      write(zbuf_test%fixbuf(1),'(a,a1)')'test data',char(0)
      zbuf_test%len_buf = 9
      call write_compress_txt_ff(ptr_s1, zbuf_test)

      write(zbuf_test%fixbuf(1),'(100(i9,a1))')                         &
     &                           (j,char(10),j=1,99), j, char(0)
      zbuf_test%len_buf = 10*100-1
      call write_compress_txt_ff(ptr_s1, zbuf_test)
      call close_gzfile_aa(ptr_s1, zbuf_test)
!
!
      write(fname,'(a)') 'test_b.txt.gz'
      call alloc_fixbuffer_for_zlib(zbuf_test)
      call open_wt_gzfile_ff(fname, zbuf_test, ptr_s1)

      write(zbuf_test%fixbuf(1),'(a,a1)') 'test data', char(0)
      zbuf_test%len_buf = 9
      call write_compress_txt_ff(ptr_s1, zbuf_test)

      zbuf_test%len_buf = 9
      do j = 1, 100
        write(zbuf_test%fixbuf(1),'(i9,a1)') j, char(0)
        call write_compress_txt_ff(ptr_s1, zbuf_test)
      end do
      call close_gzfile_aa(ptr_s1, zbuf_test)
!
!
      write(fname,'(a)') 'test_f.txt.gz'
      call alloc_fixbuffer_for_zlib(zbuf_test)
      call open_wt_gzfile_ff(fname, zbuf_test, ptr_s1)
!
      write(zbuf_test%fixbuf(1),'(a,a1)') 'test data', char(0)
      zbuf_test%len_buf = 9
      write(*,*) 'zbuf_test%len_buf', zbuf_test%len_buf

      call write_compress_txt_ff(ptr_s1, zbuf_test)
!
      do i = 1, 10
        write(zbuf_test%fixbuf(1),'(10i16,a)')                          &
     &             ((i*1000+j),j=1,10), char(0)
        zbuf_test%len_buf = 80
        call write_compress_txt_ff(ptr_s1, zbuf_test)
!
      end do
      call close_gzfile_aa(ptr_s1, zbuf_test)
!
      call alloc_fixbuffer_for_zlib(zbuf_test)
      call open_rd_gzfile_aa(fname, zbuf_test, ptr_s2)
      call get_one_line_text_from_gzz(ptr_s2, zbuf_test)
!
      write(*,*) 'nword',  zbuf_test%num_word
      write(*,*) 'nchara', zbuf_test%len_used
      read(zbuf_test%fixbuf(1),*) fname
      write(*,'(a)') trim(fname)
!
      do i = 1, 10
        call get_one_line_text_from_gzz(ptr_s2, zbuf_test)
        write(*,*) 'nword',  zbuf_test%num_word
        write(*,*) 'nchara', zbuf_test%len_used
        read(zbuf_test%fixbuf(1),*) k(1:zbuf_test%num_word)
        write(*,*) k
      end do
      call close_gzfile_aa(ptr_s2, zbuf_test)
!
      stop
!
!  ---------------------------------------------------------------------
      contains
!  ---------------------------------------------------------------------
!
      subroutine write_compress_txt_ff(ptr_s, zbuf)
!
      use gzip_file_access
!
      character, pointer, intent(in) :: ptr_s 
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_text_buffer_for_zlib(len(zbuf%fixbuf(1)),               &
     &                               zbuf%fixbuf(1), zbuf)
      call write_compress_txt_c(C_loc(ptr_s), zbuf%len_buf,             &
     &                          C_LOC(zbuf%buf_p(1)))
      call unlink_text_buffer_for_zlib(zbuf)
!
      end subroutine write_compress_txt_ff
!
!  ---------------------------------------------------------------------
!
      subroutine open_wt_gzfile_ff(gzip_name, zbuf, ptr_s)
!
      use set_parallel_file_name
!
      character(len = kchara), intent(in) :: gzip_name
      type(buffer_4_gzip), intent(inout) :: zbuf
      character, pointer, intent(inout) :: ptr_s 
!
      type(C_ptr) :: FP_z
!
!
      call link_text_buffer_for_zlib                                    &
     &   (kchara, add_null_character(gzip_name), zbuf)
      FP_z = open_wt_gzfile_c(zbuf%buf_p)
      call c_f_pointer(FP_z, ptr_s)
      call unlink_text_buffer_for_zlib(zbuf)
!      write(*,'(a,Z16)') 'Open ptr_s', C_loc(ptr_s)
!
      end subroutine open_wt_gzfile_ff
!
!------------------------------------------------------------------
!
      subroutine open_rd_gzfile_aa(gzip_name, zbuf, ptr_s)
!
      use gzip_file_access
      use set_parallel_file_name
!
      character(len = kchara), intent(in) :: gzip_name
      type(buffer_4_gzip) , intent(inout):: zbuf
      character, pointer, intent(inout) :: ptr_s 
!
      type(C_ptr) :: FP_z
!
!
      call link_text_buffer_for_zlib                                    &
     &   (kchara, add_null_character(gzip_name), zbuf)
      FP_z = open_rd_gzfile_c(zbuf%buf_p)
      call c_f_pointer(FP_z, ptr_s)
      call unlink_text_buffer_for_zlib(zbuf)
!      write(*,'(a,Z16)') 'Open ptr_s to read', C_loc(ptr_s)
!
      end subroutine open_rd_gzfile_aa
!
! ----------------------------------------------------------------------
!
      subroutine close_gzfile_aa(ptr_s, zbuf)
!
      use gzip_file_access
!
      character, pointer, intent(inout) :: ptr_s 
      type(buffer_4_gzip), intent(inout):: zbuf
!
!
      call close_gzfile_c(C_loc(ptr_s))
      call dealloc_fixbuffer_for_zlib(zbuf)
      nullify(ptr_s)
!      write(*,'(a,Z16)') 'FP_z close', C_loc(ptr_s)
!
      end subroutine close_gzfile_aa
!
!------------------------------------------------------------------
!
      subroutine get_one_line_text_from_gzz(ptr_s, zbuf)
!
      use gzip_file_access
!
      character, pointer, intent(in) :: ptr_s 
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_text_buffer_for_zlib                                    &
     &   (len(zbuf%fixbuf(1)), zbuf%fixbuf(1), zbuf)
      call get_one_line_from_gz_c(C_loc(ptr_s), zbuf%len_buf,           &
     &    zbuf%num_word, zbuf%len_used, C_LOC(zbuf%buf_p(1)))
      call unlink_text_buffer_for_zlib(zbuf)
!
      end subroutine get_one_line_text_from_gzz
!
!  ---------------------------------------------------------------------
!
      end program test_zlib_lines
