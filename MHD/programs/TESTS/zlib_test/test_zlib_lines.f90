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
        subroutine write_compress_txt(nchara, line_buf)                 &
     &            BIND(C, name = 'write_compress_txt')
          use ISO_C_BINDING
!
          integer(C_int), intent(in) :: nchara
          type(C_ptr), value :: line_buf
        end subroutine write_compress_txt
      end interface
!  -----------------
!
      integer(kind = kint) :: i,j, k(10)
      character(len=kchara) :: fname
      integer(kind = 4), parameter :: nbuffer = 2000
      character(len=nbuffer) :: input_txt
      type(buffer_4_gzip) :: zbuf_write, zbuf_test
!
!
      call alloc_fixbuffer_for_zlib(zbuf_test)
!
      write(fname,'(a)') 'test_a.txt.gz'
      call open_wt_gzfile_a(fname, zbuf_test)

      write(zbuf_test%fixbuf(1),'(a,a1)')'test data',char(0)
      zbuf_test%len_buf = 9
      call write_compress_txt_f(zbuf_test)

      write(zbuf_test%fixbuf(1),'(100(i9,a1))')                         &
     &                           (j,char(10),j=1,99), j, char(0)
      zbuf_test%len_buf = 10*100-1
      call write_compress_txt_f(zbuf_test)
      call close_gzfile_a(zbuf_test)
!
!
      write(fname,'(a)') 'test_b.txt.gz'
      call open_wt_gzfile(fname)

      write(zbuf_test%fixbuf(1),'(a,a1)') 'test data', char(0)
      zbuf_test%len_buf = 9
      call write_compress_txt_f(zbuf_test)

      zbuf_test%len_buf = 9
      do j = 1, 100
        write(zbuf_test%fixbuf(1),'(i9,a1)') j, char(0)
        call write_compress_txt_f(zbuf_test)
      end do
      call close_gzfile_a(zbuf_test)
!
      write(fname,'(a)') 'test_f.txt.gz'
!
      write(zbuf_test%fixbuf(1),'(a,a1)') 'test data', char(0)
      zbuf_test%len_buf = 9
      write(*,*) 'zbuf_test%len_buf', zbuf_test%len_buf

      call open_wt_gzfile(fname)
      call write_compress_txt_f(zbuf_test)
!
      do i = 1, 10
        write(zbuf_test%fixbuf(1),'(10i16,a)')                          &
     &             ((i*1000+j),j=1,10), char(0)
        zbuf_test%len_buf = 80
        call write_compress_txt_f(zbuf_test)
!
      end do
!
      call close_gzfile_a(zbuf_test)
!
      call open_rd_gzfile_a(fname, zbuf_test)
      call get_one_line_text_from_gz(zbuf_test)
!
      write(*,*) 'nword',  zbuf_test%num_word
      write(*,*) 'nchara', zbuf_test%len_used
      read(zbuf_test%fixbuf(1),*) fname
      write(*,'(a)') trim(fname)
!
      do i = 1, 10
        call get_one_line_text_from_gz(zbuf_test)
        write(*,*) 'nword',  zbuf_test%num_word
        write(*,*) 'nchara', zbuf_test%len_used
        read(zbuf_test%fixbuf(1),*) k(1:zbuf_test%num_word)
        write(*,*) k
      end do
      call close_gzfile_a(zbuf_test)
!
      stop
!
!  ---------------------------------------------------------------------
      contains
!  ---------------------------------------------------------------------
!
      subroutine write_compress_txt_f(zbuf)
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
!
      call link_text_buffer_for_zlib                                    &
     &   (len(zbuf%fixbuf(1)), zbuf%fixbuf(1), zbuf)
      call write_compress_txt(zbuf%len_buf, C_LOC(zbuf%buf_p(1)))
      call unlink_text_buffer_for_zlib(zbuf)
!
      end subroutine write_compress_txt_f
!
!  ---------------------------------------------------------------------
!
      end program test_zlib_lines
