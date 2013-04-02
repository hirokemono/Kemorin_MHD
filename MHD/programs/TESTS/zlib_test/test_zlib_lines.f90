!
      program test_zlib_lines
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: i,j, k(10)
      integer(kind = kint) :: num_txt, iflag_fin, num_word, nchara
      character(len=kchara) :: fname
      integer(kind = kint), parameter :: nbuf = 2000
      character(len=nbuf) :: input_txt
!
!
      num_txt = 4096
!      allocate( input_txt(num_txt) )
!
      write(fname,'(a,a1)') 'test_a.txt.gz', char(0)
      call open_wt_gzfile(fname)

      write(input_txt,'(a,a1)')'test data',char(0)
      num_txt = 9
      call write_compress_txt(nbuf, input_txt)

      write(input_txt,'(100(i9,a1))') (j,char(10),j=1,99), j, char(0)
      num_txt = 10*100-1
      call write_compress_txt(nbuf, input_txt)
      call close_gzfile
!
!
      write(fname,'(a,a1)') 'test_b.txt.gz', char(0)
      call open_wt_gzfile(fname)

      write(input_txt,'(a,a1)') 'test data', char(0)
      num_txt = 9
      call write_compress_txt(nbuf, input_txt)

      num_txt = 9
      do j = 1, 100
        write(input_txt,'(i9,a1)') j, char(0)
        call write_compress_txt(nbuf, input_txt)
      end do
      call close_gzfile
!
      write(fname,'(a,a1)') 'test_f.txt.gz', char(0)
!
      write(input_txt,'(a,a1)') 'test data', char(0)
      num_txt = 9
      write(*,*) 'num_txt', num_txt

      call open_wt_gzfile(fname)
      call write_compress_txt(nbuf, input_txt)
!
      do i = 1, 10
        write(input_txt,'(10i8,a)') ((i*1000+j),j=1,10), char(0)
        num_txt = 80
        call write_compress_txt(nbuf, input_txt)
!
      end do
!
      call close_gzfile
!
      call open_rd_gzfile(fname)
      call get_one_line_from_gz(nbuf, num_word, nchara, input_txt)
!
      write(*,*) 'num_word', num_word
        write(*,*) 'nchara', nchara
      read(input_txt,*) fname
      write(*,'(a)') trim(fname)
!
      do i = 1, 10
        call get_one_line_from_gz(nbuf, num_word, nchara, input_txt)
        write(*,*) 'num_word', num_word
        write(*,*) 'nchara', nchara
        read(input_txt,*) k(1:num_word)
        write(*,*) k
      end do
!
      stop
      end program test_zlib_lines
