!gz_sph_global_1d_idx_IO.f90
!      module gz_sph_global_1d_idx_IO
!
!     Written by H. Matsui on July, 2007
!
!      subroutine read_rtp_gl_1d_table_gz
!      subroutine read_rj_gl_1d_table_gz
!
!      subroutine write_rtp_gl_1d_table_gz
!      subroutine write_rj_gl_1d_table_gz
!
      module gz_sph_global_1d_idx_IO
!
      use m_precision
!
      use m_node_id_spherical_IO
      use skip_gz_comment
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_rtp_gl_1d_table_gz
!
      integer(kind = kint) :: nchara
      integer(kind = kint) :: i
!
!
      ndir_sph_IO = 3
      ncomp_itbl_1d_IO(1) = 1
      ncomp_itbl_1d_IO(2) = 1
      ncomp_itbl_1d_IO(3) = 2
!
      call skip_gz_comment_int( nidx_sph_IO(1) )
      read(textbuf,*)  nidx_sph_IO(1), ist_sph_IO(1), ied_sph_IO(1)
      call allocate_idx_sph_1d1_IO
!
      do i = 1, nidx_sph_IO(1)
        call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
        read(textbuf,*) idx_gl_1_IO(i), r_gl_1_IO(i)
      end do
!
      call skip_gz_comment_int( nidx_sph_IO(2) )
      read(textbuf,*) nidx_sph_IO(2), ist_sph_IO(2), ied_sph_IO(2)
      call allocate_idx_sph_1d2_IO
!
      do i = 1, nidx_sph_IO(2)
        call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
        read(textbuf,*) idx_gl_2_IO(i,1:ncomp_itbl_1d_IO(2))
      end do
!
      call skip_gz_comment_int( nidx_sph_IO(3) )
      read(textbuf,*) nidx_sph_IO(3), ist_sph_IO(3), ied_sph_IO(3)
      call allocate_idx_sph_1d3_IO
!
      do i = 1, nidx_sph_IO(3)
        call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
        read(textbuf,*) idx_gl_3_IO(i,1:ncomp_itbl_1d_IO(3))
      end do
!
      end subroutine read_rtp_gl_1d_table_gz
!
! -----------------------------------------------------------------------!
      subroutine read_rj_gl_1d_table_gz
!
      integer(kind = kint) :: nchara
      integer(kind = kint) :: i
!
!
      ndir_sph_IO = 2
      ncomp_itbl_1d_IO(1) = 1
      ncomp_itbl_1d_IO(2) = 3
!
      call skip_gz_comment_int( nidx_sph_IO(1) )
      read(textbuf,*) nidx_sph_IO(1), ist_sph_IO(1), ied_sph_IO(1)
      call allocate_idx_sph_1d1_IO
!
      do i = 1, nidx_sph_IO(1)
        call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
        read(textbuf,*) idx_gl_1_IO(i), r_gl_1_IO(i)
      end do
!
      call skip_gz_comment_int( nidx_sph_IO(2) )
      read(textbuf,*) nidx_sph_IO(2), ist_sph_IO(2), ied_sph_IO(2)
      call allocate_idx_sph_1d2_IO
!
      do i = 1, nidx_sph_IO(2)
        call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
        read(textbuf,*) idx_gl_2_IO(i,1:ncomp_itbl_1d_IO(2))
      end do
!
      end subroutine read_rj_gl_1d_table_gz
!
! -----------------------------------------------------------------------! -----------------------------------------------------------------------!
      subroutine write_rtp_gl_1d_table_gz
!
      character(len=kchara) :: fmt_txt
      integer(kind = kint) :: i
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(a,a1)')                                           &
     &       '! num. start and end global grids', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(a,a1)') '! r-direction', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(3i10,a1)') nidx_sph_IO(1),                        &
     &                        ist_sph_IO(1), ied_sph_IO(1), char(0)
      call write_compress_txt(nbuf, textbuf)
!
      do i = 1, nidx_sph_IO(1)
        write(textbuf,'(i10,1pE25.15e3,a1)')                            &
     &                        idx_gl_1_IO(i), r_gl_1_IO(i), char(0)
        call write_compress_txt(nbuf, textbuf)
      end do
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(a,a1)')                                           &
     &      '! num. start and end global grids', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(a,a1)') '! theta direction', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(3i10,a1)') nidx_sph_IO(2),                        &
     &                        ist_sph_IO(2), ied_sph_IO(2), char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(fmt_txt,'(a1,i3,a9)')                                       &
     &                '(', ncomp_itbl_1d_IO(2), '(i15),a1)'
      do i = 1, nidx_sph_IO(2)
        write(textbuf,fmt_txt)                                          &
     &        idx_gl_2_IO(i,1:ncomp_itbl_1d_IO(2)), char(0)
        call write_compress_txt(nbuf, textbuf)
      end do
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(a,a1)')                                           &
     &      '! num. of start and end global grids and modes', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(a,a1)') '! phi direction', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(3i10,a1)') nidx_sph_IO(3),                        &
     &                        ist_sph_IO(3), ied_sph_IO(3), char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(fmt_txt,'(a1,i3,a9)')                                       &
     &                '(', ncomp_itbl_1d_IO(3), '(i15),a1)'
      do i = 1, nidx_sph_IO(3)
        write(textbuf,fmt_txt)                                          &
     &               idx_gl_3_IO(i,1:ncomp_itbl_1d_IO(3)), char(0)
        call write_compress_txt(nbuf, textbuf)
      end do
!
      call deallocate_idx_sph_1d1_IO
      call deallocate_idx_sph_1d2_IO
      call deallocate_idx_sph_1d3_IO
!
      end subroutine write_rtp_gl_1d_table_gz
!
! ----------------------------------------------------------------------
!
      subroutine write_rj_gl_1d_table_gz
!
      character(len=kchara) :: fmt_txt
      integer(kind = kint) :: i
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(a,a1)')                                           &
     &      '! num. start and end global grids', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(a,a1)') '! r-direction', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(3i10,a1)') nidx_sph_IO(1),                        &
     &                        ist_sph_IO(1), ied_sph_IO(1), char(0)
      call write_compress_txt(nbuf, textbuf)
!
      do i = 1, nidx_sph_IO(1)
        write(textbuf,'(i10,1pE25.15e3,a1)')                            &
     &                        idx_gl_1_IO(i), r_gl_1_IO(i), char(0)
        call write_compress_txt(nbuf, textbuf)
      end do
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(a,a1)')                                           &
     &      '! num. start and end global modes', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(a,a1)')                                           &
     &      '! on sphere surface wuth degree and order', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(a,a1)') '!', char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(textbuf,'(3i10,a1)') nidx_sph_IO(2),                        &
     &                        ist_sph_IO(2), ied_sph_IO(2), char(0)
      call write_compress_txt(nbuf, textbuf)
!
      write(fmt_txt,'(a1,i3,a9)')                                       &
     &                '(', ncomp_itbl_1d_IO(2), '(i15),a1)'
      do i = 1, nidx_sph_IO(2)
        write(textbuf,fmt_txt)                                          &
     &          idx_gl_2_IO(i,1:ncomp_itbl_1d_IO(2)), char(0)
        call write_compress_txt(nbuf, textbuf)
      end do
!
      call deallocate_idx_sph_1d1_IO
      call deallocate_idx_sph_1d2_IO
!
      end subroutine write_rj_gl_1d_table_gz
!
! ----------------------------------------------------------------------
!
      end module gz_sph_global_1d_idx_IO
