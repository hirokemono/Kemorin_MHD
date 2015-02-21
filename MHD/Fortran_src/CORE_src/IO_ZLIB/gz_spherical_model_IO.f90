!gz_spherical_model_IO.f90
!      module gz_spherical_model_IO
!
!     Written by H. Matsui on July, 2007
!
!      subroutine read_rank_4_sph
!      subroutine read_gl_resolution_sph_gz
!      subroutine read_gl_nodes_sph_gz
!
!      subroutine write_rank_4_sph_gz
!      subroutine write_gl_resolution_sph_gz
!      subroutine write_gl_nodes_sph_gz
!
      module gz_spherical_model_IO
!
      use m_precision
!
      use m_node_id_spherical_IO
      use skip_gz_comment
!
      implicit none
!
      character(len=255) :: character_4_read = ''
      private :: character_4_read
!
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_rank_4_sph_gz
!
!
      call skip_gz_comment_int( sph_rank_IO(1) )
      read(textbuf,*) sph_rank_IO(1:ndir_sph_IO)
!
      end subroutine read_rank_4_sph_gz
!
! -----------------------------------------------------------------------
!
      subroutine read_gl_resolution_sph_gz
!
!
      call skip_gz_comment_int( nidx_gl_sph_IO(1) )
      read(textbuf,*) nidx_gl_sph_IO(1:ndir_sph_IO)
!
      call skip_gz_comment_int( ltr_gl_IO )
!
      end subroutine read_gl_resolution_sph_gz
!
! -----------------------------------------------------------------------
!
      subroutine read_gl_nodes_sph_gz
!
      integer(kind = kint) :: i
      integer(kind = kint_gl) :: i8
!
!
      call skip_gz_comment_int( nnod_sph_IO )
!
      call allocate_nod_id_sph_IO
!
      do i = 1, nnod_sph_IO
        call get_one_line_from_gz_f
        read(textbuf,*)                                                 &
     &           inod_gl_sph_IO(i), idx_gl_sph_IO(i,1:ndir_sph_IO)
      end do
!
      end subroutine read_gl_nodes_sph_gz
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_rank_4_sph_gz
!
      character(len=kchara) :: fmt_txt
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_f
      write(textbuf,'(a,a1)') '! Domain ID', char(0)
      call gz_write_textbuf_f
      write(textbuf,'(a,a1)') '! segment ID for each direction',       &
     &                         char(0)
      call gz_write_textbuf_f
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_f
!
      write(fmt_txt,'(a1,i2,a9)')                                       &
     &                '(', ndir_sph_IO, '(i16),a1)'
      write(textbuf,fmt_txt) sph_rank_IO(1:ndir_sph_IO), char(0)
      call gz_write_textbuf_f
!
      end subroutine write_rank_4_sph_gz
!
! -----------------------------------------------------------------------
!
      subroutine write_gl_resolution_sph_gz
!
      character(len=kchara) :: fmt_txt
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_f
      write(textbuf,'(a,a1)') '! num. of global grids', char(0)
      call gz_write_textbuf_f
      write(textbuf,'(a,a1)')                                           &
     &    '! truncation level for spherical harmonics', char(0)
      call gz_write_textbuf_f
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_f
!
      write(fmt_txt,'(a1,i2,a9)')                                       &
     &                '(', ndir_sph_IO, '(i16),a1)'
      write(textbuf,fmt_txt) nidx_gl_sph_IO(1:ndir_sph_IO), char(0)
      call gz_write_textbuf_f
!
      write(textbuf,'(i16,a1)') ltr_gl_IO, char(0)
      call gz_write_textbuf_f
!
      end subroutine write_gl_resolution_sph_gz
!
! -----------------------------------------------------------------------
!
      subroutine write_gl_nodes_sph_gz
!
      integer(kind = kint) :: i
      character(len=kchara) :: fmt_txt
!
!
      write(textbuf,'(i16,a1)') nnod_sph_IO, char(0)
      call gz_write_textbuf_f
!
      write(fmt_txt,'(a5,i2,a9)')                                       &
     &                '(i16,', ndir_sph_IO, '(i16),a1)'
      do i = 1, nnod_sph_IO
        write(textbuf,fmt_txt)                                          &
     &      inod_gl_sph_IO(i), idx_gl_sph_IO(i,1:ndir_sph_IO), char(0)
        call gz_write_textbuf_f
      end do
!
      call deallocate_nod_id_sph_IO
!
      end subroutine write_gl_nodes_sph_gz
!
! -----------------------------------------------------------------------
!
      end module gz_spherical_model_IO
