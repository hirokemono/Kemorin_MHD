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
      call skip_gz_comment_int( sph_IO1%sph_rank(1) )
      read(textbuf,*) sph_IO1%sph_rank(1:ndir_sph_IO)
!
      end subroutine read_rank_4_sph_gz
!
! -----------------------------------------------------------------------
!
      subroutine read_gl_resolution_sph_gz
!
!
      call skip_gz_comment_int( sph_IO1%nidx_gl_sph(1) )
      read(textbuf,*) sph_IO1%nidx_gl_sph(1:ndir_sph_IO)
!
      call skip_gz_comment_int( sph_IO1%ltr_gl )
!
      end subroutine read_gl_resolution_sph_gz
!
! -----------------------------------------------------------------------
!
      subroutine read_gl_nodes_sph_gz
!
      integer(kind = kint) :: i
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
      use m_sph_modes_grid_labels
!
      character(len=kchara) :: fmt_txt
!
!
      textbuf = hd_rtp_glbl() // char(0)
      call gz_write_textbuf_no_lf
!
      write(fmt_txt,'(a1,i2,a9)')                                       &
     &                '(', ndir_sph_IO, '(i16),a1)'
      write(textbuf,fmt_txt) sph_IO1%sph_rank(1:ndir_sph_IO), char(0)
      call gz_write_textbuf_w_lf
!
      end subroutine write_rank_4_sph_gz
!
! -----------------------------------------------------------------------
!
      subroutine write_gl_resolution_sph_gz
!
      use m_sph_modes_grid_labels
!
      character(len=kchara) :: fmt_txt
!
!
      textbuf = hd_trunc() // char(0)
      call gz_write_textbuf_no_lf
!
      write(fmt_txt,'(a1,i2,a9)')                                       &
     &                '(', ndir_sph_IO, '(i16),a1)'
      write(textbuf,fmt_txt) sph_IO1%nidx_gl_sph(1:ndir_sph_IO), char(0)
      call gz_write_textbuf_w_lf
!
      write(textbuf,'(i16,a1)') sph_IO1%ltr_gl, char(0)
      call gz_write_textbuf_w_lf
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
      call gz_write_textbuf_w_lf
!
      write(fmt_txt,'(a5,i2,a9)')                                       &
     &                '(i16,', ndir_sph_IO, '(i16),a1)'
      do i = 1, nnod_sph_IO
        write(textbuf,fmt_txt)                                          &
     &      inod_gl_sph_IO(i), idx_gl_sph_IO(i,1:ndir_sph_IO), char(0)
        call gz_write_textbuf_w_lf
      end do
!
      call deallocate_nod_id_sph_IO
!
      end subroutine write_gl_nodes_sph_gz
!
! -----------------------------------------------------------------------
!
      end module gz_spherical_model_IO
