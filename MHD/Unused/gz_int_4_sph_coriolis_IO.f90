!gz_int_4_sph_coriolis_IO.f90
!      module gz_int_4_sph_coriolis_IO
!
!     Written by H. Matsui on March, 2010
!
!      subroutine write_int_4_sph_coriolis_gz
!      subroutine read_int_4_sph_coriolis_gz
!
      module gz_int_4_sph_coriolis_IO
!
      use m_precision
      use m_machine_parameter
      use set_parallel_file_name
      use t_buffer_4_gzip
!
      implicit none
!
      type(buffer_4_gzip), private :: zbuf_cor
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine write_int_4_sph_coriolis_gz
!
      use m_int_4_sph_coriolis_IO
      use gz_node_geometry_IO
      use gzip_file_access
      use skip_gz_comment
!
      integer(kind = kint) :: j3, j1, j2
      character(len=kchara) :: gzip_name
!
!
      gzip_name = add_gzip_extension(sph_cor_file_name)
      write(*,*) 'Write gzipped integration file: ', trim(gzip_name)
      call open_wt_gzfile_a(gzip_name, zbuf_cor)
!
      write(zbuf_cor%fixbuf(1),'(a,2a1)') '# ----- rotate.dat -----',   &
     &                        char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf_cor)
      write(zbuf_cor%fixbuf(1),'(a,2a1)') '#  truncation',              &
     &                                char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf_cor)
      write(zbuf_cor%fixbuf(1),'(i16,2a1)')                             &
     &                        ltr_cor_IO, char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf_cor)
!
      j1 = 2
      write(zbuf_cor%fixbuf(1),'(a,2a1)') '# j1, l2_gl, j3_gl, Ki/pi',  &
     &                        char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf_cor)
      do j3 = 1 ,jmax_cor_IO
        do j2 = 1, 2
          write(zbuf_cor%fixbuf(1),'(3i16,1pE25.15e3,2a1)')             &
     &        j1, jgl_kcor_IO(j3,j2,j1), j3, gk_cor_IO(j3,j2,j1),       &
     &        char(10), char(0)
          call gz_write_textbuf_no_lf(zbuf_cor)
        end do
      end do
      write(zbuf_cor%fixbuf(1),'(a,2a1)') '# j1, l2_gl, j3_gl, Li/pi',  &
     &                                char(10), char(0)
      call gz_write_textbuf_no_lf(zbuf_cor)
      do j3 = 1 ,jmax_cor_IO
        write(zbuf_cor%fixbuf(1),'(3i16,1pE25.15e3,2a1)')               &
     &          j1, jgl_lcor_IO(j3,1,j1), j3, el_cor_IO(j3,1,j1),       &
     &          char(10), char(0)
        call gz_write_textbuf_no_lf(zbuf_cor)
      end do
!*
!
      do j1 = 1, 3, 2
        write(zbuf_cor%fixbuf(1),'(a,2a1)')                             &
     &           '# j1, l2_gl, j3_gl, Ki/pi', char(10), char(0)
        call gz_write_textbuf_no_lf(zbuf_cor)
        do j3 = 1 ,jmax_cor_IO
          do j2 = 1, 4
            write(zbuf_cor%fixbuf(1),'(3i16,1pE25.15e3,2a1)')           &
     &         j1, jgl_kcor_IO(j3,j2,j1), j3, gk_cor_IO(j3,j2,j1),      &
     &         char(10), char(0)
            call gz_write_textbuf_no_lf(zbuf_cor)
          end do
        end do
!*
        write(zbuf_cor%fixbuf(1),'(a,2a1)')                             &
     &            '# j1, l2_gl, j3_gl, Li/pi', char(10), char(0)
        call gz_write_textbuf_no_lf(zbuf_cor)
        do j3 = 1, jmax_cor_IO
          do j2 = 1, 2
            write(zbuf_cor%fixbuf(1),'(3i16,1pE25.15e3,2a1)')           &
     &        j1, jgl_lcor_IO(j3,j2,j1), j3, el_cor_IO(j3,j2,j1),       &
     &        char(10), char(0)
            call gz_write_textbuf_no_lf(zbuf_cor)
          end do
        end do
      end do
      call close_gzfile_a(zbuf_cor)
!
      call deallocate_int_sph_cor_IO
!
      end subroutine write_int_4_sph_coriolis_gz
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_int_4_sph_coriolis_gz
!
      use m_int_4_sph_coriolis_IO
      use gz_node_geometry_IO
      use gzip_file_access
      use skip_gz_comment
!
      integer(kind = kint) :: j3, j1, j2, itmp
      character(len=kchara) :: gzip_name
!
!
      gzip_name = add_gzip_extension(sph_cor_file_name)
!
      if(iflag_debug.gt.0) write(*,*)                                   &
     &              'Read gzipped integration file: ', trim(gzip_name)
      call open_rd_gzfile_a(gzip_name, zbuf_cor)
!
      call skip_gz_comment_int(ltr_cor_IO, zbuf_cor)
      call allocate_int_sph_cor_IO
!
      j1 = 2
      call skip_gz_comment_int(itmp, zbuf_cor)
      read(zbuf_cor%fixbuf(1),*) itmp, jgl_kcor_IO(1,1,j1), itmp,       &
     &                gk_cor_IO(1,1,j1)
      call get_one_line_text_from_gz(zbuf_cor)
      read(zbuf_cor%fixbuf(1),*) itmp, jgl_kcor_IO(1,2,j1), itmp,       &
     &                gk_cor_IO(1,2,j1)
      do j3 = 2 ,jmax_cor_IO
        do j2 = 1, 2
          call get_one_line_text_from_gz(zbuf_cor)
          read(zbuf_cor%fixbuf(1),*) itmp, jgl_kcor_IO(j3,j2,j1), itmp, &
     &                gk_cor_IO(j3,j2,j1)
        end do
      end do
!
      call skip_gz_comment_int(itmp, zbuf_cor)
      read(zbuf_cor%fixbuf(1),*) itmp, jgl_lcor_IO(1,1,j1), itmp,       &
     &                el_cor_IO(1,1,j1)
      do j3 = 2 ,jmax_cor_IO
        call get_one_line_text_from_gz(zbuf_cor)
        read(zbuf_cor%fixbuf(1),*) itmp, jgl_lcor_IO(j3,1,j1), itmp,    &
     &                el_cor_IO(j3,1,j1)
      end do
!*
!
      do j1 = 1, 3, 2
        call skip_gz_comment_int(itmp, zbuf_cor)
        read(zbuf_cor%fixbuf(1),*) itmp, jgl_kcor_IO(1,1,j1), itmp,     &
     &                          gk_cor_IO(1,1,j1)
        do j2 = 2, 4
          call get_one_line_text_from_gz(zbuf_cor)
          read(zbuf_cor%fixbuf(1),*) itmp, jgl_kcor_IO(1,j2,j1), itmp,  &
     &                gk_cor_IO(1,j2,j1)
        end do
        do j3 = 2, jmax_cor_IO
          do j2 = 1, 4
            call get_one_line_text_from_gz(zbuf_cor)
            read(zbuf_cor%fixbuf(1),*) itmp, jgl_kcor_IO(j3,j2,j1),     &
     &                                itmp, gk_cor_IO(j3,j2,j1)
          end do
        end do
!*
        call skip_gz_comment_int(itmp, zbuf_cor)
        read(zbuf_cor%fixbuf(1),*) itmp, jgl_lcor_IO(1,1,j1), itmp,     &
     &                 el_cor_IO(1,1,j1)
        call get_one_line_text_from_gz(zbuf_cor)
        read(zbuf_cor%fixbuf(1),*) itmp, jgl_lcor_IO(1,2,j1), itmp,     &
     &                el_cor_IO(1,2,j1)
        do j3 = 2, jmax_cor_IO
          do j2 = 1, 2
            call get_one_line_text_from_gz(zbuf_cor)
            read(zbuf_cor%fixbuf(1),*) itmp, jgl_lcor_IO(j3,j2,j1),     &
     &                                itmp, el_cor_IO(j3,j2,j1)
          end do
        end do
      end do
!
      call close_gzfile_a(zbuf_cor)
!
      end subroutine read_int_4_sph_coriolis_gz
!
! -----------------------------------------------------------------------
!
      end module gz_int_4_sph_coriolis_IO
