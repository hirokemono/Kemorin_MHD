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
!
      implicit none
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
!
      integer(kind = kint) :: j3, j1, j2
      character(len=kchara) :: gzip_name
!
!
      call add_gzip_extension(sph_cor_file_name, gzip_name)
      write(*,*) 'Write gzipped integration file: ', trim(gzip_name)
      call open_wt_gzfile(gzip_name)
!
      write(textbuf,'(a,a1)') '# ----- rotate.dat -----', char(0)
      call gz_write_textbuf_f
      write(textbuf,'(a,a1)') '#  truncation', char(0)
      call gz_write_textbuf_f
      write(textbuf,'(i10,a1)')  ltr_cor_IO, char(0)
      call gz_write_textbuf_f
!
      j1 = 2
      write(textbuf,'(a,a1)') '# j1, l2_gl, j3_gl, Ki/pi', char(0)
      call gz_write_textbuf_f
      do j3 = 1 ,jmax_cor_IO
        do j2 = 1, 2
          write(textbuf,'(3i10,1pE25.15e3,a1)') j1,                     &
     &        jgl_kcor_IO(j3,j2,j1), j3, gk_cor_IO(j3,j2,j1), char(0)
          call gz_write_textbuf_f
        end do
      end do
      write(textbuf,'(a,a1)') '# j1, l2_gl, j3_gl, Li/pi', char(0)
      call gz_write_textbuf_f
      do j3 = 1 ,jmax_cor_IO
        write(textbuf,'(3i10,1pE25.15e3,a1)') j1,                       &
     &          jgl_lcor_IO(j3,1,j1), j3, el_cor_IO(j3,1,j1), char(0)
        call gz_write_textbuf_f
      end do
!*
!
      do j1 = 1, 3, 2
        write(textbuf,'(a,a1)') '# j1, l2_gl, j3_gl, Ki/pi', char(0)
        call gz_write_textbuf_f
        do j3 = 1 ,jmax_cor_IO
          do j2 = 1, 4
            write(textbuf,'(3i10,1pE25.15e3,a1)') j1,                   &
     &         jgl_kcor_IO(j3,j2,j1), j3, gk_cor_IO(j3,j2,j1), char(0)
            call gz_write_textbuf_f
          end do
        end do
!*
        write(textbuf,'(a,a1)') '# j1, l2_gl, j3_gl, Li/pi', char(0)
        call gz_write_textbuf_f
        do j3 = 1, jmax_cor_IO
          do j2 = 1, 2
            write(textbuf,'(3i10,1pE25.15e3,a1)') j1,                   &
     &        jgl_lcor_IO(j3,j2,j1), j3, el_cor_IO(j3,j2,j1), char(0)
            call gz_write_textbuf_f
          end do
        end do
      end do
      call close_gzfile
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
!
      integer(kind = kint) :: j3, j1, j2, itmp, nchara
      character(len=kchara) :: gzip_name
!
!
      call add_gzip_extension(sph_cor_file_name, gzip_name)
!
      if(iflag_debug.gt.0) write(*,*)                                   &
     &              'Read gzipped integration file: ', trim(gzip_name)
      call open_rd_gzfile(gzip_name)
!
      call skip_gz_comment_int(ltr_cor_IO)
      call allocate_int_sph_cor_IO
!
      j1 = 2
      call skip_gz_comment_int(itmp)
      read(textbuf,*) itmp, jgl_kcor_IO(1,1,j1), itmp,                  &
     &                gk_cor_IO(1,1,j1)
      call get_one_line_from_gz_f
      read(textbuf,*) itmp, jgl_kcor_IO(1,2,j1), itmp,                  &
     &                gk_cor_IO(1,2,j1)
      do j3 = 2 ,jmax_cor_IO
        do j2 = 1, 2
          call get_one_line_from_gz_f
          read(textbuf,*) itmp, jgl_kcor_IO(j3,j2,j1), itmp,            &
     &                gk_cor_IO(j3,j2,j1)
        end do
      end do
!
      call skip_gz_comment_int(itmp)
      read(textbuf,*) itmp, jgl_lcor_IO(1,1,j1), itmp,                  &
     &                el_cor_IO(1,1,j1)
      do j3 = 2 ,jmax_cor_IO
        call get_one_line_from_gz_f
        read(textbuf,*) itmp, jgl_lcor_IO(j3,1,j1), itmp,               &
     &                el_cor_IO(j3,1,j1)
      end do
!*
!
      do j1 = 1, 3, 2
        call skip_gz_comment_int(itmp)
        read(textbuf,*) itmp, jgl_kcor_IO(1,1,j1), itmp,                &
     &                          gk_cor_IO(1,1,j1)
        do j2 = 2, 4
          call get_one_line_from_gz_f
          read(textbuf,*) itmp, jgl_kcor_IO(1,j2,j1), itmp,             &
     &                gk_cor_IO(1,j2,j1)
        end do
        do j3 = 2, jmax_cor_IO
          do j2 = 1, 4
            call get_one_line_from_gz_f
            read(textbuf,*) itmp, jgl_kcor_IO(j3,j2,j1), itmp,          &
     &                gk_cor_IO(j3,j2,j1)
          end do
        end do
!*
        call skip_gz_comment_int(itmp)
        read(textbuf,*) itmp, jgl_lcor_IO(1,1,j1), itmp,                &
     &                 el_cor_IO(1,1,j1)
        call get_one_line_from_gz_f
        read(textbuf,*) itmp, jgl_lcor_IO(1,2,j1), itmp,                &
     &                el_cor_IO(1,2,j1)
        do j3 = 2, jmax_cor_IO
          do j2 = 1, 2
            call get_one_line_from_gz_f
            read(textbuf,*) itmp, jgl_lcor_IO(j3,j2,j1), itmp,          &
     &                el_cor_IO(j3,j2,j1)
          end do
        end do
      end do
!
      call close_gzfile
!
      end subroutine read_int_4_sph_coriolis_gz
!
! -----------------------------------------------------------------------
!
      end module gz_int_4_sph_coriolis_IO
