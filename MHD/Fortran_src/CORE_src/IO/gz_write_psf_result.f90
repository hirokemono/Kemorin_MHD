!gz_write_psf_result.f90
!      module gz_write_psf_result
!
!      Written by H. Matsui
!
!      subroutine write_gz_psf_ucd(gzip_name)
!      subroutine write_gz_psf_grd(gzip_name)
!      subroutine write_gz_psf_udt(gzip_name)
!
      module gz_write_psf_result
!
      use m_precision
!
      use m_constants
      use m_psf_results
      use skip_gz_comment
!
      implicit none
!
      private :: write_psf_grid_gz, write_psf_data_gz
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_psf_ucd(gzip_name)
!
      character(len=kchara), intent(in) :: gzip_name
!
!
      write(*,*) 'gzipped PSF UCD data: ', trim(gzip_name)
      call open_rd_gzfile(gzip_name)
!
      call write_psf_grid_gz
      call write_psf_data_gz
!
      call close_gzfile
!
      end subroutine write_gz_psf_ucd
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_gz_psf_grd(gzip_name)
!
      character(len=kchara), intent(in) :: gzip_name
!
!
      write(*,*) 'gzipped PSF grid data: ', trim(gzip_name)
      call open_rd_gzfile(gzip_name)
!
      call write_psf_grid_gz
!
      call close_gzfile
!
      end subroutine write_gz_psf_grd
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_psf_udt(gzip_name)
!
      character(len=kchara), intent(in) :: gzip_name
!
!
      write(*,*) 'gzipped PSF result data: ', trim(gzip_name)
      call open_rd_gzfile(gzip_name)
!
      call write_psf_data_gz
!
      call close_gzfile
!
      end subroutine write_gz_psf_udt
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_psf_grid_gz
!
      integer(kind = kint) :: i
!
!
      write(textbuf,'(5i10)')                                           &
     &     numnod_psf, numele_psf, ncomptot_psf, izero, izero
      call write_compress_txt(nbuf, textbuf)
!
      do i = 1, numnod_psf
        write(textbuf,'(i10,1p3e25.15E3)') inod_psf(i), xx_psf(i,1:3)
        call write_compress_txt(nbuf, textbuf)
      end do
!
      do i = 1, numele_psf
        write(textbuf,'(2i10,a5,3i10)') iele_psf(i), ione,              &
     &                                   ' tri ', ie_psf(i,1:3)
        call write_compress_txt(nbuf, textbuf)
      end do
!
      end subroutine write_psf_grid_gz
!
!-----------------------------------------------------------------------
!
      subroutine write_psf_data_gz
!
      integer(kind = kint) :: i
!
!
      write(textbuf,'(255i5)') nfield_psf, ncomp_psf(1:nfield_psf)
      call write_compress_txt(nbuf, textbuf)
!
      do i = 1, nfield_psf
        write(textbuf,'(a,a1)') trim(psf_data_name(i)),','
        call write_compress_txt(nbuf, textbuf)
      end do
!
      do i = 1, numnod_psf
        write(textbuf,'(i10,1p255e25.15E3)')                            &
     &              inod_psf(i), d_nod_psf(i,1:ncomptot_psf)
        call write_compress_txt(nbuf, textbuf)
      end do
!
      end subroutine write_psf_data_gz
!
!-----------------------------------------------------------------------
!
      end module  gz_write_psf_result
