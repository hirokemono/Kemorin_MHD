!gz_read_psf_result.f90
!      module gz_read_psf_result
!
!      Written by H. Matsui
!
!      subroutine read_allocate_psf_ucd_gz(gzip_name)
!      subroutine read_allocate_psf_udt_gz(gzip_name)
!
!      subroutine read_psf_grd_gz(gzip_name)
!      subroutine read_psf_data_udt_gz(gzip_name)
!
!      subroutine read_allocate_gz_psf_ncomps_udt
!
!      subroutine read_gz_psf_header
!      subroutine read_gz_psf_field_name
!
      module gz_read_psf_result
!
      use m_precision
!
      use m_constants
      use m_psf_results
!
      implicit none
!
      integer(kind = kint), parameter :: nbuf = 65535
      integer (kind =kint) :: num_word, nchara
      character(len=nbuf) :: textbuf
      private :: nbuf, textbuf, num_word, nchara
!
      private :: read_gz_psf_header, read_gz_psf_grid, read_gz_psf_data
      private :: read_and_count_gz_psf_ncomp, read_gz_psf_num_comp
      private :: read_gz_psf_num_comp_from_buf
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine read_allocate_psf_ucd_gz(gzip_name)
!
      character(len=kchara), intent(in) :: gzip_name
!
!
      write(*,*) 'read gzipped ucd file: ', trim(gzip_name)
!
      call open_rd_gzfile(gzip_name)
!
      call read_gz_psf_header
      call allocate_psf_results
!
      call read_gz_psf_grid
!
      call read_allocate_gz_psf_ncomps_udt
      call read_gz_psf_data
!
      call close_gzfile
!
      end subroutine read_allocate_psf_ucd_gz
!
!-----------------------------------------------------------------------
!
      subroutine read_psf_grd_gz(gzip_name)
!
      character(len=kchara), intent(in) :: gzip_name
!
!
      write(*,*) 'Read gzipped ucd  grid file: ', trim(gzip_name)
      call open_rd_gzfile(gzip_name)
!
      call read_gz_psf_header
      call allocate_psf_results
!
      call read_gz_psf_grid
!
      call close_gzfile
!
      end subroutine read_psf_grd_gz
!
!-----------------------------------------------------------------------
!
      subroutine read_allocate_psf_udt_gz(gzip_name)
!
      character(len=kchara), intent(in) :: gzip_name
!
!
      write(*,*) 'Read gzipped ucd data file: ', trim(gzip_name)
      call open_rd_gzfile(gzip_name)
!
      call read_allocate_gz_psf_ncomps_udt
      call read_gz_psf_data
!
      call close_gzfile
!
      end subroutine read_allocate_psf_udt_gz
!
!-----------------------------------------------------------------------
!
      subroutine read_psf_data_udt_gz(gzip_name)
!
      use read_psf_result
!
      character(len=kchara), intent(in) :: gzip_name
!
!
      write(*,*) 'Read gzipped ucd data file: ', trim(gzip_name)
      call open_rd_gzfile(gzip_name)
!
      call read_gz_psf_num_comp
      call count_stack_tot_psf_field
      call read_gz_psf_data
!
      call close_gzfile
!
      end subroutine read_psf_data_udt_gz
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_allocate_gz_psf_ncomps_udt
!
      use read_psf_result
!
!
      call read_and_count_gz_psf_ncomp
      call count_stack_tot_psf_field
      call allocate_psf_field_data
!
      end subroutine read_allocate_gz_psf_ncomps_udt
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_gz_psf_header
!
      integer(kind = kint) :: itmp
!
      call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
      read(textbuf,*) numnod_psf, numele_psf, ncomptot_psf, itmp, itmp
!
      end subroutine read_gz_psf_header
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_gz_psf_grid
!
      integer(kind = kint) :: i
      integer(kind = kint) :: itmp
      character(len=kchara) :: tmpchara
!
!
!
      do i = 1, numnod_psf
        call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
        read(textbuf,*) inod_psf(i), xx_psf(i,1:3)
      end do
!
!      write(*,*) 'finish node data'
!
      do i = 1, numele_psf
        call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
        read(textbuf,*) iele_psf(i), itmp, tmpchara, ie_psf(i,1:3)
      end do
!
      end subroutine read_gz_psf_grid
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_psf_field_name
!
      integer(kind = kint) :: i
!
!
      do i = 1, nfield_psf
        call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
        read(textbuf,*) psf_data_name(i)
      end do
!
!      write(*,*) 'finish field header', nfield_psf,                    &
!     &        psf_data_name(1:nfield_psf)
!
      end subroutine read_gz_psf_field_name
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_psf_data
!
      integer(kind = kint) :: i, ist
      integer(kind = kint) :: itmp
!
!
      call read_gz_psf_field_name
!
      do i = 1, numnod_psf
        call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
        read(textbuf,*) itmp, d_nod_psf(i,1:num_word-1)
!
        if(ncomptot_psf .gt. num_word-1) then
          ist = num_word-1
          do
            call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
            read(textbuf,*) d_nod_psf(i,ist+1:ist+num_word)
            ist = ist + num_word
            if(ist .gt. ncomptot_psf) exit
          end do
        end if
      end do
!
      end subroutine read_gz_psf_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_and_count_gz_psf_ncomp
!
!
      call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
      read(textbuf,*) nfield_psf
!
      call allocate_psf_num_field
!
      call read_gz_psf_num_comp_from_buf
!
      end subroutine read_and_count_gz_psf_ncomp
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_psf_num_comp
!
!
      call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
      call read_gz_psf_num_comp_from_buf
!
      end subroutine read_gz_psf_num_comp
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_psf_num_comp_from_buf
!
      integer(kind = kint) :: i, ist
!
!
      read(textbuf,*) nfield_psf, ncomp_psf(1:num_word-1)
!
      if(nfield_psf .gt. num_word-1) then
        ist = num_word-1
        do
          call get_one_line_from_gz(nbuf, num_word, nchara, textbuf)
          read(textbuf,*) ncomp_psf(ist+1:ist+num_word)
          ist = ist + num_word
          if(ist .gt. nfield_psf) exit
        end do
      end if
!
      end subroutine read_gz_psf_num_comp_from_buf
!
!-----------------------------------------------------------------------
!
      end module  gz_read_psf_result
