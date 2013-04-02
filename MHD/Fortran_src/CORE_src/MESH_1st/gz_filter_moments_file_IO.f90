!gz_filter_moments_file_IO.f90
!      module gz_filter_moments_file_IO
!
!     Written by H. Matsui in 2004
!
!      subroutine read_num_filter_mom_file_gz(file_name, my_rank)
!      subroutine read_filter_elen_file_gz(file_name, my_rank,          &
!     &          nnod, nele, ierr)
!      subroutine write_filter_elen_file_gz(file_name, my_rank)
!
!      subroutine read_filter_moments_file_gz(file_name,                &
!     &          my_rank, nnod, nele, ierr)
!      subroutine write_filter_moments_file_gz(file_name, my_rank)
!
      module gz_filter_moments_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use set_parallel_file_name
      use skip_gz_comment
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine read_num_filter_mom_file_gz(file_name, my_rank)
!
      use m_filter_file_names
      use gz_filter_moment_data_IO
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
      character(len=kchara) :: gzip_name
!
!
      call add_gzip_extension(file_name, gzip_name)
!
      if(i_debug .gt. 0) then
        write(*,*) 'Read gzipped filter moment file: ',                 &
     &             trim(gzip_name)
      else if(my_rank .eq. 0) then
        write(*,*) 'Read gzipped filter moment files: ',                &
     &             trim(gzip_name)
      end if
!
      call open_rd_gzfile(gzip_name)
      call read_filter_moment_num_gz
      call close_gzfile
!
      end subroutine read_num_filter_mom_file_gz
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_filter_elen_file_gz(file_name, my_rank,           &
     &          nnod, nele, ierr)
!
      use m_filter_file_names
      use gz_filter_moment_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: nnod, nele
      integer(kind = kint), intent(inout) :: ierr
!
      character(len=kchara) :: gzip_name
!
!
      call add_gzip_extension(file_name, gzip_name)
!
      if(i_debug .gt. 0) then
        write(*,*) 'Read gzipped filter length file: ',                 &
     &             trim(gzip_name)
      else if(my_rank .eq. 0) then
        write(*,*) 'Read gzipped filter length files: ',                &
     &             trim(gzip_name)
      end if
!
      call open_rd_gzfile(gzip_name)
      call read_filter_elen_data_gz(nnod, nele, ierr)
      call close_gzfile
!
      end subroutine read_filter_elen_file_gz
!
!-----------------------------------------------------------------------
!
      subroutine write_filter_elen_file_gz(file_name, my_rank)
!
      use m_filter_file_names
      use gz_filter_moment_data_IO
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
      character(len=kchara) :: gzip_name
!
!
      call add_gzip_extension(file_name, gzip_name)
!
      if(i_debug .gt. 0) then
        write(*,*) 'Write gzipped filter length file: ',                &
     &             trim(gzip_name)
      else if(my_rank .eq. 0) then
        write(*,*) 'Write gzipped filter length files: ',               &
     &             trim(gzip_name)
      end if
!
      call open_wt_gzfile(gzip_name)
      call write_filter_elen_data_gz
      call close_gzfile
!
      end subroutine write_filter_elen_file_gz
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_filter_moments_file_gz(file_name, my_rank,        &
     &          nnod, nele, ierr)
!
      use m_filter_file_names
      use gz_filter_moment_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: nnod, nele
      integer(kind = kint), intent(inout) :: ierr
!
      character(len=kchara) :: gzip_name
!
!
      call add_gzip_extension(file_name, gzip_name)
!
      if(i_debug .gt. 0) then
        write(*,*) 'Read gzipped filter moment file: ',                 &
     &             trim(gzip_name)
      else if(my_rank .eq. 0) then
        write(*,*) 'Read gzipped filter moment files: ',                &
     &             trim(gzip_name)
      end if
!
      call open_rd_gzfile(gzip_name)
      call read_filter_moments_data_gz(nnod, nele, ierr)
      call close_gzfile
!
      end subroutine read_filter_moments_file_gz
!
!-----------------------------------------------------------------------
!
      subroutine write_filter_moments_file_gz(file_name, my_rank)
!
      use m_filter_file_names
      use gz_filter_moment_data_IO
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
      character(len=kchara) :: gzip_name
!
!
      call add_gzip_extension(file_name, gzip_name)
!
      if(i_debug .gt. 0) then
        write(*,*) 'Write gzipped filter moment file: ',                &
     &             trim(gzip_name)
      else if(my_rank .eq. 0) then
        write(*,*) 'Write gzipped filter moment files: ',               &
     &             trim(gzip_name)
      end if
!
      call open_wt_gzfile(gzip_name)
      call write_filter_moments_data_gz
      call close_gzfile
!
      end subroutine write_filter_moments_file_gz
!
!-----------------------------------------------------------------------
!
      end module gz_filter_moments_file_IO
