!
!     module filter_moments_file_IO
!
!     programmed by H. Matsui on May, 2008
!
!      subroutine read_num_filter_mom_file(file_name, my_rank)
!      subroutine read_filter_elen_file(file_name, my_rank,             &
!     &          nnod, nele, ierr)
!      subroutine write_filter_elen_file(file_name, my_rank)
!
!      subroutine read_filter_moments_file(file_name, my_rank,          &
!     &          nnod, nele, ierr)
!      subroutine write_filter_moments_file(file_name, my_rank)
!
!      subroutine read_num_filter_mom_file_b(file_name, my_rank)
!      subroutine read_filter_elen_file_b(file_name, my_rank,           &
!     &          nnod, nele, ierr)
!      subroutine write_filter_elen_file_b(file_name, my_rank)
!
!      subroutine read_filter_moments_file_b(file_name, my_rank,        &
!     &          nnod, nele, ierr)
!      subroutine write_filter_moments_file_b(file_name, my_rank)
!
      module filter_moments_file_IO
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine read_num_filter_mom_file(file_name, my_rank)
!
      use m_filter_file_names
      use filter_moment_data_IO
      !
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
!
      if(i_debug .gt. 0) then
        write(*,*) 'Read ascii num of filter file: ', trim(file_name)
      else if(my_rank .eq. 0) then
        write(*,*) 'Read ascii num of filer files: ', trim(file_name)
      end if
!
      open(filter_file_code, file=file_name,                            &
     &        form='formatted', status= 'old')
      call read_filter_moment_num(filter_file_code)
      close(filter_file_code)
!
      end subroutine read_num_filter_mom_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_filter_elen_file(file_name, my_rank,              &
     &          nnod, nele, ierr)
!
      use m_filter_file_names
      use filter_moment_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: nnod, nele
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(i_debug .gt. 0) then
        write(*,*) 'Read ascii filter length file: ', trim(file_name)
      else if(my_rank .eq. 0) then
        write(*,*) 'Read ascii filter length files: ', trim(file_name)
      end if
!
      open(filter_file_code, file=file_name,                            &
     &        form='formatted', status= 'old')
      call read_filter_elen_data(filter_file_code, nnod, nele, ierr)
      close(filter_file_code)
!
      end subroutine read_filter_elen_file
!
!-----------------------------------------------------------------------
!
      subroutine write_filter_elen_file(file_name, my_rank)
!
      use m_filter_file_names
      use filter_moment_data_IO
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
!
      if(i_debug .gt. 0) then
        write(*,*) 'Write ascii filter length file: ', trim(file_name)
      else if(my_rank .eq. 0) then
        write(*,*) 'Write ascii filter length files: ', trim(file_name)
      end if
!
      open(filter_file_code, file=file_name, form='formatted')
      call write_filter_elen_data(filter_file_code)
      close(filter_file_code)
!
      end subroutine write_filter_elen_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_filter_moments_file(file_name, my_rank,           &
     &          nnod, nele, ierr)
!
      use m_filter_file_names
      use filter_moment_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: nnod, nele
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(i_debug .gt. 0) then
        write(*,*) 'Read ascii filter moment file: ', trim(file_name)
      else if(my_rank .eq. 0) then
        write(*,*) 'Read ascii filter moment files: ', trim(file_name)
      end if
!
      open(filter_file_code, file=file_name, form='formatted')
      call read_filter_moments_data(filter_file_code, nnod, nele,       &
     &        ierr)
      close(filter_file_code)
!
      end subroutine read_filter_moments_file
!
!-----------------------------------------------------------------------
!
      subroutine write_filter_moments_file(file_name, my_rank)
!
      use m_filter_file_names
      use filter_moment_data_IO
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
!
      if(i_debug .gt. 0) then
        write(*,*) 'Write ascii filter moment file: ', trim(file_name)
      else if(my_rank .eq. 0) then
        write(*,*) 'Write ascii filter moment files: ', trim(file_name)
      end if
!
      open(filter_file_code, file=file_name, form='formatted')
      call write_filter_moments_data(filter_file_code)
      close(filter_file_code)
!
      end subroutine write_filter_moments_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_num_filter_mom_file_b(file_name, my_rank)
!
      use m_filter_file_names
      use filter_moment_data_IO_b
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
!
      if(i_debug .gt. 0) then
        write(*,*) 'Read binary num of filter file: ', trim(file_name)
      else if(my_rank .eq. 0) then
        write(*,*) 'Read binary num of filer files: ', trim(file_name)
      end if
!
      open(filter_file_code, file=file_name,                            &
     &        form='unformatted', status= 'old')
      call read_filter_moment_num_b(filter_file_code)
      close(filter_file_code)
!
      end subroutine read_num_filter_mom_file_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_filter_elen_file_b(file_name, my_rank,            &
     &          nnod, nele, ierr)
!
      use m_filter_file_names
      use filter_moment_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: nnod, nele
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(i_debug .gt. 0) then
        write(*,*) 'Read binary filter length file: ', trim(file_name)
      else if(my_rank .eq. 0) then
        write(*,*) 'Read binary filter length files: ', trim(file_name)
      end if
!
      open(filter_file_code, file=file_name,                            &
     &        form='unformatted', status= 'old')
      call read_filter_elen_data_b(filter_file_code,                    &
     &        nnod, nele, ierr)
      close(filter_file_code)
!
      end subroutine read_filter_elen_file_b
!
!-----------------------------------------------------------------------
!
      subroutine write_filter_elen_file_b(file_name, my_rank)
!
      use m_filter_file_names
      use filter_moment_data_IO_b
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
!
      if(i_debug .gt. 0) then
        write(*,*) 'Write binary filter length file: ', trim(file_name)
      else if(my_rank .eq. 0) then
        write(*,*) 'Write binary filter length files: ',trim(file_name)
      end if
!
      open(filter_file_code, file=file_name, form='unformatted')
      call write_filter_elen_data_b(filter_file_code)
      close(filter_file_code)
!
      end subroutine write_filter_elen_file_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_filter_moments_file_b(file_name, my_rank,         &
     &          nnod, nele, ierr)
!
      use m_filter_file_names
      use filter_moment_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: nnod, nele
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(i_debug .gt. 0) then
        write(*,*) 'Read binary filter moment file: ', trim(file_name)
      else if(my_rank .eq. 0) then
        write(*,*) 'Read binary filter moment files: ', trim(file_name)
      end if
!
      open(filter_file_code, file=file_name, form='unformatted')
      call read_filter_moments_data_b(filter_file_code,               &
     &      nnod, nele, ierr)
      close(filter_file_code)
!
      end subroutine read_filter_moments_file_b
!
!-----------------------------------------------------------------------
!
      subroutine write_filter_moments_file_b(file_name, my_rank)
!
      use m_filter_file_names
      use filter_moment_data_IO_b
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
!
      if(i_debug .gt. 0) then
        write(*,*) 'Write binary filter moment file: ', trim(file_name)
      else if(my_rank .eq. 0) then
        write(*,*) 'Write binary filter moment files: ',trim(file_name)
      end if
!
      open(filter_file_code, file=file_name, form='unformatted')
      call write_filter_moments_data_b(filter_file_code)
      close(filter_file_code)
!
      end subroutine write_filter_moments_file_b
!
!-----------------------------------------------------------------------
!
      end module filter_moments_file_IO
