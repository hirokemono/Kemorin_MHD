!gz_field_file_IO.f90
!      module gz_field_file_IO
!
!     Written by H. Matsui
!
!      subroutine write_gz_step_field_file(gzip_name, my_rank)
!
!      subroutine read_alloc_gz_field_file(gzip_name, my_rank)
!      subroutine read_gz_step_field_file(gzip_name, my_rank)
!      subroutine read_alloc_gz_step_field_file(gzip_name, my_rank)
!
!      subroutine read_alloc_gz_step_field_head(gzip_name, my_rank)
!
      module gz_field_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_time_data_IO
      use m_field_data_IO
      use gz_field_data_IO
      use skip_gz_comment
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_gz_step_field_file(gzip_name, my_rank)
!
      character(len=kchara), intent(in) :: gzip_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write gzipped data file: ', trim(gzip_name)
!
      call open_wt_gzfile(gzip_name)
!
      call write_gz_step_data(my_rank)
      call write_gz_field_data                                          &
     &         (numgrid_phys_IO, num_phys_data_IO, ntot_phys_data_IO,   &
     &          num_phys_comp_IO, phys_data_name_IO, phys_data_IO)
!
      call close_gzfile
!
      end subroutine write_gz_step_field_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_alloc_gz_field_file(gzip_name, my_rank)
!
      character(len=kchara), intent(in) :: gzip_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped data file: ', trim(gzip_name)
!
      call open_rd_gzfile(gzip_name)
!
      call skip_gz_comment_int2(numgrid_phys_IO, num_phys_data_IO)
      call allocate_phys_data_name_IO
!
      call read_gz_multi_int(num_phys_data_IO, num_phys_comp_IO)
!
      call cal_istack_phys_comp_IO
      call allocate_phys_data_IO
!
      call read_gz_field_data                                           &
     &         (numgrid_phys_IO, num_phys_data_IO, ntot_phys_data_IO,   &
     &          num_phys_comp_IO, phys_data_name_IO, phys_data_IO)
!
      call close_gzfile
!
      end subroutine read_alloc_gz_field_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_gz_step_field_file(gzip_name, my_rank)
!
      character(len=kchara), intent(in) :: gzip_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped data file: ', trim(gzip_name)
!
      call open_rd_gzfile(gzip_name)
!
      call read_gz_step_data
      call skip_gz_comment_int2(numgrid_phys_IO, num_phys_data_IO)
      call read_gz_multi_int(num_phys_data_IO, num_phys_comp_IO)
      call read_gz_field_data                                           &
     &         (numgrid_phys_IO, num_phys_data_IO, ntot_phys_data_IO,   &
     &          num_phys_comp_IO, phys_data_name_IO, phys_data_IO)
!
      call close_gzfile
!
      end subroutine read_gz_step_field_file
!
!------------------------------------------------------------------
!
      subroutine read_alloc_gz_step_field_file(gzip_name, my_rank)
!
      character(len=kchara), intent(in) :: gzip_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped data file: ', trim(gzip_name)
!
      call open_rd_gzfile(gzip_name)
!
      call read_gz_step_data
      call skip_gz_comment_int2(numgrid_phys_IO, num_phys_data_IO)
      call allocate_phys_data_name_IO
!
      call read_gz_multi_int(num_phys_data_IO, num_phys_comp_IO)
!
      call cal_istack_phys_comp_IO
      call allocate_phys_data_IO
!
      call read_gz_field_data                                           &
     &         (numgrid_phys_IO, num_phys_data_IO, ntot_phys_data_IO,   &
     &          num_phys_comp_IO, phys_data_name_IO, phys_data_IO)
!
      call close_gzfile
!
      end subroutine read_alloc_gz_step_field_file
!
!------------------------------------------------------------------
!
      subroutine read_alloc_gz_step_field_head(gzip_name, my_rank)
!
      character(len=kchara), intent(in) :: gzip_name
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped data file: ', trim(gzip_name)
!
      call open_rd_gzfile(gzip_name)
!
      call read_gz_step_data
      call skip_gz_comment_int2(numgrid_phys_IO, num_phys_data_IO)
      call allocate_phys_data_name_IO
!
      call read_gz_multi_int(num_phys_data_IO, num_phys_comp_IO)
      call cal_istack_phys_comp_IO
!
      call close_gzfile
!
      end subroutine read_alloc_gz_step_field_head
!
!------------------------------------------------------------------
!
      end module gz_field_file_IO
