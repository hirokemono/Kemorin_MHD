!gz_ucd_field_file_IO.f90
!      module gz_ucd_field_file_IO
!
!     Written by H. Matsui
!
!      subroutine write_ucd_2_gz_fld_file(my_rank, istep)
!
!      subroutine read_ucd_2_gz_fld_file(my_rank, istep)
!      subroutine read_alloc_ucd_2_gz_fld_file(my_rank, istep)
!
      module gz_ucd_field_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_time_data_IO
      use m_ucd_data
      use m_field_file_format
      use gz_field_data_IO
      use skip_gz_comment
      use set_ucd_file_names
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_ucd_2_gz_fld_file(my_rank, istep)
!
      integer(kind=kint), intent(in) :: my_rank, istep
      character(len=kchara) :: gzip_name
!
!
      call set_parallel_ucd_file_name(ucd_header_name, iflag_fld_gz,    &
     &    my_rank, istep, gzip_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Write gzipped step data file: ', trim(gzip_name)
!
      call open_wt_gzfile(gzip_name)
!
      call write_gz_step_data(my_rank)
      call write_gz_field_data                                          &
     &         (nnod_ucd, num_field_ucd, ntot_comp_ucd,                 &
     &          num_comp_ucd, phys_name_ucd, d_nod_ucd)
!
      call close_gzfile
!
      end subroutine write_ucd_2_gz_fld_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_ucd_2_gz_fld_file(my_rank, istep)
!
      integer(kind=kint), intent(in) :: my_rank, istep
      character(len=kchara) :: gzip_name
!
!
      call set_parallel_ucd_file_name(ucd_header_name, iflag_fld_gz,    &
     &    my_rank, istep, gzip_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read gzipped data file: ', trim(gzip_name)
!
      call open_rd_gzfile(gzip_name)
!
      call read_gz_step_data
      call skip_gz_comment_int2(nnod_ucd, num_field_ucd)
      call read_gz_multi_int(num_field_ucd, num_comp_ucd)
      call read_gz_field_data                                           &
     &         (nnod_ucd, num_field_ucd, ntot_comp_ucd,                 &
     &          num_comp_ucd, phys_name_ucd, d_nod_ucd)
!
      call close_gzfile
!
      end subroutine read_ucd_2_gz_fld_file
!
!------------------------------------------------------------------
!
      subroutine read_alloc_ucd_2_gz_fld_file(my_rank, istep)
!
      integer(kind=kint), intent(in) :: my_rank, istep
      character(len=kchara) :: gzip_name
!
!
      call set_parallel_ucd_file_name(ucd_header_name, iflag_fld_gz,    &
     &    my_rank, istep, gzip_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &      'Read gzipped data file: ', trim(gzip_name)
!
      call open_rd_gzfile(gzip_name)
!
      write(*,*) 'read_gz_step_data'
      call read_gz_step_data
      write(*,*) 'skip_gz_comment_int2'
      call skip_gz_comment_int2(nnod_ucd, num_field_ucd)
      write(*,*) 'allocate_ucd_phys_name'
      call allocate_ucd_phys_name
!
      write(*,*) 'read_gz_multi_int'
      call read_gz_multi_int(num_field_ucd, num_comp_ucd)
!
      write(*,*) 'cal_istack_ucd_component'
      call cal_istack_ucd_component
      write(*,*) 'allocate_ucd_phys_data'
      call allocate_ucd_phys_data
!
      write(*,*) 'read_gz_field_data'
      call read_gz_field_data                                           &
     &         (nnod_ucd, num_field_ucd, ntot_comp_ucd,                 &
     &          num_comp_ucd, phys_name_ucd, d_nod_ucd)
!
      call close_gzfile
!
      end subroutine read_alloc_ucd_2_gz_fld_file
!
!------------------------------------------------------------------
!
      end module gz_ucd_field_file_IO
