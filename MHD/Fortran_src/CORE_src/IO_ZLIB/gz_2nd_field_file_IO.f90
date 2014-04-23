!gz_2nd_field_file_IO.f90
!      module gz_2nd_field_file_IO
!
!     Written by H. Matsui
!
!      subroutine write_gz_step_field2_file(gzip_name, my_rank)
!
!      subroutine read_gz_step_field2_file(gzip_name, my_rank)
!      subroutine read_and_alloc_gz_step_field2(gzip_name, my_rank)
!
!      subroutine read_alloc_gz_step_field2_head(gzip_name, my_rank)
!
      module gz_2nd_field_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_2nd_field_data_IO
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
      subroutine write_gz_step_field2_file(gzip_name, my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: gzip_name
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Write gzipped field file: ', trim(gzip_name)
      end if
!
      call open_wt_gzfile(gzip_name)
!
      call write_gz_step_data(my_rank)
      call write_gz_field_data                                          &
     &         (ngrid2_sph_IO, num_phys2_fld_IO, ntot_phys2_comp_IO,    &
     &          num_phys2_comp_IO, phys2_name_IO, phys2_data_IO)
!
      call close_gzfile
!
      end subroutine write_gz_step_field2_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_gz_step_field2_file(gzip_name, my_rank)
!
      integer(kind=kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: gzip_name
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read gzipped field file: ', trim(gzip_name)
      end if
!
      call open_rd_gzfile(gzip_name)
!
      call read_gz_step_data
      call skip_gz_comment_int2(ngrid2_sph_IO, num_phys2_fld_IO)
      call read_gz_multi_int(num_phys2_fld_IO, num_phys2_comp_IO)
      call read_gz_field_data                                           &
     &         (ngrid2_sph_IO, num_phys2_fld_IO, ntot_phys2_comp_IO,    &
     &          num_phys2_comp_IO, phys2_name_IO, phys2_data_IO)
!
      call close_gzfile
!
      end subroutine read_gz_step_field2_file
!
!------------------------------------------------------------------
!
      subroutine read_and_alloc_gz_step_field2(gzip_name, my_rank)
!
      integer(kind=kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: gzip_name
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read gzipped field file: ', trim(gzip_name)
      end if
!
      call open_rd_gzfile(gzip_name)
!
      call read_gz_step_data
      call skip_gz_comment_int2(ngrid2_sph_IO, num_phys2_fld_IO)
!
      call allocate_field2_name_IO
      call read_gz_multi_int(num_phys2_fld_IO, num_phys2_comp_IO)
!
      call cal_istack_fld2_comp_IO
      call allocate_field2_data_IO
!
      call read_gz_field_data                                           &
     &         (ngrid2_sph_IO, num_phys2_fld_IO, ntot_phys2_comp_IO,    &
     &          num_phys2_comp_IO, phys2_name_IO, phys2_data_IO)
!
      call close_gzfile
!
      end subroutine read_and_alloc_gz_step_field2
!
!------------------------------------------------------------------
!
      subroutine read_alloc_gz_step_field2_head(gzip_name, my_rank)
!
      integer(kind=kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: gzip_name
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read gzipped field file: ', trim(gzip_name)
      end if
!
      call open_rd_gzfile(gzip_name)
!
      call read_gz_step_data
      call skip_gz_comment_int2(ngrid2_sph_IO, num_phys2_fld_IO)
!
      call allocate_field2_name_IO
      call read_gz_multi_int(num_phys2_fld_IO, num_phys2_comp_IO)
!
      call close_gzfile
!
      call cal_istack_fld2_comp_IO
!
      end subroutine read_alloc_gz_step_field2_head
!
!------------------------------------------------------------------
!
      end module gz_2nd_field_file_IO
