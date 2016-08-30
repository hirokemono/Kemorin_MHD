!gz_filter_coefs_file_IO.f90
!      module gz_filter_coefs_file_IO
!
!     Written by H. Matsui in 2004
!
!!      subroutine read_sort_filter_coef_file_gz                        &
!!     &         (file_name, my_rank, IO_filters)
!!        type(filter_coefficients_type), intent(inout) :: IO_filters
!!      subroutine write_sort_filter_coef_file_gz                       &
!!     &         (file_name, my_rank, IO_filters)
!!        type(filter_coefficients_type), intent(in) :: IO_filters
!      subroutine read_filter_geometry_file_gz(file_name, my_rank)
!      subroutine write_filter_geometry_file_gz(file_name, my_rank)
!
      module gz_filter_coefs_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use t_filter_coefficients
      use set_parallel_file_name
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
      subroutine read_sort_filter_coef_file_gz                          &
     &         (file_name, my_rank, IO_filters)
!
      use m_filter_file_names
      use gz_mesh_data_IO
      use gz_filter_coef_IO
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
      type(filter_coefficients_type), intent(inout) :: IO_filters
!
      character(len=kchara) :: gzip_name
!
!
      call add_gzip_extension(file_name, gzip_name)
!
      if(i_debug .gt. 0) then
        write(*,*) 'Read gzipped filter file: ', trim(gzip_name)
      else if(my_rank .eq. 0) then
        write(*,*) 'Read gzipped filter files: ', trim(gzip_name)
      end if
!
      call open_rd_gzfile_f(gzip_name)
      call read_filter_geometry_gz
      call read_3d_filter_stack_gz(IO_filters)
      call read_3d_filter_weights_coef_gz(IO_filters)
      call close_gzfile_f
!
      end subroutine read_sort_filter_coef_file_gz
!
!------------------------------------------------------------------
!
      subroutine write_sort_filter_coef_file_gz                         &
     &         (file_name, my_rank, IO_filters)
!
      use m_filter_file_names
      use gz_mesh_data_IO
      use gz_filter_coef_IO
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara), intent(in) :: file_name
!
      type(filter_coefficients_type), intent(in) :: IO_filters
!
      character(len=kchara) :: gzip_name
!
!
      call add_gzip_extension(file_name, gzip_name)
!
      if(i_debug .gt. 0) then
        write(*,*) 'Write gzipped filter file: ', trim(gzip_name)
      else if(my_rank .eq. 0) then
        write(*,*) 'Write gzipped filter files: ', trim(gzip_name)
      end if
!
      call open_wt_gzfile_f(gzip_name)
!
      call write_filter_geometry_gz
      call write_3d_filter_stack_gz(IO_filters)
      call write_3d_filter_weights_coef_gz(IO_filters)
!
      call close_gzfile_f
!
      end subroutine write_sort_filter_coef_file_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_filter_geometry_file_gz(file_name, my_rank)
!
      use m_filter_file_names
      use gz_mesh_data_IO
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
        write(*,*) 'Read gzipped filter file: ', trim(gzip_name)
      else if(my_rank .eq. 0) then
        write(*,*) 'Read gzipped filter files: ', trim(gzip_name)
      end if
!
      call open_rd_gzfile_f(gzip_name)
      call read_filter_geometry_gz
      call close_gzfile_f
!
      end subroutine read_filter_geometry_file_gz
!
!------------------------------------------------------------------
!
      subroutine write_filter_geometry_file_gz(file_name, my_rank)
!
      use m_filter_file_names
      use gz_mesh_data_IO
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
        write(*,*) 'Write gzipped filter file: ', trim(gzip_name)
      else if(my_rank .eq. 0) then
        write(*,*) 'Write gzipped filter files: ', trim(gzip_name)
      end if
!
      call open_wt_gzfile_f(gzip_name)
      call write_filter_geometry_gz
      call close_gzfile_f
!
      end subroutine write_filter_geometry_file_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      end module gz_filter_coefs_file_IO
