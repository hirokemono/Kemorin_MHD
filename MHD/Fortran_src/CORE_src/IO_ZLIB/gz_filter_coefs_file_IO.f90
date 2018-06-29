!>@file   gz_filter_coefs_file_IO.f90
!!@brief  module gz_filter_coefs_file_IO
!!
!!@author H. Matsui
!!@date Programmed in 2004
!
!> @brief gzipped filter data file IO
!!
!!@verbatim
!!      subroutine read_sort_filter_coef_file_gz                        &
!!     &         (file_name, my_rank_IO, filter_IO, ierr)
!!      subroutine write_sort_filter_coef_file_gz                       &
!!     &         (file_name, my_rank_IO, filter_IO)
!!        type(filter_file_data), intent(inout) :: filter_IO
!!
!!      subroutine read_filter_geometry_file_gz                         &
!!     &         (file_name, my_rank_IO, filter_IO, ierr)
!!      subroutine write_filter_geometry_file_gz                        &
!!     &         (file_name, my_rank_IO, filter_IO)
!!        type(filter_file_data), intent(inout) :: filter_IO
!!@endverbatim
!
      module gz_filter_coefs_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use t_filter_file_data
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
     &         (file_name, my_rank_IO, filter_IO, ierr)
!
      use gz_mesh_data_IO
      use gz_filter_coef_IO
!
      integer(kind = kint), intent(in) :: my_rank_IO
      character(len=kchara), intent(in) :: file_name
!
      type(filter_file_data), intent(inout) :: filter_IO
      integer(kind = kint), intent(inout) :: ierr
!
      character(len=kchara) :: gzip_name
!
!
      call add_gzip_extension(file_name, gzip_name)
!
      if(i_debug .gt. 0) then
        write(*,*) 'Read gzipped filter file: ', trim(gzip_name)
      else if(my_rank_IO .eq. 0) then
        write(*,*) 'Read gzipped filter files: ', trim(gzip_name)
      end if
!
      call open_rd_gzfile_f(gzip_name)
      call gz_read_filter_geometry                                     &
     &   (my_rank_IO, filter_IO%nod_comm, filter_IO%node, ierr)
      call read_3d_filter_stack_gz(filter_IO%filters)
      call read_3d_filter_weights_coef_gz(filter_IO%filters)
      call close_gzfile_f
!
      end subroutine read_sort_filter_coef_file_gz
!
!------------------------------------------------------------------
!
      subroutine write_sort_filter_coef_file_gz                         &
     &         (file_name, my_rank_IO, filter_IO)
!
      use gz_mesh_data_IO
      use gz_filter_coef_IO
!
      integer(kind = kint), intent(in) :: my_rank_IO
      character(len=kchara), intent(in) :: file_name
!
      type(filter_file_data), intent(inout) :: filter_IO
!
      character(len=kchara) :: gzip_name
!
!
      call add_gzip_extension(file_name, gzip_name)
!
      if(i_debug .gt. 0 .or. my_rank_IO .eq. 0) then
        write(*,*) 'Write gzipped filter files: ', trim(gzip_name)
      end if
!
      call open_wt_gzfile_f(gzip_name)
!
      call gz_write_filter_geometry                                     &
     &   (my_rank_IO, filter_IO%nod_comm, filter_IO%node)
      call write_3d_filter_stack_gz(filter_IO%filters)
      call write_3d_filter_weights_coef_gz(filter_IO%filters)
!
      call close_gzfile_f
!
      call dealloc_filter_geometry_data(filter_IO)
!
      end subroutine write_sort_filter_coef_file_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_filter_geometry_file_gz                           &
     &         (file_name, my_rank_IO, filter_IO, ierr)
!
      use gz_mesh_data_IO
!
      integer(kind = kint), intent(in) :: my_rank_IO
      character(len=kchara), intent(in) :: file_name
!
      type(filter_file_data), intent(inout) :: filter_IO
      integer(kind = kint), intent(inout) :: ierr
!
      character(len=kchara) :: gzip_name
!
!
      call add_gzip_extension(file_name, gzip_name)
!
      if(i_debug .gt. 0 .or. my_rank_IO .eq. 0) then
        write(*,*) 'Read gzipped filter files: ', trim(gzip_name)
      end if
!
      call open_rd_gzfile_f(gzip_name)
      call gz_read_filter_geometry                                      &
     &  (my_rank_IO, filter_IO%nod_comm, filter_IO%node, ierr)
      call close_gzfile_f
!
      end subroutine read_filter_geometry_file_gz
!
!------------------------------------------------------------------
!
      subroutine write_filter_geometry_file_gz                          &
     &         (file_name, my_rank_IO, filter_IO)
!
      use gz_mesh_data_IO
!
      integer(kind = kint), intent(in) :: my_rank_IO
      character(len=kchara), intent(in) :: file_name
!
      type(filter_file_data), intent(inout) :: filter_IO
!
      character(len=kchara) :: gzip_name
!
!
      call add_gzip_extension(file_name, gzip_name)
!
      if(my_rank_IO .eq. 0 .or. i_debug .gt. 0) then
        write(*,*) 'Write gzipped filter file: ', trim(gzip_name)
      end if
!
      call open_wt_gzfile_f(gzip_name)
      call gz_write_filter_geometry                                     &
     &   (my_rank_IO, filter_IO%nod_comm, filter_IO%node)
      call close_gzfile_f
!
      call dealloc_filter_geometry_data(filter_IO)
!
      end subroutine write_filter_geometry_file_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      end module gz_filter_coefs_file_IO
