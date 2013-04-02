!
!      module gz_element_file_IO
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine input_element_comm_table_gz(my_rank)
!      subroutine input_element_geometries_gz(my_rank)
!
!      subroutine output_ele_comm_table_gz(my_rank)
!
!      subroutine output_element_file_gz(my_rank)
!      subroutine output_element_sph_file_gz(my_rank)
!      subroutine output_element_cyl_file_gz(my_rank)
!
      module gz_element_file_IO
!
      use m_precision
      use m_machine_parameter
!
      use m_read_mesh_data
      use set_parallel_file_name
      use gz_element_data_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine input_element_comm_table_gz(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: gzip_name
!
!
      call add_gzip_extension(mesh_file_name, gzip_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read gzipped element comm file: ', trim(gzip_name)
      end if
!
      call open_rd_gzfile(gzip_name)
!
      call read_element_comm_table_gz
      call close_gzfile
!
      end subroutine input_element_comm_table_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine input_element_geometries_gz(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: gzip_name
!
!
      call add_gzip_extension(mesh_file_name, gzip_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read gzipped element comm file: ', trim(gzip_name)
      end if
!
      call open_rd_gzfile(gzip_name)
!
      call read_element_comm_table_gz
      call read_element_geometries_gz
      call close_gzfile
!
      end subroutine input_element_geometries_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine output_ele_comm_table_gz(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: gzip_name
!
!
      call add_gzip_extension(mesh_file_name, gzip_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read gzipped element comm file: ', trim(gzip_name)
      end if
!
      call open_wt_gzfile(gzip_name)
!
      call write_element_comm_table_gz
      call close_gzfile
!
      end subroutine output_ele_comm_table_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine output_element_file_gz(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: gzip_name
!
!
      call add_gzip_extension(mesh_file_name, gzip_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Read gzipped element comm file: ', trim(gzip_name)
      end if
!
      call open_wt_gzfile(gzip_name)
!
      call write_element_comm_table_gz
      call write_element_geometry_gz
      call close_gzfile
!
      end subroutine output_element_file_gz
!
!------------------------------------------------------------------
!
      subroutine output_element_sph_file_gz(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: gzip_name
!
!
      call add_gzip_extension(mesh_file_name, gzip_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Write gzipped element comm file: ', trim(gzip_name)
      end if
!
      call open_wt_gzfile(gzip_name)
!
      call write_element_comm_table_gz
      call write_element_geometry_sph_gz
      call close_gzfile
!
      end subroutine output_element_sph_file_gz
!
!------------------------------------------------------------------
!
      subroutine output_element_cyl_file_gz(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      character(len=kchara) :: gzip_name
!
!
      call add_gzip_extension(mesh_file_name, gzip_name)
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) then
        write(*,*) 'Write gzipped element comm file: ', trim(gzip_name)
      end if
!
      call open_wt_gzfile(gzip_name)
!
      call write_element_comm_table_gz
      call write_element_geometry_cyl_gz
!
      call close_gzfile
!
      end subroutine output_element_cyl_file_gz
!
!------------------------------------------------------------------
!
      end module gz_element_file_IO
