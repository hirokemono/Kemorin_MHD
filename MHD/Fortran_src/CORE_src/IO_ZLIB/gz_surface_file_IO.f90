!
!      module gz_surface_file_IO
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine output_surface_connect_gz
!
!      subroutine output_surface_file_gz
!      subroutine output_surface_sph_file_gz
!      subroutine output_surface_cyl_file_gz
!
!      subroutine input_surface_connect_gz
!      subroutine input_surface_file_gz
!
!
      module gz_surface_file_IO
!
      use m_precision
!
      use m_read_mesh_data
      use surface_file_IO
      use set_parallel_file_name
      use gz_surface_data_IO
!
      implicit none
!
      character(len=kchara), private :: gzip_name
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine output_surface_connect_gz
!
!
      call add_gzip_extension(mesh_file_name, gzip_name)
      call open_wt_gzfile(gzip_name)
!
      call write_surface_connection_gz
      call close_gzfile
!
      end subroutine output_surface_connect_gz
!
!------------------------------------------------------------------
!
      subroutine output_surface_file_gz
!
!
      call add_gzip_extension(mesh_file_name, gzip_name)
      call open_wt_gzfile(gzip_name)
!
      call write_surface_connection_gz
      call write_surface_geometry_gz
      call close_gzfile
!
      end subroutine output_surface_file_gz
!
!------------------------------------------------------------------
!
      subroutine output_surface_sph_file_gz
!
!
      call add_gzip_extension(mesh_file_name, gzip_name)
      call open_wt_gzfile(gzip_name)
!
      call write_surface_connection_gz
      call write_surface_geometry_sph_gz
      call close_gzfile
!
      end subroutine output_surface_sph_file_gz
!
!------------------------------------------------------------------
!
      subroutine output_surface_cyl_file_gz
!
!
      call add_gzip_extension(mesh_file_name, gzip_name)
      call open_wt_gzfile(gzip_name)
!
      call write_surface_connection_gz
      call write_surface_geometry_cyl_gz
      call close_gzfile
!
      end subroutine output_surface_cyl_file_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine input_surface_connect_gz
!
!
      call add_gzip_extension(mesh_file_name, gzip_name)
      call open_rd_gzfile(gzip_name)
!
      call read_surface_connection_gz
      call close_gzfile
!
      end subroutine input_surface_connect_gz
!
!------------------------------------------------------------------
!
      subroutine input_surface_file_gz
!
!
      call add_gzip_extension(mesh_file_name, gzip_name)
      call open_rd_gzfile(gzip_name)
!
      call read_surface_connection_gz
      call read_surface_geometry_gz
      call close_gzfile
!
      end subroutine input_surface_file_gz
!
!------------------------------------------------------------------
!
      end module gz_surface_file_IO
