!
!      module gz_edge_file_IO
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine input_edge_connect_gz
!      subroutine input_edge_geometries_gz
!
!      subroutine output_edge_connect_gz
!      subroutine output_edge_geometries_gz
!
!      subroutine output_edge_geometries_sph_gz
!      subroutine output_edge_geometries_cyl_gz
!
      module gz_edge_file_IO
!
      use m_precision
!
      use m_read_mesh_data
      use edge_file_IO
      use set_parallel_file_name
      use gz_edge_data_IO
!
      implicit none
!
      character(len=kchara), private :: gzip_name
!
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine input_edge_connect_gz
!
!
      call add_gzip_extension(mesh_file_name, gzip_name)
      call open_rd_gzfile(gzip_name)
!
      call read_edge_connection_gz
      call close_gzfile
!
      end subroutine input_edge_connect_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine input_edge_geometries_gz
!
!
      call add_gzip_extension(mesh_file_name, gzip_name)
      call open_rd_gzfile(gzip_name)
!
      call read_edge_connection_gz
      call read_edge_geometry_gz
      call close_gzfile
!
      end subroutine input_edge_geometries_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine output_edge_connect_gz
!
!
      call add_gzip_extension(mesh_file_name, gzip_name)
      call open_wt_gzfile(gzip_name)
!
      call write_edge_connection_gz
      call close_gzfile
!
      end subroutine output_edge_connect_gz
!
!------------------------------------------------------------------
!
      subroutine output_edge_geometries_gz
!
!
      call add_gzip_extension(mesh_file_name, gzip_name)
      call open_wt_gzfile(gzip_name)
!
      call write_edge_connection_gz
      call write_edge_geometry_gz
      call close_gzfile
!
      end subroutine output_edge_geometries_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine output_edge_geometries_sph_gz
!
!
      call add_gzip_extension(mesh_file_name, gzip_name)
      call open_wt_gzfile(gzip_name)
!
      call write_edge_connection_gz
      call write_edge_geometry_sph_gz
      call close_gzfile
!
      end subroutine output_edge_geometries_sph_gz
!
!------------------------------------------------------------------
!
      subroutine output_edge_geometries_cyl_gz
!
!
      call add_gzip_extension(mesh_file_name, gzip_name)
      call open_wt_gzfile(gzip_name)
!
      call write_edge_connection_gz
      call write_edge_geometry_cyl_gz
      call close_gzfile
!
      end subroutine output_edge_geometries_cyl_gz
!
!------------------------------------------------------------------
!
      end module gz_edge_file_IO
