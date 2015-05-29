!
!      module gz_edge_data_IO
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine write_edge_connection_gz
!      subroutine read_edge_connection_gz
!
!      subroutine write_edge_geometry_gz
!      subroutine write_edge_geometry_sph_gz
!      subroutine write_edge_geometry_cyl_gz
!
!      subroutine read_edge_geometry_gz
!
      module gz_edge_data_IO
!
      use m_precision
!
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
      subroutine write_edge_connection_gz
!
      use gz_domain_data_IO
      use gz_element_connect_IO
!
!
      write(textbuf,'(a,a1)') '!' , char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  edge connectivity ', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  and communication table ', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!' , char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '! 1.parallel information', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '! ', char(0)
      call gz_write_textbuf_w_lf
!
      call write_domain_info_gz
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  2  edge connectivity', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  2.1  edge connectivity ', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!      (type and connection) ', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
!
      call write_element_info_gz
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  2.2  edge id for each surface', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
!
      call write_surface_4_element_gz
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  2.3   edge id for each element',      &
     &       char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
!
      call write_edge_4_element_gz
!
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '! 3.import / export information '        &
     &      , char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '! 3.1 edge ID for import ', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
!
      call write_import_data_gz
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '! 3.2 edge ID for export ', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
!
      call write_export_data_gz
!
      end subroutine write_edge_connection_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_edge_connection_gz
!
      use gz_domain_data_IO
      use gz_element_connect_IO
!
!
!      write(input_file_code,*) '! 1.parallel information'
      call read_domain_info_gz
!
!      write(input_file_code,*) '!  2  edge connectivity'
!      write(input_file_code,*) '!  2.1  edge connectivity '
      call read_number_of_element_gz
      call read_element_info_gz
!
!      write(input_file_code,*) '!  2.2  edge id for each surface'
      call read_surface_4_element_gz
!
!      write(input_file_code,*) '!  2.3   edge id for each element'
      call read_edge_4_element_gz
!
!      write(input_file_code,*) '! 3.import / export information '
!      write(input_file_code,*) '! 3.1 edge ID for import '
      call read_import_data_gz
!      write(input_file_code,*) '! 3.2 edge ID for export '
      call read_export_data_gz
!
      end subroutine read_edge_connection_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_edge_geometry_gz
!
      use gz_node_geometry_IO
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '! 4.   geometry of edge', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  4.1. center of edge', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
!
      call write_geometry_info_gz
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  4.2  direction of edge', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
!
      call write_vector_in_element_gz
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  4.3  length of edge', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
!
      call write_scalar_in_element_gz
!
      end subroutine write_edge_geometry_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_edge_geometry_sph_gz
!
      use gz_node_geometry_IO
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '! 4.   geometry of edge', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  4.1. center of edge', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  (spherical coordinate) ', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
!
      call write_geometry_info_gz
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  4.2  direction of edge', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
!
      call write_vector_in_element_gz
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  4.3  length of edge', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
!
      call write_scalar_in_element_gz
!
      end subroutine write_edge_geometry_sph_gz
!
!------------------------------------------------------------------
!
      subroutine write_edge_geometry_cyl_gz
!
      use gz_node_geometry_IO
!
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '! 4.   geometry of edge', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  4.1. center of edge', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  (cylindrical coordinate) ', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
!
      call write_geometry_info_gz
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  4.2  direction of edge', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
!
      call write_vector_in_element_gz
!
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!  4.3  length of edge', char(0)
      call gz_write_textbuf_w_lf
      write(textbuf,'(a,a1)') '!', char(0)
      call gz_write_textbuf_w_lf
!
      call write_scalar_in_element_gz
!
      end subroutine write_edge_geometry_cyl_gz
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_edge_geometry_gz
!
      use gz_node_geometry_IO
!
!
!      write(input_file_code,*) '! 4.   geometry of edge'
!      write(input_file_code,*) '!  4.1. center of edge'
      call read_number_of_node_gz
      call read_geometry_info_gz
!
!      write(input_file_code,*) '!  4.2  direction of edge'
      call read_vector_in_element_gz
!
!      write(input_file_code,*) '!  4.3  length of edge'
      call read_scalar_in_element_gz
!
      end subroutine read_edge_geometry_gz
!
!------------------------------------------------------------------
!
      end module gz_edge_data_IO
