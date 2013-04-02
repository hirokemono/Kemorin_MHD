!edge_data_IO.f90
!      module edge_data_IO
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine write_edge_connection
!      subroutine write_edge_connection_b
!      subroutine read_edge_connection
!      subroutine read_edge_connection_b
!
!      subroutine write_edge_geometry
!      subroutine write_edge_geometry_b
!      subroutine write_edge_geometry_sph
!      subroutine write_edge_geometry_cyl
!
!      subroutine read_edge_geometry
!      subroutine read_edge_geometry_b
!
!
      module edge_data_IO
!
      use m_precision
!
      use m_read_mesh_data
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine write_edge_connection
!
      use domain_data_IO
      use comm_stack_item_IO
      use element_connect_IO
!
!
      write(input_file_code,'(a)') '!' 
      write(input_file_code,'(a)') '!  edge connectivity '
      write(input_file_code,'(a)') '!  and communication table '
      write(input_file_code,'(a)') '!' 
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '! 1.parallel information'
      write(input_file_code,'(a)') '! '
!
      call write_domain_info(input_file_code)
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '!  2  edge connectivity'
      write(input_file_code,'(a)') '!  2.1  edge connectivity '
      write(input_file_code,'(a)') '!      (type and connection) '
      write(input_file_code,'(a)') '!'
!
      call write_element_info(input_file_code)
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '!  2.2  edge id for each surface'
      write(input_file_code,'(a)') '!'
!
      call write_surface_4_element(input_file_code)
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '!  2.3   edge id for each element'
      write(input_file_code,'(a)') '!'
!
      call write_edge_4_element(input_file_code)
!
!
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '! 3.import / export information '
      write(input_file_code,'(a)') '! 3.1 edge ID for import '
      write(input_file_code,'(a)') '!'
!
      call write_import_data(input_file_code)
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '! 3.2 edge ID for export '
      write(input_file_code,'(a)') '!'
!
      call write_export_data(input_file_code)
!
      end subroutine write_edge_connection
!
!------------------------------------------------------------------
!
      subroutine write_edge_connection_b
!
      use domain_data_IO
      use comm_stack_item_IO
      use element_connect_IO
!
!
!      write(input_file_code,*) '! 1.parallel information'
      call write_domain_info_b(input_file_code)
!
!      write(input_file_code,*) '!  2  edge connectivity'
!      write(input_file_code,*) '!  2.1  edge connectivity '
      call write_element_info_b(input_file_code)
!
!      write(input_file_code,*) '!  2.2  edge id for each surface'
      call write_surface_4_element_b(input_file_code)
!
!      write(input_file_code,*) '!  2.3   edge id for each element'
      call write_edge_4_element_b(input_file_code)
!
!      write(input_file_code,*) '! 3.import / export information '
!      write(input_file_code,*) '! 3.1 edge ID for import '
      call write_import_data_b(input_file_code)
!      write(input_file_code,*) '! 3.2 edge ID for export '
      call write_export_data_b(input_file_code)
!
      end subroutine write_edge_connection_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_edge_connection
!
      use domain_data_IO
      use comm_stack_item_IO
      use element_connect_IO
!
!
!      write(input_file_code,*) '! 1.parallel information'
      call read_domain_info(input_file_code)
!
!      write(input_file_code,*) '!  2  edge connectivity'
!      write(input_file_code,*) '!  2.1  edge connectivity '
      call read_number_of_element(input_file_code)
      call read_element_info(input_file_code)
!
!      write(input_file_code,*) '!  2.2  edge id for each surface'
      call read_surface_4_element(input_file_code)
!
!      write(input_file_code,*) '!  2.3   edge id for each element'
      call read_edge_4_element(input_file_code)
!
!      write(input_file_code,*) '! 3.import / export information '
!      write(input_file_code,*) '! 3.1 edge ID for import '
      call read_import_data(input_file_code)
!      write(input_file_code,*) '! 3.2 edge ID for export '
      call read_export_data(input_file_code)
!
      end subroutine read_edge_connection
!
!------------------------------------------------------------------
!
      subroutine read_edge_connection_b
!
      use domain_data_IO
      use comm_stack_item_IO
      use element_connect_IO
!
!
!      write(input_file_code,*) '! 1.parallel information'
      call read_domain_info_b(input_file_code)
!
!      write(input_file_code,*) '!  2  edge connectivity'
!      write(input_file_code,*) '!  2.1  edge connectivity '
      call read_number_of_element_b(input_file_code)
      call read_element_info_b(input_file_code)
!
!      write(input_file_code,*) '!  2.2  edge id for each surface'
      call read_surface_4_element_b(input_file_code)
!
!      write(input_file_code,*) '!  2.3   edge id for each element'
      call read_edge_4_element_b(input_file_code)
!
!
!      write(input_file_code,*) '! 3.import / export information '
!      write(input_file_code,*) '! 3.1 edge ID for import '
      call read_import_data_b(input_file_code)
!      write(input_file_code,*) '! 3.2 edge ID for export '
      call read_export_data_b(input_file_code)
!
      end subroutine read_edge_connection_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_edge_geometry
!
      use node_geometry_IO
!
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '! 4.   geometry of edge'
      write(input_file_code,'(a)') '!  4.1. center of edge'
      write(input_file_code,'(a)') '!'
!
      call write_geometry_info(input_file_code)
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '!  4.2  direction of edge'
      write(input_file_code,'(a)') '!'
!
      call write_vector_in_element(input_file_code)
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '!  4.3  length of edge'
      write(input_file_code,'(a)') '!'
!
      call write_scalar_in_element(input_file_code)
!
      end subroutine write_edge_geometry
!
!------------------------------------------------------------------
!
      subroutine write_edge_geometry_b
!
      use node_geometry_IO
!
!
!      write(input_file_code,*) '! 4.   geometry of edge'
!      write(input_file_code,*) '!  4.1. center of edge'
      call write_geometry_info_b(input_file_code)
!
!      write(input_file_code,*) '!  4.2  direction of edge'
      call write_vector_in_element_b(input_file_code)
!      write(input_file_code,*) '!  4.3  length of edge'
      call write_scalar_in_element_b(input_file_code)
!
      end subroutine write_edge_geometry_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_edge_geometry_sph
!
      use node_geometry_IO
!
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '! 4.   geometry of edge'
      write(input_file_code,'(a)') '!  4.1. center of edge'
      write(input_file_code,'(a)') '!  (spherical coordinate) '
      write(input_file_code,'(a)') '!'
!
      call write_geometry_info(input_file_code)
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '!  4.2  direction of edge'
      write(input_file_code,'(a)') '!'
!
      call write_vector_in_element(input_file_code)
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '!  4.3  length of edge'
      write(input_file_code,'(a)') '!'
!
      call write_scalar_in_element(input_file_code)
!
      end subroutine write_edge_geometry_sph
!
!------------------------------------------------------------------
!
      subroutine write_edge_geometry_cyl
!
      use node_geometry_IO
!
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '! 4.   geometry of edge'
      write(input_file_code,'(a)') '!  4.1. center of edge'
      write(input_file_code,'(a)') '!  (cylindrical coordinate) '
      write(input_file_code,'(a)') '!'
!
      call write_geometry_info(input_file_code)
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '!  4.2  direction of edge'
      write(input_file_code,'(a)') '!'
!
      call write_vector_in_element(input_file_code)
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '!  4.3  length of edge'
      write(input_file_code,'(a)') '!'
!
      call write_scalar_in_element(input_file_code)
!
      end subroutine write_edge_geometry_cyl
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_edge_geometry
!
      use node_geometry_IO
!
!
!      write(input_file_code,*) '! 4.   geometry of edge'
!      write(input_file_code,*) '!  4.1. center of edge'
      call read_number_of_node(input_file_code)
      call read_geometry_info(input_file_code)
!
!      write(input_file_code,*) '!  4.2  direction of edge'
      call read_vector_in_element(input_file_code)
!
!      write(input_file_code,*) '!  4.3  length of edge'
      call read_scalar_in_element(input_file_code)
!
      end subroutine read_edge_geometry
!
!------------------------------------------------------------------
!
      subroutine read_edge_geometry_b
!
      use node_geometry_IO
!
!
!      write(input_file_code,*) '! 4.   geometry of edge'
!      write(input_file_code,*) '!  4.1. center of edge'
      call read_number_of_node_b(input_file_code)
      call read_geometry_info_b(input_file_code)
!
!      write(input_file_code,*) '!  4.2  direction of edge'
      call read_vector_in_element_b(input_file_code)
!      write(input_file_code,*) '!  4.3  length of edge'
      call read_scalar_in_element_b(input_file_code)
!
      end subroutine read_edge_geometry_b
!
!------------------------------------------------------------------
!
      end module edge_data_IO
