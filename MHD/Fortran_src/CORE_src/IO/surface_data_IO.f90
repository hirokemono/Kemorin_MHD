!>@file  surface_data_IO.f90
!!       module surface_data_IO
!!
!!@author H. Matsui
!!@date        programmed by H.Matsui in Aug., 2006
!
!> @brief routines for surface mesh data IO
!!
!!@verbatim
!!      subroutine write_surface_connection
!!      subroutine write_surface_connection_b
!!      subroutine read_surface_connection
!!      subroutine read_surface_connection_b
!!
!!      subroutine write_surface_geometry
!!      subroutine write_surface_geometry_b
!!      subroutine write_surface_geometry_sph
!!      subroutine write_surface_geometry_cyl
!!
!!      subroutine read_surface_geometry
!!      subroutine read_surface_geometry_b
!!@endverbatim
!
      module surface_data_IO
!
      use m_precision
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine write_surface_connection
!
      use m_read_mesh_data
      use domain_data_IO
      use comm_stack_item_IO
      use element_connect_IO
!
!
      write(input_file_code,'(a)') '!' 
      write(input_file_code,'(a)') '!  surface connectivity '
      write(input_file_code,'(a)') '!  and communication table '
      write(input_file_code,'(a)') '!' 
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '! 1.parallel information'
      write(input_file_code,'(a)') '!'
!
      call write_domain_info(input_file_code)
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '!  2  surface connectivity'
      write(input_file_code,'(a)') '!  2.1  surface connectivity '
      write(input_file_code,'(a)') '!      (type and connection) '
      write(input_file_code,'(a)') '!'
!
      call write_element_info(input_file_code)
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '!  2.2 surface id for each element'
      write(input_file_code,'(a)') '!        positive: outward normal'
      write(input_file_code,'(a)') '!        normal: inward normal'
      write(input_file_code,'(a)') '!'
!
      call write_surface_4_element(input_file_code)
!
!
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '! 3.import / export information '
      write(input_file_code,'(a)') '! 3.1 surface ID for import '
      write(input_file_code,'(a)') '!'
!
      call write_import_data(input_file_code)
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '! 3.2 surface ID for export '
      write(input_file_code,'(a)') '!'
!
      call write_export_data(input_file_code)
!
      end subroutine write_surface_connection
!
!------------------------------------------------------------------
!
      subroutine write_surface_connection_b
!
      use domain_data_IO
      use comm_stack_item_IO
      use element_connect_IO
!
!      write(input_file_code,*) '! 1.parallel information'
      call write_domain_info_b(input_file_code)
!
!      write(input_file_code,*) '!  2  surface connectivity'
!      write(input_file_code,*) '!  2.1  surface connectivity '
      call write_element_info_b(input_file_code)
!
!      write(input_file_code,*) '!  2.2 surface id for each element'
      call write_surface_4_element_b(input_file_code)
!
!      write(input_file_code,*) '! 3.import / export information '
!      write(input_file_code,*) '! 3.1 surface ID for import '
      call write_import_data_b(input_file_code)
!      write(input_file_code,*) '! 3.2 surface ID for export '
      call write_export_data_b(input_file_code)
!
      end subroutine write_surface_connection_b
!
!------------------------------------------------------------------
!
      subroutine read_surface_connection
!
      use domain_data_IO
      use comm_stack_item_IO
      use element_connect_IO
!
!
!      write(input_file_code,*) '! 1.parallel information'
      call read_domain_info(input_file_code)
!
!      write(input_file_code,*) '!  2  surface connectivity'
!      write(input_file_code,*) '!  2.1  surface connectivity '
      call read_number_of_element(input_file_code)
      call read_element_info(input_file_code)
!
!      write(input_file_code,*) '!  2.2 surface id for each element'
      call read_surface_4_element(input_file_code)
!
!
!      write(input_file_code,*) '! 3.import / export information '
!      write(input_file_code,*) '! 3.1 surface ID for import '
      call read_import_data(input_file_code)
!      write(input_file_code,*) '! 3.2 surface ID for export '
      call read_export_data(input_file_code)
!
      end subroutine read_surface_connection
!
!------------------------------------------------------------------
!
      subroutine read_surface_connection_b
!
      use domain_data_IO
      use comm_stack_item_IO
      use element_connect_IO
!
!
!      write(input_file_code,*) '! 1.parallel information'
      call read_domain_info_b(input_file_code)
!
!      write(input_file_code,*) '!  2  surface connectivity'
!      write(input_file_code,*) '!  2.1  surface connectivity '
      call read_number_of_element_b(input_file_code)
      call read_element_info_b(input_file_code)
!
!      write(input_file_code,*) '!  2.2 surface id for each element'
      call read_surface_4_element_b(input_file_code)
!
!      write(input_file_code,*) '! 3.import / export information '
!      write(input_file_code,*) '! 3.1 surface ID for import '
      call read_import_data_b(input_file_code)
!      write(input_file_code,*) '! 3.2 surface ID for export '
      call read_export_data_b(input_file_code)
!
      end subroutine read_surface_connection_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_surface_geometry
!
      use node_geometry_IO
!
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '! 4.  geometry of surface'
      write(input_file_code,'(a)') '! 4.1 center of surface'
      write(input_file_code,'(a)') '!'
!
      call write_geometry_info(input_file_code)
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '!  4.2 normal vector of surface'
      write(input_file_code,'(a)') '!'
!
      call write_vector_in_element(input_file_code)
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '! 4.3 area of surface'
      write(input_file_code,'(a)') '!'
!
      call write_scalar_in_element(input_file_code)
!
      end subroutine write_surface_geometry
!
!------------------------------------------------------------------
!
      subroutine write_surface_geometry_b
!
      use node_geometry_IO
!
!
!      write(input_file_code,*) '! 4.  geometry of surface'
!      write(input_file_code,*) '! 4.1 center of surface'
      call write_geometry_info_b(input_file_code)
!
!      write(input_file_code,*) '!  4.2 normal vector of surface'
      call write_vector_in_element_b(input_file_code)
!      write(input_file_code,*) '! 4.3 area of surface'
      call write_scalar_in_element_b(input_file_code)
!
      end subroutine write_surface_geometry_b
!
!------------------------------------------------------------------
!
      subroutine write_surface_geometry_sph
!
      use node_geometry_IO
!
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '! 4   geometry of surface'
      write(input_file_code,'(a)') '! 4.1 center of surface'
      write(input_file_code,'(a)') '!  (spherical coordinate) '
      write(input_file_code,'(a)') '!'
!
      call write_geometry_info(input_file_code)
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '!  4.2 normal vector of surface'
      write(input_file_code,'(a)') '!  (spherical coordinate) '
      write(input_file_code,'(a)') '!'
!
      call write_vector_in_element(input_file_code)
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '!  4.3 area of surface'
      write(input_file_code,'(a)') '!'
!
      call write_scalar_in_element(input_file_code)
!
      end subroutine write_surface_geometry_sph
!
!------------------------------------------------------------------
!
      subroutine write_surface_geometry_cyl
!
      use m_read_mesh_data
      use node_geometry_IO
!
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '! 4   geometry of surface'
      write(input_file_code,'(a)') '! 4.1 center of surface'
      write(input_file_code,'(a)') '!  (cylindrical coordinate) '
      write(input_file_code,'(a)') '!'
!
      call write_geometry_info(input_file_code)
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '! 4.2 normal vector of surface'
      write(input_file_code,'(a)') '!  (cylindrical coordinate) '
      write(input_file_code,'(a)') '!'
!
      call write_vector_in_element(input_file_code)
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '! 4.3 area of surface'
      write(input_file_code,'(a)') '!'
!
      call write_scalar_in_element(input_file_code)
!
      end subroutine write_surface_geometry_cyl
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_surface_geometry
!
      use node_geometry_IO
!
!
!      write(input_file_code,*) '! 4   geometry of surface'
!      write(input_file_code,*) '! 4.1 center of surface'
      call read_number_of_node(input_file_code)
      call read_geometry_info(input_file_code)
!
!      write(input_file_code,*) '! 4.2 normal vector of surface'
      call read_vector_in_element(input_file_code)
!      write(input_file_code,*) '! 4.3 area of surface'
      call read_scalar_in_element(input_file_code)
!
      end subroutine read_surface_geometry
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_surface_geometry_b
!
      use node_geometry_IO
!
!
!      write(input_file_code,*) '! 4   geometry of surface'
!      write(input_file_code,*) '! 4.1 center of surface'
      call read_number_of_node_b(input_file_code)
      call read_geometry_info_b(input_file_code)
!
!      write(input_file_code,*) '! 4.2 normal vector of surface'
      call read_vector_in_element_b(input_file_code)
!      write(input_file_code,*) '! 4.3 area of surface'
      call read_scalar_in_element_b(input_file_code)
!
      end subroutine read_surface_geometry_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      end module surface_data_IO
