!>@file  edge_data_IO.f90
!!      module edge_data_IO
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2006
!
!>@brief data IO orutines for edge
!!
!!@verbatim
!!      subroutine write_edge_connection
!!
!!      subroutine write_edge_geometry
!!      subroutine write_edge_geometry_sph
!!      subroutine write_edge_geometry_cyl
!!@endverbatim
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
      use m_fem_mesh_labels
      use domain_data_IO
      use element_connect_IO
!
!
      write(input_file_code,'(a)') '!' 
      write(input_file_code,'(a)') '!  edge connectivity '
      write(input_file_code,'(a)') '!  and communication table '
      write(input_file_code,'(a)') '!' 
      write(input_file_code,'(a)', advance='NO') hd_fem_para()
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
!
      end module edge_data_IO
