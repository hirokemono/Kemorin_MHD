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
!!
!!      subroutine write_surface_geometry
!!      subroutine write_surface_geometry_sph
!!      subroutine write_surface_geometry_cyl
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
      use m_fem_mesh_labels
      use m_read_mesh_data
      use domain_data_IO
      use element_connect_IO
!
!
      write(input_file_code,'(a)') '!' 
      write(input_file_code,'(a)') '!  surface connectivity '
      write(input_file_code,'(a)') '!  and communication table '
      write(input_file_code,'(a)') '!' 
      write(input_file_code,'(a)', advance='NO') hd_fem_para()
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
!
      end module surface_data_IO
