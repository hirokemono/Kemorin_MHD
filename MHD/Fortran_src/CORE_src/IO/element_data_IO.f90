!>@file  element_data_IO.f90
!!      module element_data_IO
!!
!!@author  H. Matsui
!!@date Programmed in Oct., 2006
!
!>@brief Data IO routines for element data
!!
!!@verbatim
!!      subroutine write_element_comm_table
!!
!!      subroutine write_element_geometry
!!      subroutine write_element_geometry_sph
!!      subroutine write_element_geometry_cyl
!!@endverbatim
!
      module element_data_IO
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
      subroutine write_element_comm_table
!
      use m_comm_data_IO
      use m_fem_mesh_labels
      use domain_data_IO
!
!
      write(input_file_code,'(a)') '!' 
      write(input_file_code,'(a)') '!  element position '
      write(input_file_code,'(a)') '!  and communication table '
      write(input_file_code,'(a)') '!' 
      write(input_file_code,'(a)', advance='NO') hd_fem_para()
!
      call write_domain_info(input_file_code, my_rank_IO, comm_IO)
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '! 2.import / export information '
      write(input_file_code,'(a)') '! 2.1 element ID for import '
      write(input_file_code,'(a)') '!'
!
      call write_import_data(input_file_code, comm_IO)
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '! 2.2 element ID for export '
      write(input_file_code,'(a)') '! '
!
      call write_export_data(input_file_code, comm_IO)
!
      end subroutine write_element_comm_table
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_element_geometry
!
      use node_geometry_IO
!
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '! 3.element information'
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)')                                      &
     &     '! 3.1 center of element (position) '
      write(input_file_code,'(a)') '!'
!
      call write_geometry_info(input_file_code)
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '! 3.2 Volume of element '
      write(input_file_code,'(a)') '!'
!
      call write_scalar_in_element(input_file_code)
!
      end subroutine write_element_geometry
!
!------------------------------------------------------------------
!
      subroutine write_element_geometry_sph
!
      use node_geometry_IO
!
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '! 3.element information'
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)')                                      &
     &        '! 3.1 center of element (r,theta,phi)'
      write(input_file_code,'(a)') '!'
!
      call write_geometry_info(input_file_code)
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '! 3.2 Volume of element '
      write(input_file_code,'(a)') '!'
!
      call write_scalar_in_element(input_file_code)
!
      end subroutine write_element_geometry_sph
!
!------------------------------------------------------------------
!
      subroutine write_element_geometry_cyl
!
      use node_geometry_IO
!
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '! 3.element information'
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)')                                      &
     &      '! 3.1 center of element (r,theta,phi)'
      write(input_file_code,'(a)') '!'
!
      call write_geometry_info(input_file_code)
!
      write(input_file_code,'(a)') '!'
      write(input_file_code,'(a)') '! 3.2 Volume of element '
      write(input_file_code,'(a)') '!'
!
      call write_scalar_in_element(input_file_code)
!
      end subroutine write_element_geometry_cyl
!
!------------------------------------------------------------------
!
      end module element_data_IO
