!>@file   filter_geometry_IO.f90
!!@brief  module filter_geometry_IO
!!
!!@author H. Matsui
!!@date Programmed in 2004
!
!> @brief Geometry data IO for filter data
!!
!!@verbatim
!!      subroutine read_filter_geometry(id_file)
!!      subroutine read_filter_geometry_b(id_file)
!!
!!      subroutine write_filter_geometry(id_file)
!!      subroutine write_filter_geometry_b(id_file)
!!@endverbatim
!
      module filter_geometry_IO
!
      use m_precision
!
      use domain_data_IO
      use comm_stack_item_IO
      use node_geometry_IO
!
      implicit  none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
       subroutine read_filter_geometry(id_file)
!
      integer(kind = kint), intent(in) :: id_file
!
!        write(*,*) 'read_domain_info'
        call read_domain_info(id_file)
!        write(*,*) 'read_number_of_node'
        call read_number_of_node(id_file)
!        write(*,*) 'read_geometry_info'
        call read_geometry_info(id_file)
!
! ----  import & export 
!
!        write(*,*) 'read_import_data'
       call read_import_data(id_file)
!        write(*,*) 'read_export_data'
       call read_export_data(id_file)
!
       end subroutine read_filter_geometry
!
!------------------------------------------------------------------
!
       subroutine read_filter_geometry_b(id_file)
!
      integer(kind = kint), intent(in) :: id_file
!
!
        call read_domain_info_b(id_file)
        call read_number_of_node_b(id_file)
        call read_geometry_info_b(id_file)
!
! ----  import & export 
!
        call read_import_data_b(id_file)
        call read_export_data_b(id_file)
!
       end subroutine read_filter_geometry_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_filter_geometry(id_file)
!
      integer(kind = kint), intent(in) :: id_file
!
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! 1.parallel information'
      write(id_file,'(a)') '!'
!
      call write_domain_info(id_file)
!
!
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)')                                     &
     &      '! 2.mesh information (nodes and elements in partition)'
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! 2.1 node (position) '
      write(id_file,'(a)') '!'
!
      call write_geometry_info(id_file)
!
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! 3.import / export information '
      write(id_file,'(a)') '! 3.1 import '
      write(id_file,'(a)') '!'
!
      call write_import_data(id_file)
!
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! 3.2 export '
      write(id_file,'(a)') '!'
!
      call write_export_data(id_file)
!
      end subroutine write_filter_geometry
!
!------------------------------------------------------------------
!
      subroutine write_filter_geometry_b(id_file)
!
      integer(kind = kint), intent(in) :: id_file
!
!
      call write_domain_info_b(id_file)
!
      call write_geometry_info_b(id_file)
!
      call write_import_data_b(id_file)
      call write_export_data_b(id_file)
!
      end subroutine write_filter_geometry_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      end module filter_geometry_IO
