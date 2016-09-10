!>@file  element_data_IO.f90
!!      module element_data_IO
!!
!!@author  H. Matsui
!!@date Programmed in Oct., 2006
!
!>@brief Data IO routines for element data
!!
!!@verbatim
!!      subroutine write_element_comm_table                             &
!!     &         (id_file, my_rank_IO, comm_IO)
!!        type(communication_table), intent(inout) :: comm_IO
!!
!!      subroutine write_element_geometry(id_file, nod_IO, sfed_IO)
!!      subroutine write_element_geometry_sph(id_file, nod_IO, sfed_IO)
!!      subroutine write_element_geometry_cyl(id_file, nod_IO, sfed_IO)
!!        type(node_data), intent(inout) :: nod_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!@endverbatim
!
      module element_data_IO
!
      use m_precision
!
      use t_geometry_data
      use t_read_mesh_data
      use t_comm_table
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine write_element_comm_table                               &
     &         (id_file, my_rank_IO, comm_IO)
!
      use m_fem_mesh_labels
      use domain_data_IO
!
      integer (kind = kint), intent(in) :: id_file
      integer (kind = kint), intent(in) :: my_rank_IO
      type(communication_table), intent(inout) :: comm_IO
!
!
      write(id_file,'(a)') '!' 
      write(id_file,'(a)') '!  element position '
      write(id_file,'(a)') '!  and communication table '
      write(id_file,'(a)') '!' 
      write(id_file,'(a)', advance='NO') hd_fem_para()
!
      call write_domain_info(id_file, my_rank_IO, comm_IO)
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! 2.import / export information '
      write(id_file,'(a)') '! 2.1 element ID for import '
      write(id_file,'(a)') '!'
!
      call write_import_data(id_file, comm_IO)
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! 2.2 element ID for export '
      write(id_file,'(a)') '! '
!
      call write_export_data(id_file, comm_IO)
!
      end subroutine write_element_comm_table
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_element_geometry(id_file, nod_IO, sfed_IO)
!
      use node_geometry_IO
!
      integer (kind = kint), intent(in) :: id_file
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! 3.element information'
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! 3.1 center of element (position) '
      write(id_file,'(a)') '!'
!
      call write_geometry_info(id_file, nod_IO)
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! 3.2 Volume of element '
      write(id_file,'(a)') '!'
!
      call write_scalar_in_element(id_file, nod_IO, sfed_IO)
!
      end subroutine write_element_geometry
!
!------------------------------------------------------------------
!
      subroutine write_element_geometry_sph(id_file, nod_IO, sfed_IO)
!
      use node_geometry_IO
!
      integer (kind = kint), intent(in) :: id_file
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! 3.element information'
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! 3.1 center of element (r,theta,phi)'
      write(id_file,'(a)') '!'
!
      call write_geometry_info(id_file, nod_IO)
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! 3.2 Volume of element '
      write(id_file,'(a)') '!'
!
      call write_scalar_in_element(id_file, nod_IO, sfed_IO)
!
      end subroutine write_element_geometry_sph
!
!------------------------------------------------------------------
!
      subroutine write_element_geometry_cyl(id_file, nod_IO, sfed_IO)
!
      use node_geometry_IO
!
      integer (kind = kint), intent(in) :: id_file
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! 3.element information'
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! 3.1 center of element (r,theta,phi)'
      write(id_file,'(a)') '!'
!
      call write_geometry_info(id_file, nod_IO)
!
      write(id_file,'(a)') '!'
      write(id_file,'(a)') '! 3.2 Volume of element '
      write(id_file,'(a)') '!'
!
      call write_scalar_in_element(id_file, nod_IO, sfed_IO)
!
      end subroutine write_element_geometry_cyl
!
!------------------------------------------------------------------
!
      end module element_data_IO
