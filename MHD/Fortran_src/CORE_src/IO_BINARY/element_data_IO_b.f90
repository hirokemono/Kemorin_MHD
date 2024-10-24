!>@file  element_data_IO_b.f90
!!      module element_data_IO_b
!!
!!@author  H. Matsui
!!@date Programmed in Oct., 2006
!
!>@brief Data IO routines for element data
!!
!!@verbatim
!!      subroutine read_element_comm_table_b(id_rank, bbuf, comm_IO)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!        type(communication_table), intent(inout) :: comm_IO
!!      subroutine write_element_comm_table_b(id_rank, comm_IO, bbuf)
!!        type(communication_table), intent(in) :: comm_IO
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!
!!      subroutine read_element_geometry_b(bbuf, nod_IO, sfed_IO)
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!        type(node_data), intent(inout) :: nod_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!      subroutine write_element_geometry_b(nod_IO, sfed_IO, bbuf)
!!        type(node_data), intent(in) :: nod_IO
!!        type(surf_edge_IO_data), intent(in) :: sfed_IO
!!        type(binary_IO_buffer), intent(inout) :: bbuf
!!@endverbatim
!
      module element_data_IO_b
!
      use m_precision
!
      use t_geometry_data
      use t_read_mesh_data
      use t_comm_table
      use t_surf_edge_IO
      use t_binary_IO_buffer
      use binary_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine read_element_comm_table_b(id_rank, bbuf, comm_IO)
!
      use m_fem_mesh_labels
      use domain_data_IO_b
!
      integer, intent(in) :: id_rank
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      type(communication_table), intent(inout) :: comm_IO
!
!
      call read_domain_info_b(id_rank, bbuf, comm_IO)
      if(bbuf%ierr_bin .gt. 0) return
!
! ----  import & export 
!
      call read_import_data_b(bbuf, comm_IO)
      if(bbuf%ierr_bin .gt. 0) return
      call read_export_data_b(bbuf, comm_IO)
!
      end subroutine read_element_comm_table_b
!
!------------------------------------------------------------------
!
      subroutine write_element_comm_table_b(id_rank, comm_IO, bbuf)
!
      use m_fem_mesh_labels
      use domain_data_IO_b
!
      integer, intent(in) :: id_rank
      type(communication_table), intent(in) :: comm_IO
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call write_domain_info_b(id_rank, comm_IO, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
!
! ----  import & export 
!
      call write_import_data_b(comm_IO, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
      call write_export_data_b(comm_IO, bbuf)
!
      end subroutine write_element_comm_table_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_element_geometry_b(bbuf, nod_IO, sfed_IO)
!
      use node_geometry_IO_b
!
      type(binary_IO_buffer), intent(inout) :: bbuf
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call read_number_of_node_b(bbuf, nod_IO)
      if(bbuf%ierr_bin .gt. 0) return
!
      call read_geometry_info_b(bbuf, nod_IO)
      if(bbuf%ierr_bin .ne. 0) return
!
      call read_scalar_in_element_b(bbuf, nod_IO, sfed_IO)
!
      end subroutine read_element_geometry_b
!
!------------------------------------------------------------------
!
      subroutine write_element_geometry_b(nod_IO, sfed_IO, bbuf)
!
      use node_geometry_IO_b
!
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
      type(binary_IO_buffer), intent(inout) :: bbuf
!
!
      call write_geometry_info_b(nod_IO, bbuf)
      if(bbuf%ierr_bin .ne. 0) return
      call write_scalar_in_element_b( nod_IO, sfed_IO, bbuf)
!
      end subroutine write_element_geometry_b
!
!------------------------------------------------------------------
!
      end module element_data_IO_b
