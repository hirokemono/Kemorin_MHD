!>@file  edge_data_IO_b.f90
!!      module edge_data_IO_b
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2006
!
!>@brief data IO orutines for edge
!!
!!@verbatim
!!      subroutine read_edge_connection_b                               &
!!     &         (my_rank_IO, comm_IO, ele_IO, sfed_IO, ierr)
!!      subroutine read_edge_connection_b                              &
!!     &         (my_rank_IO, comm_IO, ele_IO, sfed_IO)
!!        type(communication_table), intent(inout) :: comm_IO
!!        type(node_data), intent(inout) :: nod_IO
!!        type(element_data), intent(inout) :: ele_IO
!!        type(surf_edge_IO_data), intent(inout) :: sfed_IO
!!
!!      subroutine write_edge_connection_b(nod_IO, sfed_IO)
!!      subroutine write_edge_geometry_b(nod_IO, sfed_IO)
!!        type(communication_table), intent(in) :: comm_IO
!!        type(node_data), intent(in) :: nod_IO
!!        type(element_data), intent(in) :: ele_IO
!!        type(surf_edge_IO_data), intent(in) :: sfed_IO
!!@endverbatim
!
!
      module edge_data_IO_b
!
      use m_precision
!
      use t_comm_table
      use t_geometry_data
      use t_read_mesh_data
      use t_surf_edge_IO
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine read_edge_connection_b                                 &
     &         (my_rank_IO, comm_IO, ele_IO, sfed_IO, ierr)
!
      use m_fem_mesh_labels
      use domain_data_IO_b
      use element_connect_IO_b
!
      integer (kind = kint), intent(in) :: my_rank_IO
      type(communication_table), intent(inout) :: comm_IO
      type(element_data), intent(inout) :: ele_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      call read_domain_info_b(my_rank_IO, comm_IO, ierr)
!
      call read_number_of_element_b(ele_IO)
      call read_element_info_b(ele_IO)
      call read_surface_4_element_b(sfed_IO)
      call read_edge_4_element_b(sfed_IO)
!
      call read_import_data_b(comm_IO)
      call read_export_data_b(comm_IO)
!
      end subroutine read_edge_connection_b
!
!------------------------------------------------------------------
!
      subroutine write_edge_connection_b                                &
     &         (my_rank_IO, comm_IO, ele_IO, sfed_IO)
!
      use m_fem_mesh_labels
      use domain_data_IO_b
      use element_connect_IO_b
!
      integer (kind = kint), intent(in) :: my_rank_IO
      type(communication_table), intent(in) :: comm_IO
      type(element_data), intent(in) :: ele_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
!
!
      call write_domain_info_b(my_rank_IO, comm_IO)
!
      call write_element_info_b(ele_IO)
      call write_surface_4_element_b(sfed_IO)
      call write_edge_4_element_b(sfed_IO)
!
      call write_import_data_b(comm_IO)
      call write_export_data_b(comm_IO)
!
      end subroutine write_edge_connection_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine read_edge_geometry_b(nod_IO, sfed_IO)
!
      use node_geometry_IO_b
!
      type(node_data), intent(inout) :: nod_IO
      type(surf_edge_IO_data), intent(inout) :: sfed_IO
!
!
      call read_number_of_node_b(nod_IO)
      call read_geometry_info_b(nod_IO)
      call read_vector_in_element_b(nod_IO, sfed_IO)
      call read_scalar_in_element_b(nod_IO, sfed_IO)
!
      end subroutine read_edge_geometry_b
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_edge_geometry_b(nod_IO, sfed_IO)
!
      use node_geometry_IO_b
!
      type(node_data), intent(in) :: nod_IO
      type(surf_edge_IO_data), intent(in) :: sfed_IO
!
!
      call write_geometry_info_b(nod_IO)
      call write_vector_in_element_b(nod_IO, sfed_IO)
      call write_scalar_in_element_b(nod_IO, sfed_IO)
!
      end subroutine write_edge_geometry_b
!
!------------------------------------------------------------------
!
      end module edge_data_IO_b
