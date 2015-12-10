!>@file   m_ele_sf_eg_comm_tables.f90
!!@brief  module m_ele_sf_eg_comm_tables
!!
!!@author H. Matsui
!!@date Programmed in June, 2015
!
!> @brief Belonged element list for each node
!!
!!@verbatim
!!      subroutine dealloc_ele_sf_eg_comm_tables
!!      subroutine const_element_comm_tables_1st
!!@endverbatim
!
      module m_ele_sf_eg_comm_tables
!
      use m_precision
      use t_comm_table
!
      use calypso_mpi
      use m_machine_parameter
!
      implicit none
!
      type(communication_table), save :: ele_comm
      type(communication_table), save :: surf_comm
      type(communication_table), save :: edge_comm
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_element_comm_tables_1st
!
      use m_nod_comm_table
      use m_geometry_data
      use const_element_comm_tables
!
!
      call const_element_comm_tbls(node1, ele1, surf1, edge1,           &
     &    nod_comm, ele_comm, surf_comm, edge_comm)
!
      end subroutine const_element_comm_tables_1st
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_ele_sf_eg_comm_tables
!
      use m_geometry_data
!
!
      call dealloc_numnod_stack(node1)
      call dealloc_numele_stack(ele1)
      call dealloc_numsurf_stack(surf1)
      call dealloc_numedge_stack(edge1)
!
      call deallocate_type_comm_tbl(ele_comm)
      call deallocate_type_comm_tbl(surf_comm)
      call deallocate_type_comm_tbl(edge_comm)
!
      end subroutine dealloc_ele_sf_eg_comm_tables
!
!-----------------------------------------------------------------------
!
      end module m_ele_sf_eg_comm_tables
