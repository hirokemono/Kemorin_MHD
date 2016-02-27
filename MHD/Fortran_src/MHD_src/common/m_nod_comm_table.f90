!>@file   m_nod_comm_table.f90
!!@brief  module m_nod_comm_table
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in 2000
!!@n     Modified in 2006 
!!@n     Modified in June, 2015
!
!> @brief Communication table for node
!!
!!@verbatim
!!      subroutine dealloc_ele_sf_eg_comm_tables
!!      subroutine const_element_comm_tables_1st
!!@endverbatim
!
      module m_nod_comm_table
!
      use m_precision
      use t_comm_table
!
      implicit  none
!
!> data structure for node communication table
!      type(communication_table), save :: nod_comm
!
!> data structure for element communication table
      type(communication_table), save :: ele_comm
!> data structure for surface communication table
      type(communication_table), save :: surf_comm
!> data structure for edge communication table
      type(communication_table), save :: edge_comm
!
!-----------------------------------------------------------------------
!
      contains
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
      end module m_nod_comm_table
