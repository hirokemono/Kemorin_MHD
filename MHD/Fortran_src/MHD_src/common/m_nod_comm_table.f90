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
!
      end module m_nod_comm_table
