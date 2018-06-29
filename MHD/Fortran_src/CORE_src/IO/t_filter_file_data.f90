!>@file   t_filter_file_data.f90
!!@brief  module t_filter_file_data
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2006
!
!>@brief Structure for filtering informations
!!
!!@verbatim
!!      subroutine dealloc_filter_geometry_data(filter_IO)
!!        type(filter_file_data), intent(inout) :: filter_IO
!!@endverbatim
!
      module t_filter_file_data
!
      use m_precision
      use m_constants
!
      use t_comm_table
      use t_geometry_data
      use t_filter_coefficients
!
      implicit none
!
      type filter_file_data
!> data structure for communication table IO
        type(communication_table) :: nod_comm
!>  structure for node data IO (position)
        type(node_data) :: node
!>  structure for filter data IO
        type (filter_coefficients_type) :: filters
      end type filter_file_data
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine dealloc_filter_geometry_data(filter_IO)
!
      type(filter_file_data), intent(inout) :: filter_IO
!
      call dealloc_comm_table(filter_IO%nod_comm)
      call dealloc_node_geometry_base(filter_IO%node)
!
      end subroutine dealloc_filter_geometry_data
!
!------------------------------------------------------------------
!
      end module t_filter_file_data
