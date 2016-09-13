!>@file   t_filter_file_data.f90
!!@brief  module t_filter_file_data
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2006
!
!>@brief Structure for filtering informations
!!
!!@verbatim
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
!
      end module t_filter_file_data
