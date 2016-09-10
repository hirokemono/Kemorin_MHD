!>@file   m_comm_data_IO.f90
!!@brief  module m_comm_data_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Array for communication table IO
!!
      module m_comm_data_IO
!
      use m_precision
      use t_comm_table
!
      implicit none
!
!> data structure for communication table IO
      type(communication_table), save :: comm_IO
!
      integer(kind = kint) :: my_rank_IO
!
      end module m_comm_data_IO
