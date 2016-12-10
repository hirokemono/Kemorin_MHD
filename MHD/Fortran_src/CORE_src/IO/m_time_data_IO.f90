!>@file   m_time_data_IO.f90
!!@brief  module m_time_data_IO
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief  time and time step data for data IO
!!
      module m_time_data_IO
!
      use m_precision
      use m_machine_parameter
      use t_time_data_IO
!
      implicit none
!
!>      Structure for time data
      type(time_params_IO), save :: t1_IO
!
      end module m_time_data_IO
