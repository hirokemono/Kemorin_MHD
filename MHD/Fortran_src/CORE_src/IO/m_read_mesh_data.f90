!>@file   m_read_mesh_data.f90
!!@brief  module m_read_mesh_data
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui
!
!>@brief Data arry for mesh_data_IO
!!
!!@verbatim
!!@endverbatim
!
      module m_read_mesh_data
!
      use m_precision
      use t_file_IO_parameter
!
      implicit  none
!
!>      Structure for field data IO paramters
      type(field_IO_params), save ::  mesh1_file
!
!
      end module m_read_mesh_data
