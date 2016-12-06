!>@file   m_control_params_2nd_files.f90
!!@brief  module m_control_params_2nd_files
!!
!!@author H. Matsui
!!@date Programmed in Nov., 2009
!
!>@brief Array for the second mesh data
!!
      module m_control_params_2nd_files
!
      use m_precision
      use m_constants
      use t_field_data_IO
!
      implicit  none
!
!>      Structure for field data IO paramters
      type(field_IO_params), save :: rj_org_param
!>      Structure for field data IO paramters
      type(field_IO_params), save :: udt_org_param
!>      Structure for original restart file  paramters
      type(field_IO_params), save :: rst_org_param
!
      end module m_control_params_2nd_files
