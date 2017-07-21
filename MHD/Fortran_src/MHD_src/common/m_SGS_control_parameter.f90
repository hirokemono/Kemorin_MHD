!>@file   m_SGS_control_parameter.f90
!!@brief  module m_SGS_control_parameter
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in 2000
!!@n modified in Feb., 2009
!
!> @brief control flags for MHD dynamo model
!!
!!@verbatim
!!@endverbatim
!
      module m_SGS_control_parameter
!
      use m_precision
      use t_SGS_control_parameter
      use t_sph_filtering
!
      implicit none
!
!
!>      Structure of input parameters for SGS model
      type(SGS_paremeters), save :: SGS_par1
!
!>      Structure of work area for dyanmic SGS model for spectrum dynamo
      type(dynamic_SGS_data_4_sph), save :: dynamic_SPH1

!
      end module m_SGS_control_parameter
