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
!
      implicit  none
!
!
      type(SGS_paremeters), save :: SGS_par1
!SGS_par1%model_p
!SGS_par1%commute_p
!SGS_par1%filter_p
!
      end module m_SGS_control_parameter
