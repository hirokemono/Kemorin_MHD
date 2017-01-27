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
      module   m_SGS_control_parameter
!
      use m_precision
      use t_SGS_control_parameter
!
      implicit  none
!
!
!      type(SGS_model_control_params), save :: SGS_param1
!
      type(commutation_control_params), save :: cmt_param1
!cmt_param1%iflag_c_hf
!
      type(SGS_filtering_params), save :: filter_param1
!
!
      end module m_SGS_control_parameter
