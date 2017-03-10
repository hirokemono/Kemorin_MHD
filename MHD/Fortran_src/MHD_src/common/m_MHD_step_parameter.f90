!>@file   m_MHD_step_parameter.f90
!!@brief  module m_MHD_step_parameter
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in July., 2001
!!@n    Modified by H. Matsui in 2003
!
!> @brief Parameteres for time steppings
!
      module m_MHD_step_parameter
!
!
      use m_precision
      use t_MHD_step_parameter
!
      implicit  none
!
!
      type(MHD_IO_step_param), save :: MHD_step1
!
      end module m_MHD_step_parameter
