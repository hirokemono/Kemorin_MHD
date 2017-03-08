!>@file   t_MHD_step_parameter.f90
!!@brief  module t_MHD_step_parameter
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in July., 2001
!!@n    Modified by H. Matsui in 2003
!
!> @brief Parameteres for time steppings
!
      module  t_MHD_step_parameter
!
!
      use m_precision
      use t_IO_step_parameter
      use t_VIZ_step_parameter
!
      implicit  none
!
!
!
      type MHD_IO_step_param
        type(IO_step_param) :: rst_step
!
        type(IO_step_param) :: ucd_step
!
        type(IO_step_param) :: rms_step
!
        type(IO_step_param) :: point_step
!
        type(IO_step_param) :: boundary_step
!
        type(VIZ_step_params) :: viz_step
      end type MHD_IO_step_param
!
!      pvr_step1%increment
!
      end module  t_MHD_step_parameter
