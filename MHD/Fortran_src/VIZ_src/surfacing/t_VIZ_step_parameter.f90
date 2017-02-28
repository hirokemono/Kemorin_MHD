!>@file   t_VIZ_step_parameter.f90
!!@brief  module t_VIZ_step_parameter
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in July., 2001
!!@n    Modified by H. Matsui in 2003
!
!> @brief Parameteres for time steppings
!
      module  t_VIZ_step_parameter
!
!
      use m_precision
      use t_IO_step_parameter
!
      implicit  none
!
      type VIZ_step_params
!>        time step paremters for sectioning
        type(IO_step_param) :: PSF_t
!>        time step paremters for isosurface
        type(IO_step_param) :: ISO_t
!>        time step paremters for volume rendering
        type(IO_step_param) :: PVR_t
!>        time step paremters for field lines
        type(IO_step_param) :: FLINE_t
      end type VIZ_step_params
!
      end module  t_VIZ_step_parameter
