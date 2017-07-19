!>@file   m_control_parameter.f90
!!@brief  module m_control_parameter
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in 2000
!!@n modified in Feb., 2009
!
!> @brief control flags for MHD dynamo model
!!
!!@verbatim
!!      subroutine allocate_force_list
!!      subroutine deallocate_force_list
!!@endverbatim
!
      module   m_control_parameter
!
      use m_precision
      use t_FEM_control_parameter
!
      implicit  none
!
!
!  Parameters for FEM dynamo
!
      type(FEM_MHD_paremeters), save :: FEM_prm1
!
      end module m_control_parameter
