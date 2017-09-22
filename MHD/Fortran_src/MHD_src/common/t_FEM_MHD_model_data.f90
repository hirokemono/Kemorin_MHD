!>@file   t_FEM_MHD_model_data.f90
!!@brief  module t_FEM_MHD_model_data
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in 2000
!!@n modified in Feb., 2009
!
!> @brief control flags for MHD dynamo model
!!
!
      module t_FEM_MHD_model_data
!
      use m_precision
      use t_FEM_control_parameter
      use t_FEM_MHD_boundary_data
!
      implicit  none
!
!
!  Parameters for FEM dynamo model
!
      type FEM_MHD_model_data
        type(FEM_MHD_paremeters) :: FEM_prm
!
        type(FEM_MHD_BC_data) :: FEM_MHD_BCs
      end type FEM_MHD_model_data
!
      end module t_FEM_MHD_model_data
