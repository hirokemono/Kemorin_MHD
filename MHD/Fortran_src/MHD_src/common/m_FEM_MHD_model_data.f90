!>@file   m_FEM_MHD_model_data.f90
!!@brief  module m_FEM_MHD_model_data
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in 2000
!!@n modified in Feb., 2009
!
!> @brief control flags for MHD dynamo model
!!
!
      module m_FEM_MHD_model_data
!
      use m_precision
      use t_FEM_MHD_model_data
      use t_FEM_mesh_field_data
      use t_FEM_MHD_mean_square
!
      implicit  none
!
!
!>      Parameters for FEM dynamo
      type(FEM_MHD_model_data), save :: FEM_model1
!
!>      Structure of FEM mesh and field structures
      type(FEM_mesh_field_data), save :: FEM_MHD1
!
!>      Structure for mean square values
      type(FEM_MHD_mean_square), save :: fem_sq1
!
      end module m_FEM_MHD_model_data
