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
      use t_control_parameter
      use t_FEM_control_parameter
      use t_geometry_data_MHD
      use t_FEM_MHD_boundary_data
      use t_bc_data_list
!
      implicit  none
!
!
!  Parameters for FEM dynamo model
!
      type FEM_MHD_model_data
        type(MHD_evolution_param) :: MHD_prop
!
        type(FEM_MHD_paremeters) :: FEM_prm
!
!>        Structure of boundary condition data
        type(FEM_MHD_BC_data) :: FEM_MHD_BCs
!>        Structure for boundary condition lists for MHD
        type(MHD_BC_lists) :: MHD_BC
!
!>         Strucutre for MHD mesh data
        type(mesh_data_MHD) :: MHD_mesh
      end type FEM_MHD_model_data
!
      end module t_FEM_MHD_model_data
