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
      use t_FEM_MHD_solvers
      use t_FEM_SGS_structure
      use t_work_FEM_SGS_MHD
      use t_FEM_MHD_time_stepping
      use t_control_data_vizs
      use t_visualizer
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
!
!>      Matrix structure for FEM_MHD
      type(FEM_MHD_solvers), save :: MHD_CG1
!
!> Structure of grouping of elements
      type(FEM_SGS_structure), save :: FEM_SGS1
!
!> Structure of Work area for dynamics model
      type(work_FEM_SGS_MHD), save :: SGS_MHD_wk1
!
!>        Structures of visualization controls
      type(visualization_controls) :: viz_ctls_F
!
!>        Structures of visualization
      type(visualize_modules), save :: vizs_F
!
      type(FEM_MHD_time_stepping), save :: flex_MHD1
!
      end module m_FEM_MHD_model_data
