!>@file   t_FEM_SGS_MHD.f90
!!@brief  module t_FEM_SGS_MHD
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in 2000
!!@n modified in Feb., 2009
!
!> @brief control flags for MHD dynamo model
!!
!
      module t_FEM_SGS_MHD
!
      use m_precision
      use t_FEM_MHD_model_data
      use t_FEM_mesh_field_data
      use t_FEM_MHD_mean_square
      use t_MHD_IO_data
      use t_MHD_step_parameter
      use t_MHD_file_parameter
!
      use t_FEM_MHD_solvers
      use t_FEM_SGS_structure
      use t_work_FEM_SGS_MHD
      use t_FEM_MHD_time_stepping
      use t_VIZ_mesh_field
      use t_visualizer
      use t_fieldline
      use t_control_data_vizs
      use t_control_data_tracers
      use t_mesh_SR
!
      implicit  none
!
!>      Structures for FEM MHD dynamo model
      type FEM_MHD
!>        Parameters for FEM dynamo
        type(FEM_MHD_model_data) :: FEM_model
!
!>        Structure of time informations
        type(MHD_step_param) :: MHD_step
!>        Structure of flexible time informations
        type(FEM_MHD_time_stepping) :: flex_MHD
!>        Structure of FEM mesh and field structures
        type(FEM_mesh_field_data) :: FEM_MHD
!>        Matrix structure for FEM_MHD
        type(FEM_MHD_solvers) :: MHD_CG
!>        Structure of work area for mesh communications
        type(mesh_SR) :: m_SR
!
!>        Structure of file name and format for MHD
        type(MHD_file_IO_params) :: MHD_files
!>        Structure of Data IO
        type(MHD_IO_data) :: MHD_IO
!>        Structure for mean square values
        type(FEM_MHD_mean_square) :: fem_sq
      end type FEM_MHD
!
      type FEM_SGS_MHD
!>        Structure of grouping of elements
        type(FEM_SGS_structure) :: FEM_SGS
!>        Structure of Work area for dynamics model
        type(work_FEM_SGS_MHD) :: SGS_MHD_wk
      end type FEM_SGS_MHD
!
      type FEM_SGS_vizs
!>        Structures of visualization and repartitioning constrol
        type(visualization_controls) :: vizs_ctl
!>        Structure of control data for tracers
        type(tracers_control) :: tracer_ctls
!
!>        Structure of data for visualization
        type(VIZ_mesh_field) :: VIZ_DAT
!>        Structures of visualization
        type(visualize_modules) :: VIZs
!>        Structures of visualization
        type(fieldline_module) ::  tracers
      end type FEM_SGS_vizs
!
      end module t_FEM_SGS_MHD
