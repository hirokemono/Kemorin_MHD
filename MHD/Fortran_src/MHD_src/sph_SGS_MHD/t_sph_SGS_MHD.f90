!>@file   t_sph_SGS_MHD.f90
!!@brief  module t_sph_SGS_MHD
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Structures for spherical SGS MHD dynamo model
      module t_sph_SGS_MHD
!
      use m_precision
!
      use t_SPH_MHD_model_data
      use t_MHD_IO_data
      use t_MHD_step_parameter
      use t_MHD_file_parameter
      use t_SPH_mesh_field_data
      use t_SPH_SGS_structure
      use t_SPH_MHD_zonal_mean_viz
      use t_FEM_mesh_field_data
      use t_VIZ_mesh_field
      use t_visualizer
      use t_sph_trans_arrays_MHD
      use t_work_SPH_MHD
      use t_mesh_SR
!
      implicit none
!
!>      Structure of the all data of program
      type sph_SGS_MHD
!>        Parameters for spectr dynamo model
        type(SPH_MHD_model_data) :: SPH_model
!
!>        Structure of time and step informations
        type(MHD_step_param) :: MHD_step
!>        Structure of spectr grid and data
        type(SPH_mesh_field_data) :: SPH_MHD
!>        Structures of SGS model in Spherical shell dynamo
        type(SPH_SGS_structure) :: SPH_SGS
!
!
!>        Structures of work area for spherical shell dynamo
        type(work_SPH_MHD) :: SPH_WK
!>        Structure of work area for mesh communications
        type(mesh_SR) :: m_SR
!
!>        Structure of file name and format for MHD
        type(MHD_file_IO_params) :: MHD_files
!>        Structure for data file IO
        type(MHD_IO_data) :: MHD_IO
      end type sph_SGS_MHD
!
!>      Structure for visualization in spherical MHD
      type VIZ_sph_MHD
!>        Structure of FEM mesh and field structures
        type(FEM_mesh_field_data) :: FEM_DAT
!>        Structure of FEM data for visualization
        type(VIZ_mesh_field) :: VIZ_FEM
!>        Structures of visualizations
        type(visualize_modules) :: VIZs
!>        Structures of zonal mean controls
        type(sph_zonal_mean_sectioning) :: zmeans
      end type VIZ_sph_MHD
!
      end module t_sph_SGS_MHD
