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
      use t_SPH_SGS_structure
      use t_SPH_MHD_zonal_mean_viz
      use t_FEM_mesh_field_data
      use t_VIZ_mesh_field
      use t_visualizer
      use t_particle_trace
!
      implicit none
!
!>      Structure for visualization in spherical MHD
      type sph_SGS_MHD
!>        Structures of SGS model in Spherical shell dynamo
        type(SPH_SGS_structure) :: SPH_SGS
!
!>        Structure of FEM mesh and field structures
        type(FEM_mesh_field_data) :: FEM_DAT
!>        Structure of FEM data for visualization
        type(VIZ_mesh_field) :: VIZ_FEM
!>        Structures of visualizations
        type(visualize_modules) :: VIZs
!>        Structures of zonal mean controls
        type(sph_zonal_mean_viz) :: zmeans
!>        Structures of visualization
        type(tracer_module) ::  tracers
      end type sph_SGS_MHD
!
      end module t_sph_SGS_MHD
