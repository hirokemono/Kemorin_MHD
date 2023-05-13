!>@file   t_sph_SGS_MHD_structure.f90
!!@brief  module t_sph_SGS_MHD_structure
!!
!!@author H. Matsui
!!@date Programmed in 2000
!!@n modified in Feb., 2022
!
!> @brief Structures for MHD dynamo model with SGS model
!!
      module t_sph_SGS_MHD_structure
!
      use m_precision
      use t_SPH_SGS_structure
      use t_SPH_mesh_field_data
      use t_VIZ_mesh_field
      use t_visualizer
      use t_ctl_data_MHD
      use t_ctl_data_SGS_MHD
!
      implicit none
!
!
      type SPH_SGS_MHD_structure
!>        Structure of spectr grid and data
        type(SPH_mesh_field_data) :: SPH_MHD1
!>        Structures of SGS model in Spherical shell dynamo
        type(SPH_SGS_structure) :: SPH_SGS1
!>        Structure of data for visualization
        type(VIZ_mesh_field) :: VIZ_DAT1
!>        Structures of visualizations
        type(visualize_modules) :: vizs1
!
!>      Control struture for MHD simulation
      type(mhd_simulation_control) :: MHD_ctl1
!>        Additional structures for spherical SGS MHD dynamo
      type(add_sgs_sph_mhd_ctl) :: add_SSMHD_ctl1
      end type SPH_SGS_MHD_structure
!
      end module t_sph_SGS_MHD_structure
