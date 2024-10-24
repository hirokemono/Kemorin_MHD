!>@file   m_SPH_SGS_structure.f90
!!@brief  module m_SPH_SGS_structure
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in 2000
!!@n modified in Feb., 2009
!
!> @brief control flags for MHD dynamo model with SGS model
!!
!!@verbatim
!!@endverbatim
!
      module m_SPH_SGS_structure
!
      use m_precision
      use t_SPH_SGS_structure
      use t_SPH_mesh_field_data
      use t_control_data_vizs
      use t_VIZ_mesh_field
      use t_visualizer
      use t_control_param_vol_grping
!
      implicit none
!
!
!>      Structure of spectr grid and data
      type(SPH_mesh_field_data), save :: SPH_MHD1
!>      Structures of SGS model in Spherical shell dynamo
      type(SPH_SGS_structure), save :: SPH_SGS1
!>      Structure of data for visualization
      type(VIZ_mesh_field), save :: VIZ_DAT1
!
!>        Structures of visualization controls
      type(visualization_controls), save :: viz_ctls1
!>        Structures of visualizations
      type(visualize_modules), save :: vizs1
!>        Structure for repartitioning parameters
      type(volume_partioning_param), save :: repart_p1
!
      end module m_SPH_SGS_structure
