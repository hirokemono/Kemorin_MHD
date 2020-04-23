!>@file   t_SPH_SGS_structure.f90
!!@brief  module t_SPH_SGS_structure
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in 2000
!!@n modified in Feb., 2009
!
!> @brief Structure for SGS model in FEM_MHD
!!
!!@verbatim
!!@endverbatim
!
      module t_SPH_SGS_structure
!
      use m_precision
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_spheric_group
!
      use t_phys_address
      use t_SGS_model_addresses
      use t_phys_data
!
      use t_SGS_control_parameter
      use t_sph_filtering
!
      implicit none
!
!
!>      Structures of SGS model in Spherical shell dynamo
      type SPH_SGS_structure
!>        Structure of input parameters for SGS model
        type(SGS_paremeters) :: SGS_par
!>        Structure of work area for dyanmic SGS model
!!        for spectrum dynamo
        type(dynamic_SGS_data_4_sph) :: dynamic
!
!
!>         Structure of grid and spectr data for spherical spectr method
        type(sph_grids) :: sph
!>        Structure for communication table for spherical transform
        type(sph_comm_tables) :: comms
!>        Structure for grid and comm table for spherical transform
        type(sph_group_data) :: groups
!
!>        address for spectr data (poloidal component for vector)
        type(phys_address) :: ipol
!>        address for spectr data (poloidal component for vector)
        type(SGS_model_addresses) :: ipol_LES
!>        Structure for field data
        type(phys_data) :: fld
      end type SPH_SGS_structure
!
      end module t_SPH_SGS_structure
