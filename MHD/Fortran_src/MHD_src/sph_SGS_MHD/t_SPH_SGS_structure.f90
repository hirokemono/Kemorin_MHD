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
      use t_SGS_control_parameter
      use t_sph_filtering
      use t_SGS_model_addresses
      use t_sph_trans_arrays_SGS_MHD
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
!>        address of spectr SGS term
        type(SGS_model_addresses) :: ipol_LES
!>        address of nodal SGS term
        type(SGS_model_addresses) :: iphys_LES
!
!>        strucutres for spherical transform for MHD dynamo with SGS
        type(works_4_sph_trans_SGS_MHD) :: trns_WK_LES
      end type SPH_SGS_structure
!
      end module t_SPH_SGS_structure
