!>@file   m_SPH_SGS_structure.f90
!!@brief  module m_SPH_SGS_structure
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in 2000
!!@n modified in Feb., 2009
!
!> @brief control flags for MHD dynamo model
!!
!!@verbatim
!!@endverbatim
!
      module m_SPH_SGS_structure
!
      use m_precision
      use t_SPH_SGS_structure
!
      implicit none
!
!
!>      Structures of SGS model in Spherical shell dynamo
      type(SPH_SGS_structure), save :: SPH_SGS1
!
      end module m_SPH_SGS_structure
