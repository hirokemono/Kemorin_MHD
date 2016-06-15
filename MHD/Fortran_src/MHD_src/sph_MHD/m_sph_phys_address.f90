!>@file   m_sph_phys_address.f90
!!@brief  module m_sph_phys_address
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2007
!
!>@brief  start addresses for spetr fields
!!
!
      module m_sph_phys_address
!
      use m_precision
      use m_constants
!
      use t_phys_address
!
      implicit  none
!
!>   address for spectr data (poloidal component for vector)
      type(phys_address), save :: ipol
!
!>   address for radial gradient for poloidal component
      type(phys_address), save :: idpdr
!
!>   address for toroidal component
      type(phys_address), save :: itor
!
      end module m_sph_phys_address
