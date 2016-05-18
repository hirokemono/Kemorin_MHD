!>@file   m_addresses_trans_sph_MHD.f90
!!@brief  module m_addresses_trans_sph_MHD
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
      module m_addresses_trans_sph_MHD
!
      use m_precision
!
      use t_phys_address
      use t_addresses_sph_transform
!
      implicit none
!
!>      strucutre for spherical transform data addresses
      type(address_4_sph_trans), save :: trns_MHD
!
      end module m_addresses_trans_sph_MHD
