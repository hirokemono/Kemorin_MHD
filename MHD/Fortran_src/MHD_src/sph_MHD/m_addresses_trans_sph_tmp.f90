!>@file   m_addresses_trans_sph_tmp.f90
!!@brief  module m_addresses_trans_sph_tmp
!!
!!@author H. Matsui
!!@date Programmed in March, 2012
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
      module m_addresses_trans_sph_tmp
!
      use m_precision
!
      use t_phys_address
      use t_addresses_sph_transform
!
      implicit none
!
!>      strucutre for spherical transform data addresses
      type(address_4_sph_trans), save :: trns_tmp
!
      end module m_addresses_trans_sph_tmp
