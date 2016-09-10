!>@file   m_group_data_sph_specr_IO.f90
!!@brief  module m_group_data_sph_specr_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Array for spectr group data
!!
!
      module m_group_data_sph_specr_IO
!
      use m_precision
      use t_spheric_mesh
!
      implicit none
!
!
!> Structure of group data for spherical transform
      type(sph_group_data), save :: sph_grp_IO
!
      end module m_group_data_sph_specr_IO
