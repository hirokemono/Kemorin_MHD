!>@file   m_group_data_sph_specr.f90
!!@brief  module m_group_data_sph_specr
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Grouping information for spherical hermonics data
!!
      module m_group_data_sph_specr
!
      use m_precision
      use t_group_data
      use t_spheric_mesh
!
      implicit none
!
!> Structure for grid and comm table for spherical transform
      type(sph_group_data), save :: sph_grps1
!
      end module m_group_data_sph_specr
