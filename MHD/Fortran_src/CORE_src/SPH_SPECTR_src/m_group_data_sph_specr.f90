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
!
      implicit none
!
!>       node group for grid space
      type(group_data), save :: bc_rtp_grp1
!>       radial group for grid space
      type(group_data), save :: radial_rtp_grp1
!>       meridional group for grid space
      type(group_data), save :: theta_rtp_grp1
!>       zonal group for grid space
      type(group_data), save :: zonal_rtp_grp
!
!>       radial group for sprctrum space
      type(group_data), save :: radial_rj_grp1
!>       spherical harmonics group for sprctrum space
      type(group_data), save :: sphere_rj_grp1
!
!
      end module m_group_data_sph_specr
