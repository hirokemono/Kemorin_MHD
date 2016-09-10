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
!sph_grp_IO%bc_rtp_grp
!
!>       node group for grid space
!      type(group_data), save :: bc_rtp_grp_IO
!
!>       radial group for grid space
!      type(group_data), save :: radial_rtp_grp_IO
!
!>       meridional group for grid space
!      type(group_data), save :: theta_rtp_grp_IO
!
!>       zonal group for grid space
!      type(group_data), save :: zonal_rtp_grp_IO
!
!
!>       radial group for sprctrum space
!      type(group_data), save :: radial_rj_grp_IO
!
!>       spherical harmonics group for sprctrum space
!      type(group_data), save :: sphere_rj_grp_IO
!
      end module m_group_data_sph_specr_IO
