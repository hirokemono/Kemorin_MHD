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
      use t_group_data
!
      implicit none
!
!
!>       node group for grid space
      type(group_data), save :: bc_rtp_grp_IO
!bc_rtp_grp_IO%num_grp
!
!>       radial group for grid space
      type(group_data), save :: radial_rtp_grp_IO
!radial_rtp_grp_IO%num_grp
!
!>       meridional group for grid space
      type(group_data), save :: theta_rtp_grp_IO
!theta_rtp_grp_IO%num_grp
!
!>       zonal group for grid space
      type(group_data), save :: zonal_rtp_grp_IO
!zonal_rtp_grp_IO%num_grp
!
!
!>       radial group for sprctrum space
      type(group_data), save :: radial_rj_grp_IO
!radial_rj_grp_IO%num_grp
!
!>       spherical harmonics group for sprctrum space
      type(group_data), save :: sphere_rj_grp_IO
!sphere_rj_grp_IO%num_grp
!
      end module m_group_data_sph_specr_IO
