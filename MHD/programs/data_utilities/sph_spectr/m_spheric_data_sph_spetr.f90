!>@file   m_spheric_data_sph_spetr.f90
!!@brief  module m_spheric_data_sph_spetr
!!
!!@author H. Matsui
!!@date Programmed on July, 2007
!!
!!@brief  indexing table of speherical harmonics transform
!!
!
      module m_spheric_data_sph_spetr
!
      use m_precision
      use t_SPH_mesh_field_data
      use t_time_data
      use t_field_data_IO
      use t_schmidt_poly_on_rtm
      use t_rms_4_sph_spectr
      use t_sum_sph_rms_data
!
      implicit none
!
!
!>      Structure of spetr grid and data
      type(SPH_mesh_field_data), save :: SPH_dat_ss
!
!>  Structure for field data IO
      type(time_data) :: spec_time_IO
      type(field_IO), save :: sph_spec_IO
!
      type(legendre_4_sph_trans), save :: leg_s
!
!
!>      Structure of mean square data
      type(sph_mean_squares), save :: pwr_spec
!>      Work structure of mean square data
      type(sph_mean_square_work), save :: WK_pwr_spec
!
      end module m_spheric_data_sph_spetr
