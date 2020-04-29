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
      use t_ctl_data_4_sph_utils
      use t_SPH_mesh_field_data
      use t_time_data
      use t_field_data_IO
      use t_schmidt_poly_on_rtm
      use t_spheric_data_sph_spetr
      use t_SPH_SGS_structure
!
      implicit none
!
!
!>      Structure of mean square data
      type(spherical_spectr_data_util_ctl), save :: spu_ctl1
!>      Structure of spetr grid and data
      type(SPH_mesh_field_data), save :: SPH_dat_ss
!>        address of nodal SGS term
      type(SGS_model_addresses), save :: ipol_LES_ss
!
!>       Structure for field data IO
      type(time_data) :: spec_time_IO
      type(field_IO), save :: sph_spec_IO
!
      type(legendre_4_sph_trans), save :: leg_s
!
!
!>      Structure of mean square data
      type(sph_spectr_monitor_data), save :: monitor_ss
!
      end module m_spheric_data_sph_spetr
