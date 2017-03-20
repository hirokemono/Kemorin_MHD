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
      use t_spheric_mesh
      use t_phys_data
      use t_phys_address
      use t_time_data
      use t_field_data_IO
      use t_schmidt_poly_on_rtm
      use t_rms_4_sph_spectr
      use t_sum_sph_rms_data
!
      implicit none
!
!>  Structure of grid and spectr data for spherical spectr method
      type(sph_mesh_data), save :: sph_mesh_spec
!
!>   address for spectr data (poloidal component for vector)
      type(phys_address), save :: ipol_spec
!>   address for radial gradient for poloidal component
      type(phys_address), save :: idpdr_spec
!>   address for toroidal component
      type(phys_address), save :: itor_spec
!>  Structure for field data
      type(phys_data), save :: rj_fld_spec
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
