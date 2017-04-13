!>@file   m_radial_matrices_sph.f90
!!@brief  module m_radial_matrices_sph
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2009
!
!>@brief Radial band matrix for time evolutions
!!
      module m_radial_matrices_sph
!
      use m_precision
      use t_spheric_rj_data
      use t_sph_matrices
      use t_radial_matrices_sph_MHD
!
      implicit none
!
!>      Structure of band matrices for poloidal velocity
      type(band_matrices_type), save :: band_vp_evo
!
!>      Structure of band matrices for toroidal velocity
      type(band_matrices_type), save :: band_vt_evo
!
!>      Structure of band matrices for toroidal vorticity
      type(band_matrices_type), save :: band_wt_evo
!
!>      Structure of band matrices for poloidal velocity poisson
      type(band_matrices_type), save :: band_vs_poisson
!
!
!>      Structure of band matrices for pressure poisson
      type(band_matrices_type), save :: band_p_poisson
!
!
!>      Structure of band matrices for poloidal magnetic field
      type(band_matrices_type), save :: band_bp_evo
!
!>      Structure of band matrices for toroidal magnetic field
      type(band_matrices_type), save :: band_bt_evo
!
!
!>      Structure of band matrices for temperature
      type(band_matrices_type), save :: band_temp_evo
!
!>      Structure of band matrices for composition
      type(band_matrices_type), save :: band_comp_evo
!
!
      end module m_radial_matrices_sph
