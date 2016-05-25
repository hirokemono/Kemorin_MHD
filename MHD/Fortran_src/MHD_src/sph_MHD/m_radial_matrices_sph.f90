!>@file   m_radial_matrices_sph.f90
!!@brief  module m_radial_matrices_sph
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2009
!
!>@brief Radial band matrix for time evolutions
!!
!!@verbatim
!!      subroutine allocate_temp_mat_sph(sph_rj)
!!      subroutine allocate_velo_mat_sph(sph_rj)
!!      subroutine allocate_magne_mat_sph(sph_rj)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!
!!      subroutine check_vorticity_matrices_sph(my_rank, sph_rj)
!!      subroutine check_press_matrices_sph(my_rank, sph_rj)
!!      subroutine check_magne_matrices_sph(my_rank, sph_rj)
!!      subroutine check_temp_matrices_sph(my_rank, sph_rj)
!!      subroutine check_composit_matrix_sph(my_rank, sph_rj)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!@endverbatim
!
      module m_radial_matrices_sph
!
      use m_precision
      use t_spheric_rj_data
      use t_sph_matrices
!
      implicit none
!
!>      Structure of band matrices for poloidal velocity
      type(band_matrix_type), save :: band_vp_evo
!
!>      Structure of band matrices for toroidal velocity
      type(band_matrix_type), save :: band_vt_evo
!
!>      Structure of band matrices for toroidal vorticity
      type(band_matrix_type), save :: band_wt_evo
!
!>      Structure of band matrices for poloidal velocity poisson
      type(band_matrix_type), save :: band_vs_poisson
!
!
!>      Structure of band matrices for pressure poisson
      type(band_matrix_type), save :: band_p_poisson
!
!
!>      Structure of band matrices for poloidal magnetic field
      type(band_matrix_type), save :: band_bp_evo
!
!>      Structure of band matrices for toroidal magnetic field
      type(band_matrix_type), save :: band_bt_evo
!
!
!>      Structure of band matrices for temperature
      type(band_matrix_type), save :: band_temp_evo
!
!>      Structure of band matrices for composition
      type(band_matrix_type), save :: band_comp_evo
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_vorticity_matrices_sph(my_rank, sph_rj)
!
      use check_sph_radial_mat
!
      integer(kind = kint), intent(in) :: my_rank
      type(sph_rj_grid), intent(in) :: sph_rj
!
!
      write(50+my_rank,'(a)') 'evolution matrix for toroidal velocity'
      call check_radial_band_mat(my_rank, sph_rj, band_vt_evo)
!
      write(50+my_rank,'(a)') 'evolution matrix for toroidal vorticity'
      call check_radial_band_mat(my_rank, sph_rj, band_wt_evo)
!
      write(50+my_rank,'(a)') 'evolution matrix for poloidal velocity'
      call check_radial_band_mat(my_rank, sph_rj, band_vp_evo)
!
      end subroutine check_vorticity_matrices_sph
!
! -----------------------------------------------------------------------
!
      subroutine check_press_matrices_sph(my_rank, sph_rj)
!
      use check_sph_radial_mat
!
      integer(kind = kint), intent(in) :: my_rank
      type(sph_rj_grid), intent(in) :: sph_rj
!
!
      write(50+my_rank,'(a)') 'poisson matrix for pressure'
      call check_radial_band_mat(my_rank, sph_rj, band_p_poisson)
!
      end subroutine check_press_matrices_sph
!
! -----------------------------------------------------------------------
!
      subroutine check_magne_matrices_sph(my_rank, sph_rj)
!
      use check_sph_radial_mat
!
      integer(kind = kint), intent(in) :: my_rank
      type(sph_rj_grid), intent(in) :: sph_rj
!
!
      write(50+my_rank,'(a)') 'evolution matrix for poloidal magne'
      call check_radial_band_mat(my_rank, sph_rj, band_bp_evo)
!
      write(50+my_rank,'(a)') 'evolution matrix for toroidal magne'
      call check_radial_band_mat(my_rank, sph_rj, band_bt_evo)
!
      end subroutine check_magne_matrices_sph
!
! -----------------------------------------------------------------------
!
      subroutine check_temp_matrices_sph(my_rank, sph_rj)
!
      use check_sph_radial_mat
!
      integer(kind = kint), intent(in) :: my_rank
      type(sph_rj_grid), intent(in) :: sph_rj
!
!
      write(50+my_rank,'(a)') 'evolution matrix for temperature'
      call check_radial_band_mat(my_rank, sph_rj, band_temp_evo)
!
      end subroutine check_temp_matrices_sph
!
! -----------------------------------------------------------------------
!
      subroutine check_composit_matrix_sph(my_rank, sph_rj)
!
      use check_sph_radial_mat
!
      integer(kind = kint), intent(in) :: my_rank
      type(sph_rj_grid), intent(in) :: sph_rj
!
!
      write(50+my_rank,'(a)') 'evolution matrix for composition'
      call check_radial_band_mat(my_rank, sph_rj, band_temp_evo)
!
      end subroutine check_composit_matrix_sph
!
! -----------------------------------------------------------------------
!
      end module m_radial_matrices_sph
