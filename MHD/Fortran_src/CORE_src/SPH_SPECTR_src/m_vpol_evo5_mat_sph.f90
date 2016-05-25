!>@file   m_vpol_evo5_mat_sph.f90
!!@brief  module m_vpol_evo5_mat_sph
!!
!!@author H. Matsui
!!@date Programmed in May., 2013
!
!>@brief Radial matrix for time evolution(nidx_rj)
!!        of poloidal velocity with 5-band matrix
!!
!!@verbatim
!!      subroutine allocate_vpol_evo5_mat_sph                           &
!!     &         (nlayer_ICB, nlayer_CMB, sph_rj)
!!      subroutine deallocate_vpol_evo5_mat_sph
!!      subroutine check_vpol_evo5_mat_sph(my_rank, nidx_rj)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!@endverbatim
!
      module m_vpol_evo5_mat_sph
!
      use m_precision
      use t_spheric_rj_data
      use t_sph_matrices
!
      implicit none
!
!>      Structure of band matrices for poloidal velocity
      type(band_matrix_type), save :: band5_vp_evo
!
!>      Structure of band matrices for poloidal velocity
      type(band_matrix_type), save :: band3_vp_poisson
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_vpol_evo5_mat_sph                             &
     &         (nlayer_ICB, nlayer_CMB, sph_rj)
!
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(sph_rj_grid), intent(in) :: sph_rj
!
      integer(kind = kint) :: nri, jmax
!
!
      call alloc_band_mat_sph(ithree, sph_rj, band5_vp_evo)
      call set_unit_on_diag(band5_vp_evo)
      call alloc_band_mat_sph(ifive, sph_rj, band3_vp_poisson)
      call set_unit_on_diag(band3_vp_poisson)
!
      end subroutine allocate_vpol_evo5_mat_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_vpol_evo5_mat_sph
!
!
      call dealloc_band_mat_sph(band5_vp_evo)
      call dealloc_band_mat_sph(band3_vp_poisson)
!
      end subroutine deallocate_vpol_evo5_mat_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_vpol_evo5_mat_sph(my_rank, sph_rj)
!
      use check_sph_radial_mat
!
      integer(kind = kint), intent(in) :: my_rank
      type(sph_rj_grid), intent(in) :: sph_rj
!
!
      write(50+my_rank,'(a)') 'poisson matrix for poloidal velocity'
      call check_radial_band_mat(my_rank, sph_rj, band3_vp_poisson)
!
      write(50+my_rank,'(a)') 'crank matrix for poloidal velocity'
      call check_radial_band_mat(my_rank, sph_rj, band5_vp_evo)
!
      end subroutine check_vpol_evo5_mat_sph
!
! -----------------------------------------------------------------------
!
      end module m_vpol_evo5_mat_sph
