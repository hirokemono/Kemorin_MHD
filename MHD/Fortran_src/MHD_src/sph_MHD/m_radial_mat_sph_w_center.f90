!>@file   m_radial_mat_sph_w_center.f90
!!@brief  module m_radial_mat_sph_w_center
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2013
!
!>@brief Radial band matrix for time evolutions
!!
!!@verbatim
!!      subroutine allocate_temp00_mat_sph(sph_rj)
!!      subroutine allocate_press00_mat_sph(sph_rj)
!!      subroutine allocate_comp00_mat_sph(sph_rj)
!!
!!      subroutine deallocate_temp00_mat_sph
!!      subroutine deallocate_press00_mat_sph
!!      subroutine deallocate_average_w_center
!!
!!      subroutine check_press00_mat_sph(my_rank, sph_rj)
!!      subroutine check_temp00_mat_sph(my_rank, sph_rj)
!!      subroutine check_comp00_mat_sph(my_rank, sph_rj)
!!@endverbatim
!
      module m_radial_mat_sph_w_center
!
      use m_precision
      use t_spheric_rj_data
      use t_sph_matrix
!
      implicit none
!
!>      Structure of band matrices for pressure poisson
      type(band_matrix_type), save :: band_p00_poisson
!>      Structure of band matrices for pressure poisson
      type(band_matrix_type), save :: band_temp00_evo
!>      Structure of band matrices for pressure poisson
      type(band_matrix_type), save :: band_comp00_evo
!
!>     Temporal space for average with center
      real(kind = kreal), allocatable :: x00_w_center(:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_average_w_center(sph_rj)
!
      type(sph_rj_grid), intent(in) :: sph_rj
!
      integer(kind = kint) :: nri
!
!
      nri =  sph_rj%nidx_rj(1)
      allocate( x00_w_center(0:nri) )
      x00_w_center = 0.0d0
!
      end subroutine allocate_average_w_center
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_average_w_center
!
!
      deallocate( x00_w_center )
!
      end subroutine deallocate_average_w_center
!
! -----------------------------------------------------------------------
!
      end module m_radial_mat_sph_w_center
