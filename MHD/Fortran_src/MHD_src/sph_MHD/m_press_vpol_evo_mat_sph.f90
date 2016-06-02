!>@file   m_press_vpol_evo_mat_sph.f90
!!@brief  module m_press_vpol_evo_mat_sph
!!
!!@author H. Matsui
!!@date Programmed in May., 2013
!
!>@brief Radial matrix for time evolution
!!        of poloidal velocity and pressure
!!
!!@verbatim
!!      subroutine allocate_press_vpol_mat_sph(sph_rj, smat)
!!      subroutine deallocate_press_vpol_mat_sph(smat)
!!      subroutine check_velocity_matrices_sph                          &
!!     &         (my_rank, sph_rj, band_vt_evo, smat)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(band_matrices_type), intent(in) :: band_vt_evo
!!        type(band_matrices_type), intent(in) :: smat
!!@endverbatim
!
!
      module m_press_vpol_evo_mat_sph
!
      use m_precision
      use t_spheric_rj_data
      use t_sph_matrices
!
      implicit none
!
!>      Structure of band matrices for poloidal velocity
      type(band_matrices_type), save :: band_vsp_evo
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_press_vpol_mat_sph(sph_rj, smat)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(band_matrices_type), intent(inout) :: smat
!
      integer(kind = kint) :: nri, jmax
!
      smat%n_vect =    2*sph_rj%nidx_rj(1)
      smat%n_comp =      sph_rj%nidx_rj(2)
      smat%n_band =      iseven
      smat%n_band_lu = 2*smat%n_band - 1
!
!
      allocate( smat%mat(smat%n_band,smat%n_vect,smat%n_comp) )
      allocate( smat%lu(smat%n_band_lu ,smat%n_vect,smat%n_comp) )
      allocate( smat%det(smat%n_comp) )
      allocate( smat%i_pivot(smat%n_vect,smat%n_comp) )
!
      smat%mat =   0.0d0
      smat%lu =    0.0d0
      smat%det =   0.0d0
      smat%i_pivot =   0
!
      smat%mat(4,1:2*nri,1:jmax) = 1.0d0
!
      end subroutine allocate_press_vpol_mat_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_press_vpol_mat_sph(smat)
!
      type(band_matrices_type), intent(inout) :: smat
!
!
      call dealloc_band_mat_sph(smat)
!
      end subroutine deallocate_press_vpol_mat_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_velocity_matrices_sph                            &
     &         (my_rank, sph_rj, band_vt_evo, smat)
!
      use check_sph_radial_mat
!
      integer(kind = kint), intent(in) :: my_rank
      type(sph_rj_grid), intent(in) :: sph_rj
      type(band_matrices_type), intent(in) :: band_vt_evo
      type(band_matrices_type), intent(in) :: smat
!
      real(kind = kreal) :: rr(2*sph_rj%nidx_rj(1))
      integer(kind = kint) :: k
!
!
      do k = 1, sph_rj%nidx_rj(1)
        rr(k) = sph_rj%radius_1d_rj_r(k)
        rr(sph_rj%nidx_rj(1)+k) = sph_rj%radius_1d_rj_r(k)
      end do
!
      call check_radial_7band_mat(my_rank, (2*sph_rj%nidx_rj(1)),       &
     &    sph_rj%nidx_rj(2), sph_rj%idx_gl_1d_rj_j, rr, smat%mat)
!
      call check_radial_band_mat(my_rank, sph_rj, band_vt_evo)
!
      end subroutine check_velocity_matrices_sph
!
! -----------------------------------------------------------------------
!
      end module m_press_vpol_evo_mat_sph
