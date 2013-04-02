!set_sph_scalar_mat_bc.f90
!      module set_sph_scalar_mat_bc
!
!     Written by H. Matsui on Oct., 2009
!
!      subroutine set_fix_scalar_icb_rmat_sph(nri, jmax, evo_mat)
!      subroutine set_fix_flux_icb_rmat_sph(nri, jmax, coef_imp, coef_d,&
!     &          evo_mat)
!
!      subroutine set_fix_scalar_cmb_rmat_sph(nri, jmax, evo_mat)
!      subroutine set_fix_flux_cmb_rmat_sph(nri, jmax, coef_imp, coef_d,&
!     &          evo_mat)
!
      module set_sph_scalar_mat_bc
!
      use m_precision
!
      use m_constants
      use m_t_int_parameter
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_radial_matrices_sph
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_fix_scalar_icb_rmat_sph(nri, jmax, evo_mat)
!
      integer(kind = kint), intent(in) :: jmax, nri
      real(kind = kreal), intent(inout) :: evo_mat(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
        evo_mat(2,nlayer_ICB,  j) = one
        evo_mat(1,nlayer_ICB+1,j) = zero
      end do
!
      end subroutine set_fix_scalar_icb_rmat_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_fix_flux_icb_rmat_sph(nri, jmax, coef_imp, coef_d, &
     &          evo_mat)
!
      use m_coef_fdm_fixed_ICB
!
      integer(kind = kint), intent(in) :: jmax, nri
      real(kind = kreal), intent(in) :: coef_imp, coef_d
!
      real(kind = kreal), intent(inout) :: evo_mat(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
          evo_mat(2,nlayer_ICB,  j) = one + coef_imp*dt*coef_d          &
     &                         * ( -coef_fdm_fix_dr_ICB_2( 0,3)         &
     &                          + g_sph_rj(j,3)*ar_1d_rj(nlayer_ICB,2))
          evo_mat(1,nlayer_ICB+1,j) =     - coef_imp*dt*coef_d          &
     &                         *    coef_fdm_fix_dr_ICB_2( 1,3)
      end do
!
      end subroutine set_fix_flux_icb_rmat_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_fix_scalar_cmb_rmat_sph(nri, jmax, evo_mat)
!
      integer(kind = kint), intent(in) :: jmax, nri
      real(kind = kreal), intent(inout) :: evo_mat(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
        evo_mat(3,nlayer_CMB-1,j) = zero
        evo_mat(2,nlayer_CMB,  j) = one
      end do
!
      end subroutine set_fix_scalar_cmb_rmat_sph
!
! -----------------------------------------------------------------------
!
      subroutine set_fix_flux_cmb_rmat_sph(nri, jmax, coef_imp, coef_d, &
     &          evo_mat)
!
      use m_coef_fdm_fixed_CMB
!
      integer(kind = kint), intent(in) :: jmax, nri
      real(kind = kreal), intent(in) :: coef_imp, coef_d
!
      real(kind = kreal), intent(inout) :: evo_mat(3,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
          evo_mat(3,nlayer_CMB-1,j) =     - coef_imp*dt*coef_d          &
     &                         *    coef_fdm_fix_dr_CMB_2(-1,3)
          evo_mat(2,nlayer_CMB,  j) = one + coef_imp*dt*coef_d          &
     &                         * ( -coef_fdm_fix_dr_CMB_2( 0,3)         &
     &                          + g_sph_rj(j,3)*ar_1d_rj(nlayer_ICB,2))
      end do
!
      end subroutine set_fix_flux_cmb_rmat_sph
!
! -----------------------------------------------------------------------
!
      end module set_sph_scalar_mat_bc
