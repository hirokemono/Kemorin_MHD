!>@file   initial_reference_setup.f90
!!@brief  module initial_reference_setup
!!
!!@author H. Matsui
!!@date Programmed in March, 2008
!
!> @brief Set initial data for spectrum dynamos
!!
!!@verbatim
!!     Tempareture at fluid core
!!       T = const_OC + coef_OC / r - (1/6) source_OC r^2
!!     Tempareture at inner core
!!       T = const_IC - (1/6) source_IC r^2
!!
!!      subroutine boundary_data_for_reference(sph, sph_bc_T, bcs_T,    &
!!     &          kr_in, kr_out, r_in, r_out, val_ICB, val_CMB)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_boundary_type), intent(in) :: sph_bc_T
!!        type(sph_scalar_boundary_data), intent(in) :: bcs_T
!!        integer(kind = kint), intent(inout) :: kr_in, kr_out
!!        real(kind = kreal), intent(inout) :: r_in, r_out
!!        real(kind = kreal), intent(inout) :: val_ICB, val_CMB
!!
!!      subroutine copy_radius_rj(sph, nri_1d, r_1d)
!!        type(sph_grids), intent(in) :: sph
!!        integer(kind = kint), intent(in) :: nri_1d
!!        real(kind = kreal), intent(inout) :: r_1d(0:nri_1d)
!!      subroutine copy_from_reference(sph, nri_1d, reference,          &
!!     &                               n_point, field_rj)
!!        type(sph_grids), intent(in) :: sph
!!        integer(kind = kint), intent(in) :: nri_1d
!!        real(kind = kreal), intent(in) :: reference(0:nri_1d)
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(inout) :: field_rj(n_point)
!!@endverbatim
!
      module initial_reference_setup
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      use t_spheric_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine boundary_data_for_reference(sph, sph_bc_T, bcs_T,      &
     &          kr_in, kr_out, r_in, r_out, val_ICB, val_CMB)
!
      use t_boundary_params_sph_MHD
      use t_boundary_sph_spectr
      use spherical_indices_picker
      use sph_boundary_data_picker
!
      type(sph_grids), intent(in) :: sph
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(sph_scalar_boundary_data), intent(in) :: bcs_T
!
      integer(kind = kint), intent(inout) :: kr_in, kr_out
      real(kind = kreal), intent(inout) :: r_in, r_out
      real(kind = kreal), intent(inout) :: val_ICB, val_CMB
!
      integer(kind = kint) :: jj
!
      jj = idx_rj_degree_zero(sph)
      kr_in =   sph_inner_boundary_r_grid(sph_bc_T)
      kr_out =  sph_outer_boundary_r_grid(sph_bc_T)
      r_in =    sph_inner_boundary_radius(sph_bc_T)
      r_out =   sph_outer_boundary_radius(sph_bc_T)
      val_ICB = sph_inner_boundary_scalar_coef(bcs_T, jj)
      val_CMB = sph_outer_boundary_scalar_coef(bcs_T, jj)
!
      end subroutine boundary_data_for_reference
!
!-----------------------------------------------------------------------
!
      subroutine copy_radius_rj(sph, nri_1d, r_1d)
!
      use spherical_indices_picker
!
      type(sph_grids), intent(in) :: sph
      integer(kind = kint), intent(in) :: nri_1d
!
      real(kind = kreal), intent(inout) :: r_1d(0:nri_1d)
!
      integer(kind = kint) :: k
!
      r_1d(0) = zero
      do k = 1, num_rj_radial_point(sph)
        r_1d(k) = radius_1d_rj_r(sph,k)
      end do
!
      end subroutine copy_radius_rj
!
!-----------------------------------------------------------------------
!
      subroutine copy_from_reference(sph, nri_1d, reference,            &
     &                               n_point, field_rj)
!
      use spherical_indices_picker
!
      type(sph_grids), intent(in) :: sph
      integer(kind = kint), intent(in) :: nri_1d
      real(kind = kreal), intent(in) :: reference(0:nri_1d)
      integer(kind = kint), intent(in) :: n_point
!
      real(kind = kreal), intent(inout) :: field_rj(n_point)
!
      integer(kind = kint) :: k, jj, inod
!
!   set reference temperature (l = m = 0)
      jj = idx_rj_degree_zero(sph)
      do k = 1, num_rj_radial_point(sph)
        inod = local_sph_data_address(sph, k, jj)
        field_rj(inod) = reference(k)
      end do
!
!   Center
      inod = inod_rj_center(sph)
      if(inod .gt. 0) field_rj(inod) = reference(0)
!
      end subroutine copy_from_reference
!
!-----------------------------------------------------------------------
!
      end module initial_reference_setup
