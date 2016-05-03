!>@file   const_radial_forces_on_bc.f90
!!@brief  module const_radial_forces_on_bc
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Sep., 2009
!
!>@brief Evaluate radial component of forces at boundaries
!!
!!@verbatim
!!      subroutine s_const_radial_forces_on_bc(rj_fld)
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module const_radial_forces_on_bc
!
      use m_precision
!
      use m_machine_parameter
      use m_constants
      use m_control_parameter
      use m_spherical_harmonics
      use m_sph_phys_address
!
      implicit none
!
      private :: cal_radial_force_on_sph
!
!   ------------------------------------------------------------------
!
      contains
!
!   ------------------------------------------------------------------
!
      subroutine s_const_radial_forces_on_bc(rj_fld)
!
      use m_sph_phys_address
      use m_physical_property
      use m_spheric_parameter
      use m_boundary_params_sph_MHD
!
      use t_phys_data
!
      use cal_r_buoyancies_on_sph
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call s_cal_r_buoyancies_on_sph(sph_bc_U%kr_in, rj_fld)
      call s_cal_r_buoyancies_on_sph(sph_bc_U%kr_out, rj_fld)
!
!$omp parallel
      call cal_radial_force_on_sph(sph_bc_U%kr_in,                      &
     &      ipol%i_v_diffuse, ipol%i_div_viscous,                       &
     &      nnod_rj, sph_rj1%nidx_rj, sph_rj1%ar_1d_rj,                 &
     &      rj_fld%ntot_phys, rj_fld%d_fld)
      call cal_radial_force_on_sph(sph_bc_U%kr_out,                     &
     &      ipol%i_v_diffuse, ipol%i_div_viscous,                       &
     &      nnod_rj, sph_rj1%nidx_rj, sph_rj1%ar_1d_rj,                 &
     &      rj_fld%ntot_phys, rj_fld%d_fld)
!
      call cal_radial_force_on_sph(sph_bc_U%kr_in,                      &
     &      ipol%i_m_advect, ipol%i_div_inertia,                        &
     &      nnod_rj, sph_rj1%nidx_rj, sph_rj1%ar_1d_rj,                 &
     &      rj_fld%ntot_phys, rj_fld%d_fld)
      call cal_radial_force_on_sph(sph_bc_U%kr_out,                     &
     &      ipol%i_m_advect, ipol%i_div_inertia,                        &
     &      nnod_rj, sph_rj1%nidx_rj, sph_rj1%ar_1d_rj,                 &
     &       rj_fld%ntot_phys, rj_fld%d_fld)
!
      if( iflag_4_lorentz .gt. id_turn_OFF) then
        call cal_radial_force_on_sph(sph_bc_U%kr_in,                    &
     &      ipol%i_lorentz, ipol%i_div_inertia,                         &
     &      nnod_rj, sph_rj1%nidx_rj, sph_rj1%ar_1d_rj,                 &
     &      rj_fld%ntot_phys, rj_fld%d_fld)
        call cal_radial_force_on_sph(sph_bc_U%kr_out,                   &
     &      ipol%i_lorentz, ipol%i_div_inertia,                         &
     &      nnod_rj, sph_rj1%nidx_rj, sph_rj1%ar_1d_rj,                 &
     &      rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!$omp end parallel
!
      end subroutine s_const_radial_forces_on_bc
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_radial_force_on_sph(kr, is_fld, is_fr,             &
     &          nnod_rj, nidx_rj, ar_1d_rj, ntot_phys_rj, d_rj)
!
      use m_schmidt_poly_on_rtm
!
      integer(kind = kint), intent(in) :: is_fld, is_fr
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      integer(kind = kint), intent(in) :: nidx_rj(2)
      real(kind = kreal), intent(in) :: ar_1d_rj(nidx_rj(1),3)
!
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: inod, j
!
!
!$omp do private(inod,j)
      do j = 1, nidx_rj(2)
        inod = j + (kr-1) * nidx_rj(2)
        d_rj(inod,is_fr) = d_rj(inod,is_fld)                            &
     &                     * max(g_sph_rj(j,3),half)*ar_1d_rj(kr,2)
      end do
!$omp end do
!
      end subroutine cal_radial_force_on_sph
!
!-----------------------------------------------------------------------
!
      end module const_radial_forces_on_bc
