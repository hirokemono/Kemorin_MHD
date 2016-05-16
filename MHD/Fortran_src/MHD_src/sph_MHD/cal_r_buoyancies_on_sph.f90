!>@file   cal_r_buoyancies_on_sph.f90
!!@brief  module cal_r_buoyancies_on_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evaluate buoyancy at specific radius
!!
!!@verbatim
!!      subroutine s_cal_r_buoyancies_on_sph(kr, sph_rj, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@param kr  Radial grid ID
!
      module cal_r_buoyancies_on_sph
!
      use m_precision
!
      use m_constants
      use m_control_parameter
      use m_sph_phys_address
      use m_schmidt_poly_on_rtm
!
      implicit  none
!
      private :: cal_r_double_buoyancy_on_sph, cal_r_buoyancy_on_sph
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_r_buoyancies_on_sph(kr, sph_rj, rj_fld)
!
      use m_machine_parameter
      use m_physical_property
!
      use t_spheric_rj_data
      use t_phys_data
!
      integer(kind= kint), intent(in) :: kr
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_data), intent(inout) :: rj_fld
!
!
      if ((iflag_4_gravity*iflag_4_composit_buo) .gt. id_turn_OFF) then
!
        if(iflag_4_ref_temp .ne. id_sphere_ref_temp) then
          if (iflag_debug.eq.1)                                         &
     &      write(*,*)'cal_r_double_buoyancy_on_sph', ipol%i_temp
          call cal_r_double_buoyancy_on_sph                             &
     &       (kr, ipol%i_temp, ipol%i_div_buoyancy,                     &
     &        sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                    &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        else
          if (iflag_debug.eq.1)                                         &
     &      write(*,*)'cal_r_double_buoyancy_on_sph', ipol%i_par_temp
          call cal_r_double_buoyancy_on_sph                             &
     &       (kr, ipol%i_par_temp,ipol%i_div_buoyancy,                  &
     &        sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                    &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
!
      else if ( iflag_4_gravity .gt. id_turn_OFF) then
!
        if(iflag_4_ref_temp .ne. id_sphere_ref_temp) then
          if (iflag_debug.eq.1) write(*,*) 'cal_r_buoyancy_on_sph'
          call cal_r_buoyancy_on_sph                                    &
     &       (kr, coef_buo, ipol%i_temp, ipol%i_div_buoyancy,           &
     &        sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                    &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        else
          if (iflag_debug.eq.1) write(*,*) 'cal_r_buoyancy_on_sph'
          call cal_r_buoyancy_on_sph                                    &
     &       (kr, coef_buo, ipol%i_par_temp, ipol%i_div_buoyancy,       &
     &        sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                    &
     &        rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
!
      else if (iflag_4_composit_buo .gt. id_turn_OFF) then
        if (iflag_debug.eq.1) write(*,*) 'cal_r_buoyancy_on_sph'
        call cal_r_buoyancy_on_sph                                      &
     &     (kr, coef_comp_buo, ipol%i_light, ipol%i_div_comp_buo,       &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      else if (iflag_4_filter_gravity .gt. id_turn_OFF) then
        if (iflag_debug.eq.1) write(*,*) 'cal_r_buoyancy_on_sph'
        call cal_r_buoyancy_on_sph                                      &
     &     (kr, coef_buo, ipol%i_filter_temp, ipol%i_div_filter_buo,    &
     &      sph_rj%nidx_rj, sph_rj%radius_1d_rj_r,                      &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      end subroutine s_cal_r_buoyancies_on_sph
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_r_double_buoyancy_on_sph(kr, is_t, is_fr,          &
     &          nidx_rj, radius_1d_rj_r, nnod_rj, ntot_phys_rj, d_rj)
!
      use m_physical_property
!
      integer(kind= kint), intent(in) :: is_t, is_fr, kr
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind= kint) :: inod, j
!
!
!$omp parallel do private (inod,j)
      do j = 1, nidx_rj(2)
        inod = j + (kr-1) * nidx_rj(2)
        d_rj(inod,is_fr) = (coef_buo*d_rj(inod,is_t)                    &
     &                     + coef_comp_buo*d_rj(inod,ipol%i_light))     &
     &                     * radius_1d_rj_r(kr)
      end do
!$omp end parallel do
!
      end subroutine cal_r_double_buoyancy_on_sph
!
!-----------------------------------------------------------------------
!
      subroutine cal_r_buoyancy_on_sph(kr, coef, is_fld, is_fr,         &
     &          nidx_rj, radius_1d_rj_r, nnod_rj, ntot_phys_rj, d_rj)
!
      use m_physical_property
!
      integer(kind= kint), intent(in) :: is_fld, is_fr, kr
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: nnod_rj, ntot_phys_rj
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nidx_rj(1))
      real(kind = kreal), intent(in) :: coef
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind= kint) :: inod, j
!
!
!$omp parallel do private (inod,j)
      do j = 1, nidx_rj(2)
        inod = j + (kr-1) * nidx_rj(2)
        d_rj(inod,is_fr) = coef*d_rj(inod,is_fld)*radius_1d_rj_r(kr)
      end do
!$omp end parallel do
!
      end subroutine cal_r_buoyancy_on_sph
!
!-----------------------------------------------------------------------
!
      end module cal_r_buoyancies_on_sph
