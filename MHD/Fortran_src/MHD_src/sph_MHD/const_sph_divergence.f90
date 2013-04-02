!const_sph_divergence.f90
!      module const_sph_divergence
!
!      modified by H. Matsui on Oct., 2009
!
!
!      subroutine const_sph_heat_advect
!      subroutine const_sph_scalar_advect
!
!      subroutine const_sph_div_force(is_fld, is_div)
!
      module const_sph_divergence
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_phys_address
      use m_control_params_sph_MHD
      use cal_sph_exp_1st_diff
      use cal_sph_exp_rotation
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_heat_advect
!
      use cal_sph_exp_fixed_scalar
      use cal_sph_exp_fixed_flux
!
      integer(kind = kint) :: kr_st, kr_ed
!
!
      kr_st = nlayer_ICB+1
      kr_ed = nlayer_CMB-1
      call cal_sph_nod_vect_div2(kr_st, kr_ed,                          &
     &    ipol%i_h_flux, ipol%i_h_advect)
!
      if (iflag_hflux_icb .eq. 1) then
        call cal_div_sph_icb_fix_flux_2(nidx_rj(2), h_flux_ICB_bc,      &
     &      ipol%i_h_flux, ipol%i_h_advect)
      else
        call cal_sph_div_flux_4_icb_fix(nidx_rj(2), temp_ICB_bc,        &
     &      ipol%i_h_flux, ipol%i_h_advect)
      end if
!
      if (iflag_hflux_cmb .eq. 1) then
        call cal_div_sph_cmb_fix_flux_2(nidx_rj(2), h_flux_CMB_bc,      &
     &      ipol%i_h_flux, ipol%i_h_advect)
      else
        call cal_sph_div_flux_4_cmb_fix(nidx_rj(2), temp_CMB_bc,        &
     &      ipol%i_h_flux, ipol%i_h_advect)
      end if
!
      end subroutine const_sph_heat_advect
!
! -----------------------------------------------------------------------
!
      subroutine const_sph_scalar_advect
!
      use cal_sph_exp_fixed_scalar
      use cal_sph_exp_fixed_flux
!
      integer(kind = kint) :: kr_st, kr_ed
!
!
      kr_st = nlayer_ICB+1
      kr_ed = nlayer_CMB-1
      call cal_sph_nod_vect_div2(kr_st, kr_ed,                          &
     &    ipol%i_c_flux, ipol%i_c_advect)
!
      if (iflag_hflux_icb .eq. 1) then
        call cal_div_sph_icb_fix_flux_2(nidx_rj(2), c_flux_ICB_bc,      &
     &      ipol%i_c_flux, ipol%i_c_advect)
      else
        call cal_sph_div_flux_4_icb_fix(nidx_rj(2), composition_ICB_bc, &
     &      ipol%i_c_flux, ipol%i_c_advect)
      end if
!
      if (iflag_hflux_cmb .eq. 1) then
        call cal_div_sph_cmb_fix_flux_2(nidx_rj(2), c_flux_CMB_bc,      &
     &      ipol%i_c_flux, ipol%i_c_advect)
      else
        call cal_sph_div_flux_4_icb_fix(nidx_rj(2), composition_CMB_bc, &
     &      ipol%i_c_flux, ipol%i_c_advect)
      end if
!
      end subroutine const_sph_scalar_advect
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_sph_div_force(is_fld, is_div)
!
      use m_coef_fdm_fixed_ICB
      use m_coef_fdm_fixed_CMB
      use cal_sph_exp_nod_none_bc
!
      integer(kind = kint), intent(in) :: is_fld, is_div
      integer(kind = kint) :: kr_st, kr_ed
!
!
      kr_st = nlayer_ICB+1
      kr_ed = nlayer_CMB-1
      call cal_sph_nod_vect_div2(kr_st, kr_ed, is_fld, is_div )
!
      call cal_sph_nod_nobc_in_div2(coef_fdm_fix_ICB_2, nlayer_ICB,     &
     &    is_fld, is_div)
      call cal_sph_nod_nobc_out_div2(coef_fdm_fix_CMB_2, nlayer_CMB,    &
     &    is_fld, is_div)
!
      end subroutine const_sph_div_force
!
! -----------------------------------------------------------------------
!
      end module const_sph_divergence
