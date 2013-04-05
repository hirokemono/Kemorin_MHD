!const_sph_radial_grad.f90
!      module const_sph_radial_grad
!
!      modified by H. Matsui on Oct., 2009
!
!      subroutine const_radial_grad_temp
!      subroutine const_radial_grad_d_scalar
!
!      subroutine const_grad_vp_and_vorticity
!        Input:    ipol%i_velo, itor%i_velo
!        Solution: idpdr%i_velo, ipol%i_vort, itor%i_vort, idpdr%i_vort
!
!      subroutine const_grad_bp_and_current
!        Input:    ipol%i_magne, itor%i_magne
!        Solution: idpdr%i_magne,
!                  ipol%i_current, itor%i_current, idpdr%i_current
!
!      subroutine const_grad_vp_and_vorticity
!        Input:    ipol%i_velo, itor%i_velo
!        Solution: idpdr%i_velo
!      subroutine const_grad_poloidal_magne
!        Input:    ipol%i_magne, itor%i_magne
!        Solution: idpdr%i_magne
!
      module const_sph_radial_grad
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_sph_phys_address
      use m_control_params_sph_MHD
      use cal_sph_exp_1st_diff
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_grad_temp
!
      use cal_sph_exp_fixed_scalar
      use cal_sph_exp_fixed_flux
!
      integer(kind = kint) :: kr_st, kr_ed
!
!
      kr_st = nlayer_ICB+1
      kr_ed = nlayer_CMB-1
      call cal_sph_nod_gradient_2(kr_st, kr_ed,                         &
     &    d_rj(1,ipol%i_temp), d_rj(1,ipol%i_grad_t) )
!
      if (iflag_icb_temp .eq. iflag_fixed_flux) then
        call cal_dsdr_sph_icb_fix_flux_2(nidx_rj(2), h_flux_ICB_bc,     &
     &      ipol%i_temp, ipol%i_grad_t)
      else
        call cal_dsdr_sph_icb_fix_scalar_2(nidx_rj(2), temp_ICB_bc,     &
     &      ipol%i_temp, ipol%i_grad_t)
      end if
!
      if (iflag_cmb_temp .eq. iflag_fixed_flux) then
        call cal_dsdr_sph_cmb_fix_flux_2(nidx_rj(2), h_flux_CMB_bc,     &
     &      ipol%i_temp, ipol%i_grad_t)
      else
        call cal_dsdr_sph_cmb_fix_scalar_2(nidx_rj(2), temp_CMB_bc,     &
     &      ipol%i_temp, ipol%i_grad_t)
      end if
!
      end subroutine const_radial_grad_temp
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_grad_d_scalar
!
      use cal_sph_exp_fixed_scalar
      use cal_sph_exp_fixed_flux
      use cal_sph_exp_rotation
!
      integer(kind = kint) :: kr_st, kr_ed
!
!
      kr_st = nlayer_ICB+1
      kr_ed = nlayer_CMB-1
      call cal_sph_nod_gradient_2(kr_st, kr_ed, d_rj(1,ipol%i_light),   &
     &    d_rj(1,ipol%i_grad_composit) )
!
      if (iflag_icb_temp .eq. iflag_fixed_flux) then
        call cal_dsdr_sph_icb_fix_flux_2(nidx_rj(2), c_flux_ICB_bc,     &
     &      ipol%i_light, ipol%i_grad_composit)
      else
        call cal_dsdr_sph_icb_fix_scalar_2(nidx_rj(2),                  &
     &      composition_ICB_bc, ipol%i_light, ipol%i_grad_composit)
      end if
!
      if (iflag_cmb_temp .eq. iflag_fixed_flux) then
        call cal_dsdr_sph_cmb_fix_flux_2(nidx_rj(2), c_flux_CMB_bc,     &
     &      ipol%i_light, ipol%i_grad_composit)
      else
        call cal_dsdr_sph_cmb_fix_scalar_2(nidx_rj(2),                  &
     &       composition_CMB_bc, ipol%i_light, ipol%i_grad_composit)
      end if
!
      end subroutine const_radial_grad_d_scalar
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_grad_vp_and_vorticity
!
      use set_sph_exp_rigid_ICB
      use set_sph_exp_rigid_CMB
      use set_sph_exp_free_ICB
      use set_sph_exp_free_CMB
      use cal_sph_exp_rotation
!
      integer(kind = kint) :: kr_st, kr_ed
!
!
      if     (iflag_icb_velocity .eq. iflag_free_slip) then
        call cal_sph_nod_icb_free_v_and_w(ipol%i_velo, ipol%i_vort)
      else if(iflag_icb_velocity .eq. iflag_rotatable_ic) then
        call cal_sph_nod_icb_rotate_velo2(ipol%i_velo)
        call cal_sph_nod_icb_rigid_rot2(ipol%i_velo, ipol%i_vort)
      else
        call cal_sph_nod_icb_rigid_velo2(ipol%i_velo)
        call cal_sph_nod_icb_rigid_rot2(ipol%i_velo, ipol%i_vort)
      end if
!
      if(iflag_cmb_velocity .eq. iflag_free_slip) then
        call cal_sph_nod_cmb_free_v_and_w(ipol%i_velo, ipol%i_vort)
      else
        call cal_sph_nod_cmb_rigid_v_and_w(ipol%i_velo, ipol%i_vort)
      end if
!
      kr_st = nlayer_ICB+1
      kr_ed = nlayer_CMB-1
      call cal_sph_diff_pol_and_rot2(kr_st, kr_ed,                      &
     &    ipol%i_velo, ipol%i_vort)
!
      end subroutine const_grad_vp_and_vorticity
!
! -----------------------------------------------------------------------
!
      subroutine const_grad_bp_and_current
!
      use cal_sph_exp_nod_icb_ins
      use cal_sph_exp_nod_cmb_ins
      use cal_sph_exp_nod_icb_qvac
      use cal_sph_exp_nod_cmb_qvac
      use set_sph_exp_nod_center
      use extend_potential_field
      use cal_sph_exp_rotation
!
      integer(kind = kint) :: kr_st, kr_ed
!
!
      if(iflag_icb_magne .eq. iflag_sph_fill_center) then
        kr_st = itwo
        call cal_sph_nod_center_b_and_j(ipol%i_magne, ipol%i_current)
      else if(iflag_icb_magne .eq. iflag_pseudo_vacuum) then
        kr_st = nlayer_ICB+1
        call cal_sph_nod_icb_qvc_b_and_j(ipol%i_magne, ipol%i_current)
      else
        kr_st = nlayer_ICB+1
        call cal_sph_nod_icb_ins_b_and_j(ipol%i_magne, ipol%i_current)
      end if
!
      kr_ed = nlayer_CMB-1
      if(iflag_cmb_magne .eq. iflag_pseudo_vacuum) then
        call cal_sph_nod_cmb_qvc_b_and_j(ipol%i_magne, ipol%i_current)
      else
        call cal_sph_nod_cmb_ins_b_and_j(ipol%i_magne, ipol%i_current)
      end if
!
!
      call cal_sph_diff_pol_and_rot2(kr_st, kr_ed, ipol%i_magne,        &
     &    ipol%i_current)
!
!      Extend potential field
      call ext_outside_potential_with_j(ipol%i_magne, ipol%i_current,   &
     &    nlayer_CMB)
      if(iflag_icb_magne .eq. iflag_sph_insulator) then
        call ext_inside_potential_with_j(ipol%i_magne, ipol%i_current,  &
     &      nlayer_ICB)
      end if
!
      end subroutine const_grad_bp_and_current
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_grad_poloidal_velo
!
      use set_sph_exp_rigid_ICB
      use set_sph_exp_rigid_CMB
      use set_sph_exp_free_ICB
      use set_sph_exp_free_CMB
      use cal_sph_exp_rotation
!
      integer(kind = kint) :: kr_st, kr_ed
!
!
      if     (iflag_icb_velocity .eq. iflag_free_slip) then
        call cal_sph_nod_icb_free_vpol2(ipol%i_velo)
      else if(iflag_icb_velocity .eq. iflag_rotatable_ic) then
        call cal_sph_nod_icb_rotate_velo2(ipol%i_velo)
      else
        call cal_sph_nod_icb_rigid_velo2(ipol%i_velo)
      end if
!
      if(iflag_cmb_velocity .eq. iflag_free_slip) then
        call cal_sph_nod_cmb_free_vpol2(ipol%i_velo)
      else
        call cal_sph_nod_cmb_rigid_velo2(ipol%i_velo)
      end if
!
      kr_st = nlayer_ICB+1
      kr_ed = nlayer_CMB-1
      call cal_sph_diff_poloidal(kr_st, kr_ed, ipol%i_velo)
!
      end subroutine const_grad_poloidal_velo
!
! -----------------------------------------------------------------------
!
      subroutine const_grad_poloidal_magne
!
      use cal_sph_exp_nod_icb_ins
      use cal_sph_exp_nod_cmb_ins
      use cal_sph_exp_nod_cmb_qvac
      use cal_sph_exp_nod_icb_qvac
      use set_sph_exp_nod_center
      use extend_potential_field
      use cal_sph_exp_rotation
!
      integer(kind = kint) :: kr_st, kr_ed
!
!
      if(iflag_icb_magne .eq. iflag_sph_fill_center) then
        kr_st = itwo
        call cal_dsdr_sph_center_2(ipol%i_magne)
      else if(iflag_icb_magne .eq. iflag_pseudo_vacuum) then
        kr_st = nlayer_ICB+1
        call cal_sph_nod_icb_qvc_mag2(ipol%i_magne)
      else
        kr_st = nlayer_ICB+1
        call cal_sph_nod_icb_ins_mag2(ipol%i_magne)
      end if
!
      kr_ed = nlayer_CMB-1
      if(iflag_cmb_magne .eq. iflag_pseudo_vacuum) then
        call cal_sph_nod_cmb_qvc_mag2(ipol%i_magne)
      else
        call cal_sph_nod_cmb_ins_mag2(ipol%i_magne)
      end if
!
!
      call cal_sph_diff_poloidal(kr_st, kr_ed, ipol%i_magne)
!
!      Extend potential field
      call ext_outside_potential(ipol%i_magne, nlayer_CMB)
      if(iflag_icb_magne .eq. iflag_sph_insulator) then
        call ext_inside_potential(ipol%i_magne, nlayer_ICB)
      end if
!
      end subroutine const_grad_poloidal_magne
!
! -----------------------------------------------------------------------
!
      end module const_sph_radial_grad
