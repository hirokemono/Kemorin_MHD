!>@file   set_sph_phys_address.f90
!!@brief  module set_sph_phys_address
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2007
!
!>@brief  start addresses for spetr fields
!!
!!@verbatim
!!      subroutine set_sph_sprctr_data_address                          &
!!     &         (sph_rj, ipol, idpdr, itor, rj_fld)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_address), intent(inout) :: ipol, idpdr, itor
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module set_sph_phys_address
!
      use m_precision
      use m_constants
!
      use t_phys_address
!
      implicit  none
!
      private :: set_sph_vect_spec_address, set_vect_sph_address
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine set_sph_sprctr_data_address                            &
     &         (sph_rj, ipol, idpdr, itor, rj_fld)
!
      use t_spheric_rj_data
      use t_phys_data
!
      use set_field_address
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_address), intent(inout) :: ipol, idpdr, itor
      type(phys_data), intent(inout) :: rj_fld
!
!   set address of spectr fields
!
      call alloc_phys_data_type(sph_rj%nnod_rj, rj_fld)
      call set_field_addresses(ione, rj_fld%num_phys,                   &
     &    rj_fld%phys_name, rj_fld%num_component, ipol)
      call set_sph_vect_spec_address(ipol, idpdr, itor)
!
      end subroutine set_sph_sprctr_data_address
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine set_sph_vect_spec_address(ipol, idpdr, itor)
!
      type(phys_address), intent(inout) :: ipol, idpdr, itor
!
!
      call set_vect_sph_address(ipol%i_velo, itor%i_velo, idpdr%i_velo)
      call set_vect_sph_address(ipol%i_vort, itor%i_vort, idpdr%i_vort)
!
      call set_vect_sph_address(ipol%i_magne, itor%i_magne,             &
     &    idpdr%i_magne)
      call set_vect_sph_address(ipol%i_vecp, itor%i_vecp,idpdr%i_vecp)
      call set_vect_sph_address(ipol%i_current, itor%i_current,         &
     &    idpdr%i_current)
!
      call set_vect_sph_address(ipol%i_filter_velo, itor%i_filter_velo, &
     &    idpdr%i_filter_velo)
      call set_vect_sph_address(ipol%i_filter_vort, itor%i_filter_vort, &
     &    idpdr%i_filter_vort)
      call set_vect_sph_address(ipol%i_filter_vecp, itor%i_filter_vecp, &
     &    idpdr%i_filter_vecp)
      call set_vect_sph_address(ipol%i_filter_magne,                    &
     &    itor%i_filter_magne, idpdr%i_filter_magne)
      call set_vect_sph_address(ipol%i_filter_current,                  &
     &    itor%i_filter_current, idpdr%i_filter_current)
!
      call set_vect_sph_address(ipol%i_truncated_B,                     &
     &    itor%i_truncated_B, idpdr%i_truncated_B)
!
      call set_vect_sph_address(ipol%wide_filter_fld%i_velo,            &
     &    itor%wide_filter_fld%i_velo, idpdr%wide_filter_fld%i_velo)
      call set_vect_sph_address(ipol%wide_filter_fld%i_vort,            &
     &    itor%wide_filter_fld%i_vort, idpdr%wide_filter_fld%i_vort)
      call set_vect_sph_address(ipol%wide_filter_fld%i_magne,           &
     &    itor%wide_filter_fld%i_magne, idpdr%wide_filter_fld%i_magne)
      call set_vect_sph_address(ipol%wide_filter_fld%i_current,         &
     &    itor%wide_filter_fld%i_current,                               &
     &    idpdr%wide_filter_fld%i_current)
      call set_vect_sph_address(ipol%wide_filter_grad%i_grad_temp,      &
     &    itor%wide_filter_grad%i_grad_temp,                            &
     &    idpdr%wide_filter_grad%i_grad_temp)
      call set_vect_sph_address(ipol%wide_filter_grad%i_grad_composit,  &
     &    itor%wide_filter_grad%i_grad_composit,                        &
     &    idpdr%wide_filter_grad%i_grad_composit)
!
      call set_vect_sph_address(ipol%dbl_filter_fld%i_velo,             &
     &    itor%dbl_filter_fld%i_velo, idpdr%dbl_filter_fld%i_velo)
      call set_vect_sph_address(ipol%dbl_filter_fld%i_vort,             &
     &    itor%dbl_filter_fld%i_vort, idpdr%dbl_filter_fld%i_vort)
      call set_vect_sph_address(ipol%dbl_filter_fld%i_magne,            &
     &    itor%dbl_filter_fld%i_magne, idpdr%dbl_filter_fld%i_magne)
      call set_vect_sph_address(ipol%dbl_filter_fld%i_current,          &
     &    itor%dbl_filter_fld%i_current,                                &
     &    idpdr%dbl_filter_fld%i_current)
!
      call set_vect_sph_address(ipol%dbl_filter_grad%i_grad_temp,       &
     &    itor%dbl_filter_grad%i_grad_temp,                             &
     &    idpdr%dbl_filter_grad%i_grad_temp)
      call set_vect_sph_address(ipol%dbl_filter_grad%i_grad_composit,   &
     &    itor%dbl_filter_grad%i_grad_composit,                         &
     &    idpdr%dbl_filter_grad%i_grad_composit)
!
      call set_vect_sph_address(ipol%i_v_diffuse, itor%i_v_diffuse,     &
     &    idpdr%i_v_diffuse)
      call set_vect_sph_address(ipol%i_w_diffuse, itor%i_w_diffuse,     &
     &    idpdr%i_w_diffuse)
      call set_vect_sph_address(ipol%i_vp_diffuse, itor%i_vp_diffuse,   &
     &    idpdr%i_vp_diffuse)
      call set_vect_sph_address(ipol%i_b_diffuse, itor%i_b_diffuse,     &
     &    idpdr%i_b_diffuse)
!
      call set_vect_sph_address(ipol%forces%i_h_flux,                   &
     &    itor%forces%i_h_flux, idpdr%forces%i_h_flux)
      call set_vect_sph_address(ipol%forces%i_ph_flux,                  &
     &    itor%forces%i_ph_flux, idpdr%forces%i_ph_flux)
      call set_vect_sph_address(ipol%forces%i_c_flux,                   &
     &    itor%forces%i_c_flux, idpdr%forces%i_c_flux)
      call set_vect_sph_address(ipol%forces%i_pc_flux,                  &
     &    itor%forces%i_pc_flux, idpdr%forces%i_pc_flux)
!
      call set_vect_sph_address(ipol%forces%i_m_advect,                 &
     &    itor%forces%i_m_advect, idpdr%forces%i_m_advect)
      call set_vect_sph_address(ipol%div_forces%i_m_flux,               &
     &    itor%div_forces%i_m_flux, idpdr%div_forces%i_m_flux)
      call set_vect_sph_address(ipol%div_forces%i_maxwell,              &
     &    itor%div_forces%i_maxwell, idpdr%div_forces%i_maxwell)
      call set_vect_sph_address(ipol%div_forces%i_induct_t,             &
     &    itor%div_forces%i_induct_t, idpdr%div_forces%i_induct_t)
      call set_vect_sph_address(ipol%forces%i_induction,                &
     &    itor%forces%i_induction, idpdr%forces%i_induction)
      call set_vect_sph_address(ipol%forces%i_vp_induct,                &
     &    itor%forces%i_vp_induct, idpdr%forces%i_vp_induct)
      call set_vect_sph_address(ipol%forces%i_mag_stretch,              &
     &    itor%forces%i_mag_stretch, idpdr%forces%i_mag_stretch)
      call set_vect_sph_address(ipol%forces%i_m_tension,                &
     &    itor%forces%i_m_tension, idpdr%forces%i_m_tension)
      call set_vect_sph_address(ipol%forces%i_lorentz,                  &
     &    itor%forces%i_lorentz, idpdr%forces%i_lorentz)
      call set_vect_sph_address(ipol%forces%i_coriolis,                 &
     &    itor%forces%i_coriolis, idpdr%forces%i_coriolis)
      call set_vect_sph_address(ipol%forces%i_buoyancy,                 &
     &    itor%forces%i_buoyancy, idpdr%forces%i_buoyancy)
      call set_vect_sph_address(ipol%forces%i_comp_buo,                 &
     &    itor%forces%i_comp_buo, idpdr%forces%i_comp_buo)
      call set_vect_sph_address(ipol%force_by_filter%i_buoyancy,        &
     &    itor%force_by_filter%i_buoyancy,                              &
     &    idpdr%force_by_filter%i_buoyancy)
      call set_vect_sph_address(ipol%force_by_filter%i_comp_buo,        &
     &    itor%force_by_filter%i_comp_buo,                              &
     &    idpdr%force_by_filter%i_comp_buo)
!
      call set_vect_sph_address(ipol%rot_forces%i_m_advect,             &
     &    itor%rot_forces%i_m_advect, idpdr%rot_forces%i_m_advect)
      call set_vect_sph_address(ipol%rot_forces%i_lorentz,              &
     &    itor%rot_forces%i_lorentz, idpdr%rot_forces%i_lorentz)
      call set_vect_sph_address(ipol%rot_forces%i_Coriolis,             &
     &    itor%rot_forces%i_Coriolis, idpdr%rot_forces%i_Coriolis)
      call set_vect_sph_address(ipol%rot_forces%i_buoyancy,             &
     &    itor%rot_forces%i_buoyancy, idpdr%rot_forces%i_buoyancy)
      call set_vect_sph_address(ipol%rot_forces%i_comp_buo,             &
     &    itor%rot_forces%i_comp_buo, idpdr%rot_forces%i_comp_buo)
      call set_vect_sph_address(ipol%rot_frc_by_filter%i_buoyancy,      &
     &    itor%rot_frc_by_filter%i_buoyancy,                            &
     &    idpdr%rot_frc_by_filter%i_buoyancy)
!
      call set_vect_sph_address(ipol%grad_fld%i_grad_temp,              &
     &    itor%grad_fld%i_grad_temp, idpdr%grad_fld%i_grad_temp)
      call set_vect_sph_address(ipol%grad_fld%i_grad_per_t,             &
     &    itor%grad_fld%i_grad_per_t, idpdr%grad_fld%i_grad_per_t)
      call set_vect_sph_address(ipol%grad_fld%i_grad_composit,          &
     &    itor%grad_fld%i_grad_composit,                                &
     &    idpdr%grad_fld%i_grad_composit)
      call set_vect_sph_address(ipol%grad_fil_fld%i_grad_temp,          &
     &    itor%grad_fil_fld%i_grad_temp,                                &
     &    idpdr%grad_fil_fld%i_grad_temp)
      call set_vect_sph_address(ipol%grad_fld%i_grad_per_c,             &
     &    itor%grad_fld%i_grad_per_c, idpdr%grad_fld%i_grad_per_c)
!
!
      call set_vect_sph_address(ipol%diff_fil_vect%i_grad_vx,           &
     &    itor%diff_fil_vect%i_grad_vx,  idpdr%diff_fil_vect%i_grad_vx)
      call set_vect_sph_address(ipol%diff_fil_vect%i_grad_vy,           &
     &    itor%diff_fil_vect%i_grad_vy,  idpdr%diff_fil_vect%i_grad_vy)
      call set_vect_sph_address(ipol%diff_fil_vect%i_grad_vz,           &
     &    itor%diff_fil_vect%i_grad_vz,  idpdr%diff_fil_vect%i_grad_vz)
      call set_vect_sph_address(ipol%diff_fil_vect%i_grad_wx,           &
     &    itor%diff_fil_vect%i_grad_wx,  idpdr%diff_fil_vect%i_grad_wx)
      call set_vect_sph_address(ipol%diff_fil_vect%i_grad_wy,           &
     &    itor%diff_fil_vect%i_grad_wy,  idpdr%diff_fil_vect%i_grad_wy)
      call set_vect_sph_address(ipol%diff_fil_vect%i_grad_wz,           &
     &    itor%diff_fil_vect%i_grad_wz,  idpdr%diff_fil_vect%i_grad_wz)
!
      call set_vect_sph_address(ipol%diff_fil_vect%i_grad_ax,           &
     &    itor%diff_fil_vect%i_grad_ax,  idpdr%diff_fil_vect%i_grad_ax)
      call set_vect_sph_address(ipol%diff_fil_vect%i_grad_ay,           &
     &    itor%diff_fil_vect%i_grad_ay,  idpdr%diff_fil_vect%i_grad_ay)
      call set_vect_sph_address(ipol%diff_fil_vect%i_grad_az,           &
     &    itor%diff_fil_vect%i_grad_az,  idpdr%diff_fil_vect%i_grad_az)
      call set_vect_sph_address(ipol%diff_fil_vect%i_grad_bx,           &
     &    itor%diff_fil_vect%i_grad_bx,  idpdr%diff_fil_vect%i_grad_bx)
      call set_vect_sph_address(ipol%diff_fil_vect%i_grad_by,           &
     &    itor%diff_fil_vect%i_grad_by,  idpdr%diff_fil_vect%i_grad_by)
      call set_vect_sph_address(ipol%diff_fil_vect%i_grad_bz,           &
     &    itor%diff_fil_vect%i_grad_bz,  idpdr%diff_fil_vect%i_grad_bz)
      call set_vect_sph_address(ipol%diff_fil_vect%i_grad_jx,           &
     &    itor%diff_fil_vect%i_grad_jx,  idpdr%diff_fil_vect%i_grad_jx)
      call set_vect_sph_address(ipol%diff_fil_vect%i_grad_jy,           &
     &    itor%diff_fil_vect%i_grad_jy,  idpdr%diff_fil_vect%i_grad_jy)
      call set_vect_sph_address(ipol%diff_fil_vect%i_grad_jz,           &
     &    itor%diff_fil_vect%i_grad_jz,  idpdr%diff_fil_vect%i_grad_jz)
!
!
      call set_vect_sph_address(ipol%diff_vector%i_grad_vx,             &
     &    itor%diff_vector%i_grad_vx, idpdr%diff_vector%i_grad_vx)
      call set_vect_sph_address(ipol%diff_vector%i_grad_vy,             &
     &    itor%diff_vector%i_grad_vy, idpdr%diff_vector%i_grad_vy)
      call set_vect_sph_address(ipol%diff_vector%i_grad_vz,             &
     &    itor%diff_vector%i_grad_vz, idpdr%diff_vector%i_grad_vz)
      call set_vect_sph_address(ipol%diff_vector%i_grad_wx,             &
     &    itor%diff_vector%i_grad_wx, idpdr%diff_vector%i_grad_wx)
      call set_vect_sph_address(ipol%diff_vector%i_grad_wy,             &
     &    itor%diff_vector%i_grad_wy, idpdr%diff_vector%i_grad_wy)
      call set_vect_sph_address(ipol%diff_vector%i_grad_wz,             &
     &    itor%diff_vector%i_grad_wz, idpdr%diff_vector%i_grad_wz)
!
      call set_vect_sph_address(ipol%diff_vector%i_grad_ax,             &
     &    itor%diff_vector%i_grad_ax, idpdr%diff_vector%i_grad_ax)
      call set_vect_sph_address(ipol%diff_vector%i_grad_ay,             &
     &    itor%diff_vector%i_grad_ay, idpdr%diff_vector%i_grad_ay)
      call set_vect_sph_address(ipol%diff_vector%i_grad_az,             &
     &    itor%diff_vector%i_grad_az, idpdr%diff_vector%i_grad_az)
      call set_vect_sph_address(ipol%diff_vector%i_grad_bx,             &
     &    itor%diff_vector%i_grad_bx, idpdr%diff_vector%i_grad_bx)
      call set_vect_sph_address(ipol%diff_vector%i_grad_by,             &
     &    itor%diff_vector%i_grad_by, idpdr%diff_vector%i_grad_by)
      call set_vect_sph_address(ipol%diff_vector%i_grad_bz,             &
     &    itor%diff_vector%i_grad_bz, idpdr%diff_vector%i_grad_bz)
      call set_vect_sph_address(ipol%diff_vector%i_grad_jx,             &
     &    itor%diff_vector%i_grad_jx, idpdr%diff_vector%i_grad_jx)
      call set_vect_sph_address(ipol%diff_vector%i_grad_jy,             &
     &    itor%diff_vector%i_grad_jy, idpdr%diff_vector%i_grad_jy)
      call set_vect_sph_address(ipol%diff_vector%i_grad_jz,             &
     &    itor%diff_vector%i_grad_jz, idpdr%diff_vector%i_grad_jz)
!
!
      call set_vect_sph_address(ipol%SGS_term%i_SGS_h_flux,             &
     &    itor%SGS_term%i_SGS_h_flux, idpdr%SGS_term%i_SGS_h_flux)
      call set_vect_sph_address(ipol%SGS_term%i_SGS_c_flux,             &
     &    itor%SGS_term%i_SGS_c_flux, idpdr%SGS_term%i_SGS_c_flux)
      call set_vect_sph_address(ipol%SGS_term%i_SGS_inertia,            &
     &    itor%SGS_term%i_SGS_inertia, idpdr%SGS_term%i_SGS_inertia)
      call set_vect_sph_address(ipol%SGS_term%i_SGS_Lorentz,            &
     &    itor%SGS_term%i_SGS_Lorentz, idpdr%SGS_term%i_SGS_Lorentz)
      call set_vect_sph_address(ipol%SGS_term%i_SGS_vp_induct,          &
     &    itor%SGS_term%i_SGS_vp_induct,                                &
     &    idpdr%SGS_term%i_SGS_vp_induct)
!
      call set_vect_sph_address(ipol%wide_SGS%i_SGS_h_flux,             &
     &    itor%wide_SGS%i_SGS_h_flux, idpdr%wide_SGS%i_SGS_h_flux)
      call set_vect_sph_address(ipol%wide_SGS%i_SGS_c_flux,             &
     &    itor%wide_SGS%i_SGS_c_flux, idpdr%wide_SGS%i_SGS_c_flux)
      call set_vect_sph_address(ipol%wide_SGS%i_SGS_inertia,            &
     &    itor%wide_SGS%i_SGS_inertia, idpdr%wide_SGS%i_SGS_inertia)
      call set_vect_sph_address(ipol%wide_SGS%i_SGS_Lorentz,            &
     &    itor%wide_SGS%i_SGS_Lorentz, idpdr%wide_SGS%i_SGS_Lorentz)
      call set_vect_sph_address(ipol%wide_SGS%i_SGS_vp_induct,          &
     &    itor%wide_SGS%i_SGS_vp_induct,                                &
     &    idpdr%wide_SGS%i_SGS_vp_induct)
!
      call set_vect_sph_address(ipol%dble_SGS%i_SGS_h_flux,             &
     &    itor%dble_SGS%i_SGS_h_flux, idpdr%dble_SGS%i_SGS_h_flux)
      call set_vect_sph_address(ipol%dble_SGS%i_SGS_c_flux,             &
     &    itor%dble_SGS%i_SGS_c_flux, idpdr%dble_SGS%i_SGS_c_flux)
      call set_vect_sph_address(ipol%dble_SGS%i_SGS_inertia,            &
     &    itor%dble_SGS%i_SGS_inertia, idpdr%dble_SGS%i_SGS_inertia)
      call set_vect_sph_address(ipol%dble_SGS%i_SGS_Lorentz,            &
     &    itor%dble_SGS%i_SGS_Lorentz, idpdr%dble_SGS%i_SGS_Lorentz)
      call set_vect_sph_address(ipol%dble_SGS%i_SGS_vp_induct,          &
     &    itor%dble_SGS%i_SGS_vp_induct,                                &
     &    idpdr%dble_SGS%i_SGS_vp_induct)
!
      call set_vect_sph_address(ipol%div_SGS%i_SGS_m_flux,              &
     &    itor%div_SGS%i_SGS_m_flux, idpdr%div_SGS%i_SGS_m_flux)
      call set_vect_sph_address(ipol%SGS_term%i_SGS_induction,          &
     &    itor%SGS_term%i_SGS_induction,                                &
     &    idpdr%SGS_term%i_SGS_induction)
      call set_vect_sph_address(ipol%SGS_term%i_SGS_buoyancy,           &
     &    itor%SGS_term%i_SGS_buoyancy, idpdr%SGS_term%i_SGS_buoyancy)
      call set_vect_sph_address(ipol%SGS_term%i_SGS_comp_buo,           &
     &    itor%SGS_term%i_SGS_comp_buo, idpdr%SGS_term%i_SGS_comp_buo)
      call set_vect_sph_address(ipol%rot_SGS%i_SGS_inertia,             &
     &    itor%rot_SGS%i_SGS_inertia, idpdr%rot_SGS%i_SGS_inertia)
      call set_vect_sph_address(ipol%rot_SGS%i_SGS_Lorentz,             &
     &    itor%rot_SGS%i_SGS_Lorentz, idpdr%rot_SGS%i_SGS_Lorentz)
!
      call set_vect_sph_address(ipol%i_forces, itor%i_forces,           &
     &    idpdr%i_forces)
      call set_vect_sph_address(ipol%i_rot_forces, itor%i_rot_forces,   &
     &    idpdr%i_rot_forces)
!
      call set_vect_sph_address(ipol%i_pre_mom, itor%i_pre_mom,         &
     &    idpdr%i_pre_mom)
      call set_vect_sph_address(ipol%i_pre_uxb, itor%i_pre_uxb,         &
     &    idpdr%i_pre_uxb)
!
      call set_vect_sph_address(ipol%i_chk_mom, itor%i_chk_mom,         &
     &    idpdr%i_chk_mom)
      call set_vect_sph_address(ipol%i_chk_uxb, itor%i_chk_uxb,         &
     &    idpdr%i_chk_uxb)
!
      call set_vect_sph_address(ipol%i_chk_mom_2, itor%i_chk_mom_2,     &
     &    idpdr%i_chk_mom_2)
      call set_vect_sph_address(ipol%i_chk_uxb_2, itor%i_chk_uxb_2,     &
     &    idpdr%i_chk_uxb_2)
!
      end subroutine set_sph_vect_spec_address
!
!  --------------------------------------------------------------------
!
      subroutine set_vect_sph_address(i_pol, i_tor, i_dpol)
!
      integer(kind = kint), intent(in) :: i_pol
      integer(kind = kint), intent(inout) :: i_tor, i_dpol
!
!
      if(i_pol .le. 0) return
      i_tor =  i_pol + 2
      i_dpol = i_pol + 1
!
      end subroutine set_vect_sph_address
!
!  --------------------------------------------------------------------
!
      end module set_sph_phys_address
