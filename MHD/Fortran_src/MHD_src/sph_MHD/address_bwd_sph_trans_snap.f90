!>@file   address_bwd_sph_trans_snap.f90
!!@brief  module address_bwd_sph_trans_snap
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine b_trans_address_vector_snap                          &
!!     &         (fl_prop, cd_prop, ht_prop, cp_prop, ipol, trns_snap)
!!      subroutine b_trans_address_scalar_snap                          &
!!     &         (ht_prop, cp_prop, ipol, trns_snap)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in)  :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(phys_address), intent(in) :: ipol
!!        type(address_4_sph_trans), intent(inout) :: trns_snap
!!
!!      subroutine set_b_trans_vector_field_snap                        &
!!     &         (icou, ipol, itor, iphys, trns_snap)
!!      subroutine set_b_trans_scalar_field_snap                        &
!!     &         (icou, ipol, itor, iphys, trns_snap)
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_snap
!!@endverbatim
!
      module address_bwd_sph_trans_snap
!
      use m_precision
!
      use m_phys_labels
      use t_phys_address
      use t_addresses_sph_transform
      use t_control_parameter
      use t_physical_property
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine b_trans_address_vector_snap(ipol, iphys, trns_snap)
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_4_sph_trans), intent(inout) :: trns_snap
!
      trns_snap%backward%num_vector = 0
!      if(b_trns%i_velo .eq. 0) then
        call add_vec_trans_flag_snap(ipol%i_velo, iphys%i_velo,         &
     &      trns_snap%backward%num_vector, trns_snap%b_trns%i_velo)
!      end if
!      if(b_trns%i_vort .eq. 0) then
        call add_vec_trans_flag_snap(ipol%i_vort, iphys%i_vort,         &
     &      trns_snap%backward%num_vector, trns_snap%b_trns%i_vort)
!      end if
!      if(b_trns%i_magne .eq. 0) then
        call add_vec_trans_flag_snap(ipol%i_magne, iphys%i_magne,       &
     &      trns_snap%backward%num_vector, trns_snap%b_trns%i_magne)
!      end if
!      if(b_trns%i_current .eq. 0) then
        call add_vec_trans_flag_snap(ipol%i_current, iphys%i_current,   &
     &      trns_snap%backward%num_vector, trns_snap%b_trns%i_current)
!      end if
!
      call add_vec_trans_flag_snap(ipol%i_v_diffuse, iphys%i_v_diffuse, &
     &    trns_snap%backward%num_vector, trns_snap%b_trns%i_v_diffuse)
      call add_vec_trans_flag_snap(ipol%i_w_diffuse, iphys%i_w_diffuse, &
     &    trns_snap%backward%num_vector, trns_snap%b_trns%i_w_diffuse)
      call add_vec_trans_flag_snap                                      &
     &   (ipol%i_vp_diffuse, iphys%i_vp_diffuse,                        &
     &    trns_snap%backward%num_vector, trns_snap%b_trns%i_vp_diffuse)
      call add_vec_trans_flag_snap(ipol%i_b_diffuse, iphys%i_b_diffuse, &
     &    trns_snap%backward%num_vector, trns_snap%b_trns%i_b_diffuse)
!
      call add_vec_trans_flag_snap                                      &
     &   (ipol%i_rot_inertia, iphys%i_rot_inertia,                      &
     &    trns_snap%backward%num_vector, trns_snap%b_trns%i_rot_inertia)
      call add_vec_trans_flag_snap(ipol%i_rot_Coriolis,                 &
     &    iphys%i_rot_Coriolis, trns_snap%backward%num_vector,             &
     &    trns_snap%b_trns%i_rot_Coriolis)
      call add_vec_trans_flag_snap                                      &
     &   (ipol%i_rot_Lorentz, iphys%i_rot_Lorentz,                      &
     &    trns_snap%backward%num_vector, trns_snap%b_trns%i_rot_Lorentz)
      call add_vec_trans_flag_snap(ipol%i_rot_buoyancy,                 &
     &    iphys%i_rot_buoyancy, trns_snap%backward%num_vector,             &
     &    trns_snap%b_trns%i_rot_buoyancy)
      call add_vec_trans_flag_snap(ipol%i_rot_comp_buo,                 &
     &    iphys%i_rot_comp_buo, trns_snap%backward%num_vector,             &
     &    trns_snap%b_trns%i_rot_comp_buo)
!
!      call add_vec_trans_flag_snap(ipol%i_SGS_inertia,                 &
!     &    iphys%i_SGS_inertia, trns_snap%backward%num_vector,             &
!     &    trns_snap%b_trns%i_SGS_inertia)
!      call add_vec_trans_flag_snap(ipol%i_SGS_Lorentz,                 &
!     &    iphys%i_SGS_Lorentz, trns_snap%backward%num_vector,             &
!     &    trns_snap%b_trns%i_SGS_Lorentz)
!      call add_vec_trans_flag_snap(ipol%i_SGS_vp_induct,               &
!     &    iphys%i_SGS_vp_induct, trns_snap%backward%num_vector,           &
!     &    trns_snap%b_trns%i_SGS_vp_induct)
!      call add_vec_trans_flag_snap(ipol%i_SGS_h_flux,                  &
!     &    iphys%i_SGS_h_flux, trns_snap%backward%num_vector,              &
!     &    trns_snap%b_trns%i_SGS_h_flux)
!      call add_vec_trans_flag_snap(ipol%i_SGS_c_flux,                  &
!     &    iphys%i_SGS_c_flux, trns_snap%backward%num_vector,              &
!     &    trns_snap%b_trns%i_SGS_c_flux)
!
      call add_vec_trans_flag_snap(ipol%i_buoyancy,                     &
     &    iphys%i_buoyancy, trns_snap%backward%num_vector,                 &
     &    trns_snap%b_trns%i_buoyancy)
      call add_vec_trans_flag_snap(ipol%i_comp_buo,                     &
     &    iphys%i_comp_buo, trns_snap%backward%num_vector,                 &
     &    trns_snap%b_trns%i_comp_buo)
!
      call add_vec_trans_flag_snap(ipol%i_geostrophic,                  &
     &    iphys%i_geostrophic, trns_snap%backward%num_vector,              &
     &    trns_snap%b_trns%i_geostrophic)
!
      call add_vec_trans_flag_snap(ipol%i_h_flux_w_sgs,                 &
     &    iphys%i_h_flux_w_sgs, trns_snap%backward%num_vector,             &
     &    trns_snap%b_trns%i_h_flux_w_sgs)
      call add_vec_trans_flag_snap(ipol%i_c_flux_w_sgs,                 &
     &    iphys%i_c_flux_w_sgs, trns_snap%backward%num_vector,             &
     &    trns_snap%b_trns%i_c_flux_w_sgs)
      call add_vec_trans_flag_snap(ipol%i_inertia_w_sgs,                &
     &    iphys%i_inertia_w_sgs, trns_snap%backward%num_vector,            &
     &    trns_snap%b_trns%i_inertia_w_sgs)
      call add_vec_trans_flag_snap(ipol%i_Lorentz_w_sgs,                &
     &    iphys%i_Lorentz_w_sgs, trns_snap%backward%num_vector,            &
     &    trns_snap%b_trns%i_Lorentz_w_sgs)
      call add_vec_trans_flag_snap(ipol%i_vp_induct_w_sgs,              &
     &    iphys%i_vp_induct_w_sgs, trns_snap%backward%num_vector,          &
     &    trns_snap%b_trns%i_vp_induct_w_sgs)
      call add_vec_trans_flag_snap(ipol%i_mag_induct_w_sgs,             &
     &    iphys%i_mag_induct_w_sgs, trns_snap%backward%num_vector,         &
     &    trns_snap%b_trns%i_mag_induct_w_sgs)
!
      call add_vec_trans_flag_snap(ipol%i_SGS_rot_inertia,              &
     &    iphys%i_SGS_rot_inertia, trns_snap%backward%num_vector,          &
     &    trns_snap%b_trns%i_SGS_rot_inertia)
      call add_vec_trans_flag_snap(ipol%i_SGS_rot_Lorentz,              &
     &    iphys%i_SGS_rot_Lorentz, trns_snap%backward%num_vector,          &
     &    trns_snap%b_trns%i_SGS_rot_Lorentz)
      call add_vec_trans_flag_snap(ipol%i_SGS_induction,                &
     &    iphys%i_SGS_induction, trns_snap%backward%num_vector,            &
     &    trns_snap%b_trns%i_SGS_induction)
!
      call add_vec_trans_flag_snap                                      &
     &   (ipol%i_press_grad, iphys%i_press_grad,                        &
     &    trns_snap%backward%num_vector, trns_snap%b_trns%i_press_grad)
      call add_vec_trans_flag_snap(ipol%i_induction, iphys%i_induction, &
     &    trns_snap%backward%num_vector, trns_snap%b_trns%i_induction)
!
      call add_vec_trans_flag_snap(ipol%i_grad_t, iphys%i_grad_t,       &
     &    trns_snap%backward%num_vector, trns_snap%b_trns%i_grad_t)
      call add_vec_trans_flag_snap(ipol%i_grad_composit,                &
     &    iphys%i_grad_composit, trns_snap%backward%num_vector,            &
     &    trns_snap%b_trns%i_grad_composit)
!
      call add_vec_trans_flag_snap(ipol%i_grad_vx, iphys%i_grad_vx,     &
     &    trns_snap%backward%num_vector, trns_snap%b_trns%i_grad_vx)
      call add_vec_trans_flag_snap(ipol%i_grad_vy, iphys%i_grad_vy,     &
     &    trns_snap%backward%num_vector, trns_snap%b_trns%i_grad_vy)
      call add_vec_trans_flag_snap(ipol%i_grad_vz, iphys%i_grad_vz,     &
     &    trns_snap%backward%num_vector, trns_snap%b_trns%i_grad_vz)
!
      call add_vec_trans_flag_snap(ipol%i_grad_wx, iphys%i_grad_wx,     &
     &    trns_snap%backward%num_vector, trns_snap%b_trns%i_grad_wx)
      call add_vec_trans_flag_snap(ipol%i_grad_wy, iphys%i_grad_wy,     &
     &    trns_snap%backward%num_vector, trns_snap%b_trns%i_grad_wy)
      call add_vec_trans_flag_snap(ipol%i_grad_wz, iphys%i_grad_wz,     &
     &    trns_snap%backward%num_vector, trns_snap%b_trns%i_grad_wz)
!
      call add_vec_trans_flag_snap(ipol%i_grad_ax, iphys%i_grad_ax,     &
     &    trns_snap%backward%num_vector, trns_snap%b_trns%i_grad_ax)
      call add_vec_trans_flag_snap(ipol%i_grad_ay, iphys%i_grad_ay,     &
     &    trns_snap%backward%num_vector, trns_snap%b_trns%i_grad_ay)
      call add_vec_trans_flag_snap(ipol%i_grad_az, iphys%i_grad_az,     &
     &    trns_snap%backward%num_vector, trns_snap%b_trns%i_grad_az)
!
      call add_vec_trans_flag_snap(ipol%i_grad_bx, iphys%i_grad_vx,     &
     &    trns_snap%backward%num_vector, trns_snap%b_trns%i_grad_vx)
      call add_vec_trans_flag_snap(ipol%i_grad_by, iphys%i_grad_vy,     &
     &    trns_snap%backward%num_vector, trns_snap%b_trns%i_grad_vy)
      call add_vec_trans_flag_snap(ipol%i_grad_bz, iphys%i_grad_bz,     &
     &    trns_snap%backward%num_vector, trns_snap%b_trns%i_grad_bz)
!
      call add_vec_trans_flag_snap(ipol%i_grad_jx, iphys%i_grad_jx,     &
     &    trns_snap%backward%num_vector, trns_snap%b_trns%i_grad_jx)
      call add_vec_trans_flag_snap(ipol%i_grad_jy, iphys%i_grad_jy,     &
     &    trns_snap%backward%num_vector, trns_snap%b_trns%i_grad_jy)
      call add_vec_trans_flag_snap(ipol%i_grad_jz, iphys%i_grad_jz,     &
     &    trns_snap%backward%num_vector, trns_snap%b_trns%i_grad_jz)
!
      end subroutine b_trans_address_vector_snap
!
!-----------------------------------------------------------------------
!
      subroutine b_trans_address_scalar_snap(ipol, iphys, trns_snap)
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_4_sph_trans), intent(inout) :: trns_snap
!
!
      trns_snap%backward%num_scalar = 0
!      if(b_trns%i_temp.eq.0 .or. ipol%i_par_temp.gt.0) then
        call add_scl_trans_flag_snap(ipol%i_temp, iphys%i_temp,         &
     &    trns_snap%backward%num_vector, trns_snap%backward%num_scalar,       &
     &    trns_snap%b_trns%i_temp)
!      end if
!      if(b_trns%i_light .eq. 0) then
        call add_scl_trans_flag_snap(ipol%i_light, iphys%i_light,       &
     &    trns_snap%backward%num_vector, trns_snap%backward%num_scalar,       &
     &    trns_snap%b_trns%i_light)
!      end if
!
      call add_scl_trans_flag_snap(ipol%i_press, iphys%i_press,         &
     &    trns_snap%backward%num_vector, trns_snap%backward%num_scalar,       &
     &    trns_snap%b_trns%i_press)
      call add_scl_trans_flag_snap(ipol%i_par_temp, iphys%i_par_temp,   &
     &    trns_snap%backward%num_vector, trns_snap%backward%num_scalar,       &
     &    trns_snap%b_trns%i_par_temp)
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_filter_temp, iphys%i_filter_temp,                      &
     &    trns_snap%backward%num_vector, trns_snap%backward%num_scalar,       &
     &    trns_snap%b_trns%i_filter_temp)
      call add_scl_trans_flag_snap(ipol%i_t_diffuse, iphys%i_t_diffuse, &
     &    trns_snap%backward%num_vector, trns_snap%backward%num_scalar,       &
     &    trns_snap%b_trns%i_t_diffuse)
      call add_scl_trans_flag_snap(ipol%i_c_diffuse, iphys%i_c_diffuse, &
     &    trns_snap%backward%num_vector, trns_snap%backward%num_scalar,       &
     &    trns_snap%b_trns%i_c_diffuse)
!
      call add_scl_trans_flag_snap(ipol%i_h_advect, iphys%i_h_advect,   &
     &    trns_snap%backward%num_vector, trns_snap%backward%num_scalar,       &
     &    trns_snap%b_trns%i_h_advect)
      call add_scl_trans_flag_snap(ipol%i_c_advect, iphys%i_c_advect,   &
     &    trns_snap%backward%num_vector, trns_snap%backward%num_scalar,       &
     &    trns_snap%b_trns%i_c_advect)
!
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_div_Coriolis, iphys%i_div_Coriolis,                    &
     &    trns_snap%backward%num_vector, trns_snap%backward%num_scalar,       &
     &    trns_snap%b_trns%i_div_Coriolis)
!
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_SGS_div_inertia, iphys%i_SGS_div_inertia,              &
     &    trns_snap%backward%num_vector, trns_snap%backward%num_scalar,       &
     &    trns_snap%b_trns%i_SGS_div_inertia)
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_SGS_div_Lorentz, iphys%i_SGS_div_Lorentz,              &
     &    trns_snap%backward%num_vector, trns_snap%backward%num_scalar,       &
     &    trns_snap%b_trns%i_SGS_div_Lorentz)
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_SGS_div_h_flux, iphys%i_SGS_div_h_flux,                &
     &    trns_snap%backward%num_vector, trns_snap%backward%num_scalar,       &
     &    trns_snap%b_trns%i_SGS_div_h_flux)
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_SGS_div_c_flux, iphys%i_SGS_div_c_flux,                &
     &    trns_snap%backward%num_vector, trns_snap%backward%num_scalar,       &
     &    trns_snap%b_trns%i_SGS_div_c_flux)
!
      end subroutine b_trans_address_scalar_snap
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_b_trans_vector_field_snap                          &
     &         (icou, ipol, itor, iphys, trns_snap)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_4_sph_trans), intent(inout) :: trns_snap
      integer(kind = kint), intent(inout) :: icou
!
!   velocity flag
      call set_field_name_4_bwd_trns(fhd_velo, trns_snap%b_trns%i_velo, &
     &    ipol%i_velo, itor%i_velo, iphys%i_velo, icou, trns_snap)
      call set_field_name_4_bwd_trns(fhd_vort, trns_snap%b_trns%i_vort, &
     &    ipol%i_vort, itor%i_vort, iphys%i_vort, icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_magne, trns_snap%b_trns%i_magne,                          &
     &    ipol%i_magne, itor%i_magne, iphys%i_magne, icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_current, trns_snap%b_trns%i_current,                      &
     &    ipol%i_current, itor%i_current, iphys%i_current,              &
     &    icou, trns_snap)
!
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_viscous, trns_snap%b_trns%i_v_diffuse,                    &
     &    ipol%i_v_diffuse, itor%i_v_diffuse, iphys%i_v_diffuse,        &
     &    icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_w_viscous, trns_snap%b_trns%i_w_diffuse,                  &
     &    ipol%i_w_diffuse, itor%i_w_diffuse, iphys%i_w_diffuse,        &
     &    icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_vecp_diffuse, trns_snap%b_trns%i_vp_diffuse,              &
     &    ipol%i_vp_diffuse, itor%i_vp_diffuse, iphys%i_vp_diffuse,     &
     &    icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_mag_diffuse, trns_snap%b_trns%i_b_diffuse,                &
     &    ipol%i_b_diffuse, itor%i_b_diffuse, iphys%i_b_diffuse,        &
     &    icou, trns_snap)
!
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_rot_inertia, trns_snap%b_trns%i_rot_inertia,              &
     &    ipol%i_rot_inertia, itor%i_rot_inertia, iphys%i_rot_inertia,  &
     &    icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_rot_Coriolis, trns_snap%b_trns%i_rot_Coriolis,            &
     &    ipol%i_rot_Coriolis, itor%i_rot_Coriolis,                     &
     &    iphys%i_rot_Coriolis, icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_rot_Lorentz, trns_snap%b_trns%i_rot_Lorentz,              &
     &    ipol%i_rot_Lorentz, itor%i_rot_Lorentz, iphys%i_rot_Lorentz,  &
     &     icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_rot_buoyancy, trns_snap%b_trns%i_rot_buoyancy,            &
     &    ipol%i_rot_buoyancy, itor%i_rot_buoyancy,                     &
     &    iphys%i_rot_buoyancy, icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_rot_comp_buo, trns_snap%b_trns%i_rot_comp_buo,            &
     &    ipol%i_rot_comp_buo, itor%i_rot_comp_buo,                     &
     &    iphys%i_rot_comp_buo, icou, trns_snap)
!
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_SGS_inertia, trns_snap%b_trns%i_SGS_inertia,              &
     &    ipol%i_SGS_inertia, itor%i_SGS_inertia, iphys%i_SGS_inertia,  &
     &    icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_SGS_Lorentz, trns_snap%b_trns%i_SGS_Lorentz,              &
     &    ipol%i_SGS_Lorentz, itor%i_SGS_Lorentz, iphys%i_SGS_Lorentz,  &
     &    icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_SGS_vp_induct, trns_snap%b_trns%i_SGS_vp_induct,          &
     &    ipol%i_SGS_vp_induct, itor%i_SGS_vp_induct,                   &
     &    iphys%i_SGS_vp_induct, icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_SGS_h_flux, trns_snap%b_trns%i_SGS_h_flux,                &
     &    ipol%i_SGS_h_flux, itor%i_SGS_h_flux, iphys%i_SGS_h_flux,     &
     &    icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_SGS_c_flux, trns_snap%b_trns%i_SGS_c_flux,                &
     &    ipol%i_SGS_c_flux, itor%i_SGS_c_flux, iphys%i_SGS_c_flux,     &
     &    icou, trns_snap)
!
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_buoyancy, trns_snap%b_trns%i_buoyancy,                    &
     &    ipol%i_buoyancy, itor%i_buoyancy, iphys%i_buoyancy,           &
     &    icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_comp_buo, trns_snap%b_trns%i_comp_buo,                    &
     &    ipol%i_comp_buo, itor%i_comp_buo, iphys%i_comp_buo,           &
     &    icou, trns_snap)
!
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_geostrophic, trns_snap%b_trns%i_geostrophic,              &
     &    ipol%i_geostrophic, itor%i_geostrophic, iphys%i_geostrophic,  &
     &    icou, trns_snap)
!
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_h_flux_w_sgs, trns_snap%b_trns%i_h_flux_w_sgs,            &
     &    ipol%i_h_flux_w_sgs, itor%i_h_flux_w_sgs,                     &
     &    iphys%i_h_flux_w_sgs, icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_c_flux_w_sgs, trns_snap%b_trns%i_c_flux_w_sgs,            &
     &    ipol%i_c_flux_w_sgs, itor%i_c_flux_w_sgs,                     &
     &    iphys%i_c_flux_w_sgs, icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_inertia_w_sgs, trns_snap%b_trns%i_inertia_w_sgs,          &
     &    ipol%i_inertia_w_sgs, itor%i_inertia_w_sgs,                   &
     &    iphys%i_inertia_w_sgs, icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_Lorentz_w_sgs, trns_snap%b_trns%i_Lorentz_w_sgs,          &
     &    ipol%i_Lorentz_w_sgs, itor%i_Lorentz_w_sgs,                   &
     &    iphys%i_Lorentz_w_sgs, icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_vp_induct_w_sgs, trns_snap%b_trns%i_vp_induct_w_sgs,      &
     &    ipol%i_vp_induct_w_sgs, itor%i_vp_induct_w_sgs,               &
     &    iphys%i_vp_induct_w_sgs, icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_mag_induct_w_sgs, trns_snap%b_trns%i_mag_induct_w_sgs,    &
     &    ipol%i_mag_induct_w_sgs, itor%i_mag_induct_w_sgs,             &
     &    iphys%i_mag_induct_w_sgs, icou, trns_snap)
!
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_SGS_rot_inertia, trns_snap%b_trns%i_SGS_rot_inertia,      &
     &    ipol%i_SGS_rot_inertia, itor%i_SGS_rot_inertia,               &
     &    iphys%i_SGS_rot_inertia, icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_SGS_rot_Lorentz, trns_snap%b_trns%i_SGS_rot_Lorentz,      &
     &    ipol%i_SGS_rot_Lorentz, itor%i_SGS_rot_Lorentz,               &
     &    iphys%i_SGS_rot_Lorentz, icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_SGS_induction, trns_snap%b_trns%i_SGS_induction,          &
     &    ipol%i_SGS_induction, itor%i_SGS_induction,                   &
     &    iphys%i_SGS_induction, icou, trns_snap)
!
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_press_grad, trns_snap%b_trns%i_press_grad,                &
     &    ipol%i_press_grad, itor%i_press_grad, iphys%i_press_grad,     &
     &    icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_mag_induct, trns_snap%b_trns%i_induction,                 &
     &    ipol%i_induction, itor%i_induction, iphys%i_induction,        &
     &    icou, trns_snap)
!
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_grad_temp, trns_snap%b_trns%i_grad_t, ipol%i_grad_t,      &
     &    itor%i_grad_t, iphys%i_grad_t, icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_grad_composit, trns_snap%b_trns%i_grad_composit,          &
     &    ipol%i_grad_composit, itor%i_grad_composit,                   &
     &    iphys%i_grad_composit, icou, trns_snap)
!
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_grad_v_1, trns_snap%b_trns%i_grad_vx,                     &
     &    ipol%i_grad_vx, itor%i_grad_vx, iphys%i_grad_vx,              &
     &    icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_grad_v_2, trns_snap%b_trns%i_grad_vy,                     &
     &    ipol%i_grad_vy, itor%i_grad_vy, iphys%i_grad_vy,              &
     &    icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_grad_v_3, trns_snap%b_trns%i_grad_vz,                     &
     &    ipol%i_grad_vz, itor%i_grad_vz, iphys%i_grad_vz,              &
     &    icou, trns_snap)
!
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_grad_w_1, trns_snap%b_trns%i_grad_wx,                     &
     &    ipol%i_grad_wx, itor%i_grad_wx, iphys%i_grad_wx,              &
     &    icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_grad_w_2, trns_snap%b_trns%i_grad_wy,                     &
     &    ipol%i_grad_wy, itor%i_grad_wy, iphys%i_grad_wy,              &
     &    icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_grad_w_3, trns_snap%b_trns%i_grad_wz,                     &
     &    ipol%i_grad_wz, itor%i_grad_wz, iphys%i_grad_wz,              &
     &    icou, trns_snap)
!
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_grad_a_1, trns_snap%b_trns%i_grad_ax,                     &
     &    ipol%i_grad_ax, itor%i_grad_ax, iphys%i_grad_ax,              &
     &    icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_grad_a_2, trns_snap%b_trns%i_grad_ay,                     &
     &    ipol%i_grad_ay, itor%i_grad_ay, iphys%i_grad_ay,              &
     &    icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_grad_a_3, trns_snap%b_trns%i_grad_az,                     &
     &    ipol%i_grad_az, itor%i_grad_az, iphys%i_grad_az,              &
     &    icou, trns_snap)
!
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_grad_b_1, trns_snap%b_trns%i_grad_bx,                     &
     &    ipol%i_grad_bx, itor%i_grad_bx, iphys%i_grad_bx,              &
     &    icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_grad_b_2, trns_snap%b_trns%i_grad_by,                     &
     &    ipol%i_grad_by, itor%i_grad_by, iphys%i_grad_by,              &
     &    icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_grad_b_3, trns_snap%b_trns%i_grad_bz,                     &
     &    ipol%i_grad_bz, itor%i_grad_bz, iphys%i_grad_bz,              &
     &    icou, trns_snap)
!
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_grad_j_1, trns_snap%b_trns%i_grad_jx,                     &
     &    ipol%i_grad_jx, itor%i_grad_jx, iphys%i_grad_jx,              &
     &    icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_grad_j_2, trns_snap%b_trns%i_grad_jy,                     &
     &    ipol%i_grad_jy, itor%i_grad_jy, iphys%i_grad_jy,              &
     &    icou, trns_snap)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_grad_j_3, trns_snap%b_trns%i_grad_jz,                     &
     &    ipol%i_grad_jz, itor%i_grad_jz, iphys%i_grad_jz,              &
     &    icou, trns_snap)
!
      end subroutine set_b_trans_vector_field_snap
!
!-----------------------------------------------------------------------
!
      subroutine set_b_trans_scalar_field_snap                          &
     &         (icou, ipol, itor, iphys, trns_snap)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_4_sph_trans), intent(inout) :: trns_snap
      integer(kind = kint), intent(inout) :: icou
!
!
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_temp, trns_snap%b_trns%i_temp, ipol%i_temp,               &
     &    itor%i_temp, iphys%i_temp, icou, trns_snap)
      call set_field_name_4_bwd_trns &
     &   (fhd_light, trns_snap%b_trns%i_light, ipol%i_light,            &
     &    itor%i_light, iphys%i_light, icou, trns_snap)
!
      call set_field_name_4_bwd_trns(fhd_press,                         &
     &    trns_snap%b_trns%i_press, ipol%i_press, itor%i_press,         &
     &    iphys%i_press, icou, trns_snap)
      call set_field_name_4_bwd_trns(fhd_part_temp,                     &
     &    trns_snap%b_trns%i_par_temp, ipol%i_par_temp,                 &
     &    itor%i_par_temp, iphys%i_par_temp, icou, trns_snap)
      call set_field_name_4_bwd_trns(fhd_filter_temp,                   &
     &    trns_snap%b_trns%i_filter_temp, ipol%i_filter_temp,           &
     &    itor%i_filter_temp, iphys%i_filter_temp, icou, trns_snap)
      call set_field_name_4_bwd_trns(fhd_thermal_diffusion,             &
     &    trns_snap%b_trns%i_t_diffuse, ipol%i_t_diffuse,               &
     &    itor%i_t_diffuse, iphys%i_t_diffuse, icou, trns_snap)
      call set_field_name_4_bwd_trns(fhd_c_diffuse,                     &
     &    trns_snap%b_trns%i_c_diffuse, ipol%i_c_diffuse,               &
     &    itor%i_c_diffuse, iphys%i_c_diffuse, icou, trns_snap)
!
      call set_field_name_4_bwd_trns(fhd_heat_advect,                   &
     &    trns_snap%b_trns%i_h_advect, ipol%i_h_advect,                 &
     &    itor%i_h_advect, iphys%i_h_advect, icou, trns_snap)
      call set_field_name_4_bwd_trns(fhd_composit_advect,               &
     &    trns_snap%b_trns%i_c_advect, ipol%i_c_advect,                 &
     &    itor%i_c_advect, iphys%i_c_advect, icou, trns_snap)
!
      call set_field_name_4_bwd_trns(fhd_div_Coriolis,                  &
     &    trns_snap%b_trns%i_div_Coriolis, ipol%i_div_Coriolis,         &
     &    itor%i_div_Coriolis, iphys%i_div_Coriolis, icou, trns_snap)
!
      call set_field_name_4_bwd_trns(fhd_SGS_div_inertia,               &
     &    trns_snap%b_trns%i_SGS_div_inertia, ipol%i_SGS_div_inertia,   &
     &    itor%i_SGS_div_inertia, iphys%i_SGS_div_inertia,              &
     &    icou, trns_snap)
      call set_field_name_4_bwd_trns(fhd_SGS_div_Lorentz,               &
     &    trns_snap%b_trns%i_SGS_div_Lorentz, ipol%i_SGS_div_Lorentz,   &
     &    itor%i_SGS_div_Lorentz, iphys%i_SGS_div_Lorentz,              &
     &    icou, trns_snap)
      call set_field_name_4_bwd_trns(fhd_div_SGS_h_flux,                &
     &    trns_snap%b_trns%i_SGS_div_h_flux, ipol%i_SGS_div_h_flux,     &
     &    itor%i_SGS_div_h_flux, iphys%i_SGS_div_h_flux,                &
     &    icou, trns_snap)
      call set_field_name_4_bwd_trns(fhd_div_SGS_c_flux,                &
     &    trns_snap%b_trns%i_SGS_div_c_flux, ipol%i_SGS_div_c_flux,     &
     &    itor%i_SGS_div_c_flux, iphys%i_SGS_div_c_flux,                &
     &    icou, trns_snap)
!
      end subroutine set_b_trans_scalar_field_snap
!
!-----------------------------------------------------------------------
!
      end module address_bwd_sph_trans_snap
