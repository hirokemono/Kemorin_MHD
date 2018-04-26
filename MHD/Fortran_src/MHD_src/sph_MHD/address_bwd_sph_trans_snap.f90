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
!!     &         (ipol, iphys, trns_back, b_trns)
!!      subroutine b_trans_address_scalar_snap                          &
!!     &         (ht_prop, cp_prop, ipol, trns_back)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in)  :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(phys_address), intent(in) :: ipol
!!        type(address_each_sph_trans), intent(inout) :: trns_back
!!        type(phys_address), intent(inout) :: b_trns
!!
!!      subroutine set_b_trans_vector_field_snap                        &
!!     &         (icou, b_trns, ipol, itor, iphys, trns_back)
!!      subroutine set_b_trans_scalar_field_snap                        &
!!     &         (icou, b_trns, ipol, itor, iphys, trns_back)
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(address_each_sph_trans), intent(inout) :: trns_back
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
      subroutine b_trans_address_vector_snap                            &
     &         (ipol, iphys, trns_back, b_trns)
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
      trns_back%num_vector = 0
!      if(b_trns%i_velo .eq. 0) then
        call add_vec_trans_flag_snap(ipol%i_velo, iphys%i_velo,         &
     &      trns_back%num_vector, b_trns%i_velo)
!      end if
!      if(b_trns%i_vort .eq. 0) then
        call add_vec_trans_flag_snap(ipol%i_vort, iphys%i_vort,         &
     &      trns_back%num_vector, b_trns%i_vort)
!      end if
!      if(b_trns%i_magne .eq. 0) then
        call add_vec_trans_flag_snap(ipol%i_magne, iphys%i_magne,       &
     &      trns_back%num_vector, b_trns%i_magne)
!      end if
!      if(b_trns%i_current .eq. 0) then
        call add_vec_trans_flag_snap(ipol%i_current, iphys%i_current,   &
     &      trns_back%num_vector, b_trns%i_current)
!      end if
!
      call add_vec_trans_flag_snap(ipol%i_v_diffuse, iphys%i_v_diffuse, &
     &    trns_back%num_vector, b_trns%i_v_diffuse)
      call add_vec_trans_flag_snap(ipol%i_w_diffuse, iphys%i_w_diffuse, &
     &    trns_back%num_vector, b_trns%i_w_diffuse)
      call add_vec_trans_flag_snap                                      &
     &   (ipol%i_vp_diffuse, iphys%i_vp_diffuse,                        &
     &    trns_back%num_vector, b_trns%i_vp_diffuse)
      call add_vec_trans_flag_snap(ipol%i_b_diffuse, iphys%i_b_diffuse, &
     &    trns_back%num_vector, b_trns%i_b_diffuse)
!
      call add_vec_trans_flag_snap                                      &
     &   (ipol%i_rot_inertia, iphys%i_rot_inertia,                      &
     &    trns_back%num_vector, b_trns%i_rot_inertia)
      call add_vec_trans_flag_snap(ipol%i_rot_Coriolis,                 &
     &    iphys%i_rot_Coriolis, trns_back%num_vector,                   &
     &    b_trns%i_rot_Coriolis)
      call add_vec_trans_flag_snap                                      &
     &   (ipol%i_rot_Lorentz, iphys%i_rot_Lorentz,                      &
     &    trns_back%num_vector, b_trns%i_rot_Lorentz)
      call add_vec_trans_flag_snap(ipol%i_rot_buoyancy,                 &
     &    iphys%i_rot_buoyancy, trns_back%num_vector,                   &
     &    b_trns%i_rot_buoyancy)
      call add_vec_trans_flag_snap(ipol%i_rot_comp_buo,                 &
     &    iphys%i_rot_comp_buo, trns_back%num_vector,                   &
     &    b_trns%i_rot_comp_buo)
!
!      call add_vec_trans_flag_snap(ipol%i_SGS_inertia,                 &
!     &    iphys%i_SGS_inertia, trns_back%num_vector,                   &
!     &    b_trns%i_SGS_inertia)
!      call add_vec_trans_flag_snap(ipol%i_SGS_Lorentz,                 &
!     &    iphys%i_SGS_Lorentz, trns_back%num_vector,                   &
!     &    b_trns%i_SGS_Lorentz)
!      call add_vec_trans_flag_snap(ipol%i_SGS_vp_induct,               &
!     &    iphys%i_SGS_vp_induct, trns_back%num_vector,                 &
!     &    b_trns%i_SGS_vp_induct)
!      call add_vec_trans_flag_snap(ipol%i_SGS_h_flux,                  &
!     &    iphys%i_SGS_h_flux, trns_back%num_vector,                    &
!     &    b_trns%i_SGS_h_flux)
!      call add_vec_trans_flag_snap(ipol%i_SGS_c_flux,                  &
!     &    iphys%i_SGS_c_flux, trns_back%num_vector,                    &
!     &    b_trns%i_SGS_c_flux)
!
      call add_vec_trans_flag_snap(ipol%i_buoyancy,                     &
     &    iphys%i_buoyancy, trns_back%num_vector,                       &
     &    b_trns%i_buoyancy)
      call add_vec_trans_flag_snap(ipol%i_comp_buo,                     &
     &    iphys%i_comp_buo, trns_back%num_vector,                       &
     &    b_trns%i_comp_buo)
!
      call add_vec_trans_flag_snap(ipol%i_geostrophic,                  &
     &    iphys%i_geostrophic, trns_back%num_vector,                    &
     &    b_trns%i_geostrophic)
!
      call add_vec_trans_flag_snap(ipol%i_h_flux_w_sgs,                 &
     &    iphys%i_h_flux_w_sgs, trns_back%num_vector,                   &
     &    b_trns%i_h_flux_w_sgs)
      call add_vec_trans_flag_snap(ipol%i_c_flux_w_sgs,                 &
     &    iphys%i_c_flux_w_sgs, trns_back%num_vector,                   &
     &    b_trns%i_c_flux_w_sgs)
      call add_vec_trans_flag_snap(ipol%i_inertia_w_sgs,                &
     &    iphys%i_inertia_w_sgs, trns_back%num_vector,                  &
     &    b_trns%i_inertia_w_sgs)
      call add_vec_trans_flag_snap(ipol%i_Lorentz_w_sgs,                &
     &    iphys%i_Lorentz_w_sgs, trns_back%num_vector,                  &
     &    b_trns%i_Lorentz_w_sgs)
      call add_vec_trans_flag_snap(ipol%i_vp_induct_w_sgs,              &
     &    iphys%i_vp_induct_w_sgs, trns_back%num_vector,                &
     &    b_trns%i_vp_induct_w_sgs)
      call add_vec_trans_flag_snap(ipol%i_mag_induct_w_sgs,             &
     &    iphys%i_mag_induct_w_sgs, trns_back%num_vector,               &
     &    b_trns%i_mag_induct_w_sgs)
!
      call add_vec_trans_flag_snap(ipol%i_SGS_rot_inertia,              &
     &    iphys%i_SGS_rot_inertia, trns_back%num_vector,                &
     &    b_trns%i_SGS_rot_inertia)
      call add_vec_trans_flag_snap(ipol%i_SGS_rot_Lorentz,              &
     &    iphys%i_SGS_rot_Lorentz, trns_back%num_vector,                &
     &    b_trns%i_SGS_rot_Lorentz)
      call add_vec_trans_flag_snap(ipol%i_SGS_induction,                &
     &    iphys%i_SGS_induction, trns_back%num_vector,                  &
     &    b_trns%i_SGS_induction)
!
      call add_vec_trans_flag_snap                                      &
     &   (ipol%i_press_grad, iphys%i_press_grad,                        &
     &    trns_back%num_vector, b_trns%i_press_grad)
      call add_vec_trans_flag_snap(ipol%i_induction, iphys%i_induction, &
     &    trns_back%num_vector, b_trns%i_induction)
!
      call add_vec_trans_flag_snap(ipol%i_grad_t, iphys%i_grad_t,       &
     &    trns_back%num_vector, b_trns%i_grad_t)
      call add_vec_trans_flag_snap(ipol%i_grad_composit,                &
     &    iphys%i_grad_composit, trns_back%num_vector,                  &
     &    b_trns%i_grad_composit)
!
      call add_vec_trans_flag_snap(ipol%i_grad_vx, iphys%i_grad_vx,     &
     &    trns_back%num_vector, b_trns%i_grad_vx)
      call add_vec_trans_flag_snap(ipol%i_grad_vy, iphys%i_grad_vy,     &
     &    trns_back%num_vector, b_trns%i_grad_vy)
      call add_vec_trans_flag_snap(ipol%i_grad_vz, iphys%i_grad_vz,     &
     &    trns_back%num_vector, b_trns%i_grad_vz)
!
      call add_vec_trans_flag_snap(ipol%i_grad_wx, iphys%i_grad_wx,     &
     &    trns_back%num_vector, b_trns%i_grad_wx)
      call add_vec_trans_flag_snap(ipol%i_grad_wy, iphys%i_grad_wy,     &
     &    trns_back%num_vector, b_trns%i_grad_wy)
      call add_vec_trans_flag_snap(ipol%i_grad_wz, iphys%i_grad_wz,     &
     &    trns_back%num_vector, b_trns%i_grad_wz)
!
      call add_vec_trans_flag_snap(ipol%i_grad_ax, iphys%i_grad_ax,     &
     &    trns_back%num_vector, b_trns%i_grad_ax)
      call add_vec_trans_flag_snap(ipol%i_grad_ay, iphys%i_grad_ay,     &
     &    trns_back%num_vector, b_trns%i_grad_ay)
      call add_vec_trans_flag_snap(ipol%i_grad_az, iphys%i_grad_az,     &
     &    trns_back%num_vector, b_trns%i_grad_az)
!
      call add_vec_trans_flag_snap(ipol%i_grad_bx, iphys%i_grad_vx,     &
     &    trns_back%num_vector, b_trns%i_grad_vx)
      call add_vec_trans_flag_snap(ipol%i_grad_by, iphys%i_grad_vy,     &
     &    trns_back%num_vector, b_trns%i_grad_vy)
      call add_vec_trans_flag_snap(ipol%i_grad_bz, iphys%i_grad_bz,     &
     &    trns_back%num_vector, b_trns%i_grad_bz)
!
      call add_vec_trans_flag_snap(ipol%i_grad_jx, iphys%i_grad_jx,     &
     &    trns_back%num_vector, b_trns%i_grad_jx)
      call add_vec_trans_flag_snap(ipol%i_grad_jy, iphys%i_grad_jy,     &
     &    trns_back%num_vector, b_trns%i_grad_jy)
      call add_vec_trans_flag_snap(ipol%i_grad_jz, iphys%i_grad_jz,     &
     &    trns_back%num_vector, b_trns%i_grad_jz)
!
      end subroutine b_trans_address_vector_snap
!
!-----------------------------------------------------------------------
!
      subroutine b_trans_address_scalar_snap(ipol, iphys, trns_back, b_trns)
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
      trns_back%num_scalar = 0
!      if(b_trns%i_temp.eq.0 .or. ipol%i_par_temp.gt.0) then
        call add_scl_trans_flag_snap(ipol%i_temp, iphys%i_temp,         &
     &    trns_back%num_vector, trns_back%num_scalar,                   &
     &    b_trns%i_temp)
!      end if
!      if(b_trns%i_light .eq. 0) then
        call add_scl_trans_flag_snap(ipol%i_light, iphys%i_light,       &
     &    trns_back%num_vector, trns_back%num_scalar,                   &
     &    b_trns%i_light)
!      end if
!
      call add_scl_trans_flag_snap(ipol%i_press, iphys%i_press,         &
     &    trns_back%num_vector, trns_back%num_scalar,                   &
     &    b_trns%i_press)
      call add_scl_trans_flag_snap(ipol%i_par_temp, iphys%i_par_temp,   &
     &    trns_back%num_vector, trns_back%num_scalar,                   &
     &    b_trns%i_par_temp)
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_filter_temp, iphys%i_filter_temp,                      &
     &    trns_back%num_vector, trns_back%num_scalar,                   &
     &    b_trns%i_filter_temp)
      call add_scl_trans_flag_snap(ipol%i_t_diffuse, iphys%i_t_diffuse, &
     &    trns_back%num_vector, trns_back%num_scalar,                   &
     &    b_trns%i_t_diffuse)
      call add_scl_trans_flag_snap(ipol%i_c_diffuse, iphys%i_c_diffuse, &
     &    trns_back%num_vector, trns_back%num_scalar,                   &
     &    b_trns%i_c_diffuse)
!
      call add_scl_trans_flag_snap(ipol%i_h_advect, iphys%i_h_advect,   &
     &    trns_back%num_vector, trns_back%num_scalar,                   &
     &    b_trns%i_h_advect)
      call add_scl_trans_flag_snap(ipol%i_c_advect, iphys%i_c_advect,   &
     &    trns_back%num_vector, trns_back%num_scalar,                   &
     &    b_trns%i_c_advect)
!
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_div_Coriolis, iphys%i_div_Coriolis,                    &
     &    trns_back%num_vector, trns_back%num_scalar,                   &
     &    b_trns%i_div_Coriolis)
!
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_SGS_div_inertia, iphys%i_SGS_div_inertia,              &
     &    trns_back%num_vector, trns_back%num_scalar,                   &
     &    b_trns%i_SGS_div_inertia)
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_SGS_div_Lorentz, iphys%i_SGS_div_Lorentz,              &
     &    trns_back%num_vector, trns_back%num_scalar,                   &
     &    b_trns%i_SGS_div_Lorentz)
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_SGS_div_h_flux, iphys%i_SGS_div_h_flux,                &
     &    trns_back%num_vector, trns_back%num_scalar,                   &
     &    b_trns%i_SGS_div_h_flux)
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_SGS_div_c_flux, iphys%i_SGS_div_c_flux,                &
     &    trns_back%num_vector, trns_back%num_scalar,                   &
     &    b_trns%i_SGS_div_c_flux)
!
      end subroutine b_trans_address_scalar_snap
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_b_trans_vector_field_snap                          &
     &         (icou, b_trns, ipol, itor, iphys, trns_back)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(phys_address), intent(in) :: b_trns
      type(address_each_sph_trans), intent(inout) :: trns_back
      integer(kind = kint), intent(inout) :: icou
!
!   velocity flag
      call set_field_name_4_sph_trns(fhd_velo, b_trns%i_velo, &
     &    ipol%i_velo, itor%i_velo, iphys%i_velo, icou, trns_back)
      call set_field_name_4_sph_trns(fhd_vort, b_trns%i_vort, &
     &    ipol%i_vort, itor%i_vort, iphys%i_vort, icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_magne, b_trns%i_magne,                                    &
     &    ipol%i_magne, itor%i_magne, iphys%i_magne, icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_current, b_trns%i_current,                                &
     &    ipol%i_current, itor%i_current, iphys%i_current,              &
     &    icou, trns_back)
!
      call set_field_name_4_sph_trns                                    &
     &   (fhd_viscous, b_trns%i_v_diffuse,                              &
     &    ipol%i_v_diffuse, itor%i_v_diffuse, iphys%i_v_diffuse,        &
     &    icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_w_viscous, b_trns%i_w_diffuse,                            &
     &    ipol%i_w_diffuse, itor%i_w_diffuse, iphys%i_w_diffuse,        &
     &    icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_vecp_diffuse, b_trns%i_vp_diffuse,                        &
     &    ipol%i_vp_diffuse, itor%i_vp_diffuse, iphys%i_vp_diffuse,     &
     &    icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_mag_diffuse, b_trns%i_b_diffuse,                          &
     &    ipol%i_b_diffuse, itor%i_b_diffuse, iphys%i_b_diffuse,        &
     &    icou, trns_back)
!
      call set_field_name_4_sph_trns                                    &
     &   (fhd_rot_inertia, b_trns%i_rot_inertia,                        &
     &    ipol%i_rot_inertia, itor%i_rot_inertia, iphys%i_rot_inertia,  &
     &    icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_rot_Coriolis, b_trns%i_rot_Coriolis,                      &
     &    ipol%i_rot_Coriolis, itor%i_rot_Coriolis,                     &
     &    iphys%i_rot_Coriolis, icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_rot_Lorentz, b_trns%i_rot_Lorentz,                        &
     &    ipol%i_rot_Lorentz, itor%i_rot_Lorentz, iphys%i_rot_Lorentz,  &
     &     icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_rot_buoyancy, b_trns%i_rot_buoyancy,                      &
     &    ipol%i_rot_buoyancy, itor%i_rot_buoyancy,                     &
     &    iphys%i_rot_buoyancy, icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_rot_comp_buo, b_trns%i_rot_comp_buo,                      &
     &    ipol%i_rot_comp_buo, itor%i_rot_comp_buo,                     &
     &    iphys%i_rot_comp_buo, icou, trns_back)
!
      call set_field_name_4_sph_trns                                    &
     &   (fhd_SGS_inertia, b_trns%i_SGS_inertia,                        &
     &    ipol%i_SGS_inertia, itor%i_SGS_inertia, iphys%i_SGS_inertia,  &
     &    icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_SGS_Lorentz, b_trns%i_SGS_Lorentz,                        &
     &    ipol%i_SGS_Lorentz, itor%i_SGS_Lorentz, iphys%i_SGS_Lorentz,  &
     &    icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_SGS_vp_induct, b_trns%i_SGS_vp_induct,                    &
     &    ipol%i_SGS_vp_induct, itor%i_SGS_vp_induct,                   &
     &    iphys%i_SGS_vp_induct, icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_SGS_h_flux, b_trns%i_SGS_h_flux,                          &
     &    ipol%i_SGS_h_flux, itor%i_SGS_h_flux, iphys%i_SGS_h_flux,     &
     &    icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_SGS_c_flux, b_trns%i_SGS_c_flux,                          &
     &    ipol%i_SGS_c_flux, itor%i_SGS_c_flux, iphys%i_SGS_c_flux,     &
     &    icou, trns_back)
!
      call set_field_name_4_sph_trns                                    &
     &   (fhd_buoyancy, b_trns%i_buoyancy,                              &
     &    ipol%i_buoyancy, itor%i_buoyancy, iphys%i_buoyancy,           &
     &    icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_comp_buo, b_trns%i_comp_buo,                              &
     &    ipol%i_comp_buo, itor%i_comp_buo, iphys%i_comp_buo,           &
     &    icou, trns_back)
!
      call set_field_name_4_sph_trns                                    &
     &   (fhd_geostrophic, b_trns%i_geostrophic,                        &
     &    ipol%i_geostrophic, itor%i_geostrophic, iphys%i_geostrophic,  &
     &    icou, trns_back)
!
      call set_field_name_4_sph_trns                                    &
     &   (fhd_h_flux_w_sgs, b_trns%i_h_flux_w_sgs,                      &
     &    ipol%i_h_flux_w_sgs, itor%i_h_flux_w_sgs,                     &
     &    iphys%i_h_flux_w_sgs, icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_c_flux_w_sgs, b_trns%i_c_flux_w_sgs,                      &
     &    ipol%i_c_flux_w_sgs, itor%i_c_flux_w_sgs,                     &
     &    iphys%i_c_flux_w_sgs, icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_inertia_w_sgs, b_trns%i_inertia_w_sgs,                    &
     &    ipol%i_inertia_w_sgs, itor%i_inertia_w_sgs,                   &
     &    iphys%i_inertia_w_sgs, icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_Lorentz_w_sgs, b_trns%i_Lorentz_w_sgs,                    &
     &    ipol%i_Lorentz_w_sgs, itor%i_Lorentz_w_sgs,                   &
     &    iphys%i_Lorentz_w_sgs, icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_vp_induct_w_sgs, b_trns%i_vp_induct_w_sgs,                &
     &    ipol%i_vp_induct_w_sgs, itor%i_vp_induct_w_sgs,               &
     &    iphys%i_vp_induct_w_sgs, icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_mag_induct_w_sgs, b_trns%i_mag_induct_w_sgs,              &
     &    ipol%i_mag_induct_w_sgs, itor%i_mag_induct_w_sgs,             &
     &    iphys%i_mag_induct_w_sgs, icou, trns_back)
!
      call set_field_name_4_sph_trns                                    &
     &   (fhd_SGS_rot_inertia, b_trns%i_SGS_rot_inertia,                &
     &    ipol%i_SGS_rot_inertia, itor%i_SGS_rot_inertia,               &
     &    iphys%i_SGS_rot_inertia, icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_SGS_rot_Lorentz, b_trns%i_SGS_rot_Lorentz,                &
     &    ipol%i_SGS_rot_Lorentz, itor%i_SGS_rot_Lorentz,               &
     &    iphys%i_SGS_rot_Lorentz, icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_SGS_induction, b_trns%i_SGS_induction,                    &
     &    ipol%i_SGS_induction, itor%i_SGS_induction,                   &
     &    iphys%i_SGS_induction, icou, trns_back)
!
      call set_field_name_4_sph_trns                                    &
     &   (fhd_press_grad, b_trns%i_press_grad,                          &
     &    ipol%i_press_grad, itor%i_press_grad, iphys%i_press_grad,     &
     &    icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_mag_induct, b_trns%i_induction,                           &
     &    ipol%i_induction, itor%i_induction, iphys%i_induction,        &
     &    icou, trns_back)
!
      call set_field_name_4_sph_trns                                    &
     &   (fhd_grad_temp, b_trns%i_grad_t, ipol%i_grad_t,                &
     &    itor%i_grad_t, iphys%i_grad_t, icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_grad_composit, b_trns%i_grad_composit,                    &
     &    ipol%i_grad_composit, itor%i_grad_composit,                   &
     &    iphys%i_grad_composit, icou, trns_back)
!
      call set_field_name_4_sph_trns                                    &
     &   (fhd_grad_v_1, b_trns%i_grad_vx,                               &
     &    ipol%i_grad_vx, itor%i_grad_vx, iphys%i_grad_vx,              &
     &    icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_grad_v_2, b_trns%i_grad_vy,                               &
     &    ipol%i_grad_vy, itor%i_grad_vy, iphys%i_grad_vy,              &
     &    icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_grad_v_3, b_trns%i_grad_vz,                               &
     &    ipol%i_grad_vz, itor%i_grad_vz, iphys%i_grad_vz,              &
     &    icou, trns_back)
!
      call set_field_name_4_sph_trns                                    &
     &   (fhd_grad_w_1, b_trns%i_grad_wx,                               &
     &    ipol%i_grad_wx, itor%i_grad_wx, iphys%i_grad_wx,              &
     &    icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_grad_w_2, b_trns%i_grad_wy,                               &
     &    ipol%i_grad_wy, itor%i_grad_wy, iphys%i_grad_wy,              &
     &    icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_grad_w_3, b_trns%i_grad_wz,                               &
     &    ipol%i_grad_wz, itor%i_grad_wz, iphys%i_grad_wz,              &
     &    icou, trns_back)
!
      call set_field_name_4_sph_trns                                    &
     &   (fhd_grad_a_1, b_trns%i_grad_ax,                               &
     &    ipol%i_grad_ax, itor%i_grad_ax, iphys%i_grad_ax,              &
     &    icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_grad_a_2, b_trns%i_grad_ay,                               &
     &    ipol%i_grad_ay, itor%i_grad_ay, iphys%i_grad_ay,              &
     &    icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_grad_a_3, b_trns%i_grad_az,                               &
     &    ipol%i_grad_az, itor%i_grad_az, iphys%i_grad_az,              &
     &    icou, trns_back)
!
      call set_field_name_4_sph_trns                                    &
     &   (fhd_grad_b_1, b_trns%i_grad_bx,                               &
     &    ipol%i_grad_bx, itor%i_grad_bx, iphys%i_grad_bx,              &
     &    icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_grad_b_2, b_trns%i_grad_by,                               &
     &    ipol%i_grad_by, itor%i_grad_by, iphys%i_grad_by,              &
     &    icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_grad_b_3, b_trns%i_grad_bz,                               &
     &    ipol%i_grad_bz, itor%i_grad_bz, iphys%i_grad_bz,              &
     &    icou, trns_back)
!
      call set_field_name_4_sph_trns                                    &
     &   (fhd_grad_j_1, b_trns%i_grad_jx,                               &
     &    ipol%i_grad_jx, itor%i_grad_jx, iphys%i_grad_jx,              &
     &    icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_grad_j_2, b_trns%i_grad_jy,                               &
     &    ipol%i_grad_jy, itor%i_grad_jy, iphys%i_grad_jy,              &
     &    icou, trns_back)
      call set_field_name_4_sph_trns                                    &
     &   (fhd_grad_j_3, b_trns%i_grad_jz,                               &
     &    ipol%i_grad_jz, itor%i_grad_jz, iphys%i_grad_jz,              &
     &    icou, trns_back)
!
      end subroutine set_b_trans_vector_field_snap
!
!-----------------------------------------------------------------------
!
      subroutine set_b_trans_scalar_field_snap                          &
     &         (icou, b_trns, ipol, itor, iphys, trns_back)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(phys_address), intent(in) :: b_trns
      type(address_each_sph_trans), intent(inout) :: trns_back
      integer(kind = kint), intent(inout) :: icou
!
!
      call set_field_name_4_sph_trns                                    &
     &   (fhd_temp, b_trns%i_temp, ipol%i_temp,                         &
     &    itor%i_temp, iphys%i_temp, icou, trns_back)
      call set_field_name_4_sph_trns &
     &   (fhd_light, b_trns%i_light, ipol%i_light,                      &
     &    itor%i_light, iphys%i_light, icou, trns_back)
!
      call set_field_name_4_sph_trns(fhd_press,                         &
     &    b_trns%i_press, ipol%i_press, itor%i_press,                   &
     &    iphys%i_press, icou, trns_back)
      call set_field_name_4_sph_trns(fhd_part_temp,                     &
     &    b_trns%i_par_temp, ipol%i_par_temp,                           &
     &    itor%i_par_temp, iphys%i_par_temp, icou, trns_back)
      call set_field_name_4_sph_trns(fhd_filter_temp,                   &
     &    b_trns%i_filter_temp, ipol%i_filter_temp,                     &
     &    itor%i_filter_temp, iphys%i_filter_temp, icou, trns_back)
      call set_field_name_4_sph_trns(fhd_thermal_diffusion,             &
     &    b_trns%i_t_diffuse, ipol%i_t_diffuse,                         &
     &    itor%i_t_diffuse, iphys%i_t_diffuse, icou, trns_back)
      call set_field_name_4_sph_trns(fhd_c_diffuse,                     &
     &    b_trns%i_c_diffuse, ipol%i_c_diffuse,                         &
     &    itor%i_c_diffuse, iphys%i_c_diffuse, icou, trns_back)
!
      call set_field_name_4_sph_trns(fhd_heat_advect,                   &
     &    b_trns%i_h_advect, ipol%i_h_advect,                           &
     &    itor%i_h_advect, iphys%i_h_advect, icou, trns_back)
      call set_field_name_4_sph_trns(fhd_composit_advect,               &
     &    b_trns%i_c_advect, ipol%i_c_advect,                           &
     &    itor%i_c_advect, iphys%i_c_advect, icou, trns_back)
!
      call set_field_name_4_sph_trns(fhd_div_Coriolis,                  &
     &    b_trns%i_div_Coriolis, ipol%i_div_Coriolis,                   &
     &    itor%i_div_Coriolis, iphys%i_div_Coriolis, icou, trns_back)
!
      call set_field_name_4_sph_trns(fhd_SGS_div_inertia,               &
     &    b_trns%i_SGS_div_inertia, ipol%i_SGS_div_inertia,             &
     &    itor%i_SGS_div_inertia, iphys%i_SGS_div_inertia,              &
     &    icou, trns_back)
      call set_field_name_4_sph_trns(fhd_SGS_div_Lorentz,               &
     &    b_trns%i_SGS_div_Lorentz, ipol%i_SGS_div_Lorentz,             &
     &    itor%i_SGS_div_Lorentz, iphys%i_SGS_div_Lorentz,              &
     &    icou, trns_back)
      call set_field_name_4_sph_trns(fhd_div_SGS_h_flux,                &
     &    b_trns%i_SGS_div_h_flux, ipol%i_SGS_div_h_flux,               &
     &    itor%i_SGS_div_h_flux, iphys%i_SGS_div_h_flux,                &
     &    icou, trns_back)
      call set_field_name_4_sph_trns(fhd_div_SGS_c_flux,                &
     &    b_trns%i_SGS_div_c_flux, ipol%i_SGS_div_c_flux,               &
     &    itor%i_SGS_div_c_flux, iphys%i_SGS_div_c_flux,                &
     &    icou, trns_back)
!
      end subroutine set_b_trans_scalar_field_snap
!
!-----------------------------------------------------------------------
!
      end module address_bwd_sph_trans_snap
