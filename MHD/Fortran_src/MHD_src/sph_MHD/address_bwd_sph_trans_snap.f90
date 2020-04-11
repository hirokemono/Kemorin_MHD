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
!!     &         (ipol, iphys, b_trns, trns_back)
!!      subroutine b_trans_address_scalar_snap                          &
!!     &         (ipol, iphys, b_trns, trns_back)
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(address_each_sph_trans), intent(inout) :: trns_back
!!        type(phys_address), intent(inout) :: b_trns
!!@endverbatim
!
      module address_bwd_sph_trans_snap
!
      use m_precision
!
      use m_phys_labels
      use m_phys_constants
      use t_phys_address
      use t_addresses_sph_transform
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
     &         (ipol, iphys, b_trns, trns_back)
!
      use m_diff_SGS_term_labels
      use m_force_w_SGS_labels
      use add_base_field_4_sph_trns
      use add_prod_field_4_sph_trns
      use add_diff_vect_to_sph_trans
      use add_field_to_sph_trans_list
      use add_SGS_term_to_sph_trans
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
      trns_back%nfield = 0
      call alloc_sph_trns_field_name(trns_back)
!
      call add_base_vector_sph_trns_snap                                &
     &   (ipol%base, iphys%base, b_trns%base, trns_back)
!
      call add_field_name_4_sph_trns_snap(viscous_diffusion,            &
     &    ipol%diffusion%i_v_diffuse, iphys%diffusion%i_v_diffuse,      &
     &    b_trns%diffusion%i_v_diffuse, trns_back)
      call add_field_name_4_sph_trns_snap(vorticity_diffusion,          &
     &    ipol%diffusion%i_w_diffuse, iphys%diffusion%i_w_diffuse,      &
     &    b_trns%diffusion%i_w_diffuse, trns_back)
      call add_field_name_4_sph_trns_snap(vector_potential_diffusion,   &
     &    ipol%diffusion%i_vp_diffuse, iphys%diffusion%i_vp_diffuse,    &
     &    b_trns%diffusion%i_vp_diffuse, trns_back)
      call add_field_name_4_sph_trns_snap(magnetic_diffusion,           &
     &    ipol%diffusion%i_b_diffuse, iphys%diffusion%i_b_diffuse,      &
     &    b_trns%diffusion%i_b_diffuse, trns_back)
!
      call add_field_name_4_sph_trns_snap(rot_inertia,                  &
     &    ipol%rot_forces%i_m_advect, iphys%rot_forces%i_m_advect,      &
     &    b_trns%rot_forces%i_m_advect, trns_back)
      call add_field_name_4_sph_trns_snap(rot_Coriolis_force,           &
     &    ipol%rot_forces%i_Coriolis, iphys%rot_forces%i_Coriolis,      &
     &    b_trns%rot_forces%i_Coriolis, trns_back)
      call add_field_name_4_sph_trns_snap(rot_Lorentz_force,            &
     &    ipol%rot_forces%i_lorentz, iphys%rot_forces%i_lorentz,        &
     &    b_trns%rot_forces%i_lorentz, trns_back)
      call add_field_name_4_sph_trns_snap(rot_buoyancy,                 &
     &    ipol%rot_forces%i_buoyancy, iphys%rot_forces%i_buoyancy,      &
     &    b_trns%rot_forces%i_buoyancy, trns_back)
      call add_field_name_4_sph_trns_snap(rot_composite_buoyancy,       &
     &    ipol%rot_forces%i_comp_buo, iphys%rot_forces%i_comp_buo,      &
     &    b_trns%rot_forces%i_comp_buo, trns_back)
!
      call add_field_name_4_sph_trns_snap(buoyancy,                     &
     &    ipol%forces%i_buoyancy, iphys%forces%i_buoyancy,              &
     &    b_trns%forces%i_buoyancy, trns_back)
      call add_field_name_4_sph_trns_snap(composite_buoyancy,           &
     &    ipol%forces%i_comp_buo, iphys%forces%i_comp_buo,              &
     &    b_trns%forces%i_comp_buo, trns_back)
!
      call add_field_name_4_sph_trns_snap(pressure_gradient,            &
     &    ipol%forces%i_press_grad, iphys%forces%i_press_grad,          &
     &    b_trns%forces%i_press_grad, trns_back)
      call add_field_name_4_sph_trns_snap(magnetic_induction,           &
     &    ipol%forces%i_induction, iphys%forces%i_induction,            &
     &    b_trns%forces%i_induction, trns_back)
!
      call add_force_w_SGS_sph_trns_snap                                &
     &   (ipol%frc_w_SGS, iphys%frc_w_SGS, b_trns%frc_w_SGS, trns_back)
!
      call add_rot_SGS_4_sph_trns_snap                                  &
     &   (ipol%rot_SGS, iphys%rot_SGS, b_trns%rot_SGS, trns_back)
!
      call add_field_4_sph_trns_by_pol(SGS_induction,                   &
     &    ipol%SGS_term%i_SGS_induction,                                &
     &    iphys%SGS_term%i_SGS_induction,                               &
     &    b_trns%SGS_term%i_SGS_induction, trns_back)
!
!   Gradient of vector field
      call add_diff_vect_sph_trns_by_pol                                &
     &   (ipol%diff_vector, iphys%diff_vector, b_trns%diff_vector,      &
     &    trns_back)
      call add_grad_4_sph_trns_snap                                     &
     &   (ipol%grad_fld, iphys%grad_fld, b_trns%grad_fld, trns_back)
!
      call add_subtracted_sph_trns_snap                                 &
     &   (ipol%prod_fld, iphys%prod_fld, b_trns%prod_fld, trns_back)
!
      trns_back%num_vector = trns_back%nfield
!
      end subroutine b_trans_address_vector_snap
!
!-----------------------------------------------------------------------
!
      subroutine b_trans_address_scalar_snap                            &
     &         (ipol, iphys, b_trns, trns_back)
!
      use m_diff_SGS_term_labels
      use m_filtered_field_labels
      use add_base_field_4_sph_trns
      use add_field_to_sph_trans_list
      use add_filter_fld_to_sph_trans
      use add_SGS_term_to_sph_trans
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
      call add_base_scalar_sph_trns_snap                                &
     &   (ipol%base, iphys%base, b_trns%base, trns_back)
!
      call add_fil_scalar_sph_trns_snap                                 &
     &   (ipol%filter_fld, iphys%filter_fld, b_trns%filter_fld,         &
     &    trns_back)
!
      call add_field_name_4_sph_trns_snap(thermal_diffusion,            &
     &    ipol%diffusion%i_t_diffuse, iphys%diffusion%i_t_diffuse,      &
     &    b_trns%diffusion%i_t_diffuse, trns_back)
      call add_field_name_4_sph_trns_snap(composition_diffusion,        &
     &    ipol%diffusion%i_c_diffuse, iphys%diffusion%i_c_diffuse,      &
     &    b_trns%diffusion%i_c_diffuse, trns_back)
!
      call add_field_name_4_sph_trns_snap(heat_advect,                  &
     &    ipol%forces%i_h_advect, iphys%forces%i_h_advect,              &
     &    b_trns%forces%i_h_advect, trns_back)
      call add_field_name_4_sph_trns_snap(composition_advect,           &
     &    ipol%forces%i_c_advect, iphys%forces%i_c_advect,              &
     &    b_trns%forces%i_c_advect, trns_back)
!
      call add_field_name_4_sph_trns_snap(div_Coriolis_force,           &
     &    ipol%div_forces%i_Coriolis, iphys%div_forces%i_Coriolis,      &
     &    b_trns%div_forces%i_Coriolis, trns_back)
!
      call add_div_SGS_4_sph_trns_snap                                  &
     &   (ipol%div_SGS, iphys%div_SGS, b_trns%div_SGS, trns_back)
!
      trns_back%num_scalar = trns_back%nfield - trns_back%num_vector
!
      end subroutine b_trans_address_scalar_snap
!
!-----------------------------------------------------------------------
!
      end module address_bwd_sph_trans_snap
