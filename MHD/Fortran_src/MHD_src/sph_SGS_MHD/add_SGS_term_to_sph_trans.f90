!>@file   add_SGS_term_to_sph_trans.f90
!!@brief  module add_SGS_term_to_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine add_SGS_term_4_sph_trns_by_pol                       &
!!     &         (ipol_SGS, iphys_SGS, f_trns_SGS, trns)
!!        type(SGS_term_address), intent(in) :: ipol_SGS, iphys_SGS
!!        type(SGS_term_address), intent(inout) :: f_trns_SGS
!!        type(spherical_transform_data), intent(inout) :: trns
!!      subroutine add_SGS_induction_sph_trns_pol                       &
!!     &         (ipol_SGS, iphys_SGS, b_trns_SGS, trns)
!!        type(SGS_term_address), intent(in) :: ipol_SGS, iphys_SGS
!!        type(SGS_term_address), intent(inout) :: b_trns_SGS
!!        type(spherical_transform_data), intent(inout) :: trns
!!      subroutine add_rot_SGS_4_sph_trns_snap                          &
!!     &         (ipol_rot_SGS, iphys_rot_SGS, b_trns_rot_SGS, trns)
!!      subroutine add_div_SGS_4_sph_trns_snap                          &
!!     &         (ipol_div_SGS, iphys_div_SGS, b_trns_div_SGS, trns)
!!        type(SGS_term_address), intent(in) :: ipol_div_SGS
!!        type(SGS_term_address), intent(in) :: iphys_div_SGS
!!        type(SGS_term_address), intent(inout) :: b_trns_div_SGS
!!        type(spherical_transform_data), intent(inout) :: trns
!!
!!      subroutine add_wide_SGS_term_4_sph_trns                         &
!!     &         (ipol_wSGS, iphys_wSGS, b_trns_wSGS, trns)
!!      type(SGS_term_address), intent(in) :: ipol_wSGS, iphys_wSGS
!!      type(SGS_term_address), intent(inout) :: b_trns_wSGS
!!      type(spherical_transform_data), intent(inout) :: trns
!!      subroutine add_double_SGS_term_4_sph_trns                       &
!!     &         (ipol_dSGS, iphys_dSGS, b_trns_dSGS, trns)
!!        type(SGS_term_address), intent(in) :: ipol_dSGS, iphys_dSGS
!!        type(SGS_term_address), intent(inout) :: b_trns_dSGS
!!        type(spherical_transform_data), intent(inout) :: trns
!!@endverbatim
!
      module add_SGS_term_to_sph_trans
!
      use m_precision
!
      use t_SGS_term_labels
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
      subroutine add_SGS_term_4_sph_trns_by_pol                         &
     &         (ipol_SGS, iphys_SGS, f_trns_SGS, trns)
!
      use add_field_to_sph_trans_list
!
      type(SGS_term_address), intent(in) :: ipol_SGS, iphys_SGS
      type(SGS_term_address), intent(inout) :: f_trns_SGS
      type(spherical_transform_data), intent(inout) :: trns
!
!
!   SGS advection flag
      call add_field_4_sph_trns_by_pol(SGS_inertia,                     &
     &    ipol_SGS%i_SGS_inertia, iphys_SGS%i_SGS_inertia,              &
     &    f_trns_SGS%i_SGS_inertia, trns)
!   SGS Lorentz force flag
      call add_field_4_sph_trns_by_pol(SGS_Lorentz,                     &
     &    ipol_SGS%i_SGS_Lorentz, iphys_SGS%i_SGS_Lorentz,              &
     &    f_trns_SGS%i_SGS_Lorentz, trns)
!   SGS induction flag
      call add_field_4_sph_trns_by_pol(SGS_vecp_induction,              &
     &    ipol_SGS%i_SGS_vp_induct, iphys_SGS%i_SGS_vp_induct,          &
     &    f_trns_SGS%i_SGS_vp_induct, trns)
!   SGS heat flux flag
      call add_field_4_sph_trns_by_pol(SGS_heat_flux,                   &
     &    ipol_SGS%i_SGS_h_flux, iphys_SGS%i_SGS_h_flux,                &
     &    f_trns_SGS%i_SGS_h_flux, trns)
!   SGS composition flux flag
      call add_field_4_sph_trns_by_pol(SGS_composit_flux,               &
     &    ipol_SGS%i_SGS_c_flux, iphys_SGS%i_SGS_c_flux,                &
     &    f_trns_SGS%i_SGS_c_flux, trns)
!
      end subroutine add_SGS_term_4_sph_trns_by_pol
!
!-----------------------------------------------------------------------
!
      subroutine add_SGS_induction_sph_trns_pol                         &
     &         (ipol_SGS, iphys_SGS, b_trns_SGS, trns)
!
      use add_field_to_sph_trans_list
!
      type(SGS_term_address), intent(in) :: ipol_SGS, iphys_SGS
      type(SGS_term_address), intent(inout) :: b_trns_SGS
      type(spherical_transform_data), intent(inout) :: trns
!
!
!   SGS magnetic induction flag
      call add_field_4_sph_trns_by_pol(SGS_induction,                   &
     &    ipol_SGS%i_SGS_induction, iphys_SGS%i_SGS_induction,          &
     &    b_trns_SGS%i_SGS_induction, trns)
!
      end subroutine add_SGS_induction_sph_trns_pol
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_force_w_SGS_sph_trns_snap                          &
     &         (ipol_frc_SGS, iphys_frc_SGS, b_trns_frc_SGS, trns)
!
      use m_force_w_SGS_labels
      use add_field_to_sph_trans_list
!
      type(SGS_term_address), intent(in) :: ipol_frc_SGS
      type(SGS_term_address), intent(in) :: iphys_frc_SGS
      type(SGS_term_address), intent(inout) :: b_trns_frc_SGS
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_field_name_4_sph_trns_snap(heat_flux_w_SGS,              &
     &    ipol_frc_SGS%i_SGS_h_flux, iphys_frc_SGS%i_SGS_h_flux,        &
     &    b_trns_frc_SGS%i_SGS_h_flux, trns)
      call add_field_name_4_sph_trns_snap(compostion_flux_w_SGS,        &
     &    ipol_frc_SGS%i_SGS_c_flux, iphys_frc_SGS%i_SGS_c_flux,        &
     &    b_trns_frc_SGS%i_SGS_c_flux, trns)
      call add_field_name_4_sph_trns_snap(intertia_w_SGS,               &
     &    ipol_frc_SGS%i_SGS_inertia, iphys_frc_SGS%i_SGS_inertia,      &
     &    b_trns_frc_SGS%i_SGS_inertia, trns)
      call add_field_name_4_sph_trns_snap(Lorentz_w_SGS,                &
     &    ipol_frc_SGS%i_SGS_Lorentz, iphys_frc_SGS%i_SGS_Lorentz,      &
     &    b_trns_frc_SGS%i_SGS_Lorentz, trns)
      call add_field_name_4_sph_trns_snap(vecp_induction_w_SGS,         &
     &    ipol_frc_SGS%i_SGS_vp_induct, iphys_frc_SGS%i_SGS_vp_induct,  &
     &    b_trns_frc_SGS%i_SGS_vp_induct, trns)
      call add_field_name_4_sph_trns_snap(induction_w_SGS,              &
     &    ipol_frc_SGS%i_SGS_induction, iphys_frc_SGS%i_SGS_induction,  &
     &    b_trns_frc_SGS%i_SGS_induction, trns)
!
      end subroutine add_force_w_SGS_sph_trns_snap
!
!-----------------------------------------------------------------------
!
      subroutine add_rot_SGS_4_sph_trns_snap                            &
     &         (ipol_rot_SGS, iphys_rot_SGS, b_trns_rot_SGS, trns)
!
      use m_diff_SGS_term_labels
      use add_field_to_sph_trans_list
!
      type(SGS_term_address), intent(in) :: ipol_rot_SGS
      type(SGS_term_address), intent(in) :: iphys_rot_SGS
      type(SGS_term_address), intent(inout) :: b_trns_rot_SGS
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_field_name_4_sph_trns_snap(rot_SGS_inertia,              &
     &    ipol_rot_SGS%i_SGS_inertia, iphys_rot_SGS%i_SGS_inertia,      &
     &    b_trns_rot_SGS%i_SGS_inertia, trns)
      call add_field_name_4_sph_trns_snap(rot_SGS_Lorentz,              &
     &    ipol_rot_SGS%i_SGS_Lorentz, iphys_rot_SGS%i_SGS_Lorentz,      &
     &    b_trns_rot_SGS%i_SGS_Lorentz, trns)
!
      end subroutine add_rot_SGS_4_sph_trns_snap
!
!-----------------------------------------------------------------------
!
      subroutine add_div_SGS_4_sph_trns_snap                            &
     &         (ipol_div_SGS, iphys_div_SGS, b_trns_div_SGS, trns)
!
      use m_diff_SGS_term_labels
      use add_field_to_sph_trans_list
!
      type(SGS_term_address), intent(in) :: ipol_div_SGS
      type(SGS_term_address), intent(in) :: iphys_div_SGS
      type(SGS_term_address), intent(inout) :: b_trns_div_SGS
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_field_name_4_sph_trns_snap(div_SGS_inertia,              &
     &    ipol_div_SGS%i_SGS_inertia, iphys_div_SGS%i_SGS_inertia,      &
     &    b_trns_div_SGS%i_SGS_inertia, trns)
      call add_field_name_4_sph_trns_snap(div_SGS_Lorentz,              &
     &    ipol_div_SGS%i_SGS_Lorentz, iphys_div_SGS%i_SGS_Lorentz,      &
     &    b_trns_div_SGS%i_SGS_Lorentz, trns)
      call add_field_name_4_sph_trns_snap(div_SGS_h_flux,               &
     &    ipol_div_SGS%i_SGS_h_flux, iphys_div_SGS%i_SGS_h_flux,        &
     &    b_trns_div_SGS%i_SGS_h_flux, trns)
      call add_field_name_4_sph_trns_snap(div_SGS_c_flux,               &
     &    ipol_div_SGS%i_SGS_c_flux, iphys_div_SGS%i_SGS_c_flux,        &
     &    b_trns_div_SGS%i_SGS_c_flux, trns)
!
      end subroutine add_div_SGS_4_sph_trns_snap
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_wide_SGS_term_4_sph_trns                           &
     &         (ipol_wSGS, iphys_wSGS, b_trns_wSGS, trns)
!
      use m_wide_SGS_term_labels
      use add_field_to_sph_trans_list
!
      type(SGS_term_address), intent(in) :: ipol_wSGS, iphys_wSGS
      type(SGS_term_address), intent(inout) :: b_trns_wSGS
      type(spherical_transform_data), intent(inout) :: trns
!
!
!   wide filtered Inertia
      call add_field_4_sph_trns_by_pol(wide_SGS_inertia,                &
     &    ipol_wSGS%i_SGS_inertia, iphys_wSGS%i_SGS_inertia,            &
     &    b_trns_wSGS%i_SGS_inertia, trns)
!   wide filtered Lorentz force
      call add_field_4_sph_trns_by_pol(wide_SGS_Lorentz,                &
     &    ipol_wSGS%i_SGS_Lorentz, iphys_wSGS%i_SGS_Lorentz,            &
     &    b_trns_wSGS%i_SGS_Lorentz, trns)
!   wide filtered induction
      call add_field_4_sph_trns_by_pol(wide_SGS_vp_induction,           &
     &    ipol_wSGS%i_SGS_vp_induct, iphys_wSGS%i_SGS_vp_induct,        &
     &    b_trns_wSGS%i_SGS_vp_induct, trns)
!   wide filtered heat flux
      call add_field_4_sph_trns_by_pol(wide_SGS_heat_flux,              &
     &    ipol_wSGS%i_SGS_h_flux, iphys_wSGS%i_SGS_h_flux,              &
     &    b_trns_wSGS%i_SGS_h_flux, trns)
!   wide filtered composition flux
      call add_field_4_sph_trns_by_pol(wide_SGS_composit_flux,          &
     &    ipol_wSGS%i_SGS_c_flux, iphys_wSGS%i_SGS_c_flux,              &
     &    b_trns_wSGS%i_SGS_c_flux, trns)
!
      end subroutine add_wide_SGS_term_4_sph_trns
!
!-----------------------------------------------------------------------
!
      subroutine add_double_SGS_term_4_sph_trns                         &
     &         (ipol_dSGS, iphys_dSGS, b_trns_dSGS, trns)
!
      use m_wide_SGS_term_labels
      use add_field_to_sph_trans_list
!
      type(SGS_term_address), intent(in) :: ipol_dSGS, iphys_dSGS
      type(SGS_term_address), intent(inout) :: b_trns_dSGS
      type(spherical_transform_data), intent(inout) :: trns
!
!
!   dual filtered Inertia
      call add_field_4_sph_trns_by_pol(double_SGS_inertia,              &
     &    ipol_dSGS%i_SGS_inertia, iphys_dSGS%i_SGS_inertia,            &
     &    b_trns_dSGS%i_SGS_inertia, trns)
!   dual filtered Lorentz force
      call add_field_4_sph_trns_by_pol(double_SGS_Lorentz,              &
     &    ipol_dSGS%i_SGS_Lorentz, iphys_dSGS%i_SGS_Lorentz,            &
     &    b_trns_dSGS%i_SGS_Lorentz, trns)
!   dual filtered induction
      call add_field_4_sph_trns_by_pol(double_SGS_vp_induction,         &
     &    ipol_dSGS%i_SGS_vp_induct, iphys_dSGS%i_SGS_vp_induct,        &
     &    b_trns_dSGS%i_SGS_vp_induct, trns)
!   dual filtered heat flux
      call add_field_4_sph_trns_by_pol(double_SGS_heat_flux,            &
     &    ipol_dSGS%i_SGS_h_flux, iphys_dSGS%i_SGS_h_flux,              &
     &    b_trns_dSGS%i_SGS_h_flux, trns)
!   dual filtered composition flux
      call add_field_4_sph_trns_by_pol(double_SGS_composit_flux,        &
     &    ipol_dSGS%i_SGS_c_flux, iphys_dSGS%i_SGS_c_flux,              &
     &    b_trns_dSGS%i_SGS_c_flux, trns)
!
      end subroutine add_double_SGS_term_4_sph_trns
!
!-----------------------------------------------------------------------
!
      end module add_SGS_term_to_sph_trans
