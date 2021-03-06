!>@file   add_Csim_4_sph_trns.f90
!!@brief  module add_Csim_4_sph_trns
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine add_Csim_4_sph_trns_by_pol                           &
!!     &         (d_rj, ipol_Csim, iphys_Csim, f_trns_Csim, trns)
!!      subroutine add_Csim_4_sph_trns_snap                             &
!!     &         (d_rj, ipol_Csim, iphys_Csim, f_trns_Csim, trns)
!!        type(phys_data), intent(in) :: d_rj
!!        type(SGS_term_address), intent(in) :: ipol_Csim, iphys_Csim
!!        type(SGS_term_address), intent(inout) :: f_trns_Csim
!!        type(spherical_transform_data), intent(inout) :: trns
!!@endverbatim
!
      module add_Csim_4_sph_trns
!
      use m_precision
!
      use t_phys_data
      use t_SGS_term_labels
      use t_SGS_model_coef_labels
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
      subroutine add_Csim_4_sph_trns_by_pol                             &
     &         (d_rj, ipol_Csim, iphys_Csim, f_trns_Csim, trns)
!
      use add_field_to_sph_trans_list
!
      type(phys_data), intent(in) :: d_rj
      type(SGS_term_address), intent(in) :: ipol_Csim, iphys_Csim
      type(SGS_term_address), intent(inout) :: f_trns_Csim
      type(spherical_transform_data), intent(inout) :: trns
!
!
!   SGS advection flag
      call add_field_4_sph_trns_by_pol(d_rj,                            &
     &    ipol_Csim%i_SGS_m_flux, iphys_Csim%i_SGS_m_flux,              &
     &    f_trns_Csim%i_SGS_m_flux, trns)
!   SGS Lorentz force flag
      call add_field_4_sph_trns_by_pol(d_rj,                            &
     &    ipol_Csim%i_SGS_Lorentz, iphys_Csim%i_SGS_Lorentz,            &
     &    f_trns_Csim%i_SGS_Lorentz, trns)
!   SGS induction flag
      call add_field_4_sph_trns_by_pol(d_rj,                            &
     &    ipol_Csim%i_SGS_vp_induct, iphys_Csim%i_SGS_vp_induct,        &
     &    f_trns_Csim%i_SGS_vp_induct, trns)
!   SGS heat flux flag
      call add_field_4_sph_trns_by_pol(d_rj,                            &
     &    ipol_Csim%i_SGS_h_flux, iphys_Csim%i_SGS_h_flux,              &
     &    f_trns_Csim%i_SGS_h_flux, trns)
!   SGS composition flux flag
      call add_field_4_sph_trns_by_pol(d_rj,                            &
     &    ipol_Csim%i_SGS_c_flux, iphys_Csim%i_SGS_c_flux,              &
     &    f_trns_Csim%i_SGS_c_flux, trns)
!
!   SGS buoyancy
      call add_field_4_sph_trns_by_pol(d_rj,                            &
     &    ipol_Csim%i_SGS_buoyancy, iphys_Csim%i_SGS_buoyancy,          &
     &    f_trns_Csim%i_SGS_buoyancy, trns)
!   SGS compostional buoyancy
      call add_field_4_sph_trns_by_pol(d_rj,                            &
     &    ipol_Csim%i_SGS_comp_buo, iphys_Csim%i_SGS_comp_buo,          &
     &    f_trns_Csim%i_SGS_comp_buo, trns)
!
      end subroutine add_Csim_4_sph_trns_by_pol
!
!-----------------------------------------------------------------------
!
      subroutine add_Csim_4_sph_trns_snap                               &
     &         (d_rj, ipol_Csim, iphys_Csim, f_trns_Csim, trns)
!
      use add_field_to_sph_trans_list
!
      type(phys_data), intent(in) :: d_rj
      type(SGS_term_address), intent(in) :: ipol_Csim, iphys_Csim
      type(SGS_term_address), intent(inout) :: f_trns_Csim
      type(spherical_transform_data), intent(inout) :: trns
!
!
!   SGS advection flag
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_Csim%i_SGS_m_flux, iphys_Csim%i_SGS_m_flux,              &
     &    f_trns_Csim%i_SGS_m_flux, trns)
!   SGS Lorentz force flag
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_Csim%i_SGS_Lorentz, iphys_Csim%i_SGS_Lorentz,            &
     &    f_trns_Csim%i_SGS_Lorentz, trns)
!   SGS induction flag
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_Csim%i_SGS_vp_induct, iphys_Csim%i_SGS_vp_induct,        &
     &    f_trns_Csim%i_SGS_vp_induct, trns)
!   SGS heat flux flag
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_Csim%i_SGS_h_flux, iphys_Csim%i_SGS_h_flux,              &
     &    f_trns_Csim%i_SGS_h_flux, trns)
!   SGS composition flux flag
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_Csim%i_SGS_c_flux, iphys_Csim%i_SGS_c_flux,              &
     &    f_trns_Csim%i_SGS_c_flux, trns)
!
!   SGS buoyancy
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_Csim%i_SGS_buoyancy, iphys_Csim%i_SGS_buoyancy,          &
     &    f_trns_Csim%i_SGS_buoyancy, trns)
!   SGS compostional buoyancy
      call add_field_name_4_sph_trns_snap(d_rj,                         &
     &    ipol_Csim%i_SGS_comp_buo, iphys_Csim%i_SGS_comp_buo,          &
     &    f_trns_Csim%i_SGS_comp_buo, trns)
!
      end subroutine add_Csim_4_sph_trns_snap
!
!-----------------------------------------------------------------------
!
      end module add_Csim_4_sph_trns
