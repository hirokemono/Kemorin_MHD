!>@file   add_SGS_eflux_to_sph_trans.f90
!!@brief  module add_SGS_eflux_to_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief energy flux by SGS terms for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine add_SGS_eflux_sph_trns_by_pol                        &
!!     &         (ipol_sef, iphys_sef, f_trns_sef, trns)
!!      subroutine add_SGS_eflux_sph_trns_snap                          &
!!     &         (ipol_sef, iphys_sef, f_trns_sef, trns)
!!        type(SGS_ene_flux_address), intent(in) :: ipol_sef, iphys_sef
!!        type(SGS_ene_flux_address), intent(inout) :: f_trns_sef
!!        type(spherical_transform_data), intent(inout) :: trns
!!      subroutine add_fil_e_flux_4_sph_trns_snap                       &
!!     &         (ipol_fefx, iphys_fefx, f_trns_fefx, trns)
!!        type(energy_flux_address), intent(in) :: ipol_fefx, iphys_fefx
!!        type(energy_flux_address), intent(inout) :: f_trns_fefx
!!        type(spherical_transform_data), intent(inout) :: trns
!!@endverbatim
!
      module add_SGS_eflux_to_sph_trans
!
      use m_precision
!
      use t_addresses_sph_transform
      use t_SGS_enegy_flux_labels
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine add_SGS_eflux_sph_trns_by_pol                          &
     &         (ipol_sef, iphys_sef, f_trns_sef, trns)
!
      use m_SGS_enegy_flux_labels
      use add_field_to_sph_trans_list
!
      type(SGS_ene_flux_address), intent(in) :: ipol_sef, iphys_sef
      type(SGS_ene_flux_address), intent(inout) :: f_trns_sef
      type(spherical_transform_data), intent(inout) :: trns
!
!
!   work of Reynolds stress
      call add_field_4_sph_trns_by_pol(Reynolds_work,                   &
     &    ipol_sef%i_reynolds_wk, iphys_sef%i_reynolds_wk,              &
     &    f_trns_sef%i_reynolds_wk, trns)
!   work of SGS buoyancy
      call add_field_4_sph_trns_by_pol(SGS_buoyancy_flux,               &
     &    ipol_sef%i_SGS_buo_wk, iphys_sef%i_SGS_buo_wk,                &
     &    f_trns_sef%i_SGS_buo_wk, trns)
!   work of SGS compositional buoyancy
      call add_field_4_sph_trns_by_pol(SGS_comp_buoyancy_flux,          &
     &    ipol_sef%i_SGS_comp_buo_wk, iphys_sef%i_SGS_comp_buo_wk,      &
     &    f_trns_sef%i_SGS_comp_buo_wk, trns)
!
      end subroutine add_SGS_eflux_sph_trns_by_pol
!
!-----------------------------------------------------------------------
!
      subroutine add_SGS_eflux_sph_trns_snap                           &
     &         (ipol_sef, iphys_sef, f_trns_sef, trns)
!
      use m_SGS_enegy_flux_labels
      use add_field_to_sph_trans_list
!
      type(SGS_ene_flux_address), intent(in) :: ipol_sef, iphys_sef
      type(SGS_ene_flux_address), intent(inout) :: f_trns_sef
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_field_name_4_sph_trns_snap(Reynolds_work,                &
     &    ipol_sef%i_reynolds_wk, iphys_sef%i_reynolds_wk,              &
     &    f_trns_sef%i_reynolds_wk, trns)
!
      call add_field_name_4_sph_trns_snap(SGS_Lorentz_work,             &
     &    ipol_sef%i_SGS_Lor_wk, iphys_sef%i_SGS_Lor_wk,                &
     &    f_trns_sef%i_SGS_Lor_wk, trns)
      call add_field_name_4_sph_trns_snap(SGS_mag_induction_flux,       &
     &    ipol_sef%i_SGS_me_gen, iphys_sef%i_SGS_me_gen,                &
     &    f_trns_sef%i_SGS_me_gen, trns)
!
      call add_field_name_4_sph_trns_snap(SGS_buoyancy_flux,            &
     &    ipol_sef%i_SGS_buo_wk, iphys_sef%i_SGS_buo_wk,                &
     &    f_trns_sef%i_SGS_buo_wk, trns)
      call add_field_name_4_sph_trns_snap(SGS_comp_buoyancy_flux,       &
     &    ipol_sef%i_SGS_comp_buo_wk, iphys_sef%i_SGS_comp_buo_wk,      &
     &    f_trns_sef%i_SGS_comp_buo_wk, trns)
!
      end subroutine add_SGS_eflux_sph_trns_snap
!
!-----------------------------------------------------------------------
!
      subroutine add_fil_e_flux_4_sph_trns_snap                         &
     &         (ipol_fefx, iphys_fefx, f_trns_fefx, trns)
!
      use t_energy_flux_labels
      use m_filtered_ene_flux_labels
      use add_field_to_sph_trans_list
!
      type(energy_flux_address), intent(in) :: ipol_fefx, iphys_fefx
      type(energy_flux_address), intent(inout) :: f_trns_fefx
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_field_name_4_sph_trns_snap(filtered_buoyancy_flux,       &
     &    ipol_fefx%i_buo_gen, iphys_fefx%i_buo_gen,                    &
     &    f_trns_fefx%i_buo_gen, trns)
      call add_field_name_4_sph_trns_snap(filtered_comp_buoyancy_flux,  &
     &    ipol_fefx%i_c_buo_gen, iphys_fefx%i_c_buo_gen,                &
     &    f_trns_fefx%i_c_buo_gen, trns)
!
      end subroutine add_fil_e_flux_4_sph_trns_snap
!
!-----------------------------------------------------------------------
!
      end module add_SGS_eflux_to_sph_trans
