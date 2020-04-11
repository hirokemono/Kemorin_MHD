!>@file   add_wide_f_fld_to_sph_trans.f90
!!@brief  module add_wide_f_fld_to_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Add wider filterd fields for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine add_wide_fil_vector_sph_trns                         &
!!     &         (ipol_wfl, iphys_wfl, b_trns_wfl, trns)
!!      subroutine add_wide_fil_scalar_sph_trns                         &
!!     &         (ipol_wfl, iphys_wfl, b_trns_wfl, trns)
!!        type(base_field_address), intent(in) :: ipol_wfl, iphys_wfl
!!        type(base_field_address), intent(inout) :: b_trns_wfl
!!        type(address_each_sph_trans), intent(inout) :: trns
!!@endverbatim
!
      module add_wide_f_fld_to_sph_trans
!
      use m_precision
!
      use t_base_field_labels
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
      subroutine add_wide_fil_vector_sph_trns                           &
     &         (ipol_wfl, iphys_wfl, b_trns_wfl, trns)
!
      use m_wide_filter_field_labels
      use add_field_to_sph_trans_list
!
      type(base_field_address), intent(in) :: ipol_wfl, iphys_wfl
      type(base_field_address), intent(inout) :: b_trns_wfl
      type(address_each_sph_trans), intent(inout) :: trns
!
!   wide filtered velocity
      call add_field_4_sph_trns_by_pol(wide_filter_velocity,            &
     &    ipol_wfl%i_velo, iphys_wfl%i_velo, b_trns_wfl%i_velo,         &
     &    trns)
!   wide filtered vorticity
      call add_field_4_sph_trns_by_pol(wide_filter_vorticity,           &
     &    ipol_wfl%i_vort, iphys_wfl%i_vort, b_trns_wfl%i_vort,         &
     &    trns)
!   wide filtered magnetic field
      call add_field_4_sph_trns_by_pol(wide_filter_magne,               &
     &    ipol_wfl%i_magne, iphys_wfl%i_magne, b_trns_wfl%i_magne,      &
     &    trns)
!   wide filtered current density
      call add_field_4_sph_trns_by_pol(wide_filter_current,             &
     &    ipol_wfl%i_current, iphys_wfl%i_current,                      &
     &    b_trns_wfl%i_current, trns)
!
      end subroutine add_wide_fil_vector_sph_trns
!
!-----------------------------------------------------------------------
!
      subroutine add_wide_fil_scalar_sph_trns                           &
     &         (ipol_wfl, iphys_wfl, b_trns_wfl, trns)
!
      use m_wide_filter_field_labels
      use add_field_to_sph_trans_list
!
      type(base_field_address), intent(in) :: ipol_wfl, iphys_wfl
      type(base_field_address), intent(inout) :: b_trns_wfl
      type(address_each_sph_trans), intent(inout) :: trns
!
!
!   wide filtered temperature
      call add_field_4_sph_trns_by_pol(wide_filter_temp,                &
     &    ipol_wfl%i_temp, iphys_wfl%i_temp, b_trns_wfl%i_temp,         &
     &    trns)
!   wide filtered composition
      call add_field_4_sph_trns_by_pol(wide_filter_composition,         &
     &    ipol_wfl%i_light, iphys_wfl%i_light, b_trns_wfl%i_light,      &
     &    trns)
!
      end subroutine add_wide_fil_scalar_sph_trns
!
!-----------------------------------------------------------------------
!
      end module add_wide_f_fld_to_sph_trans
