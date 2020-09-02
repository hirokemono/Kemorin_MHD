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
!!     &         (d_rj, ipol_wfl, iphys_wfl, b_trns_wfl, trns)
!!      subroutine add_wide_fil_scalar_sph_trns                         &
!!     &         (d_rj, ipol_wfl, iphys_wfl, b_trns_wfl, trns)
!!        type(phys_data), intent(in) :: d_rj
!!        type(base_field_address), intent(in) :: ipol_wfl, iphys_wfl
!!        type(base_field_address), intent(inout) :: b_trns_wfl
!!        type(spherical_transform_data), intent(inout) :: trns
!!@endverbatim
!
      module add_wide_f_fld_to_sph_trans
!
      use m_precision
!
      use t_phys_data
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
     &         (d_rj, ipol_wfl, iphys_wfl, b_trns_wfl, trns)
!
      use add_field_to_sph_trans_list
!
      type(phys_data), intent(in) :: d_rj
      type(base_field_address), intent(in) :: ipol_wfl, iphys_wfl
      type(base_field_address), intent(inout) :: b_trns_wfl
      type(spherical_transform_data), intent(inout) :: trns
!
!   wide filtered velocity
      call add_field_4_sph_trns_by_pol(d_rj,                            &
     &    ipol_wfl%i_velo, iphys_wfl%i_velo, b_trns_wfl%i_velo,         &
     &    trns)
!   wide filtered vorticity
      call add_field_4_sph_trns_by_pol(d_rj,                            &
     &    ipol_wfl%i_vort, iphys_wfl%i_vort, b_trns_wfl%i_vort,         &
     &    trns)
!   wide filtered magnetic field
      call add_field_4_sph_trns_by_pol(d_rj,                            &
     &    ipol_wfl%i_magne, iphys_wfl%i_magne, b_trns_wfl%i_magne,      &
     &    trns)
!   wide filtered current density
      call add_field_4_sph_trns_by_pol(d_rj,                            &
     &    ipol_wfl%i_current, iphys_wfl%i_current,                      &
     &    b_trns_wfl%i_current, trns)
!
      end subroutine add_wide_fil_vector_sph_trns
!
!-----------------------------------------------------------------------
!
      subroutine add_wide_fil_scalar_sph_trns                           &
     &         (d_rj, ipol_wfl, iphys_wfl, b_trns_wfl, trns)
!
      use add_field_to_sph_trans_list
!
      type(phys_data), intent(in) :: d_rj
      type(base_field_address), intent(in) :: ipol_wfl, iphys_wfl
      type(base_field_address), intent(inout) :: b_trns_wfl
      type(spherical_transform_data), intent(inout) :: trns
!
!
!   wide filtered temperature
      call add_field_4_sph_trns_by_pol(d_rj,                            &
     &    ipol_wfl%i_temp, iphys_wfl%i_temp, b_trns_wfl%i_temp,         &
     &    trns)
!   wide filtered composition
      call add_field_4_sph_trns_by_pol(d_rj,                            &
     &    ipol_wfl%i_light, iphys_wfl%i_light, b_trns_wfl%i_light,      &
     &    trns)
!
      end subroutine add_wide_fil_scalar_sph_trns
!
!-----------------------------------------------------------------------
!
      end module add_wide_f_fld_to_sph_trans
