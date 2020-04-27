!>@file   add_filter_fld_to_sph_trans.f90
!!@brief  module add_filter_fld_to_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine add_fil_vector_sph_trns_by_pol                       &
!!     &         (ipol_fil, iphys_fil, b_trns_fil, trns)
!!        type(base_field_address), intent(in) :: ipol_fil, iphys_fil
!!        type(base_field_address), intent(inout) :: b_trns_fil
!!        type(spherical_transform_data), intent(inout) :: trns
!!
!!      subroutine add_fil_scalar_sph_trns_by_pol                       &
!!     &         (ipol_fil, iphys_fil, b_trns_fil, trns)
!!        type(base_field_address), intent(in) :: ipol_fil, iphys_fil
!!        type(base_field_address), intent(inout) :: b_trns_fil
!!        type(spherical_transform_data), intent(inout) :: trns
!!      subroutine add_fil_scalar_sph_trns_snap                         &
!!     &         (ipol_fil, iphys_fil, b_trns_fil, trns)
!!        type(base_field_address), intent(in) :: ipol_fil, iphys_fil
!!        type(base_field_address), intent(inout) :: b_trns_fil
!!        type(spherical_transform_data), intent(inout) :: trns
!!@endverbatim
!
      module add_filter_fld_to_sph_trans
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
      subroutine add_fil_vector_sph_trns_by_pol                         &
     &         (ipol_fil, iphys_fil, b_trns_fil, trns)
!
      use m_filtered_field_labels
      use add_field_to_sph_trans_list
!
      type(base_field_address), intent(in) :: ipol_fil, iphys_fil
      type(base_field_address), intent(inout) :: b_trns_fil
      type(spherical_transform_data), intent(inout) :: trns
!
!
!   filtered velocity
      call add_field_4_sph_trns_by_pol(filter_velocity,                 &
     &    ipol_fil%i_velo, iphys_fil%i_velo, b_trns_fil%i_velo, trns)
!   filtered vorticity
      call add_field_4_sph_trns_by_pol(filter_vorticity,                &
     &    ipol_fil%i_vort, iphys_fil%i_vort, b_trns_fil%i_vort, trns)
!   filtered magnetic field
      call add_field_4_sph_trns_by_pol(filter_magne,                    &
     &    ipol_fil%i_magne, iphys_fil%i_magne, b_trns_fil%i_magne,      &
     &    trns)
!   filtered current density
      call add_field_4_sph_trns_by_pol(filter_current,                  &
     &    ipol_fil%i_current, iphys_fil%i_current,                      &
     &    b_trns_fil%i_current, trns)
!
      end subroutine add_fil_vector_sph_trns_by_pol
!
!-----------------------------------------------------------------------
!
      subroutine add_fil_scalar_sph_trns_by_pol                         &
     &         (ipol_fil, iphys_fil, b_trns_fil, trns)
!
      use m_filtered_field_labels
      use add_field_to_sph_trans_list
!
      type(base_field_address), intent(in) :: ipol_fil, iphys_fil
      type(base_field_address), intent(inout) :: b_trns_fil
      type(spherical_transform_data), intent(inout) :: trns
!
!
!   filtered temperature
      call add_field_4_sph_trns_by_pol(filter_temperature,              &
     &    ipol_fil%i_temp, iphys_fil%i_temp, b_trns_fil%i_temp, trns)
!   filtered composition
      call add_field_4_sph_trns_by_pol(filter_composition,              &
     &    ipol_fil%i_light, iphys_fil%i_light, b_trns_fil%i_light,      &
     &    trns)
!
      end subroutine add_fil_scalar_sph_trns_by_pol
!
!-----------------------------------------------------------------------
!
      subroutine add_fil_scalar_sph_trns_snap                           &
     &         (ipol_fil, iphys_fil, b_trns_fil, trns)
!
      use m_filtered_field_labels
      use add_field_to_sph_trans_list
!
      type(base_field_address), intent(in) :: ipol_fil, iphys_fil
      type(base_field_address), intent(inout) :: b_trns_fil
      type(spherical_transform_data), intent(inout) :: trns
!
!
!   filtered temperature
      call add_field_name_4_sph_trns_snap(filter_temperature,           &
     &    ipol_fil%i_temp, iphys_fil%i_temp, b_trns_fil%i_temp, trns)
!   filtered composition
      call add_field_name_4_sph_trns_snap(filter_composition,           &
     &    ipol_fil%i_light, iphys_fil%i_light, b_trns_fil%i_light,      &
     &    trns)
!
      end subroutine add_fil_scalar_sph_trns_snap
!
!-----------------------------------------------------------------------
!
      end module add_filter_fld_to_sph_trans
