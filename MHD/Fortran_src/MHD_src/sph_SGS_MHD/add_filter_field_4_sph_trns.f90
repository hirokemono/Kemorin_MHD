!>@file   add_filter_field_4_sph_trns.f90
!!@brief  module add_filter_field_4_sph_trns
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine add_filter_MHD_vec_sph_trns                          &
!!     &         (d_rj, ipol_fil, iphys_fil, b_trns_fil, trns)
!!      subroutine add_filter_MHD_scl_sph_trns                          &
!!     &         (d_rj, ipol_fil, iphys_fil, b_trns_fil, trns)
!!        type(phys_data), intent(in) :: d_rj
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in)  :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(phys_address), intent(in) :: ipol_fil, iphys_fil
!!        type(phys_address), intent(inout) :: b_trns_fil
!!        type(spherical_transform_data), intent(inout) :: trns
!!      subroutine add_filter_vec_sph_trns_snap                         &
!!     &         (d_rj, ipol_fil, iphys_fil, b_trns_fil, trns)
!!      subroutine add_filter_scl_sph_trns_snap                         &
!!     &         (d_rj, ipol_fil, iphys_fil, b_trns_fil, trns)
!!        type(phys_data), intent(in) :: d_rj
!!        type(phys_address), intent(in) :: ipol_fil, iphys_fil
!!        type(phys_address), intent(inout) :: b_trns_fil
!!        type(spherical_transform_data), intent(inout) :: trns
!!@endverbatim
!
      module add_filter_field_4_sph_trns
!
      use m_precision
!
      use t_phys_data
      use t_base_field_labels
      use t_addresses_sph_transform
      use t_physical_property
      use m_filtered_field_labels
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine add_filter_MHD_vec_sph_trns                            &
!     &         (fl_prop, cd_prop, ht_prop, cp_prop,                    &
     &         (d_rj, ipol_fil, iphys_fil, b_trns_fil, trns)
!
      use add_field_to_sph_trans_list
!
!      type(fluid_property), intent(in) :: fl_prop
!      type(conductive_property), intent(in)  :: cd_prop
!      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(phys_data), intent(in) :: d_rj
      type(base_field_address), intent(in) :: ipol_fil, iphys_fil
      type(base_field_address), intent(inout) :: b_trns_fil
      type(spherical_transform_data), intent(inout) :: trns
!
!
!   filtered velocity flag
!      if(       fl_prop%iflag_4_filter_inertia                         &
!     &     .or. cd_prop%iflag_4_filter_induction                       &
!     &     .or. ht_prop%iflag_4_filter_advection                       &
!     &     .or. cp_prop%iflag_4_filter_advection) then
        call add_field_4_sph_trns_by_pol(d_rj,                          &
     &      ipol_fil%i_velo, iphys_fil%i_velo, b_trns_fil%i_velo,       &
     &      trns)
!      end if
!   filtered vorticity flag
!      if(fl_prop%iflag_4_filter_inertia) then
        call add_field_4_sph_trns_by_pol(d_rj,                          &
     &      ipol_fil%i_vort, iphys_fil%i_vort, b_trns_fil%i_vort,       &
     &      trns)
!      end if
!   filtered magnetic field flag
!      if(       cd_prop%iflag_4_filter_induction                       &
!     &     .or. fl_prop%iflag_4_filter_lorentz) then
        call add_field_4_sph_trns_by_pol(d_rj,                          &
     &      ipol_fil%i_magne, iphys_fil%i_magne, b_trns_fil%i_magne,    &
     &      trns)
!      end if
!   filtered current density flag
!      if(fl_prop%iflag_4_filter_lorentz) then
        call add_field_4_sph_trns_by_pol(d_rj,                          &
     &      ipol_fil%i_current, iphys_fil%i_current,                    &
     &      b_trns_fil%i_current, trns)
!      end if
!
      end subroutine add_filter_MHD_vec_sph_trns
!
!-----------------------------------------------------------------------
!
!      subroutine add_filter_MHD_scl_sph_trns(ht_prop, cp_prop,         &
      subroutine add_filter_MHD_scl_sph_trns                            &
     &         (d_rj, ipol_fil, iphys_fil, b_trns_fil, trns)
!
      use add_field_to_sph_trans_list
!
!      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(phys_data), intent(in) :: d_rj
      type(base_field_address), intent(in) :: ipol_fil, iphys_fil
      type(base_field_address), intent(inout) :: b_trns_fil
      type(spherical_transform_data), intent(inout) :: trns
!
!
!   temperature flag
!      if(ht_prop%iflag_4_filter_advection) then
        call add_field_4_sph_trns_by_pol(d_rj,                          &
     &      ipol_fil%i_temp, iphys_fil%i_temp, b_trns_fil%i_temp,       &
     &      trns)
!      end if
!   composition flag
!      if(cp_prop%iflag_4_filter_advection) then
        call add_field_4_sph_trns_by_pol(d_rj,                          &
     &      ipol_fil%i_light, iphys_fil%i_light, b_trns_fil%i_light,    &
     &      trns)
!      end if
!
      end subroutine add_filter_MHD_scl_sph_trns
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_filter_vec_sph_trns_snap                           &
     &         (d_rj, ipol_fil, iphys_fil, b_trns_fil, trns)
!
      type(phys_data), intent(in) :: d_rj
      type(base_field_address), intent(in) :: ipol_fil, iphys_fil
      type(base_field_address), intent(inout) :: b_trns_fil
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_filter_MHD_vec_sph_trns                                  &
     &   (d_rj, ipol_fil, iphys_fil, b_trns_fil, trns)
!
      end subroutine add_filter_vec_sph_trns_snap
!
!-----------------------------------------------------------------------
!
      subroutine add_filter_scl_sph_trns_snap                           &
     &         (d_rj, ipol_fil, iphys_fil, b_trns_fil, trns)
!
      use add_field_to_sph_trans_list
!
      type(phys_data), intent(in) :: d_rj
      type(base_field_address), intent(in) :: ipol_fil, iphys_fil
      type(base_field_address), intent(inout) :: b_trns_fil
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_field_4_sph_trns_by_pol(d_rj,                            &
     &    ipol_fil%i_temp, iphys_fil%i_temp, b_trns_fil%i_temp,         &
     &    trns)
      call add_field_4_sph_trns_by_pol(d_rj,                            &
     &    ipol_fil%i_light, iphys_fil%i_light, b_trns_fil%i_light,      &
     &    trns)
!
      call add_field_4_sph_trns_by_pol(d_rj,                            &
     &    ipol_fil%i_per_temp, iphys_fil%i_per_temp,                    &
     &    b_trns_fil%i_per_temp, trns)
!
      call add_field_4_sph_trns_by_pol(d_rj,                            &
     &    ipol_fil%i_per_light, iphys_fil%i_per_light,                  &
     &    b_trns_fil%i_per_light, trns)
!
      end subroutine add_filter_scl_sph_trns_snap
!
!-----------------------------------------------------------------------
!
      end module add_filter_field_4_sph_trns
