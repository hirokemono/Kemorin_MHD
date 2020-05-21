!>@file   add_filter_force_4_sph_trns.f90
!!@brief  module add_filter_force_4_sph_trns
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Force addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine add_filter_force_MHD_sph_trns                        &
!!     &         (ipol_fil_frc, iphys_fil_frc, f_trns_fil_frc, trns)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(base_force_address), intent(in) :: ipol_fil_frc
!!        type(base_force_address), intent(in) :: iphys_fil_frc
!!        type(base_force_address), intent(inout) :: f_trns_fil_frc
!!        type(spherical_transform_data), intent(inout) :: trns
!!
!!      subroutine add_filter_force_fwd_trns_snap                       &
!!     &         (ipol_fil_frc, iphys_fil_frc, f_trns_fil_frc, trns)
!!        type(base_force_address), intent(in) :: ipol_fil_frc
!!        type(base_force_address), intent(in) :: iphys_fil_frc
!!        type(base_force_address), intent(inout) :: f_trns_fil_frc
!!        type(spherical_transform_data), intent(inout) :: trns
!!      subroutine add_filter_force_4_bwd_trns                            &
!!     &         (ipol_fil_frc, iphys_fil_frc, b_trns_fil_frc, trns)
!!      subroutine add_fil_scl_flux_bwd_trns_snap                       &
!!     &         (ipol_fil_frc, iphys_fil_frc, b_trns_fil_frc, trns)
!!        type(base_force_address), intent(in) :: ipol_fil_frc
!!        type(base_force_address), intent(in) :: iphys_fil_frc
!!        type(base_force_address), intent(inout) :: b_trns_fil_frc
!!        type(spherical_transform_data), intent(inout) :: trns
!!
!!      subroutine add_rot_fil_frc_sph_trns_snap                        &
!!     &         (ipol_rot_fil_frc, iphys_rot_fil_frc,                  &
!!     &          b_trns_rot_fil_frc, trns)
!!        type(base_force_address), intent(in) :: ipol_rot_fil_frc
!!        type(base_force_address), intent(in) :: iphys_rot_fil_frc
!!        type(base_force_address), intent(inout) :: b_trns_rot_fil_frc
!!        type(spherical_transform_data), intent(inout) :: trns
!!@endverbatim
!
      module add_filter_force_4_sph_trns
!
      use m_precision
      use m_constants
!
      use t_base_force_labels
      use t_addresses_sph_transform
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
      subroutine add_filter_force_MHD_sph_trns                          &
!     &         (fl_prop, cd_prop, ht_prop, cp_prop,                    &
     &         (ipol_fil_frc, iphys_fil_frc, f_trns_fil_frc, trns)
!
      use m_filtered_force_labels
      use add_field_to_sph_trans_list
!
!      type(fluid_property), intent(in) :: fl_prop
!      type(conductive_property), intent(in) :: cd_prop
!      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(base_force_address), intent(in) :: ipol_fil_frc
      type(base_force_address), intent(in) :: iphys_fil_frc
!
      type(base_force_address), intent(inout) :: f_trns_fil_frc
      type(spherical_transform_data), intent(inout) :: trns
!
!   advection
!      if(fl_prop%iflag_scheme .gt. id_no_evolution) then
!        if(fl_prop%iflag_4_filter_inertia) then
          call add_field_4_sph_trns_by_pol(inertia_by_filtered,         &
     &      ipol_fil_frc%i_m_advect, iphys_fil_frc%i_m_advect,          &
     &      f_trns_fil_frc%i_m_advect, trns)
!        end if
!   Lorentz force
!        if(fl_prop%iflag_4_filter_lorentz) then
          call add_field_4_sph_trns_by_pol(Lorentz_force_by_filtered,   &
     &        ipol_fil_frc%i_lorentz, iphys_fil_frc%i_lorentz,          &
     &        f_trns_fil_frc%i_lorentz, trns)
!        end if
!      end if
!
!   induction
!      if(cd_prop%iflag_Bevo_scheme .gt. id_no_evolution                &
!     &   .and. cd_prop%iflag_4_filter_induction) then
          call add_field_4_sph_trns_by_pol(vecp_induction_by_filtered,  &
     &        ipol_fil_frc%i_vp_induct, iphys_fil_frc%i_vp_induct,      &
     &        f_trns_fil_frc%i_vp_induct, trns)
!      end if
!
!   heat flux
!      if(ht_prop%iflag_scheme .gt. id_no_evolution                     &
!     &   .and. ht_prop%iflag_4_filter_advection) then
        call add_field_4_sph_trns_by_pol(heat_flux_by_filtered,         &
     &      ipol_fil_frc%i_h_flux, iphys_fil_frc%i_h_flux,              &
     &      f_trns_fil_frc%i_h_flux, trns)
!      end if
!
!   composition flux
!      if(cp_prop%iflag_scheme .gt. id_no_evolution                     &
!     &   .and. cp_prop%iflag_4_filter_advection) then
        call add_field_4_sph_trns_by_pol(composite_flux_by_filtered,    &
     &      ipol_fil_frc%i_c_flux, iphys_fil_frc%i_c_flux,              &
     &      f_trns_fil_frc%i_c_flux, trns)
!      end if
!
      end subroutine add_filter_force_MHD_sph_trns
!
!-----------------------------------------------------------------------
!
      subroutine add_filter_force_snap_sph_trns                         &
     &         (ipol_fil_frc, iphys_fil_frc, f_trns_fil_frc, trns)
!
      use m_filtered_force_labels
      use add_field_to_sph_trans_list
!
      type(base_force_address), intent(in) :: ipol_fil_frc
      type(base_force_address), intent(in) :: iphys_fil_frc
!
      type(base_force_address), intent(inout) :: f_trns_fil_frc
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_field_4_sph_trns_by_pol(pert_h_flux_by_filtered,         &
     &    ipol_fil_frc%i_ph_flux, iphys_fil_frc%i_ph_flux,              &
     &    f_trns_fil_frc%i_ph_flux, trns)
      call add_field_4_sph_trns_by_pol(pert_c_flux_by_filtered,         &
     &    ipol_fil_frc%i_pc_flux, iphys_fil_frc%i_pc_flux,              &
     &    f_trns_fil_frc%i_pc_flux, trns)
!
      end subroutine add_filter_force_snap_sph_trns
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_filter_force_fwd_trns_snap                         &
     &         (ipol_fil_frc, iphys_fil_frc, f_trns_fil_frc, trns)
!
      use m_filtered_force_labels
      use add_field_to_sph_trans_list
!
      type(base_force_address), intent(in) :: ipol_fil_frc
      type(base_force_address), intent(in) :: iphys_fil_frc
!
      type(base_force_address), intent(inout) :: f_trns_fil_frc
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_field_name_4_sph_trns_snap(magnetic_stretch_by_filtered, &
     &    ipol_fil_frc%i_mag_stretch, iphys_fil_frc%i_mag_stretch,      &
     &    f_trns_fil_frc%i_mag_stretch, trns)
!
      end subroutine add_filter_force_fwd_trns_snap
!
!-----------------------------------------------------------------------
!
      subroutine add_filter_force_4_bwd_trns                            &
     &         (ipol_fil_frc, iphys_fil_frc, b_trns_fil_frc, trns)
!
      use m_filtered_force_labels
      use add_field_to_sph_trans_list
!
      type(base_force_address), intent(in) :: ipol_fil_frc
      type(base_force_address), intent(in) :: iphys_fil_frc
!
      type(base_force_address), intent(inout) :: b_trns_fil_frc
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_field_4_sph_trns_by_pol(filtered_buoyancy,               &
     &    ipol_fil_frc%i_buoyancy, iphys_fil_frc%i_buoyancy,            &
     &    b_trns_fil_frc%i_buoyancy, trns)
      call add_field_4_sph_trns_by_pol(filtered_comp_buoyancy,          &
     &    ipol_fil_frc%i_comp_buo, iphys_fil_frc%i_comp_buo,            &
     &    b_trns_fil_frc%i_comp_buo, trns)
!
      call add_field_4_sph_trns_by_pol                                  &
     &   (magnetic_induction_by_filtered,                               &
     &    ipol_fil_frc%i_induction, iphys_fil_frc%i_induction,          &
     &    b_trns_fil_frc%i_induction, trns)
!
      end subroutine add_filter_force_4_bwd_trns
!
!-----------------------------------------------------------------------
!
      subroutine add_fil_scl_flux_bwd_trns_snap                         &
     &         (ipol_fil_frc, iphys_fil_frc, b_trns_fil_frc, trns)
!
      use m_filtered_force_labels
      use add_field_to_sph_trans_list
!
      type(base_force_address), intent(in) :: ipol_fil_frc
      type(base_force_address), intent(in) :: iphys_fil_frc
!
      type(base_force_address), intent(inout) :: b_trns_fil_frc
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_field_name_4_sph_trns_snap(heat_advect_by_filtered,      &
     &    ipol_fil_frc%i_h_advect, iphys_fil_frc%i_h_advect,            &
     &    b_trns_fil_frc%i_h_advect, trns)
      call add_field_name_4_sph_trns_snap(comp_advect_by_filtered,      &
     &    ipol_fil_frc%i_c_advect, iphys_fil_frc%i_c_advect,            &
     &    b_trns_fil_frc%i_c_advect, trns)
!
      end subroutine add_fil_scl_flux_bwd_trns_snap
!
!-----------------------------------------------------------------------
!
      subroutine add_rot_fil_frc_sph_trns_snap                          &
     &         (ipol_rot_fil_frc, iphys_rot_fil_frc,                    &
     &          b_trns_rot_fil_frc, trns)
!
      use m_rot_filtered_force_labels
      use add_field_to_sph_trans_list
!
      type(base_force_address), intent(in) :: ipol_rot_fil_frc
      type(base_force_address), intent(in) :: iphys_rot_fil_frc
!
      type(base_force_address), intent(inout) :: b_trns_rot_fil_frc
      type(spherical_transform_data), intent(inout) :: trns
!
!
      call add_field_name_4_sph_trns_snap(rot_inertia_by_filtered,      &
     &    ipol_rot_fil_frc%i_m_advect, iphys_rot_fil_frc%i_m_advect,    &
     &    b_trns_rot_fil_frc%i_m_advect, trns)
      call add_field_name_4_sph_trns_snap                               &
     &   (rot_Lorentz_force_by_filtered,                                &
     &    ipol_rot_fil_frc%i_lorentz, iphys_rot_fil_frc%i_lorentz,      &
     &    b_trns_rot_fil_frc%i_lorentz, trns)
      call add_field_name_4_sph_trns_snap(rot_filtered_buoyancy,        &
     &    ipol_rot_fil_frc%i_buoyancy, iphys_rot_fil_frc%i_buoyancy,    &
     &    b_trns_rot_fil_frc%i_buoyancy, trns)
      call add_field_name_4_sph_trns_snap(rot_filtered_comp_buoyancy,   &
     &    ipol_rot_fil_frc%i_comp_buo, iphys_rot_fil_frc%i_comp_buo,    &
     &    b_trns_rot_fil_frc%i_comp_buo, trns)
!
      end subroutine add_rot_fil_frc_sph_trns_snap
!
!-----------------------------------------------------------------------
!
      end module add_filter_force_4_sph_trns
