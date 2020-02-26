!>@file   check_energy_fluxes.f90
!!        module check_energy_fluxes
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic fields
!!
!!@verbatim
!!      subroutine add_energy_fluxes_ctl(field_name, field_ctl)
!!      logical function check_energy_fluxes_ctl(field_name, field_ctl)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!      integer(kind = kint) function check_base_ene_fluxes_id          &
!!     &                   (i_field, field_name,                        &
!!     &                    base_fld, base_force, ene_flux)
!!        type(base_field_address), intent(in) :: base_fld
!!        type(base_force_address), intent(in) :: base_force
!!        type(energy_flux_address), intent(in) :: ene_flux
!!@endverbatim
!!
      module check_energy_fluxes
!
      use m_precision
      use m_constants
!
      use t_base_field_labels
      use t_base_force_labels
      use t_energy_flux_labels
!
      implicit  none
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine add_energy_fluxes_ctl(field_name, field_ctl)
!
      use t_control_array_character3
      use add_nodal_fields_ctl
!
      character(len = kchara), intent(in) :: field_name
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if( (field_name .eq. fhd_inertia_work) ) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_inertia, field_ctl)
      else if( (field_name .eq. fhd_work_agst_Lorentz)                  &
     &    .or. (field_name .eq. fhd_Lorentz_work)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_Lorentz, field_ctl)
      else if( (field_name .eq. fhd_mag_tension_work)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_mag_tension, field_ctl)
!
      else if( (field_name .eq. fhd_buoyancy_flux)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_buoyancy, field_ctl)
      else if( (field_name .eq. fhd_comp_buo_flux)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_comp_buo, field_ctl)
!
      else if( (field_name .eq. fhd_mag_ene_gen)) then
        call add_phys_name_ctl(fhd_magne, field_ctl)
        call add_phys_name_ctl(fhd_mag_induct, field_ctl)
      else if( (field_name .eq. fhd_mag_stretch_flux)) then
        call add_phys_name_ctl(fhd_magne, field_ctl)
        call add_phys_name_ctl(fhd_mag_stretch, field_ctl)
!
      else if( (field_name .eq. fhd_temp_generation)) then
        call add_phys_name_ctl(fhd_temp, field_ctl)
        call add_phys_name_ctl(fhd_heat_advect, field_ctl)
      else if( (field_name .eq. fhd_part_temp_gen)) then
        call add_phys_name_ctl(fhd_part_temp, field_ctl)
        call add_phys_name_ctl(fhd_part_h_advect, field_ctl)
      else if( (field_name .eq. fhd_comp_generation)) then
        call add_phys_name_ctl(fhd_light, field_ctl)
        call add_phys_name_ctl(fhd_composit_advect, field_ctl)
      else if( (field_name .eq. fhd_part_comp_gen)) then
        call add_phys_name_ctl(fhd_part_light, field_ctl)
        call add_phys_name_ctl(fhd_part_c_advect, field_ctl)
!
!      else if( (field_name .eq. fhd_vis_ene_diffuse)) then
!        call add_phys_name_ctl(fhd_velo, field_ctl)
!        call add_phys_name_ctl(fhd_viscous, field_ctl)
!      else if( (field_name .eq. fhd_mag_ene_diffuse)) then
!        call add_phys_name_ctl(fhd_magne, field_ctl)
!        call add_phys_name_ctl(fhd_mag_diffuse, field_ctl)
      end if
!
      end subroutine add_energy_fluxes_ctl
!
! -----------------------------------------------------------------------
!
      logical function check_energy_fluxes_ctl(field_name, field_ctl)
!
      use t_control_array_character3
      use add_nodal_fields_ctl
!
      character(len = kchara), intent(in) :: field_name
      type(ctl_array_c3), intent(in) :: field_ctl
!
      logical :: flag
!
!
      flag = .TRUE.
      if( (field_name .eq. fhd_inertia_work) ) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &              .and. check_field_list_ctl(fhd_inertia, field_ctl)
      else if( (field_name .eq. fhd_work_agst_Lorentz)                  &
     &    .or. (field_name .eq. fhd_Lorentz_work)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &              .and. check_field_list_ctl(fhd_Lorentz, field_ctl)
      else if( (field_name .eq. fhd_mag_tension_work)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &        .and. check_field_list_ctl(fhd_mag_tension, field_ctl)
!
      else if( (field_name .eq. fhd_buoyancy_flux)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &              .and. check_field_list_ctl(fhd_buoyancy, field_ctl)
      else if( (field_name .eq. fhd_comp_buo_flux)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &              .and. check_field_list_ctl(fhd_comp_buo, field_ctl)
!
      else if( (field_name .eq. fhd_mag_ene_gen)) then
        flag = flag .and. check_field_list_ctl(fhd_magne, field_ctl)    &
     &        .and. check_field_list_ctl(fhd_mag_induct, field_ctl)
      else if( (field_name .eq. fhd_mag_stretch_flux)) then
        flag = flag .and. check_field_list_ctl(fhd_magne, field_ctl)    &
     &        .and. check_field_list_ctl(fhd_mag_stretch, field_ctl)
!
      else if( (field_name .eq. fhd_temp_generation)) then
        flag = flag .and. check_field_list_ctl(fhd_temp, field_ctl)     &
     &        .and. check_field_list_ctl(fhd_heat_advect, field_ctl)
      else if( (field_name .eq. fhd_part_temp_gen)) then
        flag = flag                                                     &
     &        .and. check_field_list_ctl(fhd_part_temp, field_ctl)      &
     &        .and. check_field_list_ctl(fhd_part_h_advect, field_ctl)
      else if( (field_name .eq. fhd_comp_generation)) then
        flag = flag .and. check_field_list_ctl(fhd_light, field_ctl)    &
     &      .and. check_field_list_ctl(fhd_composit_advect, field_ctl)
      else if( (field_name .eq. fhd_part_comp_gen)) then
        flag = flag                                                     &
     &        .and. check_field_list_ctl(fhd_part_light, field_ctl)     &
     &        .and. check_field_list_ctl(fhd_part_c_advect, field_ctl)
!
!      else if( (field_name .eq. fhd_vis_ene_diffuse)) then
!        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)    &
!     &        .and. check_field_list_ctl(fhd_viscous, field_ctl)
!      else if( (field_name .eq. fhd_mag_ene_diffuse)) then
!        flag = flag .and. check_field_list_ctl(fhd_magne, field_ctl)   &
!     &        .and. check_field_list_ctl(fhd_mag_diffuse, field_ctl)
      end if
      check_energy_fluxes_ctl = flag
      return
!
      end function check_energy_fluxes_ctl
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function check_base_ene_fluxes_id            &
     &                   (i_field, field_name,                          &
     &                    base_fld, base_force, ene_flux)
!
      integer(kind = kint), intent(in) :: i_field
      character(len = kchara), intent(in) :: field_name
      type(base_field_address), intent(in) :: base_fld
      type(base_force_address), intent(in) :: base_force
      type(energy_flux_address), intent(in) :: ene_flux
!
      integer(kind = kint) :: iflag
!
!
      iflag = 0
      if( (i_field .eq. ene_flux%i_m_advect_work) ) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_force%i_m_advect, fhd_inertia)
      else if( (i_field .eq. ene_flux%i_nega_ujb)                       &
     &    .or. (i_field .eq. ene_flux%i_ujb)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_force%i_lorentz, fhd_Lorentz)
      else if( (i_field .eq. ene_flux%i_m_tension_wk)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_force%i_m_tension, fhd_mag_tension)
!
      else if( (i_field .eq. ene_flux%i_buo_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_force%i_buoyancy, fhd_buoyancy)
      else if( (i_field .eq. ene_flux%i_c_buo_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_force%i_comp_buo, fhd_comp_buo)
!
      else if( (i_field .eq. ene_flux%i_me_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_magne, fhd_magne)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_force%i_induction, fhd_mag_induct)
      else if( (i_field .eq. ene_flux%i_mag_stretch_flux)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_magne, fhd_magne)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_force%i_mag_stretch, fhd_mag_stretch)
!
      else if( (i_field .eq. ene_flux%i_temp_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_temp, fhd_temp)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_force%i_h_advect, fhd_heat_advect)
      else if( (i_field .eq. ene_flux%i_par_t_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_per_temp, fhd_part_temp)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_force%i_ph_advect, fhd_part_h_advect)
      else if( (i_field .eq. ene_flux%i_comp_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_light, fhd_light)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_force%i_c_advect, fhd_composit_advect)
      else if( (i_field .eq. ene_flux%i_par_c_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_per_light, fhd_part_light)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_force%i_pc_advect, fhd_part_c_advect)
!
!      else if( (i_field .eq. ene_flux%i_vis_e_diffuse)) then
!        iflag = iflag + missing_field(i_field, field_name,             &
!     &                 base_fld%i_velo, fhd_velo)
!        iflag = iflag + missing_field(i_field, field_name,             &
!     &                 base_force%i_v_diffuse, fhd_viscous)
!      else if( (i_field .eq. ene_flux%i_mag_e_diffuse)) then
!        iflag = iflag + missing_field(i_field, field_name,             &
!     &                 base_fld%i_magne, fhd_magne)
!        iflag = iflag + missing_field(i_field, field_name,             &
!     &                 base_force%i_b_diffuse, fhd_mag_diffuse)
      end if
      check_base_ene_fluxes_id = iflag
      return
!
      end function check_base_ene_fluxes_id
!
! ----------------------------------------------------------------------
!
      end module check_energy_fluxes
