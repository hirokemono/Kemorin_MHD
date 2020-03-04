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
      use t_diffusion_term_labels
      use add_nodal_fields_ctl
!
      character(len = kchara), intent(in) :: field_name
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if( (field_name .eq. fhd_inertia_work) ) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(inertia%name, field_ctl)
      else if( (field_name .eq. fhd_work_agst_Lorentz)                  &
     &    .or. (field_name .eq. fhd_Lorentz_work)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(Lorentz_force%name, field_ctl)
      else if( (field_name .eq. fhd_mag_tension_work)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(magnetic_tension%name, field_ctl)
!
      else if( (field_name .eq. fhd_buoyancy_flux)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(buoyancy%name, field_ctl)
      else if( (field_name .eq. fhd_comp_buo_flux)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(composite_buoyancy%name, field_ctl)
!
      else if( (field_name .eq. fhd_mag_ene_gen)) then
        call add_phys_name_ctl(fhd_magne, field_ctl)
        call add_phys_name_ctl(magnetic_induction%name, field_ctl)
      else if( (field_name .eq. fhd_mag_stretch_flux)) then
        call add_phys_name_ctl(fhd_magne, field_ctl)
        call add_phys_name_ctl(magnetic_stretch%name, field_ctl)
!
      else if( (field_name .eq. fhd_temp_generation)) then
        call add_phys_name_ctl(fhd_temp, field_ctl)
        call add_phys_name_ctl(heat_advect%name, field_ctl)
      else if( (field_name .eq. fhd_part_temp_gen)) then
        call add_phys_name_ctl(fhd_part_temp, field_ctl)
        call add_phys_name_ctl(pert_heat_advect%name, field_ctl)
      else if( (field_name .eq. fhd_comp_generation)) then
        call add_phys_name_ctl(fhd_light, field_ctl)
        call add_phys_name_ctl(composition_advect%name, field_ctl)
      else if( (field_name .eq. fhd_part_comp_gen)) then
        call add_phys_name_ctl(fhd_part_light, field_ctl)
        call add_phys_name_ctl(pert_comp_advect%name, field_ctl)
!
      else if( (field_name .eq. fhd_vis_ene_diffuse)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_viscous, field_ctl)
      else if( (field_name .eq. fhd_mag_ene_diffuse)) then
        call add_phys_name_ctl(fhd_magne, field_ctl)
        call add_phys_name_ctl(fhd_mag_diffuse, field_ctl)
!
      else if( (field_name .eq. pressure_work%name)) then
        call add_phys_name_ctl(fhd_press, field_ctl)
      else if( (field_name .eq. m_potential_work%name)) then
        call add_phys_name_ctl(fhd_magne, field_ctl)
        call add_phys_name_ctl(fhd_mag_diffuse, field_ctl)
      end if
!
      end subroutine add_energy_fluxes_ctl
!
! -----------------------------------------------------------------------
!
      end module check_energy_fluxes
