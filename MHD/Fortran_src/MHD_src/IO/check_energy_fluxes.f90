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
      if( (field_name .eq. inertia_work%name) ) then
        call add_phys_name_ctl(velocity%name, field_ctl)
        call add_phys_name_ctl(inertia%name, field_ctl)
      else if( (field_name .eq. work_against_Lorentz%name)              &
     &    .or. (field_name .eq. Lorentz_work%name)) then
        call add_phys_name_ctl(velocity%name, field_ctl)
        call add_phys_name_ctl(magnetic_field%name, field_ctl)
        call add_phys_name_ctl(fhd_current, field_ctl)
        call add_phys_name_ctl(Lorentz_force%name, field_ctl)
      else if( (field_name .eq. mag_tension_work%name)) then
        call add_phys_name_ctl(velocity%name, field_ctl)
        call add_phys_name_ctl(magnetic_tension%name, field_ctl)
!
      else if( (field_name .eq. buoyancy_flux%name)) then
        call add_phys_name_ctl(velocity%name, field_ctl)
        call add_phys_name_ctl(fhd_temp, field_ctl)
        call add_phys_name_ctl(buoyancy%name, field_ctl)
      else if( (field_name .eq. composite_buoyancy_flux%name)) then
        call add_phys_name_ctl(velocity%name, field_ctl)
        call add_phys_name_ctl(fhd_light, field_ctl)
        call add_phys_name_ctl(composite_buoyancy%name, field_ctl)
!
      else if( (field_name .eq. magnetic_ene_generation%name)) then
        call add_phys_name_ctl(magnetic_field%name, field_ctl)
        call add_phys_name_ctl(magnetic_induction%name, field_ctl)
      else if( (field_name .eq. magnetic_stretch_flux%name)) then
        call add_phys_name_ctl(magnetic_field%name, field_ctl)
        call add_phys_name_ctl(magnetic_stretch%name, field_ctl)
!
      else if( (field_name .eq. temp_generation%name)) then
        call add_phys_name_ctl(fhd_temp, field_ctl)
        call add_phys_name_ctl(heat_advect%name, field_ctl)
      else if( (field_name .eq. pert_temp_generation%name)) then
        call add_phys_name_ctl(fhd_part_temp, field_ctl)
        call add_phys_name_ctl(pert_heat_advect%name, field_ctl)
      else if( (field_name .eq. comp_generation%name)) then
        call add_phys_name_ctl(fhd_light, field_ctl)
        call add_phys_name_ctl(composition_advect%name, field_ctl)
      else if( (field_name .eq. pert_comp_generation%name)) then
        call add_phys_name_ctl(fhd_part_light, field_ctl)
        call add_phys_name_ctl(pert_comp_advect%name, field_ctl)
!
      else if( (field_name .eq. viscous_ene_diffusion%name)) then
        call add_phys_name_ctl(velocity%name, field_ctl)
        call add_phys_name_ctl(viscous_diffusion%name, field_ctl)
      else if( (field_name .eq. magnetic_ene_diffusion%name)) then
        call add_phys_name_ctl(magnetic_field%name, field_ctl)
        call add_phys_name_ctl(magnetic_diffusion%name, field_ctl)
      end if
!
      end subroutine add_energy_fluxes_ctl
!
! -----------------------------------------------------------------------
!
      end module check_energy_fluxes
