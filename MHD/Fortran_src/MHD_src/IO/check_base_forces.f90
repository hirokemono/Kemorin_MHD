!>@file   check_base_forces.f90
!!        module check_base_forces
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic fields
!!
!!@verbatim
!!      subroutine add_base_forces_ctl(field_name, field_ctl)
!!      logical function check_base_forces_ctl(field_name, field_ctl)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!      integer(kind = kint) function check_base_force_id               &
!!     &                    (i_field, field_name, base_fld, base_force)
!!        type(base_field_address), intent(in) :: base_fld
!!        type(base_force_address), intent(in) :: base_force
!!@endverbatim
!!
      module check_base_forces
!
      use m_precision
      use m_constants
!
      use t_base_field_labels
      use t_base_force_labels
!
      implicit  none
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine add_base_forces_ctl(field_name, field_ctl)
!
      use t_control_array_character3
      use add_nodal_fields_ctl
!
      character(len = kchara), intent(in) :: field_name
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(      (field_name .eq. pressure_gradient%name)) then
        call add_phys_name_ctl(fhd_press, field_ctl)
      else if( (field_name .eq. inertia%name)                           &
     &    .or. (field_name .eq. momentum_flux%name)) then
        call add_phys_name_ctl(fhd_vort, field_ctl)
        call add_phys_name_ctl(fhd_velo, field_ctl)
      else if( (field_name .eq. Lorentz_force%name)) then
        call add_phys_name_ctl(fhd_current, field_ctl)
        call add_phys_name_ctl(fhd_magne, field_ctl)
      else if( (field_name .eq. maxwell_tensor%name)                    &
     &    .or. (field_name .eq. magnetic_tension%name)) then
        call add_phys_name_ctl(fhd_magne, field_ctl)
      else if( (field_name .eq. Coriolis_force%name)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
      else if( (field_name .eq. buoyancy%name)) then
        call add_phys_name_ctl(fhd_temp, field_ctl)
      else if( (field_name .eq. composite_buoyancy%name)) then
        call add_phys_name_ctl(fhd_light, field_ctl)
!
      else if( (field_name .eq. vecp_induction%name)                    &
     &    .or. (field_name .eq. magnetic_stretch%name)                  &
     &    .or. (field_name .eq. induction_tensor%name)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_magne, field_ctl)
      else if( (field_name .eq. magnetic_induction%name)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_magne, field_ctl)
        call add_phys_name_ctl(vecp_induction%name, field_ctl)
!
      else if( (field_name .eq. heat_advect%name)                       &
     &    .or. (field_name .eq. heat_flux%name)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_temp, field_ctl)
      else if( (field_name .eq. pert_heat_advect%name)                  &
     &    .or. (field_name .eq. pert_heat_flux%name)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_part_temp, field_ctl)
      else if( (field_name .eq. composition_advect%name)                &
     &    .or. (field_name .eq. composite_flux%name)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_light, field_ctl)
      else if( (field_name .eq. pert_comp_advect%name)                  &
     &    .or. (field_name .eq. pert_comp_flux%name)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_part_light, field_ctl)
      end if
!
      end subroutine add_base_forces_ctl
!
! -----------------------------------------------------------------------
!
      end module check_base_forces
