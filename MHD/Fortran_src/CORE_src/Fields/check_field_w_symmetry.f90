!>@file   check_field_w_symmetry.f90
!!        module check_field_w_symmetry
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic fields
!!
!!@verbatim
!!      subroutine add_field_w_symmetry_ctl(field_name, field_ctl)
!!      logical function check_field_w_symmetry_ctl                     &
!!     &               (field_name, field_ctl)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!      integer(kind = kint) function check_field_w_symmetry_id         &
!!     &                            (i_field, field_name, base_fld,     &
!!     &                             sym_base_fld, asym_base_fld)
!!@endverbatim
!!
      module check_field_w_symmetry
!
      use m_precision
      use m_constants
!
      use t_base_field_labels
      use m_field_w_symmetry_labels
!
      implicit  none
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine add_field_w_symmetry_ctl(field_name, field_ctl)
!
      use t_control_array_character3
      use add_nodal_fields_ctl
!
      character(len = kchara), intent(in) :: field_name
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(      (field_name .eq. sym_velocity%name)                      &
     &    .or. (field_name .eq. asym_velocity%name)) then
        call add_phys_name_ctl(velocity, field_ctl)
      else if( (field_name .eq. sym_vorticity%name)                     &
     &    .or. (field_name .eq. asym_vorticity%name)) then
        call add_phys_name_ctl(vorticity, field_ctl)
      else if( (field_name .eq. sym_magnetic_field%name)                &
     &    .or. (field_name .eq. asym_magnetic_field%name)) then
        call add_phys_name_ctl(magnetic_field, field_ctl)
      else if( (field_name .eq. sym_vector_potential%name)              &
     &    .or. (field_name .eq. asym_vector_potential%name)) then
        call add_phys_name_ctl(vector_potential, field_ctl)
      else if( (field_name .eq. sym_current_density%name)               &
     &    .or. (field_name .eq. asym_current_density%name)) then
        call add_phys_name_ctl(current_density, field_ctl)
!
      else if( (field_name .eq. sym_pressure%name)                      &
     &    .or. (field_name .eq. asym_pressure%name)) then
        call add_phys_name_ctl(pressure, field_ctl)
      else if( (field_name .eq. sym_magnetic_potential%name)            &
     &    .or. (field_name .eq. asym_magnetic_potential%name)) then
        call add_phys_name_ctl(magnetic_potential, field_ctl)
      else if( (field_name .eq. sym_scalar_potential%name)              &
     &    .or. (field_name .eq. asym_scalar_potential%name)) then
        call add_phys_name_ctl(scalar_potential, field_ctl)
!
      else if( (field_name .eq. sym_temperature%name)                   &
     &    .or. (field_name .eq. asym_temperature%name)) then
        call add_phys_name_ctl(temperature, field_ctl)
      else if( (field_name .eq. sym_composition%name)                   &
     &    .or. (field_name .eq. asym_composition%name)) then
        call add_phys_name_ctl(composition, field_ctl)
      else if( (field_name .eq. sym_density%name)                       &
     &    .or. (field_name .eq. asym_density%name)) then
        call add_phys_name_ctl(density, field_ctl)
      else if( (field_name .eq. sym_entropy%name)                       &
     &    .or. (field_name .eq. asym_entropy%name)) then
        call add_phys_name_ctl(entropy, field_ctl)
!
      else if( (field_name .eq. sym_perturbation_temp%name)             &
     &    .or. (field_name .eq. asym_perturbation_temp%name)) then
        call add_phys_name_ctl(perturbation_temp, field_ctl)
      else if( (field_name .eq. sym_perturbation_composition%name)      &
     &  .or. (field_name .eq. asym_perturbation_composition%name)) then
        call add_phys_name_ctl(perturbation_composition, field_ctl)
      else if( (field_name .eq. sym_perturbation_density%name)          &
     &    .or. (field_name .eq. asym_perturbation_density%name)) then
        call add_phys_name_ctl(perturbation_density, field_ctl)
      else if( (field_name .eq. sym_perturbation_entropy%name)          &
     &    .or. (field_name .eq. asym_perturbation_entropy%name)) then
        call add_phys_name_ctl(perturbation_entropy, field_ctl)
      end if
!
      end subroutine add_field_w_symmetry_ctl
!
! -----------------------------------------------------------------------
!
      end module check_field_w_symmetry
