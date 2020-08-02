!>@file   set_field_w_symmetry_labels.f90
!!        module set_field_w_symmetry_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic fields with equatorial symmetry
!!
!!@verbatim
!!      subroutine base_vector_w_sym_addresses                          &
!!     &         (i_phys, field_name, sym_base_fld, asym_base_fld, flag)
!!      subroutine base_scalar_w_sym_addresses                          &
!!     &         (i_phys, field_name, sym_base_fld, asym_base_fld, flag)
!!        type(base_field_address), intent(inout) :: sym_base_fld
!!        type(base_field_address), intent(inout) :: asym_base_fld
!!
!! !!!!!  Base field names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   sym_velocity,           asym_velocity:           velocity    u
!!   sym_temperature,        asym_temperature:        temperature T
!!   sym_pressure,           asym_pressure:           pressure    P
!!   sym_density,            asym_density:            density     \rho
!!   sym_vorticity,          asym_vorticity:          vorticity   \omega
!!   sym_vector_potential,   asym_vector_potential:   vector potential A
!!   sym_magnetic_field,     asym_magnetic_field:     magnetic field B
!!   sym_current_density,    asym_current_density:    current density  J
!!   sym_magnetic_potential, asym_magnetic_potential: potential \phi
!!   sym_composition,        asym_composition:        Composition  C
!!   sym_entropy,            asym_entropy:            Entropy S
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module set_field_w_symmetry_labels
!
      use m_precision
      use m_constants
      use t_base_field_labels
!
      implicit  none
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine base_vector_w_sym_addresses                            &
     &         (i_phys, field_name, sym_base_fld, asym_base_fld, flag)
!
      use m_field_w_symmetry_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_field_address), intent(inout) :: sym_base_fld
      type(base_field_address), intent(inout) :: asym_base_fld
      logical, intent(inout) :: flag
!
!
      flag = check_base_vector_symmetry(field_name)
      if(flag) then
        if (field_name .eq. sym_velocity%name) then
          sym_base_fld%i_velo = i_phys
        else if (field_name .eq. sym_vorticity%name) then
          sym_base_fld%i_vort = i_phys
!
        else if (field_name .eq. sym_magnetic_field%name) then
          sym_base_fld%i_magne =    i_phys
        else if (field_name .eq. sym_vector_potential%name) then
          sym_base_fld%i_vecp =     i_phys
        else if (field_name .eq. sym_current_density%name) then
          sym_base_fld%i_current =  i_phys
!
        else if (field_name .eq. asym_velocity%name) then
          asym_base_fld%i_velo = i_phys
        else if (field_name .eq. asym_vorticity%name) then
          asym_base_fld%i_vort = i_phys
!
        else if (field_name .eq. asym_magnetic_field%name) then
          asym_base_fld%i_magne =    i_phys
        else if (field_name .eq. asym_vector_potential%name) then
          asym_base_fld%i_vecp =     i_phys
        else if (field_name .eq. asym_current_density%name) then
          asym_base_fld%i_current =  i_phys
        end if
      end if
!
      end subroutine base_vector_w_sym_addresses
!
! ----------------------------------------------------------------------
!
      subroutine base_scalar_w_sym_addresses                            &
     &         (i_phys, field_name, sym_base_fld, asym_base_fld, flag)
!
      use m_field_w_symmetry_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_field_address), intent(inout) :: sym_base_fld
      type(base_field_address), intent(inout) :: asym_base_fld
      logical, intent(inout) :: flag
!
!
      flag = check_base_scalar_w_symmetry(field_name)
      if(flag) then
        if (field_name .eq. sym_pressure%name) then
          sym_base_fld%i_press = i_phys
        else if (field_name .eq. sym_magnetic_potential%name) then
          sym_base_fld%i_mag_p =    i_phys
        else if (field_name .eq. sym_scalar_potential%name) then
          sym_base_fld%i_scalar_p = i_phys
!
        else if (field_name .eq. sym_temperature%name) then
          sym_base_fld%i_temp =           i_phys
        else if (field_name .eq. sym_composition%name) then
          sym_base_fld%i_light =          i_phys
        else if (field_name .eq. sym_density%name) then
          sym_base_fld%i_density =        i_phys
        else if (field_name .eq. sym_entropy%name) then
          sym_base_fld%i_entropy =        i_phys
!
        else if (field_name .eq. sym_perturbation_temp%name) then
          sym_base_fld%i_per_temp =           i_phys
        else if (field_name                                             &
     &             .eq. sym_perturbation_composition%name) then
          sym_base_fld%i_per_light =          i_phys
        else if (field_name .eq. sym_perturbation_density%name) then
          sym_base_fld%i_per_density =        i_phys
        else if (field_name .eq. sym_perturbation_entropy%name) then
          sym_base_fld%i_per_entropy =        i_phys
!
        else if (field_name .eq. asym_pressure%name) then
          asym_base_fld%i_press = i_phys
        else if (field_name .eq. asym_magnetic_potential%name) then
          asym_base_fld%i_mag_p =    i_phys
        else if (field_name .eq. asym_scalar_potential%name) then
          asym_base_fld%i_scalar_p = i_phys
!
        else if (field_name .eq. asym_temperature%name) then
          asym_base_fld%i_temp =           i_phys
        else if (field_name .eq. asym_composition%name) then
          asym_base_fld%i_light =          i_phys
        else if (field_name .eq. asym_density%name) then
          asym_base_fld%i_density =        i_phys
        else if (field_name .eq. asym_entropy%name) then
          asym_base_fld%i_entropy =        i_phys
!
        else if (field_name .eq. asym_perturbation_temp%name) then
          asym_base_fld%i_per_temp =           i_phys
        else if (field_name                                             &
     &            .eq. asym_perturbation_composition%name) then
          asym_base_fld%i_per_light =          i_phys
        else if (field_name .eq. asym_perturbation_density%name) then
          asym_base_fld%i_per_density =        i_phys
        else if (field_name .eq. asym_perturbation_entropy%name) then
          asym_base_fld%i_per_entropy =        i_phys
        end if
      end if  
!
      end subroutine base_scalar_w_sym_addresses
!
! ----------------------------------------------------------------------
!
      end module set_field_w_symmetry_labels
