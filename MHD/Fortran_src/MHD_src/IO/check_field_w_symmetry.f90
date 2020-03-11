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
      if(      (field_name .eq. fhd_sym_velo)                           &
     &    .or. (field_name .eq. fhd_asym_velo)) then 
        call add_phys_name_ctl(velocity%name, field_ctl)
      else if( (field_name .eq. fhd_sym_vort)                           &
     &    .or. (field_name .eq. fhd_asym_vort)) then 
        call add_phys_name_ctl(vorticity%name, field_ctl)
      else if( (field_name .eq. fhd_sym_magne)                          &
     &    .or. (field_name .eq. fhd_asym_magne)) then 
        call add_phys_name_ctl(magnetic_field%name, field_ctl)
      else if( (field_name .eq. fhd_sym_vecp)                           &
     &    .or. (field_name .eq. fhd_asym_vecp)) then 
        call add_phys_name_ctl(vector_potential%name, field_ctl)
      else if( (field_name .eq. fhd_sym_current)                        &
     &    .or. (field_name .eq. fhd_asym_current)) then 
        call add_phys_name_ctl(current_density%name, field_ctl)
!
      else if( (field_name .eq. fhd_sym_press)                          &
     &    .or. (field_name .eq. fhd_asym_press)) then 
        call add_phys_name_ctl(pressure%name, field_ctl)
      else if( (field_name .eq. fhd_sym_mag_potential)                  &
     &    .or. (field_name .eq. fhd_asym_mag_potential)) then 
        call add_phys_name_ctl(magnetic_potential%name, field_ctl)
      else if( (field_name .eq. fhd_sym_scalar_potential)               &
     &    .or. (field_name .eq. fhd_asym_scalar_potential)) then 
        call add_phys_name_ctl(scalar_potential%name, field_ctl)
!
      else if( (field_name .eq. fhd_sym_temp)                           &
     &    .or. (field_name .eq. fhd_asym_temp)) then 
        call add_phys_name_ctl(temperature%name, field_ctl)
      else if( (field_name .eq. fhd_sym_light)                          &
     &    .or. (field_name .eq. fhd_asym_light)) then 
        call add_phys_name_ctl(composition%name, field_ctl)
      else if( (field_name .eq. fhd_sym_density)                        &
     &    .or. (field_name .eq. fhd_asym_density)) then 
        call add_phys_name_ctl(density%name, field_ctl)
      else if( (field_name .eq. fhd_sym_entropy)                        &
     &    .or. (field_name .eq. fhd_asym_entropy)) then 
        call add_phys_name_ctl(entropy%name, field_ctl)
!
      else if( (field_name .eq. fhd_sym_per_temp)                       &
     &    .or. (field_name .eq. fhd_asym_per_temp)) then 
        call add_phys_name_ctl(perturbation_temp%name, field_ctl)
      else if( (field_name .eq. fhd_sym_per_light)                      &
     &    .or. (field_name .eq. fhd_asym_per_light)) then 
        call add_phys_name_ctl(perturbation_composition%name,           &
     &      field_ctl)
      else if( (field_name .eq. fhd_sym_per_density)                    &
     &    .or. (field_name .eq. fhd_asym_per_density)) then 
        call add_phys_name_ctl(perturbation_density%name, field_ctl)
      else if( (field_name .eq. fhd_sym_per_entropy)                    &
     &    .or. (field_name .eq. fhd_asym_per_entropy)) then 
        call add_phys_name_ctl(perturbation_entropy%name, field_ctl)
      end if
!
      end subroutine add_field_w_symmetry_ctl
!
! -----------------------------------------------------------------------
!
      end module check_field_w_symmetry
