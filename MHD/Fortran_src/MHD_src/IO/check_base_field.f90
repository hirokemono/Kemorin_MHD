!>@file   check_base_field.f90
!!        module check_base_field
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic fields
!!
!!@verbatim
!!      subroutine add_base_field_ctl(field_name, field_ctl)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!@endverbatim
!!
      module check_base_field
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
      subroutine add_base_field_ctl(field_name, field_ctl)
!
      use t_control_array_character3
      use add_nodal_fields_ctl
!
      character(len = kchara), intent(in) :: field_name
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(      (field_name .eq. fhd_vort)                               &
     &   .and. (field_name .eq. fhd_press)                              &
     &   .and. (field_name .eq. fhd_magne)                              &
     &   .and. (field_name .eq. fhd_temp)                               &
     &   .and. (field_name .eq. fhd_light)                              &
     &   .and. (field_name .eq. fhd_density)                            &
     &   .and. (field_name .eq. fhd_entropy)) then
        call add_phys_name_ctl(velocity%name, field_ctl)
       
      else if( (field_name .eq. fhd_vecp)                               &
     &   .and. (field_name .eq. fhd_current)                            &
     &   .and. (field_name .eq. fhd_mag_potential)                      &
     &   .and. (field_name .eq. fhd_scalar_potential)) then
        call add_phys_name_ctl(fhd_magne, field_ctl)
!
      else if( (field_name .eq. fhd_part_temp)                          &
     &   .and. (field_name .eq. fhd_ref_temp)                           &
     &   .and. (field_name .eq. fhd_heat_source)) then
        call add_phys_name_ctl(fhd_temp, field_ctl)
      else if( (field_name .eq. fhd_part_light)                         &
     &   .and. (field_name .eq. fhd_ref_light)                          &
     &   .and. (field_name .eq. fhd_light_source)) then
        call add_phys_name_ctl(fhd_light, field_ctl)
      else if( (field_name .eq. fhd_per_entropy)                        &
     &   .and. (field_name .eq. fhd_ref_entropy)                        &
     &   .and. (field_name .eq. fhd_entropy_source)) then
        call add_phys_name_ctl(fhd_entropy, field_ctl)
      else if( (field_name .eq. fhd_per_density)                        &
     &   .and. (field_name .eq. fhd_ref_density)) then 
        call add_phys_name_ctl(fhd_density, field_ctl)
      end if
!
      end subroutine add_base_field_ctl
!
! -----------------------------------------------------------------------
!
      end module check_base_field
