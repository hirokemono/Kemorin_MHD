!>@file   check_double_filter_field.f90
!!        module check_double_filter_field
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic fields
!!
!!@verbatim
!!      subroutine add_double_filter_field_ctl(field_name, field_ctl)
!!      logical function check_double_filter_field_ctl                  &
!!     &               (field_name, field_ctl)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!      integer(kind = kint) function check_double_filter_field_id      &
!!     &                   (i_field, field_name, filter_fld, base_fld)
!!@endverbatim
!!
      module check_double_filter_field
!
      use m_precision
      use m_constants
      use t_base_field_labels
      use m_filtered_field_labels
      use m_dble_filter_field_labels
!
      implicit  none
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine add_double_filter_field_ctl(field_name, field_ctl)
!
      use t_control_array_character3
      use add_nodal_fields_ctl
!
      character(len = kchara), intent(in) :: field_name
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(field_name .eq. double_filter_vorticity%name) then
        call add_phys_name_ctl(double_filter_velocity%name, field_ctl)
      else if(field_name .eq. double_filter_current%name) then
        call add_phys_name_ctl(double_filter_magne%name, field_ctl)
!
      else if(field_name .eq. double_filter_grad_temp%name) then
        call add_phys_name_ctl(double_filter_temp%name, field_ctl)
      else if(field_name .eq. double_filter_grad_comp%name) then
        call add_phys_name_ctl                                          &
     &      (double_filter_composition%name, field_ctl)
      end if
!
      if(     field_name .eq. double_filter_velocity%name) then
        call add_phys_name_ctl(fhd_filter_velo, field_ctl)
      else if(field_name .eq. double_filter_magne%name) then
        call add_phys_name_ctl(fhd_filter_magne, field_ctl)
      else if(field_name .eq. double_filter_vector_potential%name) then
        call add_phys_name_ctl(fhd_filter_vecp, field_ctl)
!
      else if(field_name .eq. double_filter_temp%name) then
        call add_phys_name_ctl(fhd_filter_temp, field_ctl)
      else if(field_name .eq. double_filter_composition%name) then
        call add_phys_name_ctl(fhd_filter_comp, field_ctl)
      else if(field_name .eq. double_filter_density%name) then
        call add_phys_name_ctl(fhd_filter_density, field_ctl)
      else if(field_name .eq. double_filter_entropy%name) then
        call add_phys_name_ctl(fhd_filter_entropy, field_ctl)
!
      else if(field_name .eq. double_filter_pert_temp%name) then
        call add_phys_name_ctl(fhd_filter_pert_temp, field_ctl)
      else if(field_name .eq. double_filter_pert_comp%name) then
        call add_phys_name_ctl(fhd_filter_pert_comp, field_ctl)
      else if(field_name .eq. double_filter_pert_density%name) then
        call add_phys_name_ctl(fhd_filter_pert_density, field_ctl)
      else if(field_name .eq. double_filter_pert_entropy%name) then
        call add_phys_name_ctl(fhd_filter_pert_entropy, field_ctl)
      end if
!
      end subroutine add_double_filter_field_ctl
!
! -----------------------------------------------------------------------
!
      end module check_double_filter_field
