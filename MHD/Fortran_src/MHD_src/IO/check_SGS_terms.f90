!>@file   check_SGS_terms.f90
!!        module check_SGS_terms
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic fields
!!
!!@verbatim
!!      subroutine add_field_ctl_4_SGS_terms(field_ctl)
!!@endverbatim
!!
      module check_SGS_terms
!
      use m_precision
      use m_constants
!
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
      subroutine add_field_ctl_4_SGS_terms(field_ctl)
!
      use t_control_array_character3
      use t_SGS_term_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(SGS_buoyancy%name, field_ctl)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(SGS_heat_flux%name, field_ctl)
      else if(check_field_list_ctl(SGS_composit_buoyancy%name,          &
     &                             field_ctl)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(SGS_composit_flux%name, field_ctl)
      else if(check_field_list_ctl(SGS_induction%name, field_ctl)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_magne, field_ctl)
        call add_phys_name_ctl(SGS_vecp_induction%name, field_ctl)
      else if(check_field_list_ctl(div_SGS_m_flux%name,                 &
     &                             field_ctl)) then
        call add_phys_name_ctl(SGS_momentum_flux%name, field_ctl)
      else if(check_field_list_ctl(div_SGS_h_flux%name,                 &
     &                             field_ctl)) then
        call add_phys_name_ctl(SGS_heat_flux%name, field_ctl)
      else if(check_field_list_ctl(div_SGS_c_flux%name,                 &
     &                             field_ctl)) then
        call add_phys_name_ctl(SGS_composit_flux%name, field_ctl)
      end if
!
!
      if(check_field_list_ctl(SGS_inertia%name, field_ctl)) then
        call add_phys_name_ctl(fhd_vort, field_ctl)
        call add_phys_name_ctl(fhd_velo, field_ctl)
      else if(check_field_list_ctl(SGS_momentum_flux%name,              &
     &                             field_ctl)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
      else if(check_field_list_ctl(SGS_Lorentz%name, field_ctl)         &
     &   .or. check_field_list_ctl(SGS_maxwell_tensor%name,             &
     &                             field_ctl)) then
        call add_phys_name_ctl(fhd_magne, field_ctl)
!
      else if(check_field_list_ctl(SGS_vecp_induction%name, field_ctl)  &
     &   .or. check_field_list_ctl(SGS_induct_tensor%name,              &
     &                             field_ctl)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_magne, field_ctl)
!
      else if(check_field_list_ctl(SGS_heat_flux%name, field_ctl)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_temp, field_ctl)
      else if(check_field_list_ctl(SGS_composit_flux%name,             &
     &                             field_ctl)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_light, field_ctl)
      end if
!
      end subroutine add_field_ctl_4_SGS_terms
!
! -----------------------------------------------------------------------
!
      end module check_SGS_terms
