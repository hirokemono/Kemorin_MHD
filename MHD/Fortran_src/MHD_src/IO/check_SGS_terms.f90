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
!!      subroutine add_SGS_terms_ctl(field_name, field_ctl)
!!@endverbatim
!!
      module check_SGS_terms
!
      use m_precision
      use m_constants
!
      use t_base_field_labels
      use t_SGS_term_labels
!
      implicit  none
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine add_SGS_terms_ctl(field_name, field_ctl)
!
      use t_control_array_character3
      use add_nodal_fields_ctl
!
      character(len = kchara), intent(in) :: field_name
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if( (field_name .eq. SGS_buoyancy%name)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(SGS_heat_flux%name, field_ctl)
      else if( (field_name .eq. SGS_composit_buoyancy%name)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(SGS_composit_flux%name, field_ctl)
      else if( (field_name .eq. fhd_SGS_induction)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_magne, field_ctl)
        call add_phys_name_ctl(fhd_SGS_vp_induct, field_ctl)
      else if( (field_name .eq. fhd_div_SGS_m_flux)) then
        call add_phys_name_ctl(SGS_momentum_flux%name, field_ctl)
      else if( (field_name .eq. fhd_div_SGS_h_flux)) then
        call add_phys_name_ctl(SGS_heat_flux%name, field_ctl)
      else if( (field_name .eq. fhd_div_SGS_c_flux)) then
        call add_phys_name_ctl(SGS_composit_flux%name, field_ctl)
      end if
!
!
      if( (field_name .eq. SGS_inertia%name)) then
        call add_phys_name_ctl(fhd_vort, field_ctl)
        call add_phys_name_ctl(fhd_velo, field_ctl)
      else if( (field_name .eq. SGS_momentum_flux%name)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
      else if( (field_name .eq. SGS_Lorentz%name)                       &
     &    .or. (field_name .eq. SGS_maxwell_tensor%name) ) then
        call add_phys_name_ctl(fhd_magne, field_ctl)
!
      else if( (field_name .eq. fhd_SGS_vp_induct)                      &
     &    .or. (field_name .eq. SGS_induct_tensor%name)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_magne, field_ctl)
!
      else if( (field_name .eq. SGS_heat_flux%name)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_temp, field_ctl)
      else if((field_name .eq. SGS_composit_flux%name)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_light, field_ctl)
      end if
!
      end subroutine add_SGS_terms_ctl
!
! -----------------------------------------------------------------------
!
      end module check_SGS_terms
