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
!!      logical function check_SGS_terms_ctl(field_name, field_ctl)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!      integer(kind = kint) function check_SGS_terms_id                &
!!     &                    (i_field, field_name, base_fld, SGS_term)
!!        type(base_field_address), intent(in) :: base_fld
!!        type(SGS_term_address), intent(in) :: SGS_term
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
      if( (field_name .eq. fhd_SGS_buoyancy)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_SGS_h_flux, field_ctl)
      else if( (field_name .eq. fhd_SGS_comp_buo)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_SGS_c_flux, field_ctl)
      else if( (field_name .eq. fhd_SGS_induction)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_magne, field_ctl)
        call add_phys_name_ctl(fhd_SGS_vp_induct, field_ctl)
      else if( (field_name .eq. fhd_div_SGS_m_flux)) then
        call add_phys_name_ctl(fhd_SGS_m_flux, field_ctl)
      else if( (field_name .eq. fhd_div_SGS_h_flux)) then
        call add_phys_name_ctl(fhd_SGS_h_flux, field_ctl)
      else if( (field_name .eq. fhd_div_SGS_c_flux)) then
        call add_phys_name_ctl(fhd_SGS_c_flux, field_ctl)
      end if
!
!
      if( (field_name .eq. fhd_SGS_inertia)) then
        call add_phys_name_ctl(fhd_vort, field_ctl)
        call add_phys_name_ctl(fhd_velo, field_ctl)
      else if( (field_name .eq. fhd_SGS_m_flux)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
      else if( (field_name .eq. fhd_SGS_Lorentz)                        &
     &    .or. (field_name .eq. fhd_SGS_maxwell_t) ) then
        call add_phys_name_ctl(fhd_magne, field_ctl)
!
      else if( (field_name .eq. fhd_SGS_vp_induct)                      &
     &    .or. (field_name .eq. fhd_SGS_induct_t)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_magne, field_ctl)
!
      else if( (field_name .eq. fhd_SGS_h_flux)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_temp, field_ctl)
      else if((field_name .eq. fhd_SGS_c_flux)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_light, field_ctl)
      end if
!
      end subroutine add_SGS_terms_ctl
!
! -----------------------------------------------------------------------
!
      logical function check_SGS_terms_ctl(field_name, field_ctl)
!
      use t_control_array_character3
      use add_nodal_fields_ctl
!
      character(len = kchara), intent(in) :: field_name
      type(ctl_array_c3), intent(in) :: field_ctl
!
      logical :: flag
!
!
      flag = .TRUE.
      if( (field_name .eq. fhd_SGS_buoyancy)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &        .and. check_field_list_ctl(fhd_SGS_h_flux, field_ctl)
      else if( (field_name .eq. fhd_SGS_comp_buo)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &        .and. check_field_list_ctl(fhd_SGS_c_flux, field_ctl)
      else if( (field_name .eq. fhd_SGS_induction)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &        .and. check_field_list_ctl(fhd_magne, field_ctl)          &
     &        .and. check_field_list_ctl(fhd_SGS_vp_induct, field_ctl)
      else if( (field_name .eq. fhd_div_SGS_m_flux)) then
        flag = flag                                                     &
     &        .and. check_field_list_ctl(fhd_SGS_m_flux, field_ctl)
      else if( (field_name .eq. fhd_div_SGS_h_flux)) then
        flag = flag                                                     &
     &        .and. check_field_list_ctl(fhd_SGS_h_flux, field_ctl)
      else if( (field_name .eq. fhd_div_SGS_c_flux)) then
        flag = flag                                                     &
     &        .and. check_field_list_ctl(fhd_SGS_c_flux, field_ctl)
      end if
!
      if( (field_name .eq. fhd_SGS_inertia)) then
        flag = flag .and. check_field_list_ctl(fhd_vort, field_ctl)     &
     &              .and. check_field_list_ctl(fhd_velo, field_ctl)
      else if( (field_name .eq. fhd_SGS_m_flux)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)
      else if( (field_name .eq. fhd_SGS_Lorentz)                        &
     &    .or. (field_name .eq. fhd_SGS_maxwell_t) ) then
        flag = flag .and. check_field_list_ctl(fhd_magne, field_ctl)
!
      else if( (field_name .eq. fhd_SGS_vp_induct)                      &
     &    .or. (field_name .eq. fhd_SGS_induct_t)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &              .and. check_field_list_ctl(fhd_magne, field_ctl)
!
      else if( (field_name .eq. fhd_SGS_h_flux)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &              .and. check_field_list_ctl(fhd_temp, field_ctl)
      else if( (field_name .eq. fhd_SGS_c_flux)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &              .and. check_field_list_ctl(fhd_light, field_ctl)
      end if
      check_SGS_terms_ctl = flag
      return
!
      end function check_SGS_terms_ctl
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function check_SGS_terms_id                  &
     &                    (i_field, field_name, base_fld, SGS_term)
!
      integer(kind = kint), intent(in) :: i_field
      character(len = kchara), intent(in) :: field_name
      type(base_field_address), intent(in) :: base_fld
      type(SGS_term_address), intent(in) :: SGS_term
!
      integer(kind = kint) :: iflag
!
!
      iflag = 0
      if( (i_field .eq. SGS_term%i_SGS_buoyancy)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 SGS_term%i_SGS_h_flux, fhd_SGS_h_flux)
      else if( (i_field .eq. SGS_term%i_SGS_comp_buo)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 SGS_term%i_SGS_c_flux, fhd_SGS_c_flux)
      else if( (i_field .eq. SGS_term%i_SGS_induction)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_magne, fhd_magne)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 SGS_term%i_SGS_vp_induct, fhd_SGS_vp_induct)
      else if( (i_field .eq. SGS_term%i_SGS_div_m_flux)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 SGS_term%i_SGS_m_flux, fhd_SGS_m_flux)
      else if( (i_field .eq. SGS_term%i_SGS_div_h_flux)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 SGS_term%i_SGS_h_flux, fhd_SGS_h_flux)
      else if( (i_field .eq. SGS_term%i_SGS_div_c_flux)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 SGS_term%i_SGS_c_flux, fhd_SGS_c_flux)
      end if
!
      if( (i_field .eq. SGS_term%i_SGS_inertia)                         &
     &    .or. (i_field .eq. SGS_term%i_SGS_m_flux)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_vort, fhd_vort)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
      else if( (i_field .eq. SGS_term%i_SGS_m_flux)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
      else if( (i_field .eq. SGS_term%i_SGS_maxwell)                    &
     &    .or. (i_field .eq. SGS_term%i_SGS_Lorentz)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_magne, fhd_magne)
!
      else if( (i_field .eq. SGS_term%i_SGS_vp_induct)                  &
     &    .or. (i_field .eq. SGS_term%i_SGS_induct_t)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_magne, fhd_magne)
!
      else if( (i_field .eq. SGS_term%i_SGS_h_flux)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_temp, fhd_temp)
      else if( (i_field .eq. SGS_term%i_SGS_c_flux)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_light, fhd_light)
      end if
      check_SGS_terms_id = iflag
      return
!
      end function check_SGS_terms_id
!
! ----------------------------------------------------------------------
!
      end module check_SGS_terms
