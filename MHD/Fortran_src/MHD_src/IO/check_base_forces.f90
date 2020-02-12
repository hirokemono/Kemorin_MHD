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
      if(      (field_name .eq. fhd_press_grad)) then
        call add_phys_name_ctl(fhd_press, field_ctl)
      else if( (field_name .eq. fhd_inertia)                            &
     &    .or. (field_name .eq. fhd_mom_flux)) then
        call add_phys_name_ctl(fhd_vort, field_ctl)
        call add_phys_name_ctl(fhd_velo, field_ctl)
      else if( (field_name .eq. fhd_Lorentz)) then
        call add_phys_name_ctl(fhd_current, field_ctl)
        call add_phys_name_ctl(fhd_magne, field_ctl)
      else if( (field_name .eq. fhd_maxwell_t)                          &
     &    .or. (field_name .eq. fhd_mag_tension)) then
        call add_phys_name_ctl(fhd_magne, field_ctl)
      else if( (field_name .eq. fhd_Coriolis)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
      else if( (field_name .eq. fhd_buoyancy)) then
        call add_phys_name_ctl(fhd_temp, field_ctl)
      else if( (field_name .eq. fhd_comp_buo)) then
        call add_phys_name_ctl(fhd_light, field_ctl)
!
      else if( (field_name .eq. fhd_vp_induct)                          &
     &    .or. (field_name .eq. fhd_mag_stretch)                        &
     &    .or. (field_name .eq. fhd_induct_t)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_magne, field_ctl)
      else if( (field_name .eq. fhd_mag_induct)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_magne, field_ctl)
        call add_phys_name_ctl(fhd_vp_induct, field_ctl)
      else if( (field_name .eq. fhd_e_field)                            &
     &    .or. (field_name .eq. fhd_poynting)) then
       call add_phys_name_ctl(fhd_vp_induct, field_ctl)
       call add_phys_name_ctl(fhd_current, field_ctl)
!
      else if( (field_name .eq. fhd_heat_advect)                        &
     &    .or. (field_name .eq. fhd_h_flux)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_temp, field_ctl)
      else if( (field_name .eq. fhd_part_h_advect)                      &
     &    .or. (field_name .eq. fhd_ph_flux)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_part_temp, field_ctl)
      else if( (field_name .eq. fhd_composit_advect)                    &
     &    .or. (field_name .eq. fhd_c_flux)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_light, field_ctl)
      else if( (field_name .eq. fhd_part_c_advect)                      &
     &    .or. (field_name .eq. fhd_pc_flux)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_part_light, field_ctl)
      end if
!
      end subroutine add_base_forces_ctl
!
! -----------------------------------------------------------------------
!
      logical function check_base_forces_ctl(field_name, field_ctl)
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
      if(      (field_name .eq. fhd_press_grad)) then
        flag = flag .and. check_field_list_ctl(fhd_press, field_ctl)
      else if( (field_name .eq. fhd_inertia)                            &
     &    .or. (field_name .eq. fhd_mom_flux)) then
        flag = flag .and. check_field_list_ctl(fhd_vort, field_ctl)     &
     &              .and. check_field_list_ctl(fhd_velo, field_ctl)
      else if( (field_name .eq. fhd_Lorentz)) then
        flag = flag .and. check_field_list_ctl(fhd_current, field_ctl)  &
     &              .and. check_field_list_ctl(fhd_magne, field_ctl)
      else if( (field_name .eq. fhd_maxwell_t)                          &
     &    .or. (field_name .eq. fhd_mag_tension)) then
        flag = flag .and. check_field_list_ctl(fhd_magne, field_ctl)
      else if( (field_name .eq. fhd_Coriolis)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)
      else if( (field_name .eq. fhd_buoyancy)) then
        flag = flag .and. check_field_list_ctl(fhd_temp, field_ctl)
      else if( (field_name .eq. fhd_comp_buo)) then
        flag = flag .and. check_field_list_ctl(fhd_light, field_ctl)
!
      else if( (field_name .eq. fhd_vp_induct)                          &
     &    .or. (field_name .eq. fhd_mag_stretch)                        &
     &    .or. (field_name .eq. fhd_induct_t)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &              .and. check_field_list_ctl(fhd_magne, field_ctl)
      else if( (field_name .eq. fhd_mag_induct)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &        .and. check_field_list_ctl(fhd_magne, field_ctl)          &
     &        .and. check_field_list_ctl(fhd_vp_induct, field_ctl)
      else if( (field_name .eq. fhd_e_field)                            &
     &    .or. (field_name .eq. fhd_poynting)) then
        flag = flag                                                     &
     &        .and. check_field_list_ctl(fhd_vp_induct, field_ctl)      &
     &        .and. check_field_list_ctl(fhd_current, field_ctl)
!
      else if( (field_name .eq. fhd_heat_advect)                        &
     &    .or. (field_name .eq. fhd_h_flux)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &              .and. check_field_list_ctl(fhd_temp, field_ctl)
      else if( (field_name .eq. fhd_part_h_advect)                      &
     &    .or. (field_name .eq. fhd_ph_flux)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &        .and. check_field_list_ctl(fhd_part_temp, field_ctl)
      else if( (field_name .eq. fhd_composit_advect)                    &
     &    .or. (field_name .eq. fhd_c_flux)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &              .and. check_field_list_ctl(fhd_light, field_ctl)
      else if( (field_name .eq. fhd_part_c_advect)                      &
     &    .or. (field_name .eq. fhd_pc_flux)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &        .and. check_field_list_ctl(fhd_part_light, field_ctl)
      end if
      check_base_forces_ctl = flag
      return
!
      end function check_base_forces_ctl
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function check_base_force_id                 &
     &                    (i_field, field_name, base_fld, base_force)
!
      integer(kind = kint), intent(in) :: i_field
      character(len = kchara), intent(in) :: field_name
      type(base_field_address), intent(in) :: base_fld
      type(base_force_address), intent(in) :: base_force
!
      integer(kind = kint) :: iflag
!
!
      iflag = 0
      if(      (i_field .eq. base_force%i_press_grad)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
      else if( (i_field .eq. base_force%i_m_advect)                     &
     &    .or. (i_field .eq. base_force%i_m_flux)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_vort, fhd_vort)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
      else if( (i_field .eq. base_force%i_lorentz)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_current, fhd_current)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_magne, fhd_magne)
      else if( (i_field .eq. base_force%i_maxwell)                      &
     &    .or. (i_field .eq. base_force%i_m_tension)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_magne, fhd_magne)
      else if( (i_field .eq. base_force%i_coriolis)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
      else if( (i_field .eq. base_force%i_buoyancy)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_temp, fhd_temp)
      else if( (i_field .eq. base_force%i_comp_buo)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_light, fhd_light)
!
      else if( (i_field .eq. base_force%i_vp_induct)                    &
     &    .or. (i_field .eq. base_force%i_mag_stretch)                  &
     &    .or. (i_field .eq. base_force%i_induct_t)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_magne, fhd_magne)
      else if( (i_field .eq. base_force%i_induction)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_magne, fhd_magne)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_force%i_vp_induct, fhd_vp_induct)
      else if( (i_field .eq. base_force%i_electric)                     &
     &    .or. (i_field .eq. base_force%i_poynting)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_force%i_vp_induct, fhd_vp_induct)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_current, fhd_current)
!
      else if( (i_field .eq. base_force%i_h_advect)                     &
     &    .or. (i_field .eq. base_force%i_h_flux)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_temp, fhd_temp)
      else if( (i_field .eq. base_force%i_ph_advect)                    &
     &    .or. (i_field .eq. base_force%i_ph_flux)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                           base_fld%i_par_temp, fhd_part_temp)
      else if( (i_field .eq. base_force%i_c_advect)                     &
     &    .or. (i_field .eq. base_force%i_c_flux)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_light, fhd_light)
      else if( (i_field .eq. base_force%i_pc_advect)                    &
     &    .or. (i_field .eq. base_force%i_pc_flux)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                           base_fld%i_par_light, fhd_part_light)
      end if
      check_base_force_id = iflag
      return
!
      end function check_base_force_id
!
! ----------------------------------------------------------------------
!
      end module check_base_forces
