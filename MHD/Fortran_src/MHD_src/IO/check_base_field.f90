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
!!      logical function check_base_field_ctl(field_name, field_ctl)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!      integer(kind = kint) function check_base_field_id               &
!!     &                            (i_field, field_name, base_fld)
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
        call add_phys_name_ctl(fhd_velo, field_ctl)
       
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
      logical function check_base_field_ctl(field_name, field_ctl)
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
      if(      (field_name .eq. fhd_vort)                               &
     &   .and. (field_name .eq. fhd_press)                              &
     &   .and. (field_name .eq. fhd_magne)                              &
     &   .and. (field_name .eq. fhd_temp)                               &
     &   .and. (field_name .eq. fhd_light)                              &
     &   .and. (field_name .eq. fhd_density)                            &
     &   .and. (field_name .eq. fhd_entropy)) then
       flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)
       
      else if( (field_name .eq. fhd_vecp)                               &
     &   .and. (field_name .eq. fhd_current)                            &
     &   .and. (field_name .eq. fhd_mag_potential)                      &
     &   .and. (field_name .eq. fhd_scalar_potential)) then
       flag = flag .and. check_field_list_ctl(fhd_magne, field_ctl)
!
      else if( (field_name .eq. fhd_part_temp)                          &
     &   .and. (field_name .eq. fhd_ref_temp)                           &
     &   .and. (field_name .eq. fhd_heat_source)) then
       flag = flag .and. check_field_list_ctl(fhd_temp, field_ctl)
      else if( (field_name .eq. fhd_part_light)                         &
     &   .and. (field_name .eq. fhd_ref_light)                          &
     &   .and. (field_name .eq. fhd_light_source)) then
       flag = flag .and. check_field_list_ctl(fhd_light, field_ctl)
      else if( (field_name .eq. fhd_per_entropy)                        &
     &   .and. (field_name .eq. fhd_ref_entropy)                        &
     &   .and. (field_name .eq. fhd_entropy_source)) then
       flag = flag .and. check_field_list_ctl(fhd_entropy, field_ctl)
      else if( (field_name .eq. fhd_per_density)                        &
     &   .and. (field_name .eq. fhd_ref_density)) then 
       flag = flag .and. check_field_list_ctl(fhd_density, field_ctl)
      end if
      check_base_field_ctl = flag
      return
!
      end function check_base_field_ctl
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function check_base_field_id                 &
     &                            (i_field, field_name, base_fld)
!
      integer(kind = kint), intent(in) :: i_field
      character(len = kchara), intent(in) :: field_name
      type(base_field_address), intent(in) :: base_fld
!
      integer(kind = kint) :: iflag
!
!
      iflag = 0
      if(      (i_field .eq. base_fld%i_vort)                           &
     &    .or. (i_field .eq. base_fld%i_press)                          &
     &    .or. (i_field .eq. base_fld%i_magne)                          &
     &    .or. (i_field .eq. base_fld%i_temp)                           &
     &    .or. (i_field .eq. base_fld%i_light)                          &
     &    .or. (i_field .eq. base_fld%i_density)                        &
     &    .or. (i_field .eq. base_fld%i_entropy)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_velo, fhd_velo)
      else if( (i_field .eq. base_fld%i_vecp)                           &
     &    .or. (i_field .eq. base_fld%i_current)                        &
     &    .or. (i_field .eq. base_fld%i_mag_p)                          &
     &    .or. (i_field .eq. base_fld%i_scalar_p)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_magne, fhd_magne)
!
      else if( (i_field .eq. base_fld%i_per_temp)                       &
     &    .or. (i_field .eq. base_fld%i_ref_t)                          &
     &    .or. (i_field .eq. base_fld%i_heat_source)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_temp, fhd_temp)
      else if( (i_field .eq. base_fld%i_per_light)                      &
     &    .or. (i_field .eq. base_fld%i_ref_c)                          &
     &    .or. (i_field .eq. base_fld%i_light_source)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_light, fhd_light)
      else if( (i_field .eq. base_fld%i_per_entropy)                    &
     &    .or. (i_field .eq. base_fld%i_ref_entropy)                    &
     &    .or. (i_field .eq. base_fld%i_entropy_source)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_entropy, fhd_entropy)
      else if( (i_field .eq. base_fld%i_per_density)                    &
     &    .or. (i_field .eq. base_fld%i_ref_density)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_density, fhd_density)
      end if
      check_base_field_id = iflag
      return
!
      end function check_base_field_id
!
! ----------------------------------------------------------------------
!
      end module check_base_field
