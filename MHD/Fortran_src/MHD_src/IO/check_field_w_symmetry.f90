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
        call add_phys_name_ctl(fhd_velo, field_ctl)
      else if( (field_name .eq. fhd_sym_vort)                           &
     &    .or. (field_name .eq. fhd_asym_vort)) then 
        call add_phys_name_ctl(fhd_vort, field_ctl)
      else if( (field_name .eq. fhd_sym_magne)                          &
     &    .or. (field_name .eq. fhd_asym_magne)) then 
        call add_phys_name_ctl(fhd_magne, field_ctl)
      else if( (field_name .eq. fhd_sym_vecp)                           &
     &    .or. (field_name .eq. fhd_asym_vecp)) then 
        call add_phys_name_ctl(fhd_vecp, field_ctl)
      else if( (field_name .eq. fhd_sym_current)                        &
     &    .or. (field_name .eq. fhd_asym_current)) then 
        call add_phys_name_ctl(fhd_current, field_ctl)
!
      else if( (field_name .eq. fhd_sym_press)                          &
     &    .or. (field_name .eq. fhd_asym_press)) then 
        call add_phys_name_ctl(fhd_press, field_ctl)
      else if( (field_name .eq. fhd_sym_mag_potential)                  &
     &    .or. (field_name .eq. fhd_asym_mag_potential)) then 
        call add_phys_name_ctl(fhd_mag_potential, field_ctl)
      else if( (field_name .eq. fhd_sym_scalar_potential)               &
     &    .or. (field_name .eq. fhd_asym_scalar_potential)) then 
        call add_phys_name_ctl(fhd_scalar_potential, field_ctl)
!
      else if( (field_name .eq. fhd_sym_temp)                           &
     &    .or. (field_name .eq. fhd_asym_temp)) then 
        call add_phys_name_ctl(fhd_temp, field_ctl)
      else if( (field_name .eq. fhd_sym_light)                          &
     &    .or. (field_name .eq. fhd_asym_light)) then 
        call add_phys_name_ctl(fhd_light, field_ctl)
      else if( (field_name .eq. fhd_sym_density)                        &
     &    .or. (field_name .eq. fhd_asym_density)) then 
        call add_phys_name_ctl(fhd_density, field_ctl)
      else if( (field_name .eq. fhd_sym_entropy)                        &
     &    .or. (field_name .eq. fhd_asym_entropy)) then 
        call add_phys_name_ctl(fhd_entropy, field_ctl)
!
      else if( (field_name .eq. fhd_sym_per_temp)                       &
     &    .or. (field_name .eq. fhd_asym_per_temp)) then 
        call add_phys_name_ctl(fhd_part_temp, field_ctl)
      else if( (field_name .eq. fhd_sym_per_light)                      &
     &    .or. (field_name .eq. fhd_asym_per_light)) then 
        call add_phys_name_ctl(fhd_part_light, field_ctl)
      else if( (field_name .eq. fhd_sym_per_density)                    &
     &    .or. (field_name .eq. fhd_asym_per_density)) then 
        call add_phys_name_ctl(fhd_per_density, field_ctl)
      else if( (field_name .eq. fhd_sym_per_entropy)                    &
     &    .or. (field_name .eq. fhd_asym_per_entropy)) then 
        call add_phys_name_ctl(fhd_per_entropy, field_ctl)
      end if
!
      end subroutine add_field_w_symmetry_ctl
!
! -----------------------------------------------------------------------
!
      logical function check_field_w_symmetry_ctl                      &
     &               (field_name, field_ctl)
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
      if(      (field_name .eq. fhd_sym_velo)                           &
     &    .or. (field_name .eq. fhd_asym_velo)) then 
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)
      else if( (field_name .eq. fhd_sym_vort)                           &
     &    .or. (field_name .eq. fhd_asym_vort)) then 
        flag = flag .and. check_field_list_ctl(fhd_vort, field_ctl)
      else if( (field_name .eq. fhd_sym_magne)                          &
     &    .or. (field_name .eq. fhd_asym_magne)) then 
        flag = flag .and. check_field_list_ctl(fhd_magne, field_ctl)
      else if( (field_name .eq. fhd_sym_vecp)                           &
     &    .or. (field_name .eq. fhd_asym_vecp)) then 
        flag = flag .and. check_field_list_ctl(fhd_vecp, field_ctl)
      else if( (field_name .eq. fhd_sym_current)                        &
     &    .or. (field_name .eq. fhd_asym_current)) then 
        flag = flag .and. check_field_list_ctl(fhd_current, field_ctl)
!
      else if( (field_name .eq. fhd_sym_press)                          &
     &    .or. (field_name .eq. fhd_asym_press)) then 
        flag = flag .and. check_field_list_ctl(fhd_press, field_ctl)
      else if( (field_name .eq. fhd_sym_mag_potential)                  &
     &    .or. (field_name .eq. fhd_asym_mag_potential)) then 
        flag = flag .and. check_field_list_ctl                          &
     &                  (fhd_mag_potential, field_ctl)
      else if( (field_name .eq. fhd_sym_scalar_potential)               &
     &    .or. (field_name .eq. fhd_asym_scalar_potential)) then 
        flag = flag .and. check_field_list_ctl                          &
     &                  (fhd_scalar_potential, field_ctl)
!
      else if( (field_name .eq. fhd_sym_temp)                           &
     &    .or. (field_name .eq. fhd_asym_temp)) then 
        flag = flag .and. check_field_list_ctl(fhd_temp, field_ctl)
      else if( (field_name .eq. fhd_sym_light)                          &
     &    .or. (field_name .eq. fhd_asym_light)) then 
        flag = flag .and. check_field_list_ctl(fhd_light, field_ctl)
      else if( (field_name .eq. fhd_sym_density)                        &
     &    .or. (field_name .eq. fhd_asym_density)) then 
        flag = flag .and. check_field_list_ctl(fhd_density, field_ctl)
      else if( (field_name .eq. fhd_sym_entropy)                        &
     &    .or. (field_name .eq. fhd_asym_entropy)) then 
        flag = flag .and. check_field_list_ctl(fhd_entropy, field_ctl)
!
      else if( (field_name .eq. fhd_sym_per_temp)                       &
     &    .or. (field_name .eq. fhd_asym_per_temp)) then 
        flag = flag                                                     &
     &        .and. check_field_list_ctl(fhd_part_temp, field_ctl)
      else if( (field_name .eq. fhd_sym_per_light)                      &
     &    .or. (field_name .eq. fhd_asym_per_light)) then 
        flag = flag                                                     &
     &        .and. check_field_list_ctl(fhd_part_light, field_ctl)
      else if( (field_name .eq. fhd_sym_per_density)                    &
     &    .or. (field_name .eq. fhd_asym_per_density)) then 
        flag = flag                                                     &
     &        .and. check_field_list_ctl(fhd_per_density, field_ctl)
      else if( (field_name .eq. fhd_sym_per_entropy)                    &
     &    .or. (field_name .eq. fhd_asym_per_entropy)) then 
        flag = flag                                                     &
     &        .and. check_field_list_ctl(fhd_per_entropy, field_ctl)
      end if
      check_field_w_symmetry_ctl = flag
      return
!
      end function check_field_w_symmetry_ctl
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function check_field_w_symmetry_id           &
     &                            (i_field, field_name, base_fld,       &
     &                             sym_base_fld, asym_base_fld)
!
      integer(kind = kint), intent(in) :: i_field
      character(len = kchara), intent(in) :: field_name
      type(base_field_address), intent(in) :: base_fld
      type(base_field_address), intent(in) :: sym_base_fld
      type(base_field_address), intent(in) :: asym_base_fld
!
      integer(kind = kint) :: iflag
!
!
      iflag = 0
      if(      (i_field .eq. sym_base_fld%i_velo)                       &
     &    .or. (i_field .eq. asym_base_fld%i_velo)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
      else if( (i_field .eq. sym_base_fld%i_vort)                       &
     &    .or. (i_field .eq. asym_base_fld%i_vort)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_vort, fhd_vort)
      else if( (i_field .eq. sym_base_fld%i_magne)                      &
     &    .or. (i_field .eq. asym_base_fld%i_magne)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_magne, fhd_magne)
      else if( (i_field .eq. sym_base_fld%i_vecp)                       &
     &    .or. (i_field .eq. asym_base_fld%i_vecp)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_vecp, fhd_vecp)
      else if( (i_field .eq. sym_base_fld%i_current)                    &
     &    .or. (i_field .eq. asym_base_fld%i_current)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_current, fhd_current)
!
      else if( (i_field .eq. sym_base_fld%i_press)                      &
     &    .or. (i_field .eq. asym_base_fld%i_press)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_press, fhd_press)
      else if( (i_field .eq. sym_base_fld%i_mag_p)                      &
     &    .or. (i_field .eq. asym_base_fld%i_mag_p)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_mag_p, fhd_mag_potential)
      else if( (i_field .eq. sym_base_fld%i_scalar_p)                   &
     &    .or. (i_field .eq. asym_base_fld%i_scalar_p)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_scalar_p, fhd_scalar_potential)
!
      else if( (i_field .eq. sym_base_fld%i_temp)                       &
     &    .or. (i_field .eq. asym_base_fld%i_temp)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_temp, fhd_temp)
      else if( (i_field .eq. sym_base_fld%i_light)                      &
     &    .or. (i_field .eq. asym_base_fld%i_light)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_light, fhd_light)
      else if( (i_field .eq. sym_base_fld%i_density)                    &
     &    .or. (i_field .eq. asym_base_fld%i_density)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_density, fhd_density)
      else if( (i_field .eq. sym_base_fld%i_entropy)                    &
     &    .or. (i_field .eq. asym_base_fld%i_entropy)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_entropy, fhd_entropy)
!
      else if( (i_field .eq. sym_base_fld%i_par_temp)                   &
     &    .or. (i_field .eq. asym_base_fld%i_par_temp)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_par_temp, fhd_part_temp)
      else if( (i_field .eq. sym_base_fld%i_par_light)                  &
     &    .or. (i_field .eq. asym_base_fld%i_par_light)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_par_light, fhd_part_light)
      else if( (i_field .eq. sym_base_fld%i_par_density)                &
     &    .or. (i_field .eq. asym_base_fld%i_par_density)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_par_density, fhd_per_density)
      else if( (i_field .eq. sym_base_fld%i_par_entropy)                &
     &    .or. (i_field .eq. asym_base_fld%i_par_entropy)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_par_entropy, fhd_per_entropy)
      end if
      check_field_w_symmetry_id = iflag
      return
!
      end function check_field_w_symmetry_id
!
! ----------------------------------------------------------------------
!
      end module check_field_w_symmetry
