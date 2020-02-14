!>@file   check_filter_field.f90
!!        module check_filter_field
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic fields
!!
!!@verbatim
!!      subroutine add_filter_field_ctl(field_name, field_ctl)
!!      logical function check_filter_field_ctl(field_name, field_ctl)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!      integer(kind = kint) function check_filter_field_id             &
!!     &                   (i_field, field_name, filter_fld, base_fld)
!!        type(base_field_address), intent(in) :: filter_fld
!!        type(base_field_address), intent(in) :: base_fld
!!@endverbatim
!!
      module check_filter_field
!
      use m_precision
      use m_constants
      use t_base_field_labels
      use m_filtered_field_labels
!
      implicit  none
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine add_filter_field_ctl(field_name, field_ctl)
!
      use t_control_array_character3
      use add_nodal_fields_ctl
!
      character(len = kchara), intent(in) :: field_name
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(field_name .eq. fhd_filter_vort) then
        call add_phys_name_ctl(fhd_filter_velo, field_ctl)
      else if(field_name .eq. fhd_filter_current) then
        call add_phys_name_ctl(fhd_filter_magne, field_ctl)
      end if
!
      if(     field_name .eq. fhd_filter_velo) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
      else if(field_name .eq. fhd_filter_magne) then
        call add_phys_name_ctl(fhd_magne, field_ctl)
      else if(field_name .eq. fhd_filter_vecp) then
        call add_phys_name_ctl(fhd_vecp, field_ctl)
!
      else if(field_name .eq. fhd_filter_temp) then
        call add_phys_name_ctl(fhd_temp, field_ctl)
      else if(field_name .eq. fhd_filter_comp) then
        call add_phys_name_ctl(fhd_light, field_ctl)
      else if(field_name .eq. fhd_filter_density) then
        call add_phys_name_ctl(fhd_density, field_ctl)
      else if(field_name .eq. fhd_filter_entropy) then
        call add_phys_name_ctl(fhd_entropy, field_ctl)
!
      else if(field_name .eq. fhd_filter_pert_temp) then
        call add_phys_name_ctl(fhd_part_temp, field_ctl)
      else if(field_name .eq. fhd_filter_pert_comp) then
        call add_phys_name_ctl(fhd_part_light, field_ctl)
      else if(field_name .eq. fhd_filter_pert_density) then
        call add_phys_name_ctl(fhd_per_density, field_ctl)
      else if(field_name .eq. fhd_filter_pert_entropy) then
        call add_phys_name_ctl(fhd_per_entropy, field_ctl)
      end if
!
      end subroutine add_filter_field_ctl
!
! -----------------------------------------------------------------------
!
      logical function check_filter_field_ctl(field_name, field_ctl)
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
      if(field_name .eq. fhd_filter_vort) then
        flag = flag                                                     &
     &        .and. check_field_list_ctl(fhd_filter_velo, field_ctl)
      else if(field_name .eq. fhd_filter_current) then
        flag = flag                                                     &
     &        .and. check_field_list_ctl(fhd_filter_magne, field_ctl)
      end if
!
      if(     field_name .eq. fhd_filter_velo) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)
      else if(field_name .eq. fhd_filter_magne) then
        flag = flag .and. check_field_list_ctl(fhd_magne, field_ctl)
      else if(field_name .eq. fhd_filter_vecp) then
        flag = flag .and. check_field_list_ctl(fhd_vecp, field_ctl)
!
      else if(field_name .eq. fhd_filter_temp) then
        flag = flag .and. check_field_list_ctl(fhd_temp, field_ctl)
      else if(field_name .eq. fhd_filter_comp) then
        flag = flag .and. check_field_list_ctl(fhd_light, field_ctl)
      else if(field_name .eq. fhd_filter_density) then
        flag = flag .and. check_field_list_ctl(fhd_density, field_ctl)
      else if(field_name .eq. fhd_filter_entropy) then
        flag = flag .and. check_field_list_ctl(fhd_entropy, field_ctl)
!
      else if(field_name .eq. fhd_filter_pert_temp) then
        flag = flag                                                     &
     &        .and. check_field_list_ctl(fhd_part_temp, field_ctl)
      else if(field_name .eq. fhd_filter_pert_comp) then
        flag = flag                                                     &
     &        .and. check_field_list_ctl(fhd_part_light, field_ctl)
      else if(field_name .eq. fhd_filter_pert_density) then
        flag = flag                                                     &
     &        .and. check_field_list_ctl(fhd_per_density, field_ctl)
      else if(field_name .eq. fhd_filter_pert_entropy) then
        flag = flag                                                     &
     &        .and. check_field_list_ctl(fhd_per_entropy, field_ctl)
      end if
      check_filter_field_ctl = flag
      return
!
      end function check_filter_field_ctl
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function check_filter_field_id               &
     &                   (i_field, field_name, filter_fld, base_fld)
!
      integer(kind = kint), intent(in) :: i_field
      character(len = kchara), intent(in) :: field_name
      type(base_field_address), intent(in) :: filter_fld
      type(base_field_address), intent(in) :: base_fld
!
      integer(kind = kint) :: iflag
!
!
      iflag = 0
      if(      (i_field .eq. filter_fld%i_velo)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_velo, fhd_velo)
      else if( (i_field .eq. filter_fld%i_vort)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 filter_fld%i_velo, fhd_filter_velo)
      else if( (i_field .eq. filter_fld%i_vort)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_magne, fhd_magne)
      else if( (i_field .eq. filter_fld%i_vecp)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_vecp, fhd_vecp)
      else if( (i_field .eq. filter_fld%i_current)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 filter_fld%i_magne, fhd_filter_magne)
!
      else if( (i_field .eq. filter_fld%i_temp)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_temp, fhd_temp)
      else if( (i_field .eq. filter_fld%i_light)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_vecp, fhd_light)
      else if( (i_field .eq. filter_fld%i_density)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_density, fhd_density)
      else if( (i_field .eq. filter_fld%i_entropy)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                                base_fld%i_entropy, fhd_entropy)
!
      else if( (i_field .eq. filter_fld%i_par_temp)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_par_temp, fhd_part_temp)
      else if( (i_field .eq. filter_fld%i_par_light)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_par_light, fhd_part_light)
      else if( (i_field .eq. filter_fld%i_par_density)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_par_density, fhd_per_density)
      else if( (i_field .eq. filter_fld%i_par_entropy)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_par_entropy, fhd_per_entropy)
      end if
      check_filter_field_id = iflag
      return
!
      end function check_filter_field_id
!
! ----------------------------------------------------------------------
!
      end module check_filter_field
