!>@file   check_ene_flux_w_symmetry.f90
!!        module check_ene_flux_w_symmetry
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic fields
!!
!!@verbatim
!!      subroutine add_ene_flux_by_sym_sym_ctl(field_name, field_ctl)
!!      subroutine add_ene_flux_by_asym_asym_ctl(field_name, field_ctl)
!!      subroutine add_ene_flux_by_sym_asym_ctl(field_name, field_ctl)
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!@endverbatim
!!
      module check_ene_flux_w_symmetry
!
      use m_precision
      use m_constants
!
      use t_base_field_labels
      use m_field_w_symmetry_labels
      use m_energy_flux_w_sym_labels
!
      implicit  none
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine add_ene_flux_by_sym_sym_ctl(field_name, field_ctl)
!
      use t_control_array_character3
      use add_nodal_fields_ctl
!
      character(len = kchara), intent(in) :: field_name
      type(ctl_array_c3), intent(inout) :: field_ctl
!
      if(      (field_name .eq. fhd_u_dot_wsym_x_usym)) then
        call add_phys_name_ctl(velocity%name, field_ctl)
        call add_phys_name_ctl(fhd_wsym_x_usym, field_ctl)
      else if( (field_name .eq. fhd_urev_Jsym_x_Bsym)                   &
     &    .or. (field_name .eq. fhd_u_dot_Jsym_x_Bsym)) then
        call add_phys_name_ctl(velocity%name, field_ctl)
        call add_phys_name_ctl(fhd_Jsym_x_Bsym, field_ctl)
      else if( (field_name .eq. fhd_u_dot_Bsym_nabla_Bsym)) then
        call add_phys_name_ctl(velocity%name, field_ctl)
        call add_phys_name_ctl(fhd_Bsym_nabla_Bsym, field_ctl)
!
      else if( (field_name .eq. fhd_sym_buo_flux)) then
        call add_phys_name_ctl(velocity%name, field_ctl)
        call add_phys_name_ctl(fhd_sym_buoyancy, field_ctl)
      else if( (field_name .eq. fhd_sym_comp_buo_flux)) then
        call add_phys_name_ctl(velocity%name, field_ctl)
        call add_phys_name_ctl(fhd_sym_comp_buo, field_ctl)
!
      else if((field_name .eq. fhd_B_rot_Bsym_x_usym)) then
        call add_phys_name_ctl(magnetic_field%name, field_ctl)
        call add_phys_name_ctl(fhd_rot_usym_x_Bsym, field_ctl)
      else if((field_name .eq. fhd_Bdot_Bsym_nabla_usym)) then
        call add_phys_name_ctl(magnetic_field%name, field_ctl)
        call add_phys_name_ctl(fhd_Bsym_nabla_usym, field_ctl)
!
      else if( (field_name .eq. fhd_T_usym_nabla_Tsym)) then
        call add_phys_name_ctl(temperature%name, field_ctl)
        call add_phys_name_ctl(fhd_usym_nabla_Tsym, field_ctl)
      else if( (field_name .eq. fhd_pT_usym_nabla_pTsym)) then
        call add_phys_name_ctl(fhd_part_temp, field_ctl)
        call add_phys_name_ctl(fhd_usym_nabla_pTsym, field_ctl)
!
      else if( (field_name .eq. fhd_C_usym_nabla_Csym)) then
        call add_phys_name_ctl(fhd_light, field_ctl)
        call add_phys_name_ctl(fhd_usym_nabla_Csym, field_ctl)
      else if( (field_name .eq. fhd_pC_usym_nabla_pCsym)) then
        call add_phys_name_ctl(fhd_part_light, field_ctl)
        call add_phys_name_ctl(fhd_usym_nabla_pCsym, field_ctl)
      end if
!
      end subroutine add_ene_flux_by_sym_sym_ctl
!
! ----------------------------------------------------------------------
!
      subroutine add_ene_flux_by_asym_asym_ctl(field_name, field_ctl)
!
      use t_control_array_character3
      use add_nodal_fields_ctl
!
      character(len = kchara), intent(in) :: field_name
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(      (field_name .eq. fhd_u_dot_wasym_x_uasym)) then
        call add_phys_name_ctl(velocity%name, field_ctl)
        call add_phys_name_ctl(fhd_wasym_x_uasym, field_ctl)
      else if( (field_name .eq. fhd_urev_Jasym_x_Basym)                 &
     &    .or. (field_name .eq. fhd_u_dot_Jasym_x_Basym)) then
        call add_phys_name_ctl(velocity%name, field_ctl)
        call add_phys_name_ctl(fhd_Jasym_x_Basym, field_ctl)
      else if((field_name .eq. fhd_u_dot_Basym_nabla_Basym)) then
        call add_phys_name_ctl(velocity%name, field_ctl)
        call add_phys_name_ctl(fhd_Basym_nabla_Basym,field_ctl)
!
      else if( (field_name .eq. fhd_asym_buo_flux)) then
        call add_phys_name_ctl(velocity%name, field_ctl)
        call add_phys_name_ctl(fhd_asym_buoyancy, field_ctl)
      else if( (field_name .eq. fhd_asym_comp_buo_flux)) then
        call add_phys_name_ctl(velocity%name, field_ctl)
        call add_phys_name_ctl(fhd_asym_comp_buo, field_ctl)
!
      else if( (field_name .eq. fhd_B_rot_Basym_x_uasym)) then
        call add_phys_name_ctl(magnetic_field%name, field_ctl)
        call add_phys_name_ctl(fhd_rot_uasym_x_Basym,field_ctl)
      else if( (field_name .eq. fhd_Bdot_Basym_nabla_uasym))            &
     &    then
        call add_phys_name_ctl(magnetic_field%name, field_ctl)
        call add_phys_name_ctl(fhd_Basym_nabla_uasym,field_ctl)
!
      else if( (field_name .eq. fhd_T_uasym_nabla_Tasym)) then
        call add_phys_name_ctl(temperature%name, field_ctl)
        call add_phys_name_ctl(fhd_uasym_nabla_Tasym,field_ctl)
      else if( (field_name .eq. fhd_pT_uasym_nabla_pTasym)) then
        call add_phys_name_ctl(fhd_part_temp, field_ctl)
        call add_phys_name_ctl(fhd_uasym_nabla_pTasym,field_ctl)
!
      else if( (field_name .eq. fhd_C_uasym_nabla_Casym)) then
        call add_phys_name_ctl(fhd_light, field_ctl)
        call add_phys_name_ctl(fhd_uasym_nabla_Casym,field_ctl)
      else if( (field_name .eq. fhd_pC_uasym_nabla_pCasym)) then
        call add_phys_name_ctl(fhd_part_light, field_ctl)
        call add_phys_name_ctl(fhd_uasym_nabla_pCasym,field_ctl)
      end if
!
      end subroutine add_ene_flux_by_asym_asym_ctl
!
! ----------------------------------------------------------------------
!
      subroutine add_ene_flux_by_sym_asym_ctl(field_name, field_ctl)
!
      use t_control_array_character3
      use add_nodal_fields_ctl
!
      character(len = kchara), intent(in) :: field_name
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(      (field_name .eq. fhd_u_dot_wsym_x_uasym)) then
        call add_phys_name_ctl(velocity%name, field_ctl)
        call add_phys_name_ctl(fhd_wsym_x_uasym, field_ctl)
      else if( (field_name .eq. fhd_urev_Jsym_x_Basym)                  &
     &    .or. (field_name .eq. fhd_u_dot_Jsym_x_Basym)  ) then
        call add_phys_name_ctl(velocity%name, field_ctl)
        call add_phys_name_ctl(fhd_Jsym_x_Basym, field_ctl)
      else if( (field_name .eq. fhd_u_dot_Bsym_nabla_Basym)) then
        call add_phys_name_ctl(velocity%name, field_ctl)
        call add_phys_name_ctl(fhd_Bsym_nabla_Basym, field_ctl)
!
      else if( (field_name .eq. fhd_B_rot_Bsym_x_uasym)) then
        call add_phys_name_ctl(magnetic_field%name, field_ctl)
        call add_phys_name_ctl(fhd_rot_usym_x_Basym, field_ctl)
      else if( (field_name .eq. fhd_Bdot_Bsym_nabla_uasym)) then
        call add_phys_name_ctl(magnetic_field%name, field_ctl)
        call add_phys_name_ctl(fhd_Bsym_nabla_uasym, field_ctl)
!
      else if( (field_name .eq. fhd_T_usym_nabla_Tasym)) then
        call add_phys_name_ctl(temperature%name, field_ctl)
        call add_phys_name_ctl(fhd_usym_nabla_Tasym, field_ctl)
      else if( (field_name .eq. fhd_pT_usym_nabla_pTasym)) then
        call add_phys_name_ctl(fhd_part_temp, field_ctl)
        call add_phys_name_ctl(fhd_usym_nabla_pTasym,field_ctl)
!
      else if( (field_name .eq. fhd_C_usym_nabla_Casym)) then
        call add_phys_name_ctl(fhd_light, field_ctl)
        call add_phys_name_ctl(fhd_usym_nabla_Casym, field_ctl)
      else if( (field_name .eq. fhd_pC_usym_nabla_pCasym)) then
        call add_phys_name_ctl(fhd_part_light, field_ctl)
        call add_phys_name_ctl(fhd_usym_nabla_pCasym,field_ctl)
!
!
      else if( (field_name .eq. fhd_u_dot_wasym_x_usym)) then
        call add_phys_name_ctl(velocity%name, field_ctl)
        call add_phys_name_ctl(fhd_wasym_x_uaym, field_ctl)
!
      else if( (field_name .eq. fhd_urev_Jasym_x_Bsym)                  &
     &    .or. (field_name .eq. fhd_u_dot_Jasym_x_Bsym) ) then
        call add_phys_name_ctl(velocity%name, field_ctl)
        call add_phys_name_ctl(fhd_Jasym_x_Bsym, field_ctl)
      else if( (field_name .eq. fhd_u_dot_Basym_nabla_Bsym)) then
        call add_phys_name_ctl(velocity%name, field_ctl)
        call add_phys_name_ctl(fhd_Basym_nabla_Bsym, field_ctl)
!
      else if( (field_name .eq. fhd_B_rot_Basym_x_usym)) then
        call add_phys_name_ctl(magnetic_field%name, field_ctl)
        call add_phys_name_ctl(fhd_rot_uasym_x_Bsym, field_ctl)
      else if( (field_name .eq. fhd_Bdot_Basym_nabla_usym)) then
        call add_phys_name_ctl(magnetic_field%name, field_ctl)
        call add_phys_name_ctl(fhd_Basym_nabla_usym, field_ctl)
!
      else if( (field_name .eq. fhd_T_uasym_nabla_Tsym)) then
        call add_phys_name_ctl(temperature%name, field_ctl)
        call add_phys_name_ctl(fhd_usym_nabla_Tasym, field_ctl)
      else if( (field_name .eq. fhd_pT_uasym_nabla_pTsym)) then
        call add_phys_name_ctl(fhd_part_temp, field_ctl)
        call add_phys_name_ctl(fhd_uasym_nabla_pTsym,field_ctl)
!
      else if( (field_name .eq. fhd_C_uasym_nabla_Csym)) then
        call add_phys_name_ctl(fhd_light, field_ctl)
        call add_phys_name_ctl(fhd_uasym_nabla_Csym, field_ctl)
      else if( (field_name .eq. fhd_pC_uasym_nabla_pCsym)) then
        call add_phys_name_ctl(fhd_part_light, field_ctl)
        call add_phys_name_ctl(fhd_uasym_nabla_pCsym,field_ctl)
      end if
!
      end subroutine add_ene_flux_by_sym_asym_ctl
!
! ----------------------------------------------------------------------
!
      end module check_ene_flux_w_symmetry
