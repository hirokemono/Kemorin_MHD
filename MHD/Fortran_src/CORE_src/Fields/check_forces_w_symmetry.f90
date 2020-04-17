!>@file   check_forces_w_symmetry.f90
!!        module check_forces_w_symmetry
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic fields
!!
!!@verbatim
!!      subroutine add_force_by_sym_asym_ctl(field_name, field_ctl)
!!      subroutine add_force_by_asym_asym_ctl(field_name, field_ctl)
!!      subroutine add_force_by_sym_sym_ctl(field_name, field_ctl)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!@endverbatim
!!
      module check_forces_w_symmetry
!
      use m_precision
      use m_constants
!
      use t_base_force_labels
      use m_field_w_symmetry_labels
      use m_force_w_sym_labels
!
      implicit  none
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine add_force_by_sym_sym_ctl(field_name, field_ctl)
!
      use t_control_array_character3
      use add_nodal_fields_ctl
!
      character(len = kchara), intent(in) :: field_name
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(      (field_name .eq. fhd_wsym_x_usym)                        &
     &    .or. (field_name .eq. fhd_m_flux_sym_sym) ) then
        call add_phys_name_ctl(fhd_sym_vort, field_ctl)
        call add_phys_name_ctl(fhd_sym_velo, field_ctl)
      else if( (field_name .eq. fhd_Jsym_x_Bsym) ) then
        call add_phys_name_ctl(fhd_sym_current, field_ctl)
        call add_phys_name_ctl(fhd_sym_magne, field_ctl)
      else if( (field_name .eq. fhd_maxwell_sym_sym)                    &
     &    .or. (field_name .eq. fhd_Bsym_nabla_Bsym) ) then
        call add_phys_name_ctl(fhd_sym_magne, field_ctl)
!
      else if( (field_name .eq. fhd_sym_buoyancy)) then
        call add_phys_name_ctl(fhd_sym_temp, field_ctl)
      else if( (field_name .eq. fhd_sym_comp_buo)) then
        call add_phys_name_ctl(fhd_sym_light, field_ctl)
!
      else if( (field_name .eq. fhd_usym_x_Bsym)                        &
     &    .or. (field_name .eq. fhd_rot_usym_x_Bsym)                    &
     &    .or. (field_name .eq. fhd_Bsym_nabla_usym)                    &
     &    .or. (field_name .eq. fhd_usym_Bsym) ) then
        call add_phys_name_ctl(fhd_sym_velo, field_ctl)
        call add_phys_name_ctl(fhd_sym_magne, field_ctl)
!
      else if( (field_name .eq. fhd_usym_nabla_Tsym)                    &
     &    .or. (field_name .eq. fhd_h_flux_sym_sym) ) then
        call add_phys_name_ctl(fhd_sym_velo, field_ctl)
        call add_phys_name_ctl(fhd_sym_temp, field_ctl)
      else if( (field_name .eq. fhd_usym_nabla_pTsym)                   &
     &    .or. (field_name .eq. fhd_ph_flux_sym_sym) ) then
        call add_phys_name_ctl(fhd_sym_velo, field_ctl)
        call add_phys_name_ctl(fhd_sym_per_temp, field_ctl)
      else if( (field_name .eq. fhd_usym_nabla_Csym)                    &
     &    .or. (field_name .eq. fhd_c_flux_sym_sym) ) then
        call add_phys_name_ctl(fhd_sym_velo, field_ctl)
        call add_phys_name_ctl(fhd_sym_light, field_ctl)
      else if( (field_name .eq. fhd_usym_nabla_pCsym)                   &
     &    .or. (field_name .eq. fhd_pc_flux_sym_sym) ) then
        call add_phys_name_ctl(fhd_sym_velo, field_ctl)
        call add_phys_name_ctl(fhd_sym_per_light, field_ctl)
      end if
!
      end subroutine add_force_by_sym_sym_ctl
!
! ----------------------------------------------------------------------
!
      subroutine add_force_by_asym_asym_ctl(field_name, field_ctl)
!
      use t_control_array_character3
      use add_nodal_fields_ctl
!
      character(len = kchara), intent(in) :: field_name
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(      (field_name .eq. fhd_wasym_x_uasym)                      &
     &    .or. (field_name .eq. fhd_m_flux_asym_asym) ) then
        call add_phys_name_ctl(fhd_asym_vort, field_ctl)
        call add_phys_name_ctl(fhd_asym_velo, field_ctl)
      else if( (field_name .eq. fhd_Jasym_x_Basym) ) then
        call add_phys_name_ctl(fhd_asym_current, field_ctl)
        call add_phys_name_ctl(fhd_asym_magne, field_ctl)
      else if( (field_name .eq. fhd_maxwell_asym_asym)                  &
     &    .or. (field_name .eq. fhd_Basym_nabla_Basym) ) then
        call add_phys_name_ctl(fhd_asym_magne, field_ctl)
!
      else if( (field_name .eq. fhd_asym_buoyancy)) then
        call add_phys_name_ctl(fhd_asym_temp, field_ctl)
      else if( (field_name .eq. fhd_asym_comp_buo)) then
        call add_phys_name_ctl(fhd_asym_light, field_ctl)
!
      else if( (field_name .eq. fhd_uasym_x_Basym)                      &
     &    .or. (field_name .eq. fhd_rot_uasym_x_Basym)                  &
     &    .or. (field_name .eq. fhd_Basym_nabla_uasym)                  &
     &    .or. (field_name .eq. fhd_uasym_Basym) ) then
        call add_phys_name_ctl(fhd_asym_velo, field_ctl)
        call add_phys_name_ctl(fhd_asym_magne, field_ctl)
      else if( (field_name .eq. fhd_uasym_nabla_Tasym)                  &
     &    .or. (field_name .eq. fhd_h_flux_asym_asym) ) then
        call add_phys_name_ctl(fhd_asym_velo, field_ctl)
        call add_phys_name_ctl(fhd_asym_temp, field_ctl)
      else if( (field_name .eq. fhd_uasym_nabla_pTasym)                 &
     &    .or. (field_name .eq. fhd_ph_flux_asym_asym) ) then
        call add_phys_name_ctl(fhd_asym_velo, field_ctl)
        call add_phys_name_ctl(fhd_asym_per_temp, field_ctl)
!
      else if( (field_name .eq. fhd_uasym_nabla_Casym)                  &
     &    .or. (field_name .eq. fhd_c_flux_asym_asym) ) then
        call add_phys_name_ctl(fhd_asym_velo, field_ctl)
        call add_phys_name_ctl(fhd_asym_light, field_ctl)
      else if( (field_name .eq. fhd_uasym_nabla_pCasym)                 &
     &    .or. (field_name .eq. fhd_pc_flux_asym_asym) ) then
        call add_phys_name_ctl(fhd_asym_velo, field_ctl)
        call add_phys_name_ctl(fhd_asym_per_light, field_ctl)
      end if
!
      end subroutine add_force_by_asym_asym_ctl
!
! ----------------------------------------------------------------------
!
      subroutine add_force_by_sym_asym_ctl(field_name, field_ctl)
!
      use t_control_array_character3
      use add_nodal_fields_ctl
!
      character(len = kchara), intent(in) :: field_name
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(      (field_name .eq. fhd_wsym_x_uasym)                       &
     &    .or. (field_name .eq. fhd_m_flux_sym_asym) ) then
        call add_phys_name_ctl(fhd_sym_vort, field_ctl)
        call add_phys_name_ctl(fhd_asym_velo, field_ctl)
      else if( (field_name .eq. fhd_Jsym_x_Basym)  ) then
        call add_phys_name_ctl(fhd_sym_current, field_ctl)
        call add_phys_name_ctl(fhd_asym_magne, field_ctl)
!
      else if( (field_name .eq. fhd_maxwell_sym_asym)                   &
     &    .or. (field_name .eq. fhd_Bsym_nabla_Basym) ) then
        call add_phys_name_ctl(fhd_sym_magne, field_ctl)
        call add_phys_name_ctl(fhd_asym_magne, field_ctl)
!
      else if( (field_name .eq. fhd_usym_x_Basym)                       &
     &    .or. (field_name .eq. fhd_rot_usym_x_Basym)                   &
     &    .or. (field_name .eq. fhd_Bsym_nabla_uasym)                   &
     &    .or. (field_name .eq. fhd_uasym_Bsym) ) then
        call add_phys_name_ctl(fhd_sym_velo, field_ctl)
        call add_phys_name_ctl(fhd_asym_magne, field_ctl)
!
      else if( (field_name .eq. fhd_usym_nabla_Tasym)                   &
     &    .or. (field_name .eq. fhd_h_flux_sym_asym)  ) then
        call add_phys_name_ctl(fhd_sym_velo, field_ctl)
        call add_phys_name_ctl(fhd_asym_temp, field_ctl)
      else if( (field_name .eq. fhd_usym_nabla_pTasym)                  &
     &    .or. (field_name .eq. fhd_ph_flux_sym_asym)  ) then
        call add_phys_name_ctl(fhd_sym_velo, field_ctl)
        call add_phys_name_ctl(fhd_asym_per_temp, field_ctl)
      else if( (field_name .eq. fhd_usym_nabla_Casym)                   &
     &    .or. (field_name .eq. fhd_c_flux_sym_asym)  ) then
        call add_phys_name_ctl(fhd_sym_velo, field_ctl)
        call add_phys_name_ctl(fhd_asym_light, field_ctl)
      else if( (field_name .eq. fhd_usym_nabla_pCasym)                  &
     &    .or. (field_name .eq. fhd_pc_flux_sym_asym)  ) then
        call add_phys_name_ctl(fhd_sym_velo, field_ctl)
        call add_phys_name_ctl(fhd_asym_per_light, field_ctl)
!
!
      else if( (field_name .eq. fhd_wasym_x_uaym)) then
        call add_phys_name_ctl(fhd_asym_vort, field_ctl)
        call add_phys_name_ctl(fhd_sym_velo, field_ctl)
!
      else if( (field_name .eq. fhd_Jasym_x_Bsym)) then
        call add_phys_name_ctl(fhd_asym_current, field_ctl)
        call add_phys_name_ctl(fhd_sym_magne, field_ctl)
!
      else if( (field_name .eq. fhd_Basym_nabla_Bsym)) then
        call add_phys_name_ctl(fhd_asym_magne, field_ctl)
        call add_phys_name_ctl(fhd_sym_magne, field_ctl)
!
      else if( (field_name .eq. fhd_uasym_x_Bsym)                       &
     &    .or. (field_name .eq. fhd_rot_uasym_x_Bsym)                   &
     &    .or. (field_name .eq. fhd_Basym_nabla_usym)) then
        call add_phys_name_ctl(fhd_asym_velo, field_ctl)
        call add_phys_name_ctl(fhd_sym_magne, field_ctl)
!
      else if( (field_name .eq. fhd_uasym_nabla_Tsym)                   &
     &    .or. (field_name .eq. fhd_h_flux_asym_sym)) then
        call add_phys_name_ctl(fhd_asym_velo, field_ctl)
        call add_phys_name_ctl(fhd_asym_temp, field_ctl)
!
      else if( (field_name .eq. fhd_uasym_nabla_pTsym)                  &
     &    .or. (field_name .eq. fhd_ph_flux_asym_sym)) then
        call add_phys_name_ctl(fhd_asym_velo, field_ctl)
        call add_phys_name_ctl(fhd_sym_per_temp, field_ctl)
      else if( (field_name .eq. fhd_uasym_nabla_Csym)                   &
     &    .or. (field_name .eq. fhd_c_flux_asym_sym)  ) then
        call add_phys_name_ctl(fhd_asym_velo, field_ctl)
        call add_phys_name_ctl(fhd_sym_light, field_ctl)
!
      else if( (field_name .eq. fhd_uasym_nabla_pCsym)                  &
     &    .or. (field_name .eq. fhd_pc_flux_asym_sym)  ) then
        call add_phys_name_ctl(fhd_asym_velo, field_ctl)
        call add_phys_name_ctl(fhd_sym_per_light, field_ctl)
      end if
!
      end subroutine add_force_by_sym_asym_ctl
!
! ----------------------------------------------------------------------
!
      end module check_forces_w_symmetry
