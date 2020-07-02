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
      if(      (field_name .eq. wsym_x_usym%name)                       &
     &    .or. (field_name .eq. m_flux_sym_sym%name) ) then
        call add_phys_name_ctl(sym_vorticity, field_ctl)
        call add_phys_name_ctl(sym_velocity, field_ctl)
      else if( (field_name .eq. Jsym_x_Bsym%name) ) then
        call add_phys_name_ctl(sym_current_density, field_ctl)
        call add_phys_name_ctl(sym_magnetic_field, field_ctl)
      else if( (field_name .eq. maxwell_tensor_sym_sym%name)            &
     &    .or. (field_name .eq. Bsym_nabla_Bsym%name) ) then
        call add_phys_name_ctl(sym_magnetic_field, field_ctl)
!
      else if( (field_name .eq. sym_termal_buoyancy%name)) then
        call add_phys_name_ctl(sym_temperature, field_ctl)
      else if( (field_name .eq. sym_composite_buoyancy%name)) then
        call add_phys_name_ctl(sym_composition, field_ctl)
!
      else if( (field_name .eq. usym_x_Bsym%name)                       &
     &    .or. (field_name .eq. rot_usym_x_Bsym%name)                   &
     &    .or. (field_name .eq. Bsym_nabla_usym%name)                   &
     &    .or. (field_name .eq. usym_Bsym%name) ) then
        call add_phys_name_ctl(sym_velocity, field_ctl)
        call add_phys_name_ctl(sym_magnetic_field, field_ctl)
!
      else if( (field_name .eq. usym_nabla_Tsym%name)                   &
     &    .or. (field_name .eq. heat_flux_sym_sym%name) ) then
        call add_phys_name_ctl(sym_velocity, field_ctl)
        call add_phys_name_ctl(sym_temperature, field_ctl)
      else if( (field_name .eq. usym_nabla_pTsym%name)                  &
     &    .or. (field_name .eq. part_h_flux_sym_sym%name) ) then
        call add_phys_name_ctl(sym_velocity, field_ctl)
        call add_phys_name_ctl(sym_perturbation_temp, field_ctl)
      else if( (field_name .eq. usym_nabla_Csym%name)                   &
     &    .or. (field_name .eq. composite_flux_sym_sym%name) ) then
        call add_phys_name_ctl(sym_velocity, field_ctl)
        call add_phys_name_ctl(sym_composition, field_ctl)
      else if( (field_name .eq. usym_nabla_pCsym%name)                  &
     &    .or. (field_name .eq. part_c_flux_sym_sym%name) ) then
        call add_phys_name_ctl(sym_velocity, field_ctl)
        call add_phys_name_ctl(sym_perturbation_composition, field_ctl)
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
      if(      (field_name .eq. wasym_x_uasym%name)                     &
     &    .or. (field_name .eq. m_flux_asym_asym%name) ) then
        call add_phys_name_ctl(asym_vorticity, field_ctl)
        call add_phys_name_ctl(asym_velocity, field_ctl)
      else if( (field_name .eq. Jasym_x_Basym%name) ) then
        call add_phys_name_ctl(asym_current_density, field_ctl)
        call add_phys_name_ctl(asym_magnetic_field, field_ctl)
      else if( (field_name .eq. maxwell_tensor_asym_asym%name)          &
     &    .or. (field_name .eq. Basym_nabla_Basym%name) ) then
        call add_phys_name_ctl(asym_magnetic_field, field_ctl)
!
      else if( (field_name .eq. asym_termal_buoyancy%name)) then
        call add_phys_name_ctl(asym_temperature, field_ctl)
      else if( (field_name .eq. asym_composite_buoyancy%name)) then
        call add_phys_name_ctl(asym_composition, field_ctl)
!
      else if( (field_name .eq. uasym_x_Basym%name)                     &
     &    .or. (field_name .eq. rot_uasym_x_Basym%name)                 &
     &    .or. (field_name .eq. Basym_nabla_uasym%name)                 &
     &    .or. (field_name .eq. uasym_Basym%name) ) then
        call add_phys_name_ctl(asym_velocity, field_ctl)
        call add_phys_name_ctl(asym_magnetic_field, field_ctl)
      else if( (field_name .eq. uasym_nabla_Tasym%name)                 &
     &    .or. (field_name .eq. heat_flux_asym_asym%name) ) then
        call add_phys_name_ctl(asym_velocity, field_ctl)
        call add_phys_name_ctl(asym_temperature, field_ctl)
      else if( (field_name .eq. uasym_nabla_pTasym%name)                &
     &    .or. (field_name .eq. part_h_flux_asym_asym%name) ) then
        call add_phys_name_ctl(asym_velocity, field_ctl)
        call add_phys_name_ctl(asym_perturbation_temp, field_ctl)
!
      else if( (field_name .eq. uasym_nabla_Casym%name)                 &
     &    .or. (field_name .eq. composite_flux_asym_asym%name) ) then
        call add_phys_name_ctl(asym_velocity, field_ctl)
        call add_phys_name_ctl(asym_composition, field_ctl)
      else if( (field_name .eq. uasym_nabla_pCasym%name)                &
     &    .or. (field_name .eq. part_c_flux_asym_asym%name) ) then
        call add_phys_name_ctl(asym_velocity, field_ctl)
        call add_phys_name_ctl                                          &
     &     (asym_perturbation_composition, field_ctl)
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
      if(      (field_name .eq. wsym_x_uasym%name)                      &
     &    .or. (field_name .eq. m_flux_sym_asym%name) ) then
        call add_phys_name_ctl(sym_vorticity, field_ctl)
        call add_phys_name_ctl(asym_velocity, field_ctl)
      else if( (field_name .eq. Jsym_x_Basym%name)  ) then
        call add_phys_name_ctl(sym_current_density, field_ctl)
        call add_phys_name_ctl(asym_magnetic_field, field_ctl)
!
      else if( (field_name .eq. maxwell_tensor_sym_asym%name)           &
     &    .or. (field_name .eq. Bsym_nabla_Basym%name) ) then
        call add_phys_name_ctl(sym_magnetic_field, field_ctl)
        call add_phys_name_ctl(asym_magnetic_field, field_ctl)
!
      else if( (field_name .eq. usym_x_Basym%name)                      &
     &    .or. (field_name .eq. rot_usym_x_Basym%name)                  &
     &    .or. (field_name .eq. Bsym_nabla_uasym%name)                  &
     &    .or. (field_name .eq. usym_Basym%name) ) then
        call add_phys_name_ctl(sym_velocity, field_ctl)
        call add_phys_name_ctl(asym_magnetic_field, field_ctl)
!
      else if( (field_name .eq. usym_nabla_Tasym%name)                  &
     &    .or. (field_name .eq. heat_flux_sym_asym%name)  ) then
        call add_phys_name_ctl(sym_velocity, field_ctl)
        call add_phys_name_ctl(asym_temperature, field_ctl)
      else if( (field_name .eq. usym_nabla_pTasym%name)                 &
     &    .or. (field_name .eq. part_h_flux_sym_asym%name)  ) then
        call add_phys_name_ctl(sym_velocity, field_ctl)
        call add_phys_name_ctl(asym_perturbation_temp, field_ctl)
      else if( (field_name .eq. usym_nabla_Casym%name)                  &
     &    .or. (field_name .eq. composite_flux_sym_asym%name)  ) then
        call add_phys_name_ctl(sym_velocity, field_ctl)
        call add_phys_name_ctl(asym_composition, field_ctl)
      else if( (field_name .eq. usym_nabla_pCasym%name)                 &
     &    .or. (field_name .eq. part_c_flux_sym_asym%name)  ) then
        call add_phys_name_ctl(sym_velocity, field_ctl)
        call add_phys_name_ctl                                          &
     &     (asym_perturbation_composition, field_ctl)
!
!
      else if( (field_name .eq. wasym_x_usym%name)) then
        call add_phys_name_ctl(asym_vorticity, field_ctl)
        call add_phys_name_ctl(sym_velocity, field_ctl)
!
      else if( (field_name .eq. Jasym_x_Bsym%name)) then
        call add_phys_name_ctl(asym_current_density, field_ctl)
        call add_phys_name_ctl(sym_magnetic_field, field_ctl)
!
      else if( (field_name .eq. Basym_nabla_Bsym%name)) then
        call add_phys_name_ctl(asym_magnetic_field, field_ctl)
        call add_phys_name_ctl(sym_magnetic_field, field_ctl)
!
      else if( (field_name .eq. uasym_x_Bsym%name)                      &
     &    .or. (field_name .eq. rot_uasym_x_Bsym%name)                  &
     &    .or. (field_name .eq. Basym_nabla_usym%name)                  &
     &    .or. (field_name .eq. uasym_Bsym%name)) then
        call add_phys_name_ctl(asym_velocity, field_ctl)
        call add_phys_name_ctl(sym_magnetic_field, field_ctl)
!
      else if( (field_name .eq. uasym_nabla_Tsym%name)                  &
     &    .or. (field_name .eq. heat_flux_asym_sym%name)) then
        call add_phys_name_ctl(asym_velocity, field_ctl)
        call add_phys_name_ctl(asym_temperature, field_ctl)
!
      else if( (field_name .eq. uasym_nabla_pTsym%name)                 &
     &    .or. (field_name .eq. part_h_flux_asym_sym%name)) then
        call add_phys_name_ctl(asym_velocity, field_ctl)
        call add_phys_name_ctl(sym_perturbation_temp, field_ctl)
      else if( (field_name .eq. uasym_nabla_Csym%name)                  &
     &    .or. (field_name .eq. composite_flux_asym_sym%name)  ) then
        call add_phys_name_ctl(asym_velocity, field_ctl)
        call add_phys_name_ctl(sym_composition, field_ctl)
!
      else if( (field_name .eq. uasym_nabla_pCsym%name)                 &
     &    .or. (field_name .eq. part_c_flux_asym_sym%name)  ) then
        call add_phys_name_ctl(asym_velocity, field_ctl)
        call add_phys_name_ctl(sym_perturbation_composition, field_ctl)
      end if
!
      end subroutine add_force_by_sym_asym_ctl
!
! ----------------------------------------------------------------------
!
      end module check_forces_w_symmetry
