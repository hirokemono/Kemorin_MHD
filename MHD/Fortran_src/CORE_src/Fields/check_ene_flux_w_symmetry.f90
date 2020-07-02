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
      use m_force_w_sym_labels
      use add_nodal_fields_ctl
!
      character(len = kchara), intent(in) :: field_name
      type(ctl_array_c3), intent(inout) :: field_ctl
!
      if(      (field_name .eq. u_dot_wsym_x_usym%name)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(wsym_x_usym, field_ctl)
      else if( (field_name .eq. rev_u_dot_Jsym_x_Bsym%name)             &
     &    .or. (field_name .eq. u_dot_Jsym_x_Bsym%name)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(Jsym_x_Bsym, field_ctl)
      else if( (field_name .eq. u_dot_Bsym_nabla_Bsym%name)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(Bsym_nabla_Bsym, field_ctl)
!
      else if( (field_name .eq. sym_termal_buo_flux%name)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(sym_termal_buoyancy, field_ctl)
      else if( (field_name .eq. sym_composite_buo_flux%name)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(sym_composite_buoyancy, field_ctl)
!
      else if((field_name .eq. B_rot_Bsym_x_usym%name)) then
        call add_phys_name_ctl(magnetic_field, field_ctl)
        call add_phys_name_ctl(rot_usym_x_Bsym, field_ctl)
      else if((field_name .eq. B_dot_Bsym_nabla_usym%name)) then
        call add_phys_name_ctl(magnetic_field, field_ctl)
        call add_phys_name_ctl(Bsym_nabla_usym, field_ctl)
!
      else if( (field_name .eq. T_usym_nabla_Tsym%name)) then
        call add_phys_name_ctl(temperature, field_ctl)
        call add_phys_name_ctl(usym_nabla_Tsym, field_ctl)
      else if( (field_name .eq. pT_usym_nabla_pTsym%name)) then
        call add_phys_name_ctl(perturbation_temp, field_ctl)
        call add_phys_name_ctl(usym_nabla_pTsym, field_ctl)
!
      else if( (field_name .eq. C_usym_nabla_Csym%name)) then
        call add_phys_name_ctl(composition, field_ctl)
        call add_phys_name_ctl(usym_nabla_Csym, field_ctl)
      else if( (field_name .eq. pC_usym_nabla_pCsym%name)) then
        call add_phys_name_ctl(perturbation_composition, field_ctl)
        call add_phys_name_ctl(usym_nabla_pCsym, field_ctl)
      end if
!
      end subroutine add_ene_flux_by_sym_sym_ctl
!
! ----------------------------------------------------------------------
!
      subroutine add_ene_flux_by_asym_asym_ctl(field_name, field_ctl)
!
      use t_control_array_character3
      use m_force_w_sym_labels
      use add_nodal_fields_ctl
!
      character(len = kchara), intent(in) :: field_name
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(      (field_name .eq. u_dot_wasym_x_uasym%name)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(wasym_x_uasym, field_ctl)
      else if( (field_name .eq. rev_u_dot_Jasym_x_Basym%name)           &
     &    .or. (field_name .eq. u_dot_Jasym_x_Basym%name)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(Jasym_x_Basym, field_ctl)
      else if((field_name .eq. u_dot_Basym_nabla_Basym%name)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(Basym_nabla_Basym, field_ctl)
!
      else if( (field_name .eq. asym_termal_buo_flux%name)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(asym_termal_buoyancy, field_ctl)
      else if( (field_name .eq. asym_composite_buo_flux%name)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(asym_composite_buoyancy, field_ctl)
!
      else if( (field_name .eq. B_rot_Basym_x_uasym%name)) then
        call add_phys_name_ctl(magnetic_field, field_ctl)
        call add_phys_name_ctl(rot_uasym_x_Basym, field_ctl)
      else if( (field_name .eq. B_dot_Basym_nabla_uasym%name)) then
        call add_phys_name_ctl(magnetic_field, field_ctl)
        call add_phys_name_ctl(Basym_nabla_uasym, field_ctl)
!
      else if( (field_name .eq. T_uasym_nabla_Tasym%name)) then
        call add_phys_name_ctl(temperature, field_ctl)
        call add_phys_name_ctl(uasym_nabla_Tasym, field_ctl)
      else if( (field_name .eq. pT_uasym_nabla_pTasym%name)) then
        call add_phys_name_ctl(perturbation_temp, field_ctl)
        call add_phys_name_ctl(uasym_nabla_pTasym, field_ctl)
!
      else if( (field_name .eq. C_uasym_nabla_Casym%name)) then
        call add_phys_name_ctl(composition, field_ctl)
        call add_phys_name_ctl(uasym_nabla_Casym, field_ctl)
      else if( (field_name .eq. pC_uasym_nabla_pCasym%name)) then
        call add_phys_name_ctl(perturbation_composition, field_ctl)
        call add_phys_name_ctl(uasym_nabla_pCasym, field_ctl)
      end if
!
      end subroutine add_ene_flux_by_asym_asym_ctl
!
! ----------------------------------------------------------------------
!
      subroutine add_ene_flux_by_sym_asym_ctl(field_name, field_ctl)
!
      use t_control_array_character3
      use m_force_w_sym_labels
      use add_nodal_fields_ctl
!
      character(len = kchara), intent(in) :: field_name
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(      (field_name .eq. u_dot_wsym_x_uasym%name)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(wsym_x_uasym, field_ctl)
      else if( (field_name .eq. rev_u_dot_Jsym_x_Basym%name)            &
     &    .or. (field_name .eq. u_dot_Jsym_x_Basym%name)  ) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(Jsym_x_Basym, field_ctl)
      else if( (field_name .eq. u_dot_Bsym_nabla_Basym%name)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(Bsym_nabla_Basym, field_ctl)
!
      else if( (field_name .eq. B_rot_Bsym_x_uasym%name)) then
        call add_phys_name_ctl(magnetic_field, field_ctl)
        call add_phys_name_ctl(rot_usym_x_Basym, field_ctl)
      else if( (field_name .eq. B_dot_Bsym_nabla_uasym%name)) then
        call add_phys_name_ctl(magnetic_field, field_ctl)
        call add_phys_name_ctl(Bsym_nabla_uasym, field_ctl)
!
      else if( (field_name .eq. T_usym_nabla_Tasym%name)) then
        call add_phys_name_ctl(temperature, field_ctl)
        call add_phys_name_ctl(usym_nabla_Tasym, field_ctl)
      else if( (field_name .eq. pT_usym_nabla_pTasym%name)) then
        call add_phys_name_ctl(perturbation_temp, field_ctl)
        call add_phys_name_ctl(usym_nabla_pTasym, field_ctl)
!
      else if( (field_name .eq. C_usym_nabla_Casym%name)) then
        call add_phys_name_ctl(composition, field_ctl)
        call add_phys_name_ctl(usym_nabla_Casym, field_ctl)
      else if( (field_name .eq. pC_usym_nabla_pCasym%name)) then
        call add_phys_name_ctl(perturbation_composition, field_ctl)
        call add_phys_name_ctl(usym_nabla_pCasym, field_ctl)
!
!
      else if( (field_name .eq. u_dot_wasym_x_usym%name)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(wasym_x_usym, field_ctl)
!
      else if( (field_name .eq. rev_u_dot_Jasym_x_Bsym%name)            &
     &    .or. (field_name .eq. u_dot_Jasym_x_Bsym%name) ) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(Jasym_x_Bsym, field_ctl)
      else if( (field_name .eq. u_dot_Basym_nabla_Bsym%name)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(Basym_nabla_Bsym, field_ctl)
!
      else if( (field_name .eq. B_rot_Basym_x_usym%name)) then
        call add_phys_name_ctl(magnetic_field, field_ctl)
        call add_phys_name_ctl(rot_uasym_x_Bsym, field_ctl)
      else if( (field_name .eq. B_dot_Basym_nabla_usym%name)) then
        call add_phys_name_ctl(magnetic_field, field_ctl)
        call add_phys_name_ctl(Basym_nabla_usym, field_ctl)
!
      else if( (field_name .eq. T_uasym_nabla_Tsym%name)) then
        call add_phys_name_ctl(temperature, field_ctl)
        call add_phys_name_ctl(uasym_nabla_Tsym, field_ctl)
      else if( (field_name .eq. pT_uasym_nabla_pTsym%name)) then
        call add_phys_name_ctl(perturbation_temp, field_ctl)
        call add_phys_name_ctl(uasym_nabla_pTsym, field_ctl)
!
      else if( (field_name .eq. C_uasym_nabla_Csym%name)) then
        call add_phys_name_ctl(composition, field_ctl)
        call add_phys_name_ctl(uasym_nabla_Csym, field_ctl)
      else if( (field_name .eq. pC_uasym_nabla_pCsym%name)) then
        call add_phys_name_ctl(perturbation_composition, field_ctl)
        call add_phys_name_ctl(uasym_nabla_pCsym, field_ctl)
      end if
!
      end subroutine add_ene_flux_by_sym_asym_ctl
!
! ----------------------------------------------------------------------
!
      end module check_ene_flux_w_symmetry
