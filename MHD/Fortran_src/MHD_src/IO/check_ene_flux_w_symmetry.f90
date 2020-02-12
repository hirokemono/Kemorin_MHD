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
!!
!!      logical function check_field_w_symmetry_ctl                     &
!!     &               (field_name, field_ctl)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!      logical function check_ene_flux_by_sym_sym_ctl                  &
!!     &               (field_name, field_ctl)
!!      logical function check_ene_flux_by_asym_asym_ctl                &
!!     &               (field_name, field_ctl)
!!      logical function check_ene_flux_by_sym_asym_ctl                 &
!!     &               (field_name, field_ctl)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!
!!      integer(kind = kint) function check_ene_flux_by_sym_sym_id      &
!!     &                    (i_field, field_name, base_fld,             &
!!     &                     force_sym1_sym2, eflux_sym1_sym2)
!!        type(base_field_address), intent(in) :: base_fld
!!        type(base_force_address), intent(in) :: force_sym1_sym2
!!        type(energy_flux_address), intent(in) :: eflux_sym1_sym2
!!      integer(kind = kint) function check_ene_flux_by_asym_asym_id    &
!!     &                   (i_field, field_name, base_fld,              &
!!     &                    force_asym1_asym2, eflux_asym1_asym2)
!!        type(base_field_address), intent(in) :: base_fld
!!        type(base_force_address), intent(in) :: force_asym1_asym2
!!        type(energy_flux_address), intent(in) :: eflux_asym1_asym2
!!      integer(kind = kint) function check_ene_flux_by_sym_asym_id     &
!!     &         (i_field, field_name, base_fld, force_sym1_asym2,      &
!!     &          force_asym1_sym2, eflux_sym1_asym2, eflux_asym1_sym2)
!!        type(base_field_address), intent(in) :: base_fld
!!        type(base_force_address), intent(in) :: force_sym1_asym2
!!        type(base_force_address), intent(in) :: force_asym1_sym2
!!        type(energy_flux_address), intent(in) :: eflux_sym1_asym2
!!        type(energy_flux_address), intent(in) :: eflux_asym1_sym2
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
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_wsym_x_usym, field_ctl)
      else if( (field_name .eq. fhd_urev_Jsym_x_Bsym)                   &
     &    .or. (field_name .eq. fhd_u_dot_Jsym_x_Bsym)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_Jsym_x_Bsym, field_ctl)
      else if( (field_name .eq. fhd_u_dot_Bsym_nabla_Bsym)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_Bsym_nabla_Bsym, field_ctl)
!
      else if( (field_name .eq. fhd_sym_buo_flux)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_sym_buoyancy, field_ctl)
      else if( (field_name .eq. fhd_sym_comp_buo_flux)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_sym_comp_buo, field_ctl)
!
      else if((field_name .eq. fhd_B_rot_Bsym_x_usym)) then
        call add_phys_name_ctl(fhd_magne, field_ctl)
        call add_phys_name_ctl(fhd_rot_usym_x_Bsym, field_ctl)
      else if((field_name .eq. fhd_Bdot_Bsym_nabla_usym)) then
        call add_phys_name_ctl(fhd_magne, field_ctl)
        call add_phys_name_ctl(fhd_Bsym_nabla_usym, field_ctl)
!
      else if( (field_name .eq. fhd_T_usym_nabla_Tsym)) then
        call add_phys_name_ctl(fhd_temp, field_ctl)
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
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_wasym_x_uasym, field_ctl)
      else if( (field_name .eq. fhd_urev_Jasym_x_Basym)                 &
     &    .or. (field_name .eq. fhd_u_dot_Jasym_x_Basym)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_Jasym_x_Basym, field_ctl)
      else if((field_name .eq. fhd_u_dot_Basym_nabla_Basym)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_Basym_nabla_Basym,field_ctl)
!
      else if( (field_name .eq. fhd_asym_buo_flux)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_asym_buoyancy, field_ctl)
      else if( (field_name .eq. fhd_asym_comp_buo_flux)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_asym_comp_buo, field_ctl)
!
      else if( (field_name .eq. fhd_B_rot_Basym_x_uasym)) then
        call add_phys_name_ctl(fhd_magne, field_ctl)
        call add_phys_name_ctl(fhd_rot_uasym_x_Basym,field_ctl)
      else if( (field_name .eq. fhd_Bdot_Basym_nabla_uasym))            &
     &    then
        call add_phys_name_ctl(fhd_magne, field_ctl)
        call add_phys_name_ctl(fhd_Basym_nabla_uasym,field_ctl)
!
      else if( (field_name .eq. fhd_T_uasym_nabla_Tasym)) then
        call add_phys_name_ctl(fhd_temp, field_ctl)
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
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_wsym_x_uasym, field_ctl)
      else if( (field_name .eq. fhd_urev_Jsym_x_Basym)                  &
     &    .or. (field_name .eq. fhd_u_dot_Jsym_x_Basym)  ) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_Jsym_x_Basym, field_ctl)
      else if( (field_name .eq. fhd_u_dot_Bsym_nabla_Basym)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_Bsym_nabla_Basym, field_ctl)
!
      else if( (field_name .eq. fhd_B_rot_Bsym_x_uasym)) then
        call add_phys_name_ctl(fhd_magne, field_ctl)
        call add_phys_name_ctl(fhd_rot_usym_x_Basym, field_ctl)
      else if( (field_name .eq. fhd_Bdot_Bsym_nabla_uasym)) then
        call add_phys_name_ctl(fhd_magne, field_ctl)
        call add_phys_name_ctl(fhd_Bsym_nabla_uasym, field_ctl)
!
      else if( (field_name .eq. fhd_T_usym_nabla_Tasym)) then
        call add_phys_name_ctl(fhd_temp, field_ctl)
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
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_wasym_x_uaym, field_ctl)
!
      else if( (field_name .eq. fhd_urev_Jasym_x_Bsym)                  &
     &    .or. (field_name .eq. fhd_u_dot_Jasym_x_Bsym) ) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_Jasym_x_Bsym, field_ctl)
      else if( (field_name .eq. fhd_u_dot_Basym_nabla_Bsym)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_Basym_nabla_Bsym, field_ctl)
!
      else if( (field_name .eq. fhd_B_rot_Basym_x_usym)) then
        call add_phys_name_ctl(fhd_magne, field_ctl)
        call add_phys_name_ctl(fhd_rot_uasym_x_Bsym, field_ctl)
      else if( (field_name .eq. fhd_Bdot_Basym_nabla_usym)) then
        call add_phys_name_ctl(fhd_magne, field_ctl)
        call add_phys_name_ctl(fhd_Basym_nabla_usym, field_ctl)
!
      else if( (field_name .eq. fhd_T_uasym_nabla_Tsym)) then
        call add_phys_name_ctl(fhd_temp, field_ctl)
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
! ----------------------------------------------------------------------
!
      logical function check_ene_flux_by_sym_sym_ctl                    &
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
      if(      (field_name .eq. fhd_u_dot_wsym_x_usym)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &      .and. check_field_list_ctl(fhd_wsym_x_usym, field_ctl)
      else if( (field_name .eq. fhd_urev_Jsym_x_Bsym)                   &
     &    .or. (field_name .eq. fhd_u_dot_Jsym_x_Bsym)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &      .and. check_field_list_ctl(fhd_Jsym_x_Bsym, field_ctl)
      else if( (field_name .eq. fhd_u_dot_Bsym_nabla_Bsym)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &      .and. check_field_list_ctl(fhd_Bsym_nabla_Bsym, field_ctl)
!
      else if( (field_name .eq. fhd_sym_buo_flux)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &      .and. check_field_list_ctl(fhd_sym_buoyancy, field_ctl)
      else if( (field_name .eq. fhd_sym_comp_buo_flux)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &      .and. check_field_list_ctl(fhd_sym_comp_buo, field_ctl)
!
      else if((field_name .eq. fhd_B_rot_Bsym_x_usym)) then
        flag = flag .and. check_field_list_ctl(fhd_magne, field_ctl)    &
     &      .and. check_field_list_ctl(fhd_rot_usym_x_Bsym, field_ctl)
      else if((field_name .eq. fhd_Bdot_Bsym_nabla_usym)) then
        flag = flag .and. check_field_list_ctl(fhd_magne, field_ctl)    &
     &      .and. check_field_list_ctl(fhd_Bsym_nabla_usym, field_ctl)
!
      else if( (field_name .eq. fhd_T_usym_nabla_Tsym)) then
        flag = flag .and. check_field_list_ctl(fhd_temp, field_ctl)     &
     &      .and. check_field_list_ctl(fhd_usym_nabla_Tsym, field_ctl)
      else if( (field_name .eq. fhd_pT_usym_nabla_pTsym)) then
        flag = flag                                                     &
     &      .and. check_field_list_ctl(fhd_part_temp, field_ctl)        &
     &      .and. check_field_list_ctl(fhd_usym_nabla_pTsym, field_ctl)
!
      else if( (field_name .eq. fhd_C_usym_nabla_Csym)) then
        flag = flag .and. check_field_list_ctl(fhd_light, field_ctl)    &
     &      .and. check_field_list_ctl(fhd_usym_nabla_Csym, field_ctl)
      else if( (field_name .eq. fhd_pC_usym_nabla_pCsym)) then
        flag = flag                                                     &
     &      .and. check_field_list_ctl(fhd_part_light, field_ctl)       &
     &      .and. check_field_list_ctl(fhd_usym_nabla_pCsym, field_ctl)
      end if
      check_ene_flux_by_sym_sym_ctl = flag
      return
!
      end function check_ene_flux_by_sym_sym_ctl
!
! ----------------------------------------------------------------------
!
      logical function check_ene_flux_by_asym_asym_ctl                  &
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
      if(      (field_name .eq. fhd_u_dot_wasym_x_uasym)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &      .and. check_field_list_ctl(fhd_wasym_x_uasym, field_ctl)
      else if( (field_name .eq. fhd_urev_Jasym_x_Basym)                 &
     &    .or. (field_name .eq. fhd_u_dot_Jasym_x_Basym)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &      .and. check_field_list_ctl(fhd_Jasym_x_Basym, field_ctl)
      else if((field_name .eq. fhd_u_dot_Basym_nabla_Basym)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &      .and. check_field_list_ctl(fhd_Basym_nabla_Basym,field_ctl)
!
      else if( (field_name .eq. fhd_asym_buo_flux)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &      .and. check_field_list_ctl(fhd_asym_buoyancy, field_ctl)
      else if( (field_name .eq. fhd_asym_comp_buo_flux)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &      .and. check_field_list_ctl(fhd_asym_comp_buo, field_ctl)
!
      else if( (field_name .eq. fhd_B_rot_Basym_x_uasym)) then
        flag = flag .and. check_field_list_ctl(fhd_magne, field_ctl)    &
     &      .and. check_field_list_ctl(fhd_rot_uasym_x_Basym,field_ctl)
      else if( (field_name .eq. fhd_Bdot_Basym_nabla_uasym))            &
     &    then
        flag = flag .and. check_field_list_ctl(fhd_magne, field_ctl)    &
     &      .and. check_field_list_ctl(fhd_Basym_nabla_uasym,field_ctl)
!
      else if( (field_name .eq. fhd_T_uasym_nabla_Tasym)) then
        flag = flag .and. check_field_list_ctl(fhd_temp, field_ctl)     &
     &     .and. check_field_list_ctl(fhd_uasym_nabla_Tasym,field_ctl)
      else if( (field_name .eq. fhd_pT_uasym_nabla_pTasym)) then
        flag = flag                                                     &
     &     .and. check_field_list_ctl(fhd_part_temp, field_ctl)         &
     &     .and. check_field_list_ctl(fhd_uasym_nabla_pTasym,field_ctl)
!
      else if( (field_name .eq. fhd_C_uasym_nabla_Casym)) then
        flag = flag .and. check_field_list_ctl(fhd_light, field_ctl)    &
     &     .and. check_field_list_ctl(fhd_uasym_nabla_Casym,field_ctl)
      else if( (field_name .eq. fhd_pC_uasym_nabla_pCasym)) then
        flag = flag                                                     &
     &     .and. check_field_list_ctl(fhd_part_light, field_ctl)        &
     &     .and. check_field_list_ctl(fhd_uasym_nabla_pCasym,field_ctl)
      end if
      check_ene_flux_by_asym_asym_ctl = flag
      return
!
      end function check_ene_flux_by_asym_asym_ctl
!
! ----------------------------------------------------------------------
!
      logical function check_ene_flux_by_sym_asym_ctl                   &
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
      if(      (field_name .eq. fhd_u_dot_wsym_x_uasym)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &      .and. check_field_list_ctl(fhd_wsym_x_uasym, field_ctl)
      else if( (field_name .eq. fhd_urev_Jsym_x_Basym)                  &
     &    .or. (field_name .eq. fhd_u_dot_Jsym_x_Basym)  ) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &      .and. check_field_list_ctl(fhd_Jsym_x_Basym, field_ctl)
      else if( (field_name .eq. fhd_u_dot_Bsym_nabla_Basym)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &      .and. check_field_list_ctl(fhd_Bsym_nabla_Basym, field_ctl)
!
      else if( (field_name .eq. fhd_B_rot_Bsym_x_uasym)) then
        flag = flag .and. check_field_list_ctl(fhd_magne, field_ctl)    &
     &      .and. check_field_list_ctl(fhd_rot_usym_x_Basym, field_ctl)
      else if( (field_name .eq. fhd_Bdot_Bsym_nabla_uasym)) then
        flag = flag .and. check_field_list_ctl(fhd_magne, field_ctl)    &
     &      .and. check_field_list_ctl(fhd_Bsym_nabla_uasym, field_ctl)
!
      else if( (field_name .eq. fhd_T_usym_nabla_Tasym)) then
        flag = flag .and. check_field_list_ctl(fhd_temp, field_ctl)     &
     &      .and. check_field_list_ctl(fhd_usym_nabla_Tasym, field_ctl)
      else if( (field_name .eq. fhd_pT_usym_nabla_pTasym)) then
        flag = flag                                                     &
     &      .and. check_field_list_ctl(fhd_part_temp, field_ctl)        &
     &      .and. check_field_list_ctl(fhd_usym_nabla_pTasym,field_ctl)
!
      else if( (field_name .eq. fhd_C_usym_nabla_Casym)) then
        flag = flag .and. check_field_list_ctl(fhd_light, field_ctl)    &
     &      .and. check_field_list_ctl(fhd_usym_nabla_Casym, field_ctl)
      else if( (field_name .eq. fhd_pC_usym_nabla_pCasym)) then
        flag = flag                                                     &
     &      .and. check_field_list_ctl(fhd_part_light, field_ctl)       &
     &      .and. check_field_list_ctl(fhd_usym_nabla_pCasym,field_ctl)
!
!
      else if( (field_name .eq. fhd_u_dot_wasym_x_usym)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &      .and. check_field_list_ctl(fhd_wasym_x_uaym, field_ctl)
!
      else if( (field_name .eq. fhd_urev_Jasym_x_Bsym)                  &
     &    .or. (field_name .eq. fhd_u_dot_Jasym_x_Bsym) ) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &      .and. check_field_list_ctl(fhd_Jasym_x_Bsym, field_ctl)
      else if( (field_name .eq. fhd_u_dot_Basym_nabla_Bsym)) then
        flag = flag .and. check_field_list_ctl(fhd_velo, field_ctl)     &
     &      .and. check_field_list_ctl(fhd_Basym_nabla_Bsym, field_ctl)
!
      else if( (field_name .eq. fhd_B_rot_Basym_x_usym)) then
        flag = flag .and. check_field_list_ctl(fhd_magne, field_ctl)    &
     &      .and. check_field_list_ctl(fhd_rot_uasym_x_Bsym, field_ctl)
      else if( (field_name .eq. fhd_Bdot_Basym_nabla_usym)) then
        flag = flag .and. check_field_list_ctl(fhd_magne, field_ctl)    &
     &      .and. check_field_list_ctl(fhd_Basym_nabla_usym, field_ctl)
!
      else if( (field_name .eq. fhd_T_uasym_nabla_Tsym)) then
        flag = flag .and. check_field_list_ctl(fhd_temp, field_ctl)     &
     &      .and. check_field_list_ctl(fhd_usym_nabla_Tasym, field_ctl)
      else if( (field_name .eq. fhd_pT_uasym_nabla_pTsym)) then
        flag = flag                                                     &
     &      .and. check_field_list_ctl(fhd_part_temp, field_ctl)        &
     &      .and. check_field_list_ctl(fhd_uasym_nabla_pTsym,field_ctl)
!
      else if( (field_name .eq. fhd_C_uasym_nabla_Csym)) then
        flag = flag .and. check_field_list_ctl(fhd_light, field_ctl)    &
     &      .and. check_field_list_ctl(fhd_uasym_nabla_Csym, field_ctl)
      else if( (field_name .eq. fhd_pC_uasym_nabla_pCsym)) then
        flag = flag                                                     &
     &      .and. check_field_list_ctl(fhd_part_light, field_ctl)       &
     &      .and. check_field_list_ctl(fhd_uasym_nabla_pCsym,field_ctl)
      end if
      check_ene_flux_by_sym_asym_ctl = flag
      return
!
      end function check_ene_flux_by_sym_asym_ctl
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function check_ene_flux_by_sym_sym_id        &
     &                    (i_field, field_name, base_fld,               &
     &                     force_sym1_sym2, eflux_sym1_sym2)
!
      integer(kind = kint), intent(in) :: i_field
      character(len = kchara), intent(in) :: field_name
      type(base_field_address), intent(in) :: base_fld
      type(base_force_address), intent(in) :: force_sym1_sym2
      type(energy_flux_address), intent(in) :: eflux_sym1_sym2
!
      integer(kind = kint) :: iflag
!
!
      iflag = 0
      if(      (i_field .eq. eflux_sym1_sym2%i_m_advect_work)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_sym2%i_m_advect, fhd_wsym_x_usym)
      else if( (i_field .eq. eflux_sym1_sym2%i_nega_ujb)                &
     &    .or. (i_field .eq. eflux_sym1_sym2%i_ujb)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_sym2%i_lorentz, fhd_Jsym_x_Bsym)
      else if( (i_field .eq. eflux_sym1_sym2%i_m_tension_wk)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_sym2%i_m_tension,                     &
     &                 fhd_Bsym_nabla_Bsym)
!
      else if( (i_field .eq. eflux_sym1_sym2%i_buo_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_sym2%i_buoyancy, fhd_sym_buoyancy)
      else if( (i_field .eq. eflux_sym1_sym2%i_c_buo_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_sym2%i_comp_buo, fhd_sym_comp_buo)
!
      else if((i_field .eq. eflux_sym1_sym2%i_me_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_magne, fhd_magne)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_sym2%i_induction,                     &
     &                 fhd_rot_usym_x_Bsym)
      else if((i_field .eq. eflux_sym1_sym2%i_mag_stretch_flux)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_magne, fhd_magne)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_sym2%i_mag_stretch,                   &
     &                 fhd_Bsym_nabla_usym)
!
      else if( (i_field .eq. eflux_sym1_sym2%i_temp_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_temp, fhd_temp)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_sym2%i_h_advect, fhd_usym_nabla_Tsym)
      else if( (i_field .eq. eflux_sym1_sym2%i_par_t_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_par_temp, fhd_part_temp)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_sym2%i_ph_advect,                     &
     &                 fhd_usym_nabla_pTsym)
!
      else if( (i_field .eq. eflux_sym1_sym2%i_comp_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_light, fhd_light)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_sym2%i_c_advect, fhd_usym_nabla_Csym)
      else if( (i_field .eq. eflux_sym1_sym2%i_par_c_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_par_light, fhd_part_light)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_sym2%i_pc_advect,                     &
     &                 fhd_usym_nabla_pCsym)
      end if
      check_ene_flux_by_sym_sym_id = iflag
      return
!
      end function check_ene_flux_by_sym_sym_id
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function check_ene_flux_by_asym_asym_id      &
     &                   (i_field, field_name, base_fld,                &
     &                    force_asym1_asym2, eflux_asym1_asym2)
!
      integer(kind = kint), intent(in) :: i_field
      character(len = kchara), intent(in) :: field_name
      type(base_field_address), intent(in) :: base_fld
      type(base_force_address), intent(in) :: force_asym1_asym2
      type(energy_flux_address), intent(in) :: eflux_asym1_asym2
!
      integer(kind = kint) :: iflag
!
!
      iflag = 0
      if(      (i_field .eq. eflux_asym1_asym2%i_m_advect_work)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_asym2%i_m_advect, fhd_wasym_x_uasym)
      else if( (i_field .eq. eflux_asym1_asym2%i_nega_ujb)              &
     &    .or. (i_field .eq. eflux_asym1_asym2%i_ujb)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_asym2%i_lorentz, fhd_Jasym_x_Basym)
      else if((i_field .eq. eflux_asym1_asym2%i_m_tension_wk)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_asym2%i_m_tension,                   &
     &                 fhd_Basym_nabla_Basym)
!
      else if( (i_field .eq. eflux_asym1_asym2%i_buo_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_asym2%i_buoyancy, fhd_asym_buoyancy)
      else if( (i_field .eq. eflux_asym1_asym2%i_c_buo_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_asym2%i_comp_buo, fhd_asym_comp_buo)
!
      else if( (i_field .eq. eflux_asym1_asym2%i_me_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_magne, fhd_magne)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_asym2%i_induction,                   &
     &                 fhd_rot_uasym_x_Basym)
      else if( (i_field .eq. eflux_asym1_asym2%i_mag_stretch_flux))     &
     &    then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_magne, fhd_magne)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_asym2%i_mag_stretch,                 &
     &                 fhd_Basym_nabla_uasym)
!
      else if( (i_field .eq. eflux_asym1_asym2%i_temp_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_temp, fhd_temp)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_asym2%i_h_advect,                    &
     &                 fhd_uasym_nabla_Tasym)
      else if( (i_field .eq. eflux_asym1_asym2%i_par_t_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_par_temp, fhd_part_temp)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_asym2%i_ph_advect,                   &
     &                 fhd_uasym_nabla_pTasym)
!
      else if( (i_field .eq. eflux_asym1_asym2%i_comp_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_light, fhd_light)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_asym2%i_c_advect,                    &
     &                 fhd_uasym_nabla_Casym)
      else if( (i_field .eq. eflux_asym1_asym2%i_par_c_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_par_light, fhd_part_light)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_asym2%i_pc_advect,                   &
     &                 fhd_uasym_nabla_pCasym)
      end if
      check_ene_flux_by_asym_asym_id = iflag
      return
!
      end function check_ene_flux_by_asym_asym_id
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function check_ene_flux_by_sym_asym_id       &
     &         (i_field, field_name, base_fld, force_sym1_asym2,        &
     &          force_asym1_sym2, eflux_sym1_asym2, eflux_asym1_sym2)
!
      integer(kind = kint), intent(in) :: i_field
      character(len = kchara), intent(in) :: field_name
      type(base_field_address), intent(in) :: base_fld
      type(base_force_address), intent(in) :: force_sym1_asym2
      type(base_force_address), intent(in) :: force_asym1_sym2
      type(energy_flux_address), intent(in) :: eflux_sym1_asym2
      type(energy_flux_address), intent(in) :: eflux_asym1_sym2
!
      integer(kind = kint) :: iflag
!
!
      iflag = 0
      if(      (i_field .eq. eflux_sym1_asym2%i_m_advect_work)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_asym2%i_m_advect, fhd_wsym_x_uasym)
      else if( (i_field .eq. eflux_sym1_asym2%i_nega_ujb)               &
     &    .or. (i_field .eq. eflux_sym1_asym2%i_ujb)  ) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_asym2%i_lorentz, fhd_Jsym_x_Basym)
      else if( (i_field .eq. eflux_sym1_asym2%i_m_tension_wk)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_asym2%i_m_tension,                    &
     &                 fhd_Bsym_nabla_Basym)
!
      else if( (i_field .eq. eflux_sym1_asym2%i_me_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_magne, fhd_magne)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_asym2%i_induction,                    &
     &                 fhd_rot_usym_x_Basym)
      else if( (i_field .eq. eflux_sym1_asym2%i_mag_stretch_flux)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_magne, fhd_magne)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_asym2%i_mag_stretch,                  &
     &                 fhd_Bsym_nabla_uasym)
!
      else if( (i_field .eq. eflux_sym1_asym2%i_temp_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_temp, fhd_temp)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_asym2%i_h_advect,                     &
     &                 fhd_usym_nabla_Tasym)
      else if( (i_field .eq. eflux_sym1_asym2%i_par_t_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_par_temp, fhd_part_temp)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_asym2%i_ph_advect,                    &
     &                 fhd_usym_nabla_pTasym)
!
      else if( (i_field .eq. eflux_sym1_asym2%i_comp_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_light, fhd_light)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_asym2%i_c_advect,                     &
     &                 fhd_usym_nabla_Casym)
      else if( (i_field .eq. eflux_sym1_asym2%i_par_c_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_par_light, fhd_part_light)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_sym1_asym2%i_pc_advect,                    &
     &                 fhd_usym_nabla_pCasym)
!
!
      else if( (i_field .eq. eflux_asym1_sym2%i_m_advect_work)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_sym2%i_m_advect, fhd_wasym_x_uaym)
!
      else if( (i_field .eq. eflux_asym1_sym2%i_nega_ujb)               &
     &    .or. (i_field .eq. eflux_asym1_sym2%i_ujb) ) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_sym2%i_lorentz, fhd_Jasym_x_Bsym)
      else if( (i_field .eq. eflux_asym1_sym2%i_m_tension_wk)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_sym2%i_m_tension,                    &
     &                 fhd_Basym_nabla_Bsym)
!
      else if( (i_field .eq. eflux_asym1_sym2%i_me_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_magne, fhd_magne)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_sym2%i_induction,                    &
     &                 fhd_rot_uasym_x_Bsym)
      else if( (i_field .eq. eflux_asym1_sym2%i_mag_stretch_flux)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_magne, fhd_magne)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_sym2%i_mag_stretch,                  &
     &                 fhd_Basym_nabla_usym)
!
      else if( (i_field .eq. eflux_asym1_sym2%i_temp_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_temp, fhd_temp)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_sym2%i_h_advect,                     &
     &                 fhd_usym_nabla_Tasym)
      else if( (i_field .eq. eflux_asym1_sym2%i_par_t_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_par_temp, fhd_part_temp)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_sym2%i_ph_advect,                    &
     &                 fhd_uasym_nabla_pTsym)
!
      else if( (i_field .eq. eflux_asym1_sym2%i_comp_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_light, fhd_light)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_sym2%i_c_advect,                     &
     &                 fhd_uasym_nabla_Csym)
      else if( (i_field .eq. eflux_asym1_sym2%i_par_c_gen)) then
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_par_light, fhd_part_light)
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 force_asym1_sym2%i_pc_advect,                    &
     &                 fhd_uasym_nabla_pCsym)
      end if
      check_ene_flux_by_sym_asym_id = iflag
      return
!
      end function check_ene_flux_by_sym_asym_id
!
! ----------------------------------------------------------------------
!
      end module check_ene_flux_w_symmetry
