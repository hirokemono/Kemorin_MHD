!>@file   add_nodal_fields_4_MHD.f90
!!@brief  module add_nodal_fields_4_MHD
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed by H. Matsui Sep., 2006
!
!> @brief Add missing field for MHD dynamo to field list
!!
!!@verbatim
!!      subroutine add_field_name_4_mhd(MHD_prop, field_ctl)
!!      subroutine add_ctl_4_ref_temp                                   &
!!     &         (ref_param_T, ref_param_C, field_ctl)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(reference_scalar_param), intent(in) :: ref_param_T
!!        type(reference_scalar_param), intent(in) :: ref_param_C
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!@endverbatim
!
      module add_nodal_fields_4_MHD
!
      use m_precision
!
      use m_machine_parameter
      use m_phys_labels
      use t_control_parameter
      use t_physical_property
      use add_nodal_fields_ctl
      use calypso_mpi
!
      implicit  none
!
      private :: add_work_area_4_potentials, add_ctl_4_forces
      private :: add_data_4_previous_step, add_data_4_check_step
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine add_dependent_SGS_field(SGS_param, field_ctl)
!
      use t_SGS_control_parameter
      use check_wide_SGS_terms
      use check_SGS_terms
      use check_filtered_field
      use check_filtered_forces
      use check_double_filter_field
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      call add_field_ctl_4_model_coefs(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_model_coefs end'
!
      call add_field_ctl_4_dble_SGS_terms(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_dble_SGS_terms end'
!
      if(SGS_param%iflag_SGS .eq. id_SGS_similarity) then
        call add_field_ctl_4_simi_wide_SGS(field_ctl)
        if (iflag_debug .ge. iflag_routine_msg) write(*,*)              &
     &      'add_field_ctl_4_simi_wide_SGS end'
      else if(SGS_param%iflag_SGS .eq. id_SGS_NL_grad) then
        call add_field_ctl_4_grad_wide_SGS(field_ctl)
        if (iflag_debug .ge. iflag_routine_msg) write(*,*)              &
     &    'add_field_ctl_4_grad_wide_SGS end'
      end if
!
      call add_field_ctl_4_force_w_SGS(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_force_w_SGS end'
      call add_field_ctl_4_true_SGS(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_true_SGS end'
!
      call add_field_ctl_4_SGS_ene_fluxes(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_SGS_ene_fluxes end'
      call add_field_ctl_4_diff_SGS_terms(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_diff_SGS_terms end'
      call add_field_ctl_4_SGS_terms(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_SGS_terms end'
!
      call add_field_ctl_4_dbl_fil_field(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_dbl_fil_field end'
      call add_field_ctl_4_wide_fil_field(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_wide_fil_field end'
!
      call add_field_ctl_4_diff_fil_vect(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_diff_fil_vect end'
      call add_field_ctl_4_grad_fil_field(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_grad_fil_field end'
!
      call add_field_ctl_4_fil_ene_flux(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_fil_ene_flux end'
!
      call add_field_ctl_4_rot_fil_forces(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_rot_fil_forces end'
      call add_field_ctl_4_div_fil_forces(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_div_fil_forces end'
!
      call add_field_ctl_4_filter_forces(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_filter_forces end'
!
      call add_field_ctl_4_filterd_field(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_filterd_field end'
!
      end subroutine add_dependent_SGS_field
!
! -----------------------------------------------------------------------
!
      subroutine add_dependent_field(field_ctl)
!
      use check_base_forces
      use check_base_field
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      call add_field_ctl_4_field_products(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_field_products end'
!
      call add_field_ctl_4_diffusions(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_diffusions end'
!
      call add_field_ctl_4_div_forces(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_div_forces end'
      call add_field_ctl_4_rot_forces(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_rot_forces end'
      call add_field_ctl_4_forces(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_forces end'
!
      call add_field_ctl_4_diff_vector(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_diff_vector end'
      call add_field_ctl_4_grad_field(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_grad_field end'
!
      call add_field_ctl_4_base_field(field_ctl)
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &    'add_field_ctl_4_base_field end'
!
      end subroutine add_dependent_field
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine add_field_name_4_mhd(MHD_prop, field_ctl)
!
      use t_control_array_character3
      use t_reference_scalar_param
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
!    set work fields for potentials
!
      if (iflag_debug.eq.1) write(*,*) 'add_work_area_4_potentials'
      call add_work_area_4_potentials                                   &
     &   (MHD_prop%fl_prop, MHD_prop%cd_prop, field_ctl)
!
!    set work fields for reference temperature
!
      if (iflag_debug.eq.1) write(*,*) 'add_ctl_4_forces'
      call add_ctl_4_forces                                             &
     &   (MHD_prop%fl_prop, MHD_prop%ref_param_T, MHD_prop%ref_param_C, &
     &    field_ctl)
!
!     set work fields for adams-bashforth
!
      if (iflag_debug.eq.1) write(*,*) 'add_data_4_previous_step'
      call add_data_4_previous_step                                     &
     &   (MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop, field_ctl)
!
!     set work fields for evolution check
!
      if (iflag_debug.eq.1) write(*,*) 'add_data_4_check_step'
      call add_data_4_check_step                                        &
     &   (MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop, field_ctl)
!
      end subroutine add_field_name_4_mhd
!
! -----------------------------------------------------------------------
!
      subroutine add_ctl_4_ref_temp                                     &
     &        (ref_param_T, ref_param_C, field_ctl)
!
      use t_control_array_character3
      use t_reference_scalar_param
!
      type(reference_scalar_param), intent(in) :: ref_param_T
      type(reference_scalar_param), intent(in) :: ref_param_C
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if (ref_param_T%iflag_reference .ne. id_no_ref_temp) then
        call add_phys_name_ctl(reference_temperature%name, field_ctl)
        call add_phys_name_ctl(grad_reference_temp%name, field_ctl)
      end if
!
      if (ref_param_C%iflag_reference .ne. id_no_ref_temp) then
        call add_phys_name_ctl(reference_composition%name, field_ctl)
        call add_phys_name_ctl(grad_reference_composition%name,         &
     &      field_ctl)
      end if
!
      end subroutine add_ctl_4_ref_temp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine add_ctl_4_forces                                       &
     &         (fl_prop, ref_param_T, ref_param_C, field_ctl)
!
      use t_control_array_character3
      use t_reference_scalar_param
!
      type(fluid_property), intent(in) :: fl_prop
      type(reference_scalar_param), intent(in) :: ref_param_T
      type(reference_scalar_param), intent(in) :: ref_param_C
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if (ref_param_T%iflag_reference .ne. id_no_ref_temp) then
        call add_phys_name_ctl(perturbation_temp%name, field_ctl)
      end if
!
      if (ref_param_C%iflag_reference .ne. id_no_ref_temp) then
        call add_phys_name_ctl                                          &
     &     (perturbation_composition%name, field_ctl)
      end if
!
      if (fl_prop%iflag_4_coriolis .gt. id_turn_OFF)                    &
     &  call add_phys_name_ctl(Coriolis_force%name, field_ctl)
      if (fl_prop%iflag_4_gravity .eq. id_FORCE_at_node)                &
     &  call add_phys_name_ctl(buoyancy%name, field_ctl)
      if (fl_prop%iflag_4_composit_buo .eq. id_FORCE_at_node)           &
     &  call add_phys_name_ctl(composite_buoyancy%name, field_ctl)
      if (fl_prop%iflag_4_filter_gravity .eq. id_FORCE_at_node)         &
     &  call add_phys_name_ctl(filtered_buoyancy%name, field_ctl)
      if (fl_prop%iflag_4_filter_comp_buo .eq. id_FORCE_at_node)        &
     &  call add_phys_name_ctl(filtered_comp_buoyancy%name, field_ctl)
!
      end subroutine add_ctl_4_forces
!
! -----------------------------------------------------------------------
!
      subroutine add_work_area_4_potentials                             &
     &         (fl_prop, cd_prop, field_ctl)
!
      use t_control_array_character3
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!    set work fields for potentials
!
      if(fl_prop%iflag_scheme .gt. id_no_evolution) then
        call add_phys_name_ctl(pressure_work%name, field_ctl)
      end if
      if (cd_prop%iflag_Bevo_scheme .gt. id_no_evolution                &
     &   .or. cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        call add_phys_name_ctl(m_potential_work%name, field_ctl)
      end if
!
      end subroutine add_work_area_4_potentials
!
! -----------------------------------------------------------------------
!
      subroutine add_data_4_previous_step                               &
     &         (fl_prop, cd_prop, ht_prop, cp_prop, field_ctl)
!
      use t_control_array_character3
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(fl_prop%iflag_scheme .ne. id_no_evolution) then
        call add_phys_name_ctl(previous_momentum%name, field_ctl)
        call add_phys_name_ctl(previous_pressure%name, field_ctl)
!
        call add_phys_name_ctl(sum_forces%name, field_ctl)
        call add_phys_name_ctl(div_sum_forces%name, field_ctl)
      end if
      if      (cd_prop%iflag_Bevo_scheme .ne. id_no_evolution           &
     &    .or. cd_prop%iflag_Aevo_scheme .ne. id_no_evolution) then
        call add_phys_name_ctl(previous_induction%name, field_ctl)
      end if
      if(ht_prop%iflag_scheme .ne.  id_no_evolution) then
        call add_phys_name_ctl(previous_heat%name, field_ctl)
      end if
      if(cp_prop%iflag_scheme .ne.  id_no_evolution) then
        call add_phys_name_ctl(previous_composition%name, field_ctl)
      end if
!
      end subroutine add_data_4_previous_step
!
! -----------------------------------------------------------------------
!
      subroutine add_data_4_check_step                                  &
     &         (fl_prop, cd_prop,  ht_prop, cp_prop, field_ctl)
!
      use t_control_array_character3
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(fl_prop%iflag_scheme .ne. id_no_evolution) then
        call add_phys_name_ctl(check_momentum%name, field_ctl)
        call add_phys_name_ctl(check_pressure%name, field_ctl)
      end if
      if(cd_prop%iflag_Bevo_scheme .ne. id_no_evolution                 &
     &     .or. cd_prop%iflag_Aevo_scheme .ne. id_no_evolution) then
        call add_phys_name_ctl(check_induction%name, field_ctl)
        call add_phys_name_ctl(check_potential%name, field_ctl)
      end if
      if(ht_prop%iflag_scheme .ne.  id_no_evolution) then
        call add_phys_name_ctl(check_heat%name, field_ctl)
      end if
      if(cp_prop%iflag_scheme .ne.  id_no_evolution) then
        call add_phys_name_ctl(check_composition%name, field_ctl)
      end if
!
!      if(fl_prop%iflag_scheme .ge. id_Crank_nicolson) then
!        call add_phys_name_ctl(fhd_chk_mom_2, field_ctl)
!        call add_phys_name_ctl(fhd_chk_press_2, field_ctl)
!      end if
!      if     (cd_prop%iflag_Bevo_scheme .ge. id_Crank_nicolson         &
!     &   .or. cd_prop%iflag_Aevo_scheme .ge. id_Crank_nicolson) then
!        call add_phys_name_ctl(fhd_chk_uxb_2, field_ctl)
!        call add_phys_name_ctl(fhd_chk_potential_2, field_ctl)
!      end if
!      if(ht_prop%iflag_scheme .ge. id_Crank_nicolson) then
!        call add_phys_name_ctl(fhd_chk_heat_2, field_ctl)
!      end if
!      if(cp_prop%iflag_scheme .ge. id_Crank_nicolson) then
!        call add_phys_name_ctl(fhd_chk_composit_2, field_ctl)
!      end if
!
      end subroutine add_data_4_check_step
!
! -----------------------------------------------------------------------
!
      end module add_nodal_fields_4_MHD
