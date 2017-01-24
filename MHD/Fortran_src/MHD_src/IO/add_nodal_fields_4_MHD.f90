!>@file   add_nodal_fields_4_MHD.f90
!!@brief  module add_nodal_fields_4_MHD
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed by H. Matsui Sep., 2006
!
!> @brief Add missing field for MHD dynamo to field list
!!
!!@verbatim
!!      subroutine add_field_name_4_mhd(field_ctl)
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!@endverbatim
!
      module add_nodal_fields_4_MHD
!
      use m_precision
!
      use m_control_parameter
      use m_phys_labels
      use t_read_control_arrays
      use add_nodal_fields_ctl
!
      implicit  none
!
      private :: add_work_area_4_potentials, add_ctl_4_ref_temp
      private :: add_data_4_previous_step, add_data_4_check_step
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine add_field_name_4_mhd(field_ctl)
!
      use m_machine_parameter
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!    set work fields for potentials
!
      if (iflag_debug.eq.1) write(*,*) 'add_work_area_4_potentials'
      call add_work_area_4_potentials(field_ctl)
!
!    set work fields for reference temperature
!
      if (iflag_debug.eq.1) write(*,*) 'add_ctl_4_ref_temp'
      call add_ctl_4_ref_temp(field_ctl)
!
!     set work fields for adams-bashforth
!
      if (iflag_debug.eq.1) write(*,*) 'add_data_4_previous_step'
      call add_data_4_previous_step(field_ctl)
!
!     set work fields for evolution check
!
      if (iflag_debug.eq.1) write(*,*) 'add_data_4_check_step'
      call add_data_4_check_step(field_ctl)
!
      end subroutine add_field_name_4_mhd
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine add_ctl_4_ref_temp(field_ctl)
!
      use m_physical_property
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if (ref_param_T1%iflag_reference .ne. id_no_ref_temp) then
        call add_phys_name_ctl(fhd_part_temp, field_ctl)
        call add_phys_name_ctl(fhd_ref_temp, field_ctl)
        call add_phys_name_ctl(fhd_grad_ref_temp, field_ctl)
      end if
!
      if (iflag_4_coriolis .gt. id_turn_OFF)                            &
     &              call add_phys_name_ctl(fhd_Coriolis, field_ctl)
      if (iflag_4_gravity .eq. id_FORCE_at_node)                        &
     &              call add_phys_name_ctl(fhd_buoyancy, field_ctl)
      if (iflag_4_composit_buo .eq. id_FORCE_at_node)                   &
     &              call add_phys_name_ctl(fhd_comp_buo, field_ctl)
      if (iflag_4_filter_gravity .eq. id_FORCE_at_node)                 &
     &              call add_phys_name_ctl(fhd_filter_buo, field_ctl)
!
      end subroutine add_ctl_4_ref_temp
!
! -----------------------------------------------------------------------
!
      subroutine add_work_area_4_potentials(field_ctl)
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!    set work fields for potentials
!
      if(evo_velo%iflag_scheme .gt. id_no_evolution) then
        call add_phys_name_ctl(fhd_press_work, field_ctl)
      end if
      if (evo_magne%iflag_scheme .gt. id_no_evolution                   &
     &     .or. evo_vect_p%iflag_scheme .gt. id_no_evolution) then
        call add_phys_name_ctl(fhd_m_potential_work, field_ctl)
      end if
!
      end subroutine add_work_area_4_potentials
!
! -----------------------------------------------------------------------
!
      subroutine add_data_4_previous_step(field_ctl)
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(evo_velo%iflag_scheme .ne. id_no_evolution) then
        call add_phys_name_ctl(fhd_pre_mom, field_ctl)
        call add_phys_name_ctl(fhd_pre_press, field_ctl)
!
        call add_phys_name_ctl(fhd_forces, field_ctl)
        call add_phys_name_ctl(fhd_div_forces, field_ctl)
      end if
      if      (evo_magne%iflag_scheme .ne. id_no_evolution              &
     &    .or. evo_vect_p%iflag_scheme .ne. id_no_evolution) then
        call add_phys_name_ctl(fhd_pre_uxb, field_ctl)
      end if
      if(evo_temp%iflag_scheme .ne.  id_no_evolution) then
        call add_phys_name_ctl(fhd_pre_heat, field_ctl)
      end if
      if(evo_comp%iflag_scheme .ne.  id_no_evolution) then
        call add_phys_name_ctl(fhd_pre_composit, field_ctl)
      end if
!
      end subroutine add_data_4_previous_step
!
! -----------------------------------------------------------------------
!
      subroutine add_data_4_check_step(field_ctl)
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(evo_velo%iflag_scheme .ne. id_no_evolution) then
        call add_phys_name_ctl(fhd_chk_mom, field_ctl)
        call add_phys_name_ctl(fhd_chk_press, field_ctl)
      end if
      if(evo_magne%iflag_scheme .ne. id_no_evolution                    &
     &     .or. evo_vect_p%iflag_scheme .ne. id_no_evolution) then
        call add_phys_name_ctl(fhd_chk_uxb, field_ctl)
        call add_phys_name_ctl(fhd_chk_potential, field_ctl)
      end if
      if(evo_temp%iflag_scheme .ne.  id_no_evolution) then
        call add_phys_name_ctl(fhd_chk_heat, field_ctl)
      end if
      if(evo_comp%iflag_scheme .ne.  id_no_evolution) then
        call add_phys_name_ctl(fhd_chk_composit, field_ctl)
      end if
!
!      if(evo_velo%iflag_scheme .ge. id_Crank_nicolson) then
!        call add_phys_name_ctl(fhd_chk_mom_2, field_ctl)
!        call add_phys_name_ctl(fhd_chk_press_2, field_ctl)
!      end if
!      if(evo_magne%iflag_scheme .ge. id_Crank_nicolson                 &
!     &     .or. evo_vect_p%iflag_scheme .ge. id_Crank_nicolson) then
!        call add_phys_name_ctl(fhd_chk_uxb_2, field_ctl)
!        call add_phys_name_ctl(fhd_chk_potential_2, field_ctl)
!      end if
!      if(evo_temp%iflag_scheme .ge. id_Crank_nicolson) then
!        call add_phys_name_ctl(fhd_chk_heat_2, field_ctl)
!      end if
!      if(evo_comp%iflag_scheme .ge. id_Crank_nicolson) then
!        call add_phys_name_ctl(fhd_chk_composit_2, field_ctl)
!      end if
!
      end subroutine add_data_4_check_step
!
! -----------------------------------------------------------------------
!
      end module add_nodal_fields_4_MHD
