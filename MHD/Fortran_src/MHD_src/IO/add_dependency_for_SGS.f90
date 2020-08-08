!>@file   add_dependency_for_SGS.f90
!!@brief  module add_dependency_for_SGS
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed by H. Matsui Sep., 2006
!
!> @brief Add missing field for MHD dynamo to field list
!!
!!@verbatim
!!      subroutine add_dependent_SGS_field(SGS_param, field_ctl)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!@endverbatim
!
      module add_dependency_for_SGS
!
      use m_precision
!
      use m_machine_parameter
      use t_control_parameter
      use t_physical_property
      use add_nodal_fields_ctl
      use calypso_mpi
!
      implicit  none
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
      use check_diff_filtered_field
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
      end module add_dependency_for_SGS
