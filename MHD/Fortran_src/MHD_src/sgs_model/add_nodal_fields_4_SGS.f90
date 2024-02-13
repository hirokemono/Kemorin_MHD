!
!      module add_nodal_fields_4_SGS
!
!        programmed by H.Matsui on Sep., 2006
!
!!      subroutine add_filtered_buoyancy_4_FEM_MHD(fl_prop, field_ctl)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!      subroutine add_work_area_4_sgs_model                            &
!!     &         (SGS_param, fl_prop, field_ctl)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!
      module add_nodal_fields_4_SGS
!
      use m_precision
!
      use t_SGS_control_parameter
      use t_physical_property
      use t_control_array_character3
!
      use add_nodal_fields_ctl
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine add_filtered_buoyancy_4_FEM_MHD(fl_prop, field_ctl)
!
      use m_filtered_force_labels
!
      type(fluid_property), intent(in) :: fl_prop
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(fl_prop%iflag_FEM_gravity .eq. id_FORCE_at_node) then
        if(fl_prop%iflag_4_filter_gravity)                              &
     &    call add_phys_name_ctl(filtered_buoyancy, field_ctl)
        if(fl_prop%iflag_4_filter_comp_buo)                             &
     &    call add_phys_name_ctl(filtered_comp_buoyancy, field_ctl)
      end if
!
      end subroutine add_filtered_buoyancy_4_FEM_MHD
!
! -----------------------------------------------------------------------
!
      subroutine add_work_area_4_sgs_model                              &
     &         (SGS_param, fl_prop, field_ctl)
!
 !
      use m_base_field_labels
      use m_diff_vector_labels
!
      use m_grad_field_labels
      use m_SGS_term_labels
      use m_SGS_enegy_flux_labels
      use m_diff_SGS_term_labels
      use m_SGS_model_coef_labels
      use m_filtered_field_labels
      use m_wide_filter_field_labels
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(fluid_property), intent(in) :: fl_prop
      type(ctl_array_c3), intent(inout) :: field_ctl
!
      integer(kind = kint) :: i
!
!   work area for SGS model
!
      if (SGS_param%SGS_heat%iflag_SGS_flux .ne. id_SGS_none) then
        call add_phys_name_ctl(temp_4_SGS, field_ctl)
      end if
      if (SGS_param%SGS_light%iflag_SGS_flux .ne. id_SGS_none) then
        call add_phys_name_ctl(comp_4_SGS, field_ctl)
      end if
!
      if (SGS_param%iflag_SGS_gravity .ne. id_SGS_none) then
        call add_phys_name_ctl(SGS_momentum_flux, field_ctl)
        call add_phys_name_ctl(div_SGS_m_flux, field_ctl)
        call add_phys_name_ctl(Reynolds_work, field_ctl)
!
        if(fl_prop%iflag_4_gravity) then
          call add_phys_name_ctl(SGS_heat_flux, field_ctl)
          call add_phys_name_ctl(SGS_buoyancy_flux, field_ctl)
        end if
        if(fl_prop%iflag_4_composit_buo) then
          call add_phys_name_ctl(SGS_composit_flux, field_ctl)
          call add_phys_name_ctl(SGS_comp_buoyancy_flux, field_ctl)
        end if
      end if
!
      call add_work_area_4_dynamic_sgs(SGS_param, field_ctl)
!
!   field labels for nonlinear gradient model
!
      if(      SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF          &
     &    .or. SGS_param%iflag_SGS.eq.id_SGS_similarity) then
        do i = 1, field_ctl%num
          if(      field_ctl%c1_tbl(i) .eq. velocity%name) then
            call add_phys_name_ctl(grad_v_1, field_ctl)
            call add_phys_name_ctl(grad_v_2, field_ctl)
            call add_phys_name_ctl(grad_v_3, field_ctl)
!          if(      field_ctl%c1_tbl(i) .eq. vorticity%name) then
!            call add_phys_name_ctl(grad_w_1, field_ctl)
!            call add_phys_name_ctl(grad_w_2, field_ctl)
!            call add_phys_name_ctl(grad_w_3, field_ctl)
!          else if(field_ctl%c1_tbl(i) .eq. vector_potential%name) then
!            call add_phys_name_ctl(grad_a_1, field_ctl)
!            call add_phys_name_ctl(grad_a_2, field_ctl)
!            call add_phys_name_ctl(grad_a_3, field_ctl)
          else if(field_ctl%c1_tbl(i) .eq. magnetic_field%name) then
            call add_phys_name_ctl(grad_b_1, field_ctl)
            call add_phys_name_ctl(grad_b_2, field_ctl)
            call add_phys_name_ctl(grad_b_3, field_ctl)
!          else if(field_ctl%c1_tbl(i) .eq. current_density%name) then
!            call add_phys_name_ctl(grad_j_1, field_ctl)
!            call add_phys_name_ctl(grad_j_2, field_ctl)
!            call add_phys_name_ctl(grad_j_3, field_ctl)
          else if(field_ctl%c1_tbl(i) .eq. temperature%name) then
            call add_phys_name_ctl(grad_temp, field_ctl)
          else if(field_ctl%c1_tbl(i)                                   &
     &             .eq. perturbation_temp%name) then
            call add_phys_name_ctl(grad_pert_temp, field_ctl)
          else if(field_ctl%c1_tbl(i) .eq. composition%name) then
            call add_phys_name_ctl(grad_composition, field_ctl)
          end if
        end do
      end if
!
!   field labels for filtered field
!
      if(       SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF         &
     &     .or. SGS_param%iflag_SGS.eq.id_SGS_similarity) then
        do i = 1, field_ctl%num
          if(      field_ctl%c1_tbl(i) .eq. velocity%name) then
            call add_phys_name_ctl(filter_velocity, field_ctl)
          else if(field_ctl%c1_tbl(i) .eq. vector_potential%name) then
            call add_phys_name_ctl(filter_vector_potential, field_ctl)
          else if(field_ctl%c1_tbl(i) .eq. magnetic_field%name) then
            call add_phys_name_ctl(filter_magne, field_ctl)
          else if(field_ctl%c1_tbl(i) .eq. temperature%name) then
            call add_phys_name_ctl(filter_temperature, field_ctl)
          else if(field_ctl%c1_tbl(i) .eq. composition%name) then
            call add_phys_name_ctl(filter_composition, field_ctl)
          end if
        end do
      end if
!
!   field labels for wider filtered field
!
      if(        SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF        &
     &     .and. SGS_param%iflag_SGS.eq.id_SGS_similarity) then
        do i = 1, field_ctl%num
          if(      field_ctl%c1_tbl(i) .eq. filter_velocity%name) then
            call add_phys_name_ctl(wide_filter_velocity, field_ctl)
          else if( field_ctl%c1_tbl(i)                                  &
     &                         .eq. filter_vector_potential%name) then
            call add_phys_name_ctl                                      &
     &         (wide_filter_vector_potential, field_ctl)
          else if(field_ctl%c1_tbl(i) .eq. filter_magne%name) then
            call add_phys_name_ctl(wide_filter_magne, field_ctl)
          else if(field_ctl%c1_tbl(i)                                   &
     &                          .eq. filter_temperature%name) then
            call add_phys_name_ctl(wide_filter_temp, field_ctl)
          end if
        end do
      end if
!
!   field labels for turbulence diffusivity
!
      if (SGS_param%iflag_SGS .eq. id_SGS_diffusion) then
        call add_phys_name_ctl(SGS_diffuse, field_ctl)
      end if
!
      end subroutine add_work_area_4_sgs_model
!
! -----------------------------------------------------------------------
!
      subroutine add_work_area_4_dynamic_sgs(SGS_param, field_ctl)
!
      use m_SGS_model_coef_labels
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!   work area for dynamic SGS model
!
      if(      SGS_param%iflag_dynamic.eq.id_SGS_DYNAMIC_OFF            &
     &   .and. SGS_param%iflag_SGS.eq.id_SGS_NL_grad) then
        call add_phys_name_ctl(SGS_grad, field_ctl)
      else if (SGS_param%iflag_dynamic.eq.id_SGS_DYNAMIC_OFF            &
     &     .and. SGS_param%iflag_SGS.eq.id_SGS_similarity) then
        call add_phys_name_ctl(SGS_simi, field_ctl)
      else if (SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
        call add_phys_name_ctl(SGS_grad, field_ctl)
        call add_phys_name_ctl(SGS_simi, field_ctl)
        call add_phys_name_ctl(SGS_grad_f, field_ctl)
!
        if (SGS_param%SGS_heat%iflag_SGS_flux .ne. id_SGS_none) then
          call add_phys_name_ctl(temp_4_SGS, field_ctl)
        end if
        if (SGS_param%SGS_light%iflag_SGS_flux .ne. id_SGS_none) then
          call add_phys_name_ctl(comp_4_SGS, field_ctl)
        end if
      end if
!
      if((SGS_param%SGS_momentum%iflag_SGS_flux .eq. id_SGS_diffusion)  &
     &   .or. (SGS_param%iflag_SGS_lorentz .eq. id_SGS_diffusion)) then
        call add_phys_name_ctl(SGS_diffuse, field_ctl)
      end if
!
      end subroutine add_work_area_4_dynamic_sgs
!
! -----------------------------------------------------------------------
!
      end module add_nodal_fields_4_SGS
