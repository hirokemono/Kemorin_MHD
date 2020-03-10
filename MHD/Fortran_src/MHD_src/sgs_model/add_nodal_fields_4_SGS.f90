!
!      module add_nodal_fields_4_SGS
!
!        programmed by H.Matsui on Sep., 2006
!
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
      use m_phys_labels
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
      subroutine add_work_area_4_sgs_model                              &
     &         (SGS_param, fl_prop, field_ctl)
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(fluid_property), intent(in) :: fl_prop
      type(ctl_array_c3), intent(inout) :: field_ctl
!
      integer(kind = kint) :: i
!
!   work area for SGS model
!
      if (SGS_param%iflag_SGS_h_flux .ne. id_SGS_none) then
        call add_phys_name_ctl(temp_4_SGS%name, field_ctl)
      end if
      if (SGS_param%iflag_SGS_c_flux .ne. id_SGS_none) then
        call add_phys_name_ctl(comp_4_SGS%name, field_ctl)
      end if
!
      if (SGS_param%iflag_SGS_gravity .ne. id_SGS_none) then
        call add_phys_name_ctl(SGS_momentum_flux%name, field_ctl)
        call add_phys_name_ctl(div_SGS_m_flux%name, field_ctl)
        call add_phys_name_ctl(Reynolds_work%name, field_ctl)
!
        if(fl_prop%iflag_4_gravity .gt. id_turn_OFF) then
          call add_phys_name_ctl(SGS_heat_flux%name, field_ctl)
          call add_phys_name_ctl(SGS_buoyancy_flux%name, field_ctl)
        end if
        if(fl_prop%iflag_4_composit_buo .gt. id_turn_OFF) then
          call add_phys_name_ctl(SGS_composit_flux%name, field_ctl)
          call add_phys_name_ctl                                        &
     &       (SGS_comp_buoyancy_flux%name, field_ctl)
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
          if(      field_ctl%c1_tbl(i) .eq. fhd_velo) then
            call add_phys_name_ctl(grad_v_1%name, field_ctl)
            call add_phys_name_ctl(grad_v_2%name, field_ctl)
            call add_phys_name_ctl(grad_v_3%name, field_ctl)
!          if(      field_ctl%c1_tbl(i) .eq. fhd_vort) then
!            call add_phys_name_ctl(grad_w_1%name, field_ctl)
!            call add_phys_name_ctl(grad_w_2%name, field_ctl)
!            call add_phys_name_ctl(grad_w_3%name, field_ctl)
!          else if( field_ctl%c1_tbl(i) .eq. fhd_vecp) then
!            call add_phys_name_ctl(grad_a_1%name, field_ctl)
!            call add_phys_name_ctl(grad_a_2%name, field_ctl)
!            call add_phys_name_ctl(grad_a_3%name, field_ctl)
          else if(field_ctl%c1_tbl(i) .eq. fhd_magne) then
            call add_phys_name_ctl(grad_b_1%name, field_ctl)
            call add_phys_name_ctl(grad_b_2%name, field_ctl)
            call add_phys_name_ctl(grad_b_3%name, field_ctl)
!          else if( field_ctl%c1_tbl(i) .eq. fhd_current) then
!            call add_phys_name_ctl(grad_j_1%name, field_ctl)
!            call add_phys_name_ctl(grad_j_2%name, field_ctl)
!            call add_phys_name_ctl(grad_j_3%name, field_ctl)
          else if(field_ctl%c1_tbl(i) .eq. fhd_temp) then
            call add_phys_name_ctl(grad_temp%name, field_ctl)
          else if(field_ctl%c1_tbl(i) .eq. fhd_ref_temp) then
            call add_phys_name_ctl(grad_pert_temp%name, field_ctl)
          else if(field_ctl%c1_tbl(i) .eq. fhd_light) then
            call add_phys_name_ctl(grad_composition%name, field_ctl)
          else if(field_ctl%c1_tbl(i) .eq. fhd_ref_light) then
            call add_phys_name_ctl(grad_pert_composition%name,          &
     &                             field_ctl)
          end if
        end do
      end if
!
!   field labels for filtered field
!
      if(       SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF         &
     &     .or. SGS_param%iflag_SGS.eq.id_SGS_similarity) then
        do i = 1, field_ctl%num
          if(      field_ctl%c1_tbl(i) .eq. fhd_velo) then
            call add_phys_name_ctl(filter_velocity%name, field_ctl)
          else if( field_ctl%c1_tbl(i) .eq. fhd_vecp) then
            call add_phys_name_ctl                                      &
     &         (filter_vector_potential%name, field_ctl)
          else if( field_ctl%c1_tbl(i) .eq. fhd_magne) then
            call add_phys_name_ctl(filter_magne%name, field_ctl)
          else if( field_ctl%c1_tbl(i) .eq. fhd_temp) then
            call add_phys_name_ctl(fhd_filter_temp, field_ctl)
          else if( field_ctl%c1_tbl(i) .eq. fhd_light) then
            call add_phys_name_ctl(fhd_filter_comp, field_ctl)
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
            call add_phys_name_ctl                                      &
     &         (wide_filter_velocity%name, field_ctl)
          else if( field_ctl%c1_tbl(i)                                  &
     &                         .eq. filter_vector_potential%name) then
            call add_phys_name_ctl                                      &
     &         (wide_filter_vector_potential%name, field_ctl)
          else if( field_ctl%c1_tbl(i) .eq. filter_magne%name) then
            call add_phys_name_ctl(wide_filter_magne%name, field_ctl)
          else if( field_ctl%c1_tbl(i) .eq. fhd_filter_temp) then
            call add_phys_name_ctl(wide_filter_temp%name, field_ctl)
          end if
        end do
      end if
!
!   field labels for turbulence diffusivity
!
      if (SGS_param%iflag_SGS .eq. id_SGS_diffusion) then
        call add_phys_name_ctl(SGS_diffuse%name, field_ctl)
      end if
!
      end subroutine add_work_area_4_sgs_model
!
! -----------------------------------------------------------------------
!
      subroutine add_work_area_4_dynamic_sgs(SGS_param, field_ctl)
!
      use t_SGS_model_coef_labels
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!   work area for dynamic SGS model
!
      if(      SGS_param%iflag_dynamic.eq.id_SGS_DYNAMIC_OFF            &
     &   .and. SGS_param%iflag_SGS.eq.id_SGS_NL_grad) then
        call add_phys_name_ctl(SGS_grad%name, field_ctl)
      else if (SGS_param%iflag_dynamic.eq.id_SGS_DYNAMIC_OFF            &
     &     .and. SGS_param%iflag_SGS.eq.id_SGS_similarity) then
        call add_phys_name_ctl(SGS_simi%name, field_ctl)
      else if (SGS_param%iflag_dynamic .ne. id_SGS_DYNAMIC_OFF) then
        call add_phys_name_ctl(SGS_grad%name, field_ctl)
        call add_phys_name_ctl(SGS_simi%name, field_ctl)
        call add_phys_name_ctl(SGS_grad_f%name, field_ctl)
!
        if (SGS_param%iflag_SGS_h_flux .ne. id_SGS_none) then
          call add_phys_name_ctl(temp_4_SGS%name, field_ctl)
        end if
        if (SGS_param%iflag_SGS_c_flux .ne. id_SGS_none) then
          call add_phys_name_ctl(comp_4_SGS%name, field_ctl)
        end if
      end if
!
      if(     (SGS_param%iflag_SGS_m_flux .eq. id_SGS_diffusion)        &
     &   .or. (SGS_param%iflag_SGS_lorentz .eq. id_SGS_diffusion)) then
        call add_phys_name_ctl(SGS_diffuse%name, field_ctl)
      end if
!
      end subroutine add_work_area_4_dynamic_sgs
!
! -----------------------------------------------------------------------
!
      end module add_nodal_fields_4_SGS
