!
!      module add_nodal_fields_4_SGS
!
!        programmed by H.Matsui on Sep., 2006
!
!!      subroutine add_work_area_4_sgs_model(fl_prop, field_ctl)
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!
      module add_nodal_fields_4_SGS
!
      use m_precision
!
      use m_control_parameter
      use m_phys_labels
      use t_physical_property
      use add_nodal_fields_ctl
!
      implicit  none
!
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine add_work_area_4_sgs_model(fl_prop, field_ctl)
!
      use t_read_control_arrays
!
      type(fluid_property), intent(in) :: fl_prop
      type(ctl_array_c3), intent(inout) :: field_ctl
!
      integer(kind = kint) :: i
!
!   work area for SGS model
!
      if (iflag_SGS_heat .ne. id_SGS_none) then
        call add_phys_name_ctl(fhd_SGS_temp, field_ctl)
      end if
      if (iflag_SGS_comp_flux .ne. id_SGS_none) then
        call add_phys_name_ctl(fhd_SGS_comp, field_ctl)
      end if
!
      if (iflag_SGS_gravity .ne. id_SGS_none) then
        call add_phys_name_ctl(fhd_SGS_m_flux, field_ctl)
        call add_phys_name_ctl(fhd_div_SGS_m_flux, field_ctl)
        call add_phys_name_ctl(fhd_Reynolds_work, field_ctl)
!
        if(fl_prop%iflag_4_gravity .gt. id_turn_OFF) then
          call add_phys_name_ctl(fhd_SGS_h_flux, field_ctl)
          call add_phys_name_ctl(fhd_SGS_buo_flux, field_ctl)
        end if
        if(iflag_4_composit_buo .gt. id_turn_OFF) then
          call add_phys_name_ctl(fhd_SGS_c_flux, field_ctl)
          call add_phys_name_ctl(fhd_SGS_comp_buo_flux, field_ctl)
        end if
      end if
!
      if (iflag_dynamic_SGS.eq.id_SGS_DYNAMIC_OFF                       &
     &      .and. iflag_SGS_model.eq.id_SGS_NL_grad) then
        call add_phys_name_ctl(fhd_SGS_grad, field_ctl)
      else if (iflag_dynamic_SGS.eq.id_SGS_DYNAMIC_OFF                  &
     &     .and. iflag_SGS_model.eq.id_SGS_similarity) then
        call add_phys_name_ctl(fhd_SGS_simi, field_ctl)
      else if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
        call add_phys_name_ctl(fhd_SGS_grad, field_ctl)
        call add_phys_name_ctl(fhd_SGS_simi, field_ctl)
        call add_phys_name_ctl(fhd_SGS_grad_f, field_ctl)
      end if
!
!
!   field labels for nonlinear gradient model
!
      if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF                     &
     &      .or. iflag_SGS_model.eq.id_SGS_similarity) then
        do i = 1, field_ctl%num
          if(      field_ctl%c1_tbl(i) .eq. fhd_velo) then
            call add_phys_name_ctl(fhd_grad_v_1, field_ctl)
            call add_phys_name_ctl(fhd_grad_v_2, field_ctl)
            call add_phys_name_ctl(fhd_grad_v_3, field_ctl)
!          if(      field_ctl%c1_tbl(i) .eq. fhd_vort) then
!            call add_phys_name_ctl(fhd_grad_w_1, field_ctl)
!            call add_phys_name_ctl(fhd_grad_w_2, field_ctl)
!            call add_phys_name_ctl(fhd_grad_w_3, field_ctl)
!          else if( field_ctl%c1_tbl(i) .eq. fhd_vecp) then
!            call add_phys_name_ctl(fhd_grad_a_1, field_ctl)
!            call add_phys_name_ctl(fhd_grad_a_2, field_ctl)
!            call add_phys_name_ctl(fhd_grad_a_3, field_ctl)
          else if(field_ctl%c1_tbl(i) .eq. fhd_magne) then
            call add_phys_name_ctl(fhd_grad_b_1, field_ctl)
            call add_phys_name_ctl(fhd_grad_b_2, field_ctl)
            call add_phys_name_ctl(fhd_grad_b_3, field_ctl)
!          else if( field_ctl%c1_tbl(i) .eq. fhd_current) then
!            call add_phys_name_ctl(fhd_grad_j_1, field_ctl)
!            call add_phys_name_ctl(fhd_grad_j_2, field_ctl)
!            call add_phys_name_ctl(fhd_grad_j_3, field_ctl)
          else if(field_ctl%c1_tbl(i) .eq. fhd_temp) then
            call add_phys_name_ctl(fhd_grad_temp, field_ctl)
          else if(field_ctl%c1_tbl(i) .eq. fhd_ref_temp) then
            call add_phys_name_ctl(fhd_grad_par_temp, field_ctl)
          else if(field_ctl%c1_tbl(i) .eq. fhd_light) then
            call add_phys_name_ctl(fhd_grad_composit, field_ctl)
          else if(field_ctl%c1_tbl(i) .eq. fhd_ref_light) then
            call add_phys_name_ctl(fhd_grad_par_light, field_ctl)
          end if
        end do
      end if
!
!   field labels for filtered field
!
      if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF                     &
     &     .or. iflag_SGS_model.eq.id_SGS_similarity) then
        do i = 1, field_ctl%num
          if(      field_ctl%c1_tbl(i) .eq. fhd_velo) then
            call add_phys_name_ctl(fhd_filter_velo, field_ctl)
          else if( field_ctl%c1_tbl(i) .eq. fhd_vecp) then
            call add_phys_name_ctl(fhd_filter_vecp, field_ctl)
          else if( field_ctl%c1_tbl(i) .eq. fhd_magne) then
            call add_phys_name_ctl(fhd_filter_magne, field_ctl)
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
      if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF                     &
     &       .and. iflag_SGS_model.eq.id_SGS_similarity) then
        do i = 1, field_ctl%num
          if(      field_ctl%c1_tbl(i) .eq. fhd_filter_velo) then
            call add_phys_name_ctl(fhd_w_filter_velo, field_ctl)
          else if( field_ctl%c1_tbl(i) .eq. fhd_filter_vecp) then
            call add_phys_name_ctl(fhd_w_filter_vecp, field_ctl)
          else if( field_ctl%c1_tbl(i) .eq. fhd_filter_magne) then
            call add_phys_name_ctl(fhd_w_filter_magne, field_ctl)
          else if( field_ctl%c1_tbl(i) .eq. fhd_filter_temp) then
            call add_phys_name_ctl(fhd_w_filter_temp, field_ctl)
          end if
        end do
      end if
!
!   field labels for turbulence diffusivity
!
      if (iflag_SGS_model .eq. id_SGS_diffusion) then
        call add_phys_name_ctl(fhd_SGS_diffuse, field_ctl)
      end if
!
      end subroutine add_work_area_4_sgs_model
!
! -----------------------------------------------------------------------
!
      end module add_nodal_fields_4_SGS
