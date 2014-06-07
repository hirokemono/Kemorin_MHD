!
!      module add_nodal_fields_4_SGS
!
!        programmed by H.Matsui on Sep., 2006
!
!      subroutine add_work_area_4_sgs_model
!
      module add_nodal_fields_4_SGS
!
      use m_precision
!
      use m_control_parameter
      use m_phys_labels
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
      subroutine add_work_area_4_sgs_model
!
      use m_ctl_data_4_fields
!
      integer(kind = kint) :: i
!
!   work area for SGS model
!
      if (iflag_SGS_heat .ne. id_SGS_none) then
        call add_phys_name_tmp(fhd_SGS_temp)
      end if
      if (iflag_SGS_comp_flux .ne. id_SGS_none) then
        call add_phys_name_tmp(fhd_SGS_comp)
      end if
!
      if (iflag_SGS_gravity .ne. id_SGS_none) then
        call add_phys_name_tmp(fhd_SGS_m_flux)
        call add_phys_name_tmp(fhd_div_SGS_m_flux)
        call add_phys_name_tmp(fhd_Reynolds_work)
!
        if(iflag_4_gravity .gt. id_turn_OFF) then
          call add_phys_name_tmp(fhd_SGS_h_flux)
          call add_phys_name_tmp(fhd_SGS_buo_flux)
        end if
        if(iflag_4_composit_buo .gt. id_turn_OFF) then
          call add_phys_name_tmp(fhd_SGS_c_flux)
          call add_phys_name_tmp(fhd_SGS_comp_buo_flux)
        end if
      end if
!
      if (iflag_dynamic_SGS.eq.id_SGS_DYNAMIC_OFF                       &
     &      .and. iflag_SGS_model.eq.id_SGS_NL_grad) then
        call add_phys_name_tmp(fhd_SGS_grad)
      else if (iflag_dynamic_SGS.eq.id_SGS_DYNAMIC_OFF                  &
     &     .and. iflag_SGS_model.eq.id_SGS_similarity) then
        call add_phys_name_tmp(fhd_SGS_simi)
      else if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF) then
        call add_phys_name_tmp(fhd_SGS_grad)
        call add_phys_name_tmp(fhd_SGS_simi)
        call add_phys_name_tmp(fhd_SGS_grad_f)
      end if
!
!
!   field labels for nonlinear gradient model
!
      if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF                     &
     &      .or. iflag_SGS_model.eq.id_SGS_similarity) then
        do i = 1, field_ctl%num
          if(      field_ctl%c1_tbl(i) .eq. fhd_velo) then
            call add_phys_name_tmp(fhd_grad_v_1)
            call add_phys_name_tmp(fhd_grad_v_2)
            call add_phys_name_tmp(fhd_grad_v_3)
!          if(      field_ctl%c1_tbl(i) .eq. fhd_vort) then
!            call add_phys_name_tmp(fhd_grad_w_1)
!            call add_phys_name_tmp(fhd_grad_w_2)
!            call add_phys_name_tmp(fhd_grad_w_3)
!          else if( field_ctl%c1_tbl(i) .eq. fhd_vecp) then
!            call add_phys_name_tmp(fhd_grad_a_1)
!            call add_phys_name_tmp(fhd_grad_a_2)
!            call add_phys_name_tmp(fhd_grad_a_3)
          else if(field_ctl%c1_tbl(i) .eq. fhd_magne) then
            call add_phys_name_tmp(fhd_grad_b_1)
            call add_phys_name_tmp(fhd_grad_b_2)
            call add_phys_name_tmp(fhd_grad_b_3)
!          else if( field_ctl%c1_tbl(i) .eq. fhd_current) then
!            call add_phys_name_tmp(fhd_grad_j_1)
!            call add_phys_name_tmp(fhd_grad_j_2)
!            call add_phys_name_tmp(fhd_grad_j_3)
          else if(field_ctl%c1_tbl(i) .eq. fhd_temp) then
            call add_phys_name_tmp(fhd_grad_temp)
          else if(field_ctl%c1_tbl(i) .eq. fhd_ref_temp) then
            call add_phys_name_tmp(fhd_grad_par_temp)
          else if(field_ctl%c1_tbl(i) .eq. fhd_light) then
            call add_phys_name_tmp(fhd_grad_composit)
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
            call add_phys_name_tmp(fhd_filter_v)
          else if( field_ctl%c1_tbl(i) .eq. fhd_vecp) then
            call add_phys_name_tmp(fhd_filter_a)
          else if( field_ctl%c1_tbl(i) .eq. fhd_magne) then
            call add_phys_name_tmp(fhd_filter_b)
          else if( field_ctl%c1_tbl(i) .eq. fhd_temp) then
            call add_phys_name_tmp(fhd_filter_temp)
          else if( field_ctl%c1_tbl(i) .eq. fhd_light) then
            call add_phys_name_tmp(fhd_filter_comp)
          end if
        end do
      end if
!
!   field labels for wider filtered field
!
      if (iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF                     &
     &       .and. iflag_SGS_model.eq.id_SGS_similarity) then
        do i = 1, field_ctl%num
          if(      field_ctl%c1_tbl(i) .eq. fhd_filter_v) then
            call add_phys_name_tmp(fhd_w_filter_velo)
          else if( field_ctl%c1_tbl(i) .eq. fhd_filter_a) then
            call add_phys_name_tmp(fhd_w_filter_vecp)
          else if( field_ctl%c1_tbl(i) .eq. fhd_filter_b) then
            call add_phys_name_tmp(fhd_w_filter_magne)
          else if( field_ctl%c1_tbl(i) .eq. fhd_filter_temp) then
            call add_phys_name_tmp(fhd_w_filter_temp)
          end if
        end do
      end if
!
!   field labels for turbulence diffusivity
!
      if (iflag_SGS_model .eq. id_SGS_diffusion) then
        call add_phys_name_tmp(fhd_SGS_diffuse)
      end if
!
      end subroutine add_work_area_4_sgs_model
!
! -----------------------------------------------------------------------
!
      end module add_nodal_fields_4_SGS
