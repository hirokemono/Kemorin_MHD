!
!      module cal_model_coefficients
!
!      Written by H. Matsui
!
!      subroutine s_cal_model_coefficients
!
      module cal_model_coefficients
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use m_control_parameter
!
      implicit none
!
      private :: cal_model_coef_sgs_mom_flux
      private :: cal_model_coef_sgs_maxwell
      private :: cal_model_coef_sgs_heat_flux
      private :: cal_model_coef_sgs_mag_induct
      private :: cal_model_coef_sgs_uxb
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_model_coefficients
!
      use m_t_step_parameter
      use cal_sgs_h_flux_dynamic_simi
!
      integer (kind = kint) :: iflag
!
!
      iflag = mod(i_step_MHD, i_step_sgs_coefs)
      if(iflag_dynamic_SGS .ne. id_SGS_DYNAMIC_OFF                      &
     &      .and. iflag.eq.0) then
!
        if(my_rank .eq. 0) write(*,*)                                   &
     &            'set Csim', i_step_MHD, i_step_sgs_coefs
        call cal_model_coef_sgs_mom_flux
        call cal_model_coef_sgs_heat_flux
        call cal_model_coef_sgs_maxwell
!
        if(iflag_t_evo_4_magne .gt. id_no_evolution) then
          call cal_model_coef_sgs_mag_induct
        else if(iflag_t_evo_4_vect_p .gt. id_no_evolution) then
          call cal_model_coef_sgs_uxb
        end if
      end if
!
      end subroutine s_cal_model_coefficients
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_model_coef_sgs_heat_flux
!
      use cal_sgs_heat_flux_dynamic
      use cal_sgs_h_flux_dynamic_simi
      use cal_diff_coef_sgs_hf
!
!
      if (iflag_SGS_heat .eq. id_SGS_NL_grad) then
        if (iflag_debug.eq.1)  write(*,*) 'cal_sgs_hf_dynamic'
        call cal_sgs_hf_dynamic
!
      else if (iflag_SGS_heat .eq. id_SGS_similarity) then
        if (iflag_debug.eq.1)                                           &
     &          write(*,*) 's_cal_sgs_h_flux_dynamic_simi'
        call s_cal_sgs_h_flux_dynamic_simi
      end if
!
      if ( iflag_commute_heat .eq. id_SGS_commute_ON) then
        if (iflag_debug.eq.1)  write(*,*) 's_cal_diff_coef_sgs_hf'
        call s_cal_diff_coef_sgs_hf
      end if
!
      end subroutine cal_model_coef_sgs_heat_flux
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_model_coef_sgs_mom_flux
!
      use m_node_phys_address
      use cal_sgs_mom_flux_dynamic
      use cal_sgs_m_flux_dynamic_simi
      use cal_diff_coef_sgs_mf
!
!
        if (iflag_SGS_inertia .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)  write(*,*) 'cal_sgs_m_flux_dynamic'
          call cal_sgs_m_flux_dynamic
!
        else if (iflag_SGS_inertia .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)                                         &
     &      write(*,*) 's_cal_sgs_m_flux_dynamic_simi'
          call s_cal_sgs_m_flux_dynamic_simi
        end if
!
        if (iflag_commute_inertia .eq. id_SGS_commute_ON) then
          if (iflag_debug.eq.1)  write(*,*) 's_cal_diff_coef_sgs_mf'
          call s_cal_diff_coef_sgs_mf
        end if
!
      end subroutine cal_model_coef_sgs_mom_flux
!
!-----------------------------------------------------------------------
!
      subroutine cal_model_coef_sgs_maxwell
!
      use m_node_phys_address
!
      use cal_sgs_maxwell_dynamic
      use cal_diff_coef_sgs_mxwl
      use cal_sgs_m_flux_dynamic_simi
!
!
        if (iflag_SGS_lorentz .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)                                         &
     &       write(*,*) 'cal_sgs_maxwell_t_dynamic'
          call cal_sgs_maxwell_t_dynamic
!
        else if (iflag_SGS_lorentz .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)                                         &
     &       write(*,*) 'cal_sgs_maxwell_dynamic_simi'
          call cal_sgs_maxwell_dynamic_simi
!
        end if
!
        if (iflag_commute_lorentz .eq. id_SGS_commute_ON) then
          if (iflag_debug.eq.1) write(*,*) 's_cal_diff_coef_sgs_mxwl'
          call s_cal_diff_coef_sgs_mxwl
        end if
!
      end subroutine cal_model_coef_sgs_maxwell
!
!-----------------------------------------------------------------------
!
      subroutine cal_model_coef_sgs_mag_induct
!
      use m_node_phys_address
!
      use cal_sgs_induction_dynamic
      use cal_diff_coef_sgs_induct
      use cal_sgs_uxb_dynamic_simi
!
!
        if     (iflag_SGS_induction .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)                                         &
     &      write(*,*) 'cal_sgs_induct_t_dynamic'
          call cal_sgs_induct_t_dynamic
!
        else if(iflag_SGS_induction .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)                                         &
     &      write(*,*) 'cal_sgs_induct_t_dynamic_simi'
          call cal_sgs_induct_t_dynamic_simi
!
        end if
!
        if (iflag_commute_induction .eq. id_SGS_commute_ON) then
          if(iflag_debug.eq.1)  write(*,*) 's_cal_diff_coef_sgs_induct'
          call s_cal_diff_coef_sgs_induct
        end if
!
      end subroutine cal_model_coef_sgs_mag_induct
!
!-----------------------------------------------------------------------
!
      subroutine cal_model_coef_sgs_uxb
!
      use cal_sgs_induction_dynamic
      use cal_sgs_uxb_dynamic_simi
!
!
        if     (iflag_SGS_induction .eq. id_SGS_NL_grad) then
          if (iflag_debug.eq.1)  write(*,*) 'cal_sgs_uxb_dynamic'
          call cal_sgs_uxb_dynamic
        else if(iflag_SGS_induction .eq. id_SGS_similarity) then
          if (iflag_debug.eq.1)  write(*,*) 's_cal_sgs_uxb_dynamic_simi'
        end if
!
      end subroutine cal_model_coef_sgs_uxb
!
!-----------------------------------------------------------------------
!
      end module cal_model_coefficients
