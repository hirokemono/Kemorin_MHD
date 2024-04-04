!>@file   cal_sph_dynamic_SGS.f90
!!@brief  module cal_sph_dynamic_SGS
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evaluate nonlinear terms by pseudo spectram scheme
!!
!!@verbatim
!!      subroutine SGS_by_pseudo_sph                                    &
!!     &         (i_step, SGS_par, sph, comms_sph, r_2nd, MHD_prop,     &
!!     &          sph_MHD_bc, trans_p, WK, WK_LES, dynamic_SPH,         &
!!     &          ipol, ipol_LES, rj_fld, SR_sig, SR_r)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(phys_address), intent(in) :: ipol
!!        type(SGS_model_addresses), intent(in) :: ipol_LES
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!        type(works_4_sph_trans_SGS_MHD), intent(inout) :: WK_LES
!!        type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!@endverbatim
!
!
      module cal_sph_dynamic_SGS
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
!
      use calypso_mpi
!
      use t_physical_property
      use t_SGS_control_parameter
      use t_SPH_MHD_model_data
      use t_SPH_mesh_field_data
      use t_spheric_parameter
      use t_fdm_coefs
      use t_sph_trans_arrays_MHD
      use t_sph_trans_arrays_SGS_MHD
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_sph_filtering_data
      use t_coriolis_terms_rlm
      use t_gaunt_coriolis_rlm
      use t_sph_filtering
      use t_phys_address
      use t_SGS_model_addresses
      use t_solver_SR
!
      implicit none
!
      private :: sph_dynamic_similarity
      private :: sph_dynamic_nl_gradient
!
!*   ------------------------------------------------------------------
!*
      contains
!*
!*   ------------------------------------------------------------------
!
      subroutine SGS_by_pseudo_sph                                      &
     &         (i_step, SGS_par, sph, comms_sph, r_2nd, MHD_prop,       &
     &          sph_MHD_bc, trans_p, WK, WK_LES, dynamic_SPH,           &
     &          ipol, ipol_LES, rj_fld, SR_sig, SR_r)
!
      use sph_transforms_4_SGS
      use cal_sph_rotation_of_SGS
      use product_model_coefs_sph
      use cal_dynamic_SGS_buoyancy
      use scale_similarity_sph_SGS
      use nonlinear_gradient_sph_SGS
      use dynamic_model_sph_MHD
      use copy_Csim_4_sph_MHD
      use t_IO_step_parameter
!
      integer(kind = kint), intent(in) :: i_step
      type(SGS_paremeters), intent(in) :: SGS_par
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
!
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(works_4_sph_trans_SGS_MHD), intent(inout) :: WK_LES
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
      type(phys_data), intent(inout) :: rj_fld
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer(kind = kint) :: istep_dynamic
!
!
      if(SGS_par%model_p%iflag_SGS .eq. id_SGS_none) return
      istep_dynamic = mod(i_step, SGS_par%i_step_sgs_coefs)
!
      if(SGS_par%model_p%iflag_SGS .eq. id_SGS_similarity) then
        if(iflag_SGS_time) call start_elapsed_time(ist_elapsed_SGS+2)
        call cal_scale_similarity_sph_SGS                               &
     &     (sph, comms_sph, MHD_prop, trans_p, WK%WK_leg, dynamic_SPH,  &
     &      ipol, ipol_LES, rj_fld, WK_LES%trns_SGS, SR_sig, SR_r)
        if(iflag_SGS_time) call end_elapsed_time(ist_elapsed_SGS+2)
!
        if(SGS_par%model_p%iflag_dynamic .eq. id_SGS_DYNAMIC_ON         &
     &      .and. istep_dynamic .eq. 0) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &                     'Dynamic similarity model:',                 &
     &                      i_step, SGS_par%i_step_sgs_coefs
          if(iflag_SGS_time) call start_elapsed_time(ist_elapsed_SGS+3)
          call sph_dynamic_similarity                                   &
     &       (SGS_par%model_p, sph, comms_sph, MHD_prop, trans_p,       &
     &        WK_LES%trns_SGS, WK_LES%trns_DYNS, WK%WK_leg,             &
     &        dynamic_SPH, ipol, ipol_LES, rj_fld, SR_sig, SR_r)
          if(iflag_SGS_time) call end_elapsed_time(ist_elapsed_SGS+3)
        end if
      else if(SGS_par%model_p%iflag_SGS .eq. id_SGS_NL_grad) then
        if (iflag_debug.eq.1) write(*,*)                                &
     &                'cal_nonlinear_gradient_sph_SGS'
        call cal_nonlinear_gradient_sph_SGS                             &
     &     (sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc, trans_p,       &
     &      dynamic_SPH, ipol, WK%trns_MHD, WK%WK_leg, rj_fld,          &
     &      WK_LES%trns_ngTMP, WK_LES%trns_SGS, SR_sig, SR_r)
!
        if(SGS_par%model_p%iflag_dynamic .eq. id_SGS_DYNAMIC_ON         &
     &     .and. istep_dynamic .eq. 0) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &                     'Dynamic nonlinear gradient model:',         &
     &                      i_step, SGS_par%i_step_sgs_coefs
          if(iflag_SGS_time) call start_elapsed_time(ist_elapsed_SGS+3)
          call sph_dynamic_nl_gradient(SGS_par%model_p,                 &
     &        sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc, trans_p,     &
     &        WK_LES%trns_SGS, WK_LES%trns_SIMI, WK_LES%trns_DYNG,      &
     &        WK_LES%trns_Csim, WK%WK_leg, dynamic_SPH, ipol, ipol_LES, &
     &        rj_fld, SR_sig, SR_r)
          if(iflag_SGS_time) call end_elapsed_time(ist_elapsed_SGS+3)
        end if
      end if
!
      if(SGS_par%model_p%iflag_dynamic .eq. id_SGS_DYNAMIC_ON) then
        if (iflag_debug.eq.1) write(*,*) 'product_model_coefs_4_sph'
        call product_model_coefs_4_sph(SGS_par%model_p, sph%sph_rtp,    &
     &      dynamic_SPH%sph_d_grp, dynamic_SPH%iak_sgs_term,            &
     &      WK_LES%trns_SGS%f_trns_LES%SGS_term,                        &
     &      WK_LES%trns_SGS%forward, dynamic_SPH%wk_sph_sgs)
      else
        call prod_fixed_sph_SGS_Csim                                    &
     &     (SGS_par%model_p, sph%sph_rtp, dynamic_SPH%iak_sgs_term,     &
     &      WK_LES%trns_SGS%f_trns_LES%SGS_term,                        &
     &      WK_LES%trns_SGS%forward)
      end if
!
!
      if(SGS_par%model_p%iflag_dynamic .eq. id_SGS_DYNAMIC_ON           &
     &  .and. istep_dynamic .eq. 0) then
        if(SGS_par%model_p%iflag_SGS_gravity .ne. id_SGS_none) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &                          'dynamic_buo_SGS_by_pseudo_sph'
          call const_dynamic_SGS_4_buo_sph(SGS_par%model_p%stab_weight, &
     &        sph%sph_rtp, MHD_prop%fl_prop, WK%trns_MHD,               &
     &        WK_LES%trns_SGS, WK_LES%trns_Csim, dynamic_SPH)
        end if
!
        if(iflag_debug.ge.1) write(*,*) 'copy_model_coefs_4_sph_snap'
        call copy_model_coefs_4_sph_snap                                &
     &     (sph%sph_rtp, dynamic_SPH%sph_d_grp,                         &
     &      dynamic_SPH%iak_sgs_term, WK_LES%trns_Csim%f_trns_LES%Csim, &
     &      dynamic_SPH%wk_sph_sgs, WK_LES%trns_Csim%forward)
!
        if(iflag_SMHD_time)                                             &
     &         call start_elapsed_time(ist_elapsed_SMHD+11)
        if (iflag_debug.eq.1) write(*,*)                                &
     &                     'sph_forward_trans_SGS_MHD Csim'
        call sph_forward_trans_SGS_MHD                                  &
     &     (sph, comms_sph, trans_p, WK_LES%trns_Csim%forward,          &
     &      WK%WK_leg, WK_LES%trns_Csim%WK_FFTs_SGS,                    &
     &      rj_fld, SR_sig, SR_r)
        if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+11)
      end if
!
!
!
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+12)
      if(SGS_par%model_p%iflag_SGS_gravity .ne. id_SGS_none) then
        if(iflag_debug.ge.1) write(*,*) 'product_buo_model_coefs_4_sph'
        if(iflag_SGS_time) call start_elapsed_time(ist_elapsed_SGS+4)
        call product_buo_model_coefs_4_sph                              &
     &     (istep_dynamic, SGS_par%model_p, sph, ipol_LES,              &
     &      WK_LES%trns_SGS, dynamic_SPH, rj_fld)
        if(iflag_SGS_time) call end_elapsed_time(ist_elapsed_SGS+4)
      end if
!
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+11)
      if (iflag_debug.eq.1) write(*,*)                                  &
     &            'sph_forward_trans_SGS_MHD SGS'
      call sph_forward_trans_SGS_MHD(sph, comms_sph, trans_p,           &
     &    WK_LES%trns_SGS%forward, WK%WK_leg,                           &
     &    WK_LES%trns_SGS%WK_FFTs_SGS, rj_fld, SR_sig, SR_r)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+11)
!
      if (iflag_debug.ge.1) write(*,*) 'rot_SGS_terms_exp_sph'
      call rot_SGS_terms_exp_sph(sph%sph_rj, r_2nd, sph_MHD_bc,         &
     &    trans_p%leg, ipol, ipol_LES, rj_fld)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+12)
!
      end subroutine SGS_by_pseudo_sph
!
!*   ------------------------------------------------------------------
!
      subroutine sph_dynamic_similarity(SGS_param, sph, comms_sph,      &
     &          MHD_prop, trans_p, trns_SGS, trns_DYNS,                 &
     &          WK_leg, dynamic_SPH, ipol, ipol_LES,                    &
     &          rj_fld, SR_sig, SR_r)
!
      use sph_transforms_4_SGS
      use cal_filtered_sph_fields
      use cal_SGS_terms_sph_MHD
      use dynamic_model_sph_MHD
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
!
      type(SGS_address_sph_trans), intent(inout) :: trns_SGS
      type(SGS_address_sph_trans), intent(inout) :: trns_DYNS
      type(legendre_trns_works), intent(inout) :: WK_leg
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
      type(phys_data), intent(inout) :: rj_fld
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
!
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+11)
      if (iflag_debug.eq.1) write(*,*) 'sph_forward_trans_SGS_MHD SGS'
      call sph_forward_trans_SGS_MHD(sph, comms_sph, trans_p,           &
     &    trns_SGS%forward, WK_leg, trns_DYNS%WK_FFTs_SGS,              &
     &    rj_fld, SR_sig, SR_r)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+11)
!
      call cal_sph_wide_filtering_fields                                &
     &   (sph%sph_rj, ipol%base, ipol_LES%wide_filter_fld,              &
     &    dynamic_SPH%sph_filters(2), rj_fld)
      call cal_sph_wide_filtering_forces                                &
     &   (sph%sph_rj, ipol%forces, ipol_LES%wide_SGS,                   &
     &    dynamic_SPH%sph_filters(2), rj_fld)
      call cal_sph_dble_filtering_forces                                &
     &   (sph%sph_rj, ipol_LES%SGS_term, ipol_LES%dble_SGS,             &
     &    dynamic_SPH%sph_filters(2), rj_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'sph_back_trans_SGS_MHD dyns'
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+9)
      call sph_back_trans_SGS_MHD(sph, comms_sph, trans_p,              &
     &    rj_fld, trns_DYNS%backward, WK_leg, trns_DYNS%WK_FFTs_SGS,    &
     &    SR_sig, SR_r)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+9)
!
      if (iflag_debug.eq.1) write(*,*) 'wider_similarity_SGS_rtp'
      call wider_similarity_SGS_rtp(sph%sph_rtp, MHD_prop,              &
     &    trns_DYNS%b_trns_LES%wide_filter_fld,                         &
     &    trns_DYNS%b_trns_LES%wide_SGS, trns_DYNS%backward)
!
      if (iflag_debug.eq.1) write(*,*) 'SGS_param%stab_weight'
      call const_model_coefs_4_sph(SGS_param, sph%sph_rtp,              &
     &    dynamic_SPH%sph_d_grp, trns_SGS%f_trns_LES%SGS_term,          &
     &    trns_DYNS%b_trns_LES%wide_SGS, trns_DYNS%b_trns_LES%dble_SGS, &
     &    trns_SGS%forward, trns_DYNS%backward, trns_DYNS%backward,     &
     &    dynamic_SPH%iak_sgs_term, dynamic_SPH%wk_sph_sgs)
!
      end subroutine sph_dynamic_similarity
!
!*   ------------------------------------------------------------------
!
      subroutine sph_dynamic_nl_gradient                                &
     &         (SGS_param, sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc, &
     &          trans_p, trns_SGS, trns_SIMI, trns_DYNG, trns_Csim,     &
     &          WK_leg, dynamic_SPH, ipol, ipol_LES,                    &
     &          rj_fld, SR_sig, SR_r)
!
      use scale_similarity_sph_SGS
      use sph_transforms_4_SGS
      use cal_filtered_sph_fields
      use nonlinear_gradient_sph_SGS
      use dynamic_model_sph_MHD
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
!
      type(SGS_address_sph_trans), intent(inout) :: trns_SGS
      type(SGS_address_sph_trans), intent(inout) :: trns_Csim
      type(SGS_address_sph_trans), intent(inout) :: trns_SIMI
      type(SGS_address_sph_trans), intent(inout) :: trns_DYNG
      type(legendre_trns_works), intent(inout) :: WK_leg
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
      type(phys_data), intent(inout) :: rj_fld
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
!       Make scale similarity model to grid data
      if (iflag_debug .eq. 1) write(*,*)                                &
     &        'cal_scale_similarity_sph_SGS for dynamic model'
      if(iflag_SGS_time) call start_elapsed_time(ist_elapsed_SGS+2)
      call cal_scale_similarity_sph_SGS                                 &
     &   (sph, comms_sph, MHD_prop, trans_p, WK_leg, dynamic_SPH,       &
     &    ipol, ipol_LES, rj_fld, trns_SIMI, SR_sig, SR_r)
      if(iflag_SGS_time) call end_elapsed_time(ist_elapsed_SGS+2)
!
!       Bring SGS terms by nonlinear model to spectr data
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+11)
      if (iflag_debug.eq.1) write(*,*)                                  &
     &            'sph_forward_trans_SGS_MHD SGS for dynamic nl. grad'
      call sph_forward_trans_SGS_MHD(sph, comms_sph, trans_p,           &
     &    trns_SGS%forward, WK_leg, trns_SGS%WK_FFTs_SGS,               &
     &    rj_fld, SR_sig, SR_r)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+11)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sph_dble_filtering_forces'
      call cal_sph_dble_filtering_forces                                &
     &   (sph%sph_rj, ipol_LES%SGS_term, ipol_LES%dble_SGS,             &
     &    dynamic_SPH%sph_filters(2), rj_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'cal_sph_dble_filtering_forces'
      call cal_wide_nonlinear_grad_sph_SGS                              &
     &   (sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc, trans_p,         &
     &    dynamic_SPH, ipol_LES, trns_SIMI, WK_leg,                     &
     &    rj_fld, trns_DYNG, trns_Csim, SR_sig, SR_r)
!
      if (iflag_debug.eq.1) write(*,*) 'SGS_param%stab_weight'
      call const_model_coefs_4_sph(SGS_param, sph%sph_rtp,              &
     &    dynamic_SPH%sph_d_grp, trns_SIMI%f_trns_LES%SGS_term,         &
     &    trns_Csim%b_trns_LES%wide_SGS, trns_DYNG%b_trns_LES%dble_SGS, &
     &    trns_SIMI%forward, trns_Csim%backward, trns_DYNG%backward,    &
     &    dynamic_SPH%iak_sgs_term, dynamic_SPH%wk_sph_sgs)
!
      end subroutine sph_dynamic_nl_gradient
!
!*   ------------------------------------------------------------------
!
      end module cal_sph_dynamic_SGS
