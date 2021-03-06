!>@file   cal_filtered_nonlinear.f90
!!@brief  module cal_filtered_nonlinear
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evaluate nonlinear terms by pseudo spectram scheme
!!
!!@verbatim
!!      subroutine filter_nonlinear_by_pseudo_sph(sph, comms_sph,       &
!!     &          r_2nd, MHD_prop, sph_MHD_bc, trans_p, WK_leg,         &
!!     &          dynamic_SPH, ipol, ipol_LES, rj_fld, trns_fil_MHD)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(dynamic_SGS_data_4_sph), intent(in) :: dynamic_SPH
!!        type(phys_address), intent(in) :: ipol
!!        type(SGS_model_addresses), intent(in) :: ipol_LES
!!        type(SGS_address_sph_trans), intent(inout) :: trns_fil_MHD
!!        type(legendre_trns_works), intent(inout) :: WK_leg
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
!
      module cal_filtered_nonlinear
!
      use m_precision
      use m_constants
!
      use t_control_parameter
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_physical_property
      use t_phys_data
      use t_phys_address
      use t_fdm_coefs
      use t_boundary_data_sph_MHD
      use t_SGS_model_addresses
      use t_legendre_trans_select
      use t_sph_FFT_selector
      use t_sph_trans_arrays_SGS_MHD
      use t_sph_filtering
      use t_work_4_sph_trans
!
      use m_work_time
      use m_elapsed_labels_4_MHD
!
!*   ------------------------------------------------------------------
!
      contains
!
!*   ------------------------------------------------------------------
!
      subroutine filter_nonlinear_by_pseudo_sph(sph, comms_sph,         &
     &          r_2nd, MHD_prop, sph_MHD_bc, trans_p, WK_leg,           &
     &          dynamic_SPH, ipol, ipol_LES, rj_fld, trns_fil_MHD)
!
      use sph_transforms_4_SGS
      use cal_nonlinear_sph_MHD
      use cal_filtered_sph_fields
      use self_buoyancy_w_filter_sph
      use cal_sph_rot_filtered_force
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(dynamic_SGS_data_4_sph), intent(in) :: dynamic_SPH
      type(phys_address), intent(in) :: ipol
      type(SGS_model_addresses), intent(in) :: ipol_LES
!
      type(SGS_address_sph_trans), intent(inout) :: trns_fil_MHD
      type(legendre_trns_works), intent(inout) :: WK_leg
      type(phys_data), intent(inout) :: rj_fld
!
      logical :: flag
!
!
      flag =   MHD_prop%fl_prop%iflag_4_filter_inertia                  &
     &    .or. MHD_prop%fl_prop%iflag_4_filter_lorentz                  &
     &    .or. MHD_prop%cd_prop%iflag_4_filter_induction                &
     &    .or. MHD_prop%ht_prop%iflag_4_filter_advection                &
     &    .or. MHD_prop%cp_prop%iflag_4_filter_advection
      if(flag .eqv. .FALSE.) return
!
!   ----  Lead filtered fields
      if(iflag_SGS_time) call start_elapsed_time(ist_elapsed_SGS+1)
      if (iflag_debug.ge.1) write(*,*)                                  &
     &                    'cal_sph_base_filtering_fields'
      call cal_sph_base_filtering_fields                                &
     &   (sph%sph_rj, ipol%base, ipol_LES%filter_fld,                   &
     &    dynamic_SPH%sph_filters(1), rj_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'sph_back_trans_SGS_MHD'
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+9)
      call sph_back_trans_SGS_MHD(sph, comms_sph, trans_p,              &
     &    rj_fld, trns_fil_MHD%backward, WK_leg,                        &
     &    trns_fil_MHD%WK_FFTs_SGS)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+9)
!
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+10)
      if (iflag_debug.ge.1) write(*,*) 'nonlinear_terms_in_rtp'
      call nonlinear_terms_in_rtp(sph%sph_rtp, MHD_prop, trans_p%leg,   &
     &    trns_fil_MHD%b_trns_LES%filter_fld,                           &
     &    trns_fil_MHD%f_trns_LES%force_by_filter,                      &
     &    trns_fil_MHD%backward, trns_fil_MHD%forward)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+10)
!
      if (iflag_debug.eq.1) write(*,*) 'sph_forward_trans_SGS_MHD'
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+11)
      call sph_forward_trans_SGS_MHD(sph, comms_sph, trans_p,           &
     &    trns_fil_MHD%forward, WK_leg, trns_fil_MHD%WK_FFTs_SGS,       &
     &    rj_fld)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+11)
!
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+12)
      if (iflag_debug.ge.1) write(*,*) 'rot_filtered_mom_eq_exp_sph'
      call rot_filtered_mom_eq_exp_sph                                  &
     &   (sph%sph_rj, r_2nd, sph_MHD_bc, trans_p%leg,                   &
     &    ipol_LES%force_by_filter, ipol_LES%rot_frc_by_filter, rj_fld)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+12)
!
      end subroutine filter_nonlinear_by_pseudo_sph
!
!*   ------------------------------------------------------------------
!
      end module cal_filtered_nonlinear
