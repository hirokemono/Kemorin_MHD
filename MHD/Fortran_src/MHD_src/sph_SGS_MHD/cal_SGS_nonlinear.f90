!>@file   cal_SGS_nonlinear.f90
!!@brief  module cal_SGS_nonlinear
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evaluate nonlinear terms by pseudo spectram scheme
!!
!!@verbatim
!!      subroutine nonlinear_SGS_first(i_step, r_2nd, SPH_model,        &
!!     &          trans_p, WK, SPH_SGS, SPH_MHD, SR_sig, SR_r)
!!        integer(kind = kint), intent(in) :: i_step
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(SPH_MHD_model_data), intent(in) :: SPH_model
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!        type(SPH_SGS_structure), intent(inout) :: SPH_SGS
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!      subroutine nonlinear_with_SGS                                   &
!!     &         (i_step, sph, comms_sph, r_2nd, SPH_model, trans_p,    &
!!     &          ipol, WK, SPH_SGS, rj_fld, SR_sig, SR_r)
!!        integer(kind = kint), intent(in) :: i_step
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(SPH_MHD_model_data), intent(in) :: SPH_model
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(phys_address), intent(in) :: ipol
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!        type(SPH_SGS_structure), intent(inout) :: SPH_SGS
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!@endverbatim
!
!
      module cal_SGS_nonlinear
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
      use t_SPH_SGS_structure
      use t_SPH_mesh_field_data
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_phys_address
      use t_phys_data
      use t_fdm_coefs
      use t_sph_trans_arrays_MHD
      use t_addresses_sph_transform
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_sph_filtering_data
      use t_coriolis_terms_rlm
      use t_gaunt_coriolis_rlm
      use t_sph_filtering
      use t_solver_SR
!
      implicit none
!
      private :: init_stab_weight, back_stab_weight
!
!*   ------------------------------------------------------------------
!*
      contains
!*
!*   ------------------------------------------------------------------
!*
      subroutine nonlinear_SGS_first(i_step, r_2nd, SPH_model,          &
     &          trans_p, WK, SPH_SGS, SPH_MHD, SR_sig, SR_r)
!
      integer(kind = kint), intent(in) :: i_step
      type(fdm_matrices), intent(in) :: r_2nd
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(SPH_MHD_model_data), intent(in) :: SPH_model
!
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(SPH_SGS_structure), intent(inout) :: SPH_SGS
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      real(kind = kreal), save :: tmp_stab_wt
!
!
      call init_stab_weight(tmp_stab_wt, SPH_SGS%SGS_par%model_p)
      call nonlinear_with_SGS(i_step, SPH_MHD%sph, SPH_MHD%comms,       &
     &    r_2nd, SPH_model, trans_p, SPH_MHD%ipol, WK, SPH_SGS,         &
     &    SPH_MHD%fld, SR_sig, SR_r)
      call back_stab_weight(tmp_stab_wt, SPH_SGS%SGS_par%model_p)
!
      end subroutine nonlinear_SGS_first
!*
!*   ------------------------------------------------------------------
!*
      subroutine nonlinear_with_SGS                                     &
     &         (i_step, sph, comms_sph, r_2nd, SPH_model, trans_p,      &
     &          ipol, WK, SPH_SGS, rj_fld, SR_sig, SR_r)
!
      use cal_inner_core_rotation
!
      use cal_nonlinear_sph_MHD
      use sum_rotation_of_SGS
      use sum_rotation_of_forces
      use cal_nonlinear
      use cal_filtered_nonlinear
      use cal_sph_dynamic_SGS
      use sum_rot_of_filter_forces
      use cal_self_buoyancies_sph
      use rot_self_buoyancies_sph
      use self_buoyancy_w_filter_sph
      use add_sph_ref_scalar_advect
!
      integer(kind = kint), intent(in) :: i_step
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(SPH_MHD_model_data), intent(in) :: SPH_model
      type(phys_address), intent(in) :: ipol
!
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(SPH_SGS_structure), intent(inout) :: SPH_SGS
      type(phys_data), intent(inout) :: rj_fld
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!   ----   lead rotation of buoyancies
      if(SPH_model%MHD_prop%fl_prop%iflag_scheme                        &
     &                         .gt. id_no_evolution) then
!
!   ----  lead rotation of buoyancies
        if(iflag_debug.gt.0) write(*,*) 'sel_rot_buoyancy_sph_MHD'
        call sel_rot_buoyancy_sph_MHD                                   &
     &     (sph%sph_rj, ipol%base, ipol%rot_forces,                     &
     &      SPH_model%MHD_prop%fl_prop, SPH_model%sph_MHD_bc%sph_bc_U,  &
     &      rj_fld)
!
!   ----   lead rotation of filtered buoyancies
        if(iflag_debug.gt.0) write(*,*) 'sel_rot_filter_buoyancy_sph'
        call sel_rot_filter_buoyancy_sph                                &
     &     (sph, SPH_SGS%ipol_LES, SPH_model%MHD_prop,                  &
     &      SPH_model%sph_MHD_bc%sph_bc_U, rj_fld)
!
!   ----  lead buoyancies
        call cal_self_buoyancy_sph_SGS_MHD(sph, trans_p%leg,            &
     &      ipol, SPH_SGS%ipol_LES, SPH_model%MHD_prop,                 &
     &      SPH_model%sph_MHD_bc%sph_bc_U, rj_fld)
      end if
!
!   ----  lead nonlinear terms by phesdo spectrum
      if (iflag_debug.eq.1) write(*,*) 'nonlinear_by_pseudo_sph'
      call nonlinear_by_pseudo_sph                                      &
     &   (sph, comms_sph, SPH_model%omega_sph, r_2nd,                   &
     &    SPH_model%MHD_prop, SPH_model%sph_MHD_bc, trans_p,            &
     &    WK%gt_cor, WK%trns_MHD, WK%WK_leg, WK%WK_FFTs_MHD,            &
     &    WK%cor_rlm, ipol, rj_fld, SR_sig, SR_r)
!
!   ----  lead nonlinear terms by filtered field
      if (iflag_debug.eq.1) write(*,*) 'nonlinear_by_pseudo_sph'
      call filter_nonlinear_by_pseudo_sph                               &
     &   (sph, comms_sph, r_2nd, SPH_model%MHD_prop,                    &
     &    SPH_model%sph_MHD_bc, trans_p, WK%WK_leg,                     &
     &    SPH_SGS%dynamic, ipol, SPH_SGS%ipol_LES, rj_fld,              &
     &    SPH_SGS%trns_WK_LES%trns_fil_MHD, SR_sig, SR_r)
!
!   ----  Lead SGS terms
      if (iflag_debug.eq.1) write(*,*) 'SGS_by_pseudo_sph'
      call SGS_by_pseudo_sph                                            &
     &   (i_step, SPH_SGS%SGS_par, sph, comms_sph,                      &
     &    r_2nd, SPH_model%MHD_prop, SPH_model%sph_MHD_bc, trans_p,     &
     &    WK, SPH_SGS%trns_WK_LES, SPH_SGS%dynamic, ipol,               &
     &    SPH_SGS%ipol_LES, rj_fld, SR_sig, SR_r)
!
!   ----  Lead advection of reference field
      call add_ref_advect_sph_MHD(sph%sph_rj, trans_p%leg,              &
     &    SPH_model%sph_MHD_bc, SPH_model%MHD_prop,                     &
     &    SPH_model%refs%iref_grad, SPH_model%refs%ref_field,           &
     &    ipol%base, ipol%forces, rj_fld)
!
!*  ----  copy coriolis term for inner core rotation
!*
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+8)
      call copy_icore_rot_to_tor_coriolis                               &
     &   (SPH_model%sph_MHD_bc%sph_bc_U, sph%sph_rj,                    &
     &    ipol%forces, ipol%rot_forces, rj_fld)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+8)
!
      if(SPH_model%MHD_prop%fl_prop%iflag_scheme .eq. id_no_evolution)  &
     &      return
!
!   ----  Sum all explicit forces
!        if(iflag_debug .gt. 0) write(*,*)                              &
!     &       'sum_forces_to_explicit for rotation of forces'
!        call sum_forces_to_explicit(SPH_model%MHD_prop%fl_prop,        &
!     &    ipol%exp_work, ipol%forces, rj_fld)
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &       'sum_forces_to_explicit for rotation of forces'
        call sum_forces_to_explicit(SPH_model%MHD_prop%fl_prop,         &
     &      ipol%exp_work, ipol%rot_forces, rj_fld)
!
!    ---- Add filtered rotation of forces
      if(iflag_debug .gt. 0) write(*,*) 'sum_filter_forces_to_explicit'
      call sum_filter_forces_to_explicit(SPH_model%MHD_prop%fl_prop,    &
     &    ipol%exp_work, SPH_SGS%ipol_LES%rot_frc_by_filter,            &
     &    rj_fld)
!
!
!        if(iflag_debug .gt. 0) write(*,*)                              &
!     &                'SGS_forces_to_explicit rotatin of forces'
!        call SGS_forces_to_explicit                                    &
!     &     (SPH_SGS%SGS_par%model_p, sph%sph_rj,                       &
!     &      ipol%exp_work, SPH_SGS%ipol_LES%SGS_term, rj_fld)
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                'SGS_forces_to_explicit rotatin of forces'
        call SGS_forces_to_explicit                                     &
     &     (SPH_SGS%SGS_par%model_p, sph%sph_rj,                        &
     &      ipol%exp_work, SPH_SGS%ipol_LES%rot_SGS, rj_fld)
!
      end subroutine nonlinear_with_SGS
!*
!*   ------------------------------------------------------------------
!*   ------------------------------------------------------------------
!*
      subroutine init_stab_weight(tmp_stab_wt, model_p)
!
      real(kind = kreal), intent(inout) :: tmp_stab_wt
      type(SGS_model_control_params), intent(inout) :: model_p
!
!
      tmp_stab_wt = one
      if(model_p%iflag_rst_sgs_coef_code .ne. 0) return
      tmp_stab_wt = model_p%stab_weight
      model_p%stab_weight = one
!
      end subroutine init_stab_weight
!*
!*   ------------------------------------------------------------------
!*
      subroutine back_stab_weight(tmp_stab_wt, model_p)
!
      real(kind = kreal), intent(in) :: tmp_stab_wt
      type(SGS_model_control_params), intent(inout) :: model_p
!
!
      if(model_p%iflag_rst_sgs_coef_code .eq. 0) then
        model_p%stab_weight = tmp_stab_wt
      end if
!
      end subroutine back_stab_weight
!*
!*   ------------------------------------------------------------------
!
      end module cal_SGS_nonlinear
