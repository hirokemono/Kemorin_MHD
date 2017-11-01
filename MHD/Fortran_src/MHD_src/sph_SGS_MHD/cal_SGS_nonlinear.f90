!>@file   cal_SGS_nonlinear.f90
!!@brief  module cal_SGS_nonlinear
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evaluate nonlinear terms by pseudo spectram scheme
!!
!!@verbatim
!!      subroutine nonlinear_SGS_first                                  &
!!     &         (i_step, r_2nd, SPH_model, trans_p, WK, SGS_par,       &
!!     &          dynamic_SPH, SPH_MHD)
!!      subroutine nonlinear_with_SGS(i_step, SGS_par, r_2nd, SPH_model,&
!!     &          trans_p, WK, dynamic_SPH, SPH_MHD)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(SPH_MHD_model_data), intent(in) :: SPH_model
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
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
!
      use calypso_mpi
!
      use t_physical_property
      use t_SGS_control_parameter
      use t_SPH_MHD_model_data
      use t_SPH_mesh_field_data
      use t_fdm_coefs
      use t_sph_trans_arrays_MHD
      use t_addresses_sph_transform
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_sph_filtering_data
      use t_sph_transforms
      use t_coriolis_terms_rlm
      use t_gaunt_coriolis_rlm
      use t_sph_filtering
!
      implicit none
!
      private :: SGS_by_pseudo_sph
      private :: nonlinear_by_pseudo_sph_SGS
!
!*   ------------------------------------------------------------------
!*
      contains
!*
!*   ------------------------------------------------------------------
!*
      subroutine nonlinear_SGS_first                                    &
     &         (i_step, r_2nd, SPH_model, trans_p, WK, SGS_par,         &
     &          dynamic_SPH, SPH_MHD)
!
      integer(kind = kint), intent(in) :: i_step
      type(fdm_matrices), intent(in) :: r_2nd
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(SPH_MHD_model_data), intent(in) :: SPH_model
!
      type(SGS_paremeters), intent(inout) :: SGS_par
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!
      real(kind = kreal), save :: tmp_stab_wt = one
!
!
      if(SGS_par%model_p%iflag_rst_sgs_coef_code .eq. 0) then
        tmp_stab_wt = SGS_par%model_p%stab_weight
        SGS_par%model_p%stab_weight = one
      end if
!
      call nonlinear_with_SGS(i_step, SGS_par,                          &
     &    r_2nd, SPH_model, trans_p, WK, dynamic_SPH, SPH_MHD)
!
      if(SGS_par%model_p%iflag_rst_sgs_coef_code .eq. 0) then
        SGS_par%model_p%stab_weight = tmp_stab_wt
      end if
!
      end subroutine nonlinear_SGS_first
!*
!*   ------------------------------------------------------------------
!*
      subroutine nonlinear_with_SGS(i_step, SGS_par, r_2nd, SPH_model,  &
     &          trans_p, WK, dynamic_SPH, SPH_MHD)
!
      use cal_inner_core_rotation
!
      use cal_nonlinear_sph_MHD
      use sum_rotation_of_SGS
      use sum_rotation_of_forces
!
      integer(kind = kint), intent(in) :: i_step
      type(SGS_paremeters), intent(in) :: SGS_par
      type(fdm_matrices), intent(in) :: r_2nd
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(SPH_MHD_model_data), intent(in) :: SPH_model
!
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!
!
!   ----  lead nonlinear terms by phesdo spectrum
!
      if (iflag_debug.eq.1) write(*,*) 'nonlinear_by_pseudo_sph_SGS'
      call nonlinear_by_pseudo_sph_SGS(SGS_par%model_p,                 &
     &    SPH_MHD%sph, SPH_MHD%comms, SPH_model%omega_sph, r_2nd,       &
     &    SPH_model%MHD_prop, SPH_model%sph_MHD_bc,                     &
     &    trans_p, WK%gt_cor, dynamic_SPH, WK%trns_MHD, WK%WK_sph,      &
     &    WK%MHD_mul_FFTW, WK%cor_rlm, SPH_MHD%ipol,                    &
     &    SPH_MHD%itor, SPH_MHD%fld)
!
!   ----  Lead SGS terms
      if(SGS_par%model_p%iflag_SGS .gt. id_SGS_none) then
        if (iflag_debug.eq.1) write(*,*) 'SGS_by_pseudo_sph'
        call SGS_by_pseudo_sph                                          &
     &     (i_step, SGS_par%i_step_sgs_coefs, SGS_par%model_p,          &
     &      SPH_MHD%sph, SPH_MHD%comms, r_2nd, SPH_model%MHD_prop,      &
     &      SPH_model%sph_MHD_bc, trans_p, WK%trns_MHD, WK%trns_snap,   &
     &      WK%trns_SGS, WK%WK_sph, WK%SGS_mul_FFTW, dynamic_SPH,       &
     &      SPH_MHD%ipol, SPH_MHD%itor, SPH_MHD%fld)
      end if
!
!   ----  Lead advection of reference field
      call add_ref_advect_sph_MHD                                       &
     &   (SPH_MHD%sph%sph_rj, SPH_model%sph_MHD_bc, SPH_model%MHD_prop, &
     &    trans_p%leg, SPH_model%ref_temp, SPH_model%ref_comp,          &
     &    SPH_MHD%ipol, SPH_MHD%fld)
!
!*  ----  copy coriolis term for inner core rotation
!*
      call start_elapsed_time(13)
      call copy_icore_rot_to_tor_coriolis                               &
     &   (SPH_model%sph_MHD_bc%sph_bc_U, SPH_MHD%sph%sph_rj,            &
     &    SPH_MHD%ipol, SPH_MHD%itor, SPH_MHD%fld)
      call end_elapsed_time(13)
!
      if(iflag_debug .gt. 0) write(*,*) 'sum_forces_to_explicit'
      call sum_forces_to_explicit                                       &
     &   (SPH_MHD%sph%sph_rj, SPH_model%MHD_prop%fl_prop,               &
     &    SPH_MHD%ipol, SPH_MHD%itor, SPH_MHD%fld)
!
      if(SGS_par%model_p%iflag_SGS .gt. id_SGS_none) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                'SGS_forces_to_explicit'
        call SGS_forces_to_explicit(SGS_par%model_p,                    &
     &     SPH_MHD%sph%sph_rj, SPH_model%sph_MHD_bc%sph_bc_U,           &
     &     SPH_MHD%ipol, SPH_MHD%itor, SPH_MHD%fld)
      end if
!
      end subroutine nonlinear_with_SGS
!*
!*   ------------------------------------------------------------------
!
      subroutine nonlinear_by_pseudo_sph_SGS                            &
     &         (SGS_param, sph, comms_sph, omega_sph, r_2nd, MHD_prop,  &
     &          sph_MHD_bc, trans_p, gt_cor, dynamic_SPH, trns_MHD,     &
     &          WK_sph, MHD_mul_FFTW, cor_rlm, ipol, itor, rj_fld)
!
      use sph_transforms_4_MHD
      use cal_nonlinear_sph_MHD
      use cal_sph_field_by_rotation
      use cal_filtered_sph_fields
      use cal_SGS_terms_sph_MHD
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_rotation), intent(in) :: omega_sph
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(gaunt_coriolis_rlm), intent(in) :: gt_cor
      type(dynamic_SGS_data_4_sph), intent(in) :: dynamic_SPH
      type(phys_address), intent(in) :: ipol, itor
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(work_for_sgl_FFTW), intent(inout) :: MHD_mul_FFTW
      type(coriolis_rlm_data), intent(inout) :: cor_rlm
      type(phys_data), intent(inout) :: rj_fld
!
!
!   ----  Lead filtered field for SGS terms
      if(SGS_param%iflag_SGS .gt. id_SGS_none) then
        if (iflag_debug.ge.1) write(*,*) 'cal_filtered_sph_rj_fields'
        call start_elapsed_time(81)
        call cal_filtered_sph_rj_fields                                 &
     &     (sph%sph_rj, ipol, dynamic_SPH%sph_filters, rj_fld)
        call end_elapsed_time(81)
      end if
!
!   ----  lead nonlinear terms by phesdo spectrum
!
      call start_elapsed_time(14)
      if (iflag_debug.ge.1) write(*,*) 'sph_back_trans_4_MHD'
      call sph_back_trans_4_MHD                                         &
     &   (sph, comms_sph, MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U,        &
     &    omega_sph, trans_p, gt_cor, ipol, rj_fld, trns_MHD,           &
     &    WK_sph, MHD_mul_FFTW, cor_rlm)
      call end_elapsed_time(14)
!
      call start_elapsed_time(15)
      if (iflag_debug.ge.1) write(*,*) 'nonlinear_terms_in_rtp'
      call nonlinear_terms_in_rtp                                       &
     &   (sph%sph_rtp, MHD_prop, trns_MHD%b_trns, trns_MHD%f_trns,      &
     &    trns_MHD%ncomp_rj_2_rtp, trns_MHD%ncomp_rtp_2_rj,             &
     &    trns_MHD%fld_rtp, trns_MHD%frc_rtp)
!
      if(SGS_param%iflag_SGS .gt. id_SGS_none) then
        if (iflag_debug.ge.1) write(*,*) 'filtered_nonlinear_in_rtp'
        call filtered_nonlinear_in_rtp(SGS_param, sph%sph_rtp,          &
     &      MHD_prop%fl_prop, MHD_prop%cd_prop,                         &
     &      MHD_prop%ht_prop, MHD_prop%cp_prop,                         &
     &      trns_MHD%b_trns, trns_MHD%f_trns,                           &
     &      trns_MHD%ncomp_rj_2_rtp, trns_MHD%ncomp_rtp_2_rj,           &
     &      trns_MHD%fld_rtp, trns_MHD%frc_rtp)
      end if
      call end_elapsed_time(15)
!
      call start_elapsed_time(16)
      if (iflag_debug.ge.1) write(*,*) 'sph_forward_trans_4_MHD'
      call sph_forward_trans_4_MHD                                      &
     &   (sph, comms_sph, MHD_prop%fl_prop, trans_p, cor_rlm,           &
     &    ipol, trns_MHD, WK_sph, MHD_mul_FFTW, rj_fld)
      call end_elapsed_time(16)
!
      call start_elapsed_time(17)
      if (iflag_debug.ge.1) write(*,*) 'rot_momentum_eq_exp_sph'
      call rot_momentum_eq_exp_sph(sph%sph_rj, r_2nd, sph_MHD_bc,       &
     &    trans_p%leg, ipol, itor, rj_fld)
      call end_elapsed_time(17)
!
      end subroutine nonlinear_by_pseudo_sph_SGS
!
!*   ------------------------------------------------------------------
!*   ------------------------------------------------------------------
!
      subroutine SGS_by_pseudo_sph(i_step, i_step_sgs_coefs, SGS_param, &
     &          sph, comms_sph, r_2nd, MHD_prop, sph_MHD_bc,            &
     &          trans_p, trns_MHD, trns_snap, trns_SGS, WK_sph,         &
     &          SGS_mul_FFTW, dynamic_SPH, ipol, itor, rj_fld)
!
      use t_SGS_buoyancy_sph
      use sph_transforms_4_SGS
      use cal_sph_rotation_of_SGS
      use cal_filtered_sph_fields
      use cal_SGS_terms_sph_MHD
      use dynamic_model_sph_MHD
      use copy_Csim_4_sph_MHD
!
      integer(kind = kint), intent(in) :: i_step, i_step_sgs_coefs
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol, itor
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD, trns_snap
      type(address_4_sph_trans), intent(inout) :: trns_SGS
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(work_for_sgl_FFTW), intent(inout) :: SGS_mul_FFTW
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: istep_dynamic
!
!
!   ----  Lead filtered forces for SGS terms
        if (iflag_debug.ge.1) write(*,*) 'cal_filtered_sph_rj_forces'
        call start_elapsed_time(81)
        call cal_filtered_sph_rj_forces                                 &
     &     (sph%sph_rj, ipol, dynamic_SPH%sph_filters, rj_fld)
        call end_elapsed_time(81)
!
        call start_elapsed_time(14)
        if (iflag_debug.eq.1) write(*,*) 'sph_back_trans_SGS_MHD'
        call sph_back_trans_SGS_MHD(sph, comms_sph, trans_p,            &
     &      ipol, rj_fld, trns_SGS, WK_sph, SGS_mul_FFTW)
        call end_elapsed_time(14)
!
        call start_elapsed_time(15)
        if (iflag_debug.eq.1) write(*,*) 'similarity_SGS_terms_rtp'
        call similarity_SGS_terms_rtp(sph%sph_rtp,                      &
     &      trns_MHD%f_trns, trns_SGS%b_trns, trns_SGS%f_trns,          &
     &      trns_MHD%ncomp_rtp_2_rj, trns_SGS%ncomp_rj_2_rtp,           &
     &      trns_SGS%ncomp_rtp_2_rj, trns_MHD%frc_rtp,                  &
     &      trns_SGS%fld_rtp, trns_SGS%frc_rtp)
!
        istep_dynamic = mod(i_step, i_step_sgs_coefs)
        if(SGS_param%iflag_dynamic .eq. id_SGS_DYNAMIC_ON) then
          call start_elapsed_time(83)
!
          if (iflag_debug.eq.1) write(*,*) 'wider_similarity_SGS_rtp'
          call wider_similarity_SGS_rtp(istep_dynamic, sph%sph_rtp,     &
     &        MHD_prop%fl_prop, MHD_prop%cd_prop,                       &
     &        MHD_prop%ht_prop, MHD_prop%cp_prop,                       &
     &        trns_MHD%b_trns, trns_SGS%b_trns,                         &
     &        trns_MHD%ncomp_rj_2_rtp, trns_SGS%ncomp_rj_2_rtp,         &
     &        trns_MHD%fld_rtp, trns_SGS%fld_rtp)
!
          if (iflag_debug.eq.1) write(*,*)                              &
     &                   'SGS_param%stab_weight', SGS_param%stab_weight
          call const_model_coefs_4_sph                                  &
     &       (istep_dynamic, SGS_param, sph%sph_rtp,                    &
     &        dynamic_SPH%ifld_sgs, dynamic_SPH%icomp_sgs,              &
     &        dynamic_SPH%wk_sgs, trns_SGS)
!
          if(SGS_param%iflag_SGS_gravity .ne. id_SGS_none               &
     &       .and. istep_dynamic .eq. 0) then
            if(my_rank .eq. 0) write(*,*) 'Dynamic model:',             &
     &                      i_step, i_step_sgs_coefs
            if(iflag_debug .gt. 0) write(*,*)                           &
     &           'const_dynamic_SGS_4_buo_sph', iflag_debug
            call const_dynamic_SGS_4_buo_sph                            &
     &         (SGS_param%iflag_SGS_buo_usage, SGS_param%stab_weight,   &
     &          sph%sph_rtp, MHD_prop%fl_prop,                          &
     &          trns_MHD, trns_snap, trns_SGS, dynamic_SPH)
            call copy_Csim_buo_4_sph_trans(sph%sph_rtp,                 &
     &          dynamic_SPH%ifld_sgs, dynamic_SPH%wk_sgs, trns_SGS)
          end if
          call end_elapsed_time(83)
        else
          call prod_fixed_sph_SGS_Csim                                  &
     &       (SGS_param, sph%sph_rtp, dynamic_SPH%ifld_sgs, trns_SGS)
        end if
        call end_elapsed_time(15)
!
        call start_elapsed_time(16)
        if (iflag_debug.eq.1) write(*,*) 'sph_forward_trans_SGS_MHD'
        call sph_forward_trans_SGS_MHD(sph, comms_sph, trans_p,         &
     &      ipol, trns_SGS, WK_sph, SGS_mul_FFTW, rj_fld)
        call end_elapsed_time(16)
!
        call start_elapsed_time(17)
        call start_elapsed_time(84)
        if(SGS_param%iflag_SGS_buo_usage .eq. id_use_sphere) then
          if(istep_dynamic .eq. 0) then
            if (iflag_debug.eq.1) write(*,*)                            &
     &                      'sphere_averaged_SGS_buoyancy', iflag_debug
            call calypso_mpi_barrier
            call sphere_averaged_SGS_buoyancy(sph%sph_rj, sph%sph_rtp,  &
     &          ipol, rj_fld, dynamic_SPH%wk_sgs_buo)
          end if
!
            call calypso_mpi_barrier
            if(iflag_debug.eq.1) write(*,*)                             &
     &                      'magnify_sph_ave_SGS_buoyancy'
          call magnify_sph_ave_SGS_buoyancy(sph%sph_rj, sph%sph_rtp,    &
     &        ipol, dynamic_SPH%ifld_sgs, dynamic_SPH%wk_sgs_buo,       &
     &        rj_fld, trns_SGS)
            call calypso_mpi_barrier
        else if(SGS_param%iflag_SGS_buo_usage .ne. id_use_volume) then
          if(istep_dynamic .eq. 0) then
            if (iflag_debug.eq.1) write(*,*)                            &
     &                      'volume_averaged_SGS_buoyancy', iflag_debug
            call volume_averaged_SGS_buoyancy(sph%sph_params,           &
     &          sph%sph_rj, ipol, rj_fld, dynamic_SPH%wk_sgs_buo)
          end if
!
            if(iflag_debug.eq.1) write(*,*)                             &
     &                      'magnify_vol_ave_SGS_buoyancy'
          call magnify_vol_ave_SGS_buoyancy(sph%sph_rtp, ipol,          &
     &        dynamic_SPH%ifld_sgs, dynamic_SPH%wk_sgs_buo,             &
     &        rj_fld, trns_SGS)
        end if
        call end_elapsed_time(84)
!
        if (iflag_debug.ge.1) write(*,*) 'rot_SGS_terms_exp_sph'
        call rot_SGS_terms_exp_sph(sph%sph_rj, r_2nd, sph_MHD_bc,       &
     &      trans_p%leg, ipol, itor, rj_fld)
        call end_elapsed_time(17)
!
      end subroutine SGS_by_pseudo_sph
!
!*   ------------------------------------------------------------------
!
      end module cal_SGS_nonlinear
