!>@file   cal_nonlinear.f90
!!@brief  module cal_nonlinear
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evaluate nonlinear terms by pseudo spectram scheme
!!
!!@verbatim
!!      subroutine nonlinear                                            &
!!     &         (SGS_param, sph, comms_sph, omega_sph, r_2nd, MHD_prop,&
!!     &          trans_p, ref_temp, ref_comp, ipol, itor, WK, rj_fld)
!!      subroutine licv_exp                                             &
!!     &         (ref_temp, ref_comp, MHD_prop, sph, comms_sph,         &
!!     &          omega_sph, trans_p, trns_MHD, ipol, itor, rj_fld)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(sph_rotation), intent(in) :: omega_sph
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(phys_address), intent(in) :: ipol, itor
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(reference_temperature), intent(in) :: ref_temp
!!        type(reference_temperature), intent(in) :: ref_comp
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(reference_scalar_param), intent(in) :: ref_param_T
!!        type(reference_scalar_param), intent(in) :: ref_param_C
!!@endverbatim
!
!
      module cal_nonlinear
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_control_parameter
      use t_physical_property
      use t_SGS_control_parameter
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_poloidal_rotation
      use t_phys_address
      use t_phys_data
      use t_fdm_coefs
      use t_sph_trans_arrays_MHD
      use t_addresses_sph_transform
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_sph_filtering_data
      use t_radial_reference_temp
      use t_sph_transforms
      use sph_filtering
!
      implicit none
!
      private :: SGS_by_pseudo_sph
      private :: nonlinear_by_pseudo_sph
!
!*   ------------------------------------------------------------------
!*
      contains
!*
!*   ------------------------------------------------------------------
!*
      subroutine nonlinear                                              &
     &         (SGS_param, sph, comms_sph, omega_sph, r_2nd, MHD_prop,  &
     &          trans_p, ref_temp, ref_comp, ipol, itor, WK, rj_fld)
!
      use m_boundary_params_sph_MHD
      use cal_inner_core_rotation
!
      use cal_nonlinear_sph_MHD
      use sum_rotation_of_forces
!
      use m_work_time
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(sph_rotation), intent(in) :: omega_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol, itor
      type(reference_temperature), intent(in) :: ref_temp
      type(reference_temperature), intent(in) :: ref_comp
      type(MHD_evolution_param), intent(in) :: MHD_prop
!
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(phys_data), intent(inout) :: rj_fld
!
!
!   ----  lead nonlinear terms by phesdo spectrum
!
      if (iflag_debug.eq.1) write(*,*) 'nonlinear_by_pseudo_sph'
      call nonlinear_by_pseudo_sph(SGS_param, sph, comms_sph,           &
     &    omega_sph, r_2nd, MHD_prop, trans_p, WK%dynamic_SPH,          &
     &    WK%trns_MHD, WK%WK_sph, WK%MHD_mul_FFTW, ipol, itor, rj_fld)
!
!   ----  Lead SGS terms
      if(SGS_param%iflag_SGS .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'SGS_by_pseudo_sph'
        call SGS_by_pseudo_sph                                          &
     &     (SGS_param, sph, comms_sph, r_2nd, MHD_prop,                 &
     &      trans_p, WK%trns_MHD, WK%trns_snap, WK%trns_SGS, WK%WK_sph, &
     &      WK%SGS_mul_FFTW, WK%dynamic_SPH, ipol, itor, rj_fld)
      end if
!
!   ----  Lead advection of reference field
      call add_ref_advect_sph_MHD                                       &
     &   (sph%sph_rj, MHD_prop%ht_prop, MHD_prop%cp_prop,               &
     &    MHD_prop%ref_param_T, MHD_prop%ref_param_C,                   &
     &    trans_p%leg, ref_temp, ref_comp, ipol, rj_fld)
!
!*  ----  copy coriolis term for inner core rotation
!*
      call start_eleps_time(13)
      if(sph_bc_U%iflag_icb .eq. iflag_rotatable_ic) then
        call copy_icore_rot_to_tor_coriolis                             &
     &     (sph_bc_U%kr_in, sph%sph_rj%idx_rj_degree_one,               &
     &      sph%sph_rj%nidx_rj(2), ipol, itor,                          &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
      call end_eleps_time(13)
!
      if(iflag_debug .gt. 0) write(*,*) 'sum_forces_to_explicit'
      call sum_forces_to_explicit                                       &
     &   (sph%sph_rj, MHD_prop%fl_prop, ipol, itor, rj_fld)
!
      if(SGS_param%iflag_SGS .gt. 0) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                'SGS_forces_to_explicit'
        call SGS_forces_to_explicit                                     &
     &    (SGS_param, sph%sph_rj, ipol, itor, rj_fld)
      end if
!
      end subroutine nonlinear
!*
!*   ------------------------------------------------------------------
!
      subroutine nonlinear_by_pseudo_sph                                &
     &         (SGS_param, sph, comms_sph, omega_sph, r_2nd,            &
     &          MHD_prop, trans_p, dynamic_SPH, trns_MHD, WK_sph,       &
     &          MHD_mul_FFTW, ipol, itor, rj_fld)
!
      use sph_transforms_4_MHD
      use cal_nonlinear_sph_MHD
      use cal_sph_field_by_rotation
      use cal_filtered_sph_fields
      use cal_SGS_terms_sph_MHD
!
      use m_work_time
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(sph_rotation), intent(in) :: omega_sph
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(dynamic_SGS_data_4_sph), intent(in) :: dynamic_SPH
      type(phys_address), intent(in) :: ipol, itor
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(work_for_sgl_FFTW), intent(inout) :: MHD_mul_FFTW
      type(phys_data), intent(inout) :: rj_fld
!
!
!   ----  Lead filtered field for SGS terms
      if(SGS_param%iflag_SGS .gt. 0) then
        if (iflag_debug.ge.1) write(*,*) 'cal_filtered_sph_rj_fields'
        call start_eleps_time(81)
        call cal_filtered_sph_rj_fields                                 &
     &     (sph%sph_rj, ipol, dynamic_SPH%sph_filters, rj_fld)
        call end_eleps_time(81)
      end if
!
!   ----  lead nonlinear terms by phesdo spectrum
!
      call start_eleps_time(14)
      if (iflag_debug.ge.1) write(*,*) 'sph_back_trans_4_MHD'
      call sph_back_trans_4_MHD                                         &
     &   (sph, comms_sph, MHD_prop%fl_prop, omega_sph,                  &
     &    trans_p, ipol, rj_fld, trns_MHD, WK_sph, MHD_mul_FFTW)
      call end_eleps_time(14)
!
      call start_eleps_time(15)
      if (iflag_debug.ge.1) write(*,*) 'nonlinear_terms_in_rtp'
      call nonlinear_terms_in_rtp                                       &
     &   (sph%sph_rtp, MHD_prop%fl_prop, MHD_prop%cd_prop,              &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    trns_MHD%b_trns, trns_MHD%f_trns,                             &
     &    trns_MHD%ncomp_rj_2_rtp, trns_MHD%ncomp_rtp_2_rj,             &
     &    trns_MHD%fld_rtp, trns_MHD%frc_rtp)
!
      if(SGS_param%iflag_SGS .gt. 0) then
        if (iflag_debug.ge.1) write(*,*) 'filtered_nonlinear_in_rtp'
        call filtered_nonlinear_in_rtp(SGS_param, sph%sph_rtp,          &
     &      MHD_prop%fl_prop, MHD_prop%cd_prop,                         &
     &      MHD_prop%ht_prop, MHD_prop%cp_prop,                         &
     &      trns_MHD%b_trns, trns_MHD%f_trns,                           &
     &      trns_MHD%ncomp_rj_2_rtp, trns_MHD%ncomp_rtp_2_rj,           &
     &      trns_MHD%fld_rtp, trns_MHD%frc_rtp)
      end if
      call end_eleps_time(15)
!
      call start_eleps_time(16)
      if (iflag_debug.ge.1) write(*,*) 'sph_forward_trans_4_MHD'
      call sph_forward_trans_4_MHD                                      &
     &   (sph, comms_sph, MHD_prop%fl_prop, trans_p,                    &
     &    ipol, trns_MHD, WK_sph, MHD_mul_FFTW, rj_fld)
      call end_eleps_time(16)
!
      call start_eleps_time(17)
      if (iflag_debug.ge.1) write(*,*) 'rot_momentum_eq_exp_sph'
      call rot_momentum_eq_exp_sph                                      &
     &   (sph%sph_rj, r_2nd, trans_p%leg, ipol, itor, rj_fld)
      call end_eleps_time(17)
!
      end subroutine nonlinear_by_pseudo_sph
!
!*   ------------------------------------------------------------------
!*   ------------------------------------------------------------------
!
      subroutine SGS_by_pseudo_sph                                      &
     &         (SGS_param, sph, comms_sph, r_2nd, MHD_prop,             &
     &          trans_p, trns_MHD, trns_snap, trns_SGS, WK_sph,         &
     &          SGS_mul_FFTW, dynamic_SPH, ipol, itor, rj_fld)
!
      use sph_transforms_4_SGS
      use cal_sph_rotation_of_SGS
      use cal_filtered_sph_fields
      use cal_SGS_terms_sph_MHD
      use dynamic_model_sph_MHD
      use dynamic_SGS_buoyancy_sph
!
      use m_work_time
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
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
!
!   ----  Lead filtered forces for SGS terms
        if (iflag_debug.ge.1) write(*,*) 'cal_filtered_sph_rj_forces'
        call start_eleps_time(81)
        call cal_filtered_sph_rj_forces                                 &
     &     (sph%sph_rj, ipol, dynamic_SPH%sph_filters, rj_fld)
        call end_eleps_time(81)
!
        call start_eleps_time(14)
        if (iflag_debug.eq.1) write(*,*) 'sph_back_trans_SGS_MHD'
        call sph_back_trans_SGS_MHD(sph, comms_sph, trans_p,            &
     &      ipol, rj_fld, trns_SGS, WK_sph, SGS_mul_FFTW)
        call end_eleps_time(14)
!
        call start_eleps_time(15)
        if (iflag_debug.eq.1) write(*,*) 'similarity_SGS_terms_rtp'
        call similarity_SGS_terms_rtp(sph%sph_rtp,                      &
     &      trns_MHD%f_trns, trns_SGS%b_trns, trns_SGS%f_trns,          &
     &      trns_MHD%ncomp_rtp_2_rj, trns_SGS%ncomp_rj_2_rtp,           &
     &      trns_SGS%ncomp_rtp_2_rj, trns_MHD%frc_rtp,                  &
     &      trns_SGS%fld_rtp, trns_SGS%frc_rtp)
!
        if(SGS_param%iflag_dynamic .eq. id_SGS_DYNAMIC_ON) then
          if (iflag_debug.eq.1) write(*,*) 'wider_similarity_SGS_rtp'
          call wider_similarity_SGS_rtp(sph%sph_rtp,                    &
     &        MHD_prop%fl_prop, MHD_prop%cd_prop,                       &
     &        MHD_prop%ht_prop, MHD_prop%cp_prop,                       &
     &        trns_MHD%b_trns, trns_SGS%b_trns,                         &
     &        trns_MHD%ncomp_rj_2_rtp, trns_SGS%ncomp_rj_2_rtp,         &
     &        trns_MHD%fld_rtp, trns_SGS%fld_rtp)
!
          if (iflag_debug.eq.1) write(*,*) 'const_model_coefs_4_sph'
          call const_model_coefs_4_sph                                  &
     &       (sph%sph_rtp, dynamic_SPH%ifld_sgs, dynamic_SPH%icomp_sgs, &
     &        dynamic_SPH%wk_sgs, trns_SGS)
!
          if(SGS_param%iflag_SGS_gravity .ne. id_SGS_none) then
            if(iflag_debug .gt. 0) write(*,*)                           &
     &           'const_dynamic_SGS_4_buo_sph'
            call const_dynamic_SGS_4_buo_sph                            &
     &         (sph%sph_rtp, MHD_prop%fl_prop,                          &
     &          trns_MHD, trns_snap, trns_SGS, dynamic_SPH)
          end if
        end if
        call end_eleps_time(15)
!
        call start_eleps_time(16)
        if (iflag_debug.eq.1) write(*,*) 'sph_forward_trans_SGS_MHD'
        call sph_forward_trans_SGS_MHD(sph, comms_sph, trans_p,         &
     &      ipol, trns_SGS, WK_sph, SGS_mul_FFTW, rj_fld)
        call end_eleps_time(16)
!
        call start_eleps_time(17)
        if (iflag_debug.ge.1) write(*,*) 'rot_SGS_terms_exp_sph'
        call rot_SGS_terms_exp_sph                                      &
     &     (sph%sph_rj, r_2nd, trans_p%leg, ipol, itor, rj_fld)
        call end_eleps_time(17)
!
      end subroutine SGS_by_pseudo_sph
!
!*   ------------------------------------------------------------------
!*   ------------------------------------------------------------------
!*
      subroutine licv_exp                                               &
     &         (ref_temp, ref_comp, MHD_prop, sph, comms_sph,           &
     &          omega_sph, trans_p, trns_MHD, ipol, itor, rj_fld)
!
      use m_phys_constants
      use m_boundary_params_sph_MHD
      use sph_transforms_4_MHD
      use copy_nodal_fields
      use cal_nonlinear_sph_MHD
      use sum_rotation_of_forces
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(sph_rotation), intent(in) :: omega_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(address_4_sph_trans), intent(in) :: trns_MHD
      type(phys_address), intent(in) :: ipol, itor
      type(reference_temperature), intent(in) :: ref_temp
      type(reference_temperature), intent(in) :: ref_comp
      type(MHD_evolution_param), intent(in) :: MHD_prop
!
      type(phys_data), intent(inout) :: rj_fld
!
!*  ----  copy velocity for coriolis term ------------------
!*
      if(iflag_debug.eq.1) write(*,*) 'sph_transform_4_licv'
      if(MHD_prop%fl_prop%iflag_4_coriolis .ne. id_turn_OFF) then
        call sph_transform_4_licv                                       &
     &     (sph%sph_rlm, comms_sph%comm_rlm, comms_sph%comm_rj,         &
     &      MHD_prop%fl_prop, omega_sph, trans_p%leg, trns_MHD,         &
     &      ipol, rj_fld)
      end if
!
!   ----  lead nonlinear terms by phesdo spectrum
!
      if(ipol%i_h_advect .gt. 0) then
        call clear_field_data(rj_fld, n_scalar, ipol%i_h_advect)
      end if
      if(ipol%i_c_advect .gt. 0) then
        call clear_field_data(rj_fld, n_scalar, ipol%i_c_advect)
      end if
!
      call add_ref_advect_sph_MHD                                       &
     &   (sph%sph_rj, MHD_prop%ht_prop, MHD_prop%cp_prop,                   &
     &    MHD_prop%ref_param_T, MHD_prop%ref_param_C,                   &
     &    trans_p%leg, ref_temp, ref_comp, ipol, rj_fld)
!
      call licv_forces_to_explicit                                      &
     &   (sph%sph_rj, MHD_prop%fl_prop, ipol, itor, rj_fld)
!
!
      end subroutine licv_exp
!*
!*   ------------------------------------------------------------------
!
      end module cal_nonlinear
