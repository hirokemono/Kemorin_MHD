!>@file   cal_sph_dynamic_SGS.f90
!!@brief  module cal_sph_dynamic_SGS
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evaluate nonlinear terms by pseudo spectram scheme
!!
!!@verbatim
!!      subroutine SGS_by_pseudo_sph(i_step, i_step_sgs_coefs,          &
!!     &          SGS_param, sph, comms_sph, r_2nd, MHD_prop,           &
!!     &          sph_MHD_bc, trans_p, trns_MHD, trns_SIMI, trns_DYNS,  &
!!     &          WK_sph, dynamic_SPH, ipol, itor, rj_fld)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(phys_address), intent(in) :: ipol, itor
!!        type(address_4_sph_trans), intent(inout) :: trns_MHD
!!        type(address_4_sph_trans), intent(inout) :: trns_SIMI
!!        type(address_4_sph_trans), intent(inout) :: trns_DYNS
!!        type(spherical_trns_works), intent(inout) :: WK_sph
!!        type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
!!        type(phys_data), intent(inout) :: rj_fld
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
      private :: dynamic_SGS_by_pseudo_sph
!
!*   ------------------------------------------------------------------
!*
      contains
!*
!*   ------------------------------------------------------------------
!
      subroutine SGS_by_pseudo_sph(i_step, i_step_sgs_coefs,            &
     &          SGS_param, sph, comms_sph, r_2nd, MHD_prop,             &
     &          sph_MHD_bc, trans_p, trns_MHD, trns_SGS, trns_DYNS,     &
     &          trns_ngTMP, WK_sph, dynamic_SPH, ipol, itor, rj_fld)
!
      use sph_transforms_4_SGS
      use cal_sph_rotation_of_SGS
      use product_model_coefs_sph
      use cal_dynamic_SGS_buoyancy
      use scale_similarity_sph_SGS
      use nonlinear_gradient_sph_SGS
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
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      type(address_4_sph_trans), intent(inout) :: trns_SGS
      type(address_4_sph_trans), intent(inout) :: trns_DYNS
      type(address_4_sph_trans), intent(inout) :: trns_ngTMP
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint) :: istep_dynamic
!
!
      istep_dynamic = mod(i_step, i_step_sgs_coefs)
!
      if(SGS_param%iflag_SGS .eq. id_SGS_similarity) then
        call cal_scale_similarity_sph_SGS                               &
     &     (sph, comms_sph, MHD_prop, trans_p, WK_sph,                  &
     &      dynamic_SPH, ipol, rj_fld, trns_SGS)
!
        if(SGS_param%iflag_dynamic .eq. id_SGS_DYNAMIC_ON) then
          if(iflag_debug .gt. 0) write(*,*) 'Dynamic model:',           &
     &                      i_step, i_step_sgs_coefs, istep_dynamic
          call start_elapsed_time(83)
          if(istep_dynamic .eq. 0) then
            call dynamic_SGS_by_pseudo_sph                              &
     &         (SGS_param, sph, comms_sph, MHD_prop, trans_p,           &
     &          trns_SGS, trns_DYNS, WK_sph, dynamic_SPH,               &
     &          ipol, rj_fld)
          end if
        end if
      else if(SGS_param%iflag_SGS .eq. id_SGS_NL_grad) then
        if (iflag_debug.eq.1) write(*,*)                                &
     &                'cal_nonlinear_gradient_sph_SGS'
        call cal_nonlinear_gradient_sph_SGS                             &
     &         (sph, comms_sph, r_2nd, sph_MHD_bc, MHD_prop,            &
     &          trans_p, dynamic_SPH, ipol, trns_MHD, WK_sph, rj_fld,   &
     &          trns_ngTMP, trns_SGS)
      end if
!
      if(SGS_param%iflag_dynamic .eq. id_SGS_DYNAMIC_ON) then
        if (iflag_debug.eq.1) write(*,*) 'product_model_coefs_4_sph'
        call product_model_coefs_4_sph(SGS_param, sph%sph_rtp,          &
     &      trns_SGS%f_trns, trns_SGS%forward, dynamic_SPH)
      else
        call prod_fixed_sph_SGS_Csim(SGS_param, sph%sph_rtp,            &
     &      dynamic_SPH%ifld_sgs, trns_SGS%f_trns, trns_SGS%forward)
      end if
      call end_elapsed_time(15)
!
!
      if(SGS_param%iflag_SGS_gravity .ne. id_SGS_none                   &
     &  .and. istep_dynamic .eq. 0) then
        call dynamic_buo_SGS_by_pseudo_sph(SGS_param, sph, comms_sph,   &
     &      MHD_prop, trans_p, trns_MHD, trns_SGS, trns_DYNS,           &
     &      WK_sph,  dynamic_SPH, ipol, rj_fld)
      end if
!
!
      call start_elapsed_time(17)
      if(SGS_param%iflag_SGS_gravity .ne. id_SGS_none) then
        call product_buo_model_coefs_4_sph                              &
     &         (SGS_param, sph, comms_sph, trans_p, trns_SGS,           &
     &          WK_sph, dynamic_SPH, ipol, rj_fld)
      else
        call start_elapsed_time(16)
        if (iflag_debug.eq.1) write(*,*)                                &
     &            'sph_forward_trans_SGS_MHD SGS'
        call sph_forward_trans_SGS_MHD(sph, comms_sph, trans_p,         &
     &      trns_SGS%forward, WK_sph, trns_SGS%mul_FFTW, rj_fld)
        call end_elapsed_time(16)
      end if
!
      if (iflag_debug.ge.1) write(*,*) 'rot_SGS_terms_exp_sph'
      call rot_SGS_terms_exp_sph(sph%sph_rj, r_2nd, sph_MHD_bc,         &
     &    trans_p%leg, ipol, itor, rj_fld)
      call end_elapsed_time(17)
!
      end subroutine SGS_by_pseudo_sph
!
!*   ------------------------------------------------------------------
!
      subroutine dynamic_SGS_by_pseudo_sph(SGS_param, sph, comms_sph,   &
     &          MHD_prop, trans_p, trns_SIMI, trns_DYNS, WK_sph,        &
     &          dynamic_SPH, ipol, rj_fld)
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
!
      type(address_4_sph_trans), intent(inout) :: trns_SIMI, trns_DYNS
      type(spherical_trns_works), intent(inout) :: WK_sph
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
      type(phys_data), intent(inout) :: rj_fld
!
!
!
      call start_elapsed_time(16)
      if (iflag_debug.eq.1) write(*,*) 'sph_forward_trans_SGS_MHD dyns'
      call sph_forward_trans_SGS_MHD(sph, comms_sph, trans_p,           &
     &    trns_SIMI%forward, WK_sph, trns_SIMI%mul_FFTW, rj_fld)
      call end_elapsed_time(16)
!
      call cal_sph_wide_filtering_fields                                &
     &   (sph%sph_rj, ipol, dynamic_SPH%sph_filters(2), rj_fld)
      call cal_sph_wide_filtering_forces                                &
     &   (sph%sph_rj, ipol, dynamic_SPH%sph_filters(2), rj_fld)
      call cal_sph_dble_filtering_forces                                &
     &   (sph%sph_rj, ipol, dynamic_SPH%sph_filters(2), rj_fld)
!
      call start_elapsed_time(14)
      if (iflag_debug.eq.1) write(*,*) 'sph_back_trans_SGS_MHD dyns'
      call sph_back_trans_SGS_MHD(sph, comms_sph, trans_p,              &
     &    rj_fld, trns_DYNS%backward, WK_sph, trns_DYNS%mul_FFTW)
      call end_elapsed_time(14)
!
      if (iflag_debug.eq.1) write(*,*) 'wider_similarity_SGS_rtp'
      call wider_similarity_SGS_rtp(sph%sph_rtp, MHD_prop,              &
     &   trns_DYNS%b_trns, trns_DYNS%backward)
!
      if (iflag_debug.eq.1) write(*,*) 'SGS_param%stab_weight'
      call const_model_coefs_4_sph                                      &
     &   (SGS_param, sph%sph_rtp, trns_SIMI%f_trns, trns_DYNS%b_trns,   &
     &    trns_SIMI%forward, trns_DYNS%backward, dynamic_SPH)
!
      end subroutine dynamic_SGS_by_pseudo_sph
!
!*   ------------------------------------------------------------------
!
      end module cal_sph_dynamic_SGS
