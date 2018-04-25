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
      use cal_nonlinear
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
      if (iflag_debug.eq.1) write(*,*) 'nonlinear_by_pseudo_sph'
      call nonlinear_by_pseudo_sph                                      &
     &   (SPH_MHD%sph, SPH_MHD%comms, SPH_model%omega_sph, r_2nd,       &
     &    SPH_model%MHD_prop, SPH_model%sph_MHD_bc,                     &
     &    trans_p, WK%gt_cor, WK%trns_MHD, WK%WK_sph, WK%MHD_mul_FFTW,  &
     &     WK%cor_rlm, SPH_MHD%ipol, SPH_MHD%itor, SPH_MHD%fld)
!
!   ----  Lead SGS terms
      if(SGS_par%model_p%iflag_SGS .gt. id_SGS_none) then
        if (iflag_debug.eq.1) write(*,*) 'SGS_by_pseudo_sph'
        call SGS_by_pseudo_sph                                          &
     &     (i_step, SGS_par%i_step_sgs_coefs, SGS_par%model_p,          &
     &      SPH_MHD%sph, SPH_MHD%comms, r_2nd, SPH_model%MHD_prop,      &
     &      SPH_model%sph_MHD_bc, trans_p, WK%trns_MHD,                 &
     &      WK%trns_SGS, WK%trns_DYNS, WK%WK_sph,                       &
     &      WK%SGS_mul_FFTW, WK%DYNS_mul_FFTW, dynamic_SPH,             &
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
      end module cal_SGS_nonlinear
