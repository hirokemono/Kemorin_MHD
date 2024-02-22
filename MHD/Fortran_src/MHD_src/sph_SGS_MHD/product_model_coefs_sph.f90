!>@file   product_model_coefs_sph.f90
!!@brief  module product_model_coefs_sph
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Evaluate nonlinear terms in spherical coordinate grid
!!
!!@verbatim
!!      subroutine prod_fixed_sph_SGS_Csim(SGS_param, sph_rtp,          &
!!     &          iak_sgs_term, fg_trns_SGS, trns_f_SGS)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(SGS_term_address), intent(in) :: iak_sgs_term
!!        type(SGS_term_address), intent(in) :: fg_trns_SGS
!!        type(spherical_transform_data), intent(inout) :: trns_f_SGS
!!      subroutine product_model_coefs_4_sph                            &
!!     &         (SGS_param, sph_rtp, sph_d_grp,                        &
!!     &          iak_sgs_term, fg_trns_SGS, trns_f_SGS, wk_sph_sgs)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!!        type(SGS_term_address), intent(in) :: iak_sgs_term
!!        type(SGS_term_address), intent(in) :: fg_trns_SGS
!!        type(spherical_transform_data), intent(inout) :: trns_f_SGS
!!        type(SPH_dynamic_model_data), intent(inout) :: wk_sph_sgs
!!
!!     subroutine sel_product_model_coefs                               &
!!    &         (const_Csim, sph_rtp, sph_d_grp, numdir,                &
!!    &          irtp_sgs, ifld_sgs, wk_sph_sgs, trns_f_SGS)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_dynamic_model_group), intent(in) :: sph_d_grp
!!        type(SPH_dynamic_model_data), intent(inout) :: wk_sph_sgs
!!        type(spherical_transform_data), intent(inout) :: trns_f_SGS
!!@endverbatim
!
      module product_model_coefs_sph
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
      use m_phys_constants
!
      use t_SGS_control_parameter
      use t_spheric_rtp_data
      use t_groups_sph_dynamic
      use t_SGS_term_labels
      use t_addresses_sph_transform
      use t_ele_info_4_dynamic
      use t_addresses_sph_transform
      use t_sph_filtering
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine prod_fixed_sph_SGS_Csim(SGS_param, sph_rtp,            &
     &          iak_sgs_term, fg_trns_SGS, trns_f_SGS)
!
      use prod_SGS_model_coefs_sph
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(SGS_term_address), intent(in) :: fg_trns_SGS
      type(SGS_term_address), intent(in) :: iak_sgs_term
!
      type(spherical_transform_data), intent(inout) :: trns_f_SGS
!
!
      if(iak_sgs_term%i_SGS_m_flux .gt. 0) then
        call product_fixed_model_coefs                                  &
     &     (SGS_param%SGS_momentum%SGS_factor, sph_rtp, n_vector,       &
     &      trns_f_SGS%fld_rtp(1,fg_trns_SGS%i_SGS_inertia))
      end if
!
      if(iak_sgs_term%i_SGS_Lorentz .gt. 0) then
        call product_fixed_model_coefs                                  &
     &     (SGS_param%SGS_mawell_factor, sph_rtp, n_vector,             &
     &      trns_f_SGS%fld_rtp(1,fg_trns_SGS%i_SGS_Lorentz))
      end if
!
      if(iak_sgs_term%i_SGS_induction .gt. 0) then
        call product_fixed_model_coefs                                  &
     &     (SGS_param%SGS_uxb_factor, sph_rtp, n_vector,                &
     &      trns_f_SGS%fld_rtp(1,fg_trns_SGS%i_SGS_vp_induct))
      end if
!
      if(iak_sgs_term%i_SGS_h_flux .gt. 0) then
        call product_fixed_model_coefs                                  &
     &     (SGS_param%SGS_heat%SGS_factor, sph_rtp, n_vector,           &
     &      trns_f_SGS%fld_rtp(1,fg_trns_SGS%i_SGS_h_flux))
      end if
!
      if(iak_sgs_term%i_SGS_c_flux .gt. 0) then
        call product_fixed_model_coefs                                  &
     &     (SGS_param%SGS_light%SGS_factor, sph_rtp, n_vector,          &
     &      trns_f_SGS%fld_rtp(1,fg_trns_SGS%i_SGS_c_flux))
      end if
!
      end subroutine prod_fixed_sph_SGS_Csim
!
! ----------------------------------------------------------------------
!
      subroutine product_model_coefs_4_sph                              &
     &         (SGS_param, sph_rtp, sph_d_grp,                          &
     &          iak_sgs_term, fg_trns_SGS, trns_f_SGS, wk_sph_sgs)
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
      type(SGS_term_address), intent(in) :: iak_sgs_term
      type(SGS_term_address), intent(in) :: fg_trns_SGS
!
      type(SPH_dynamic_model_data), intent(inout) :: wk_sph_sgs
      type(spherical_transform_data), intent(inout) :: trns_f_SGS
!
!
      if(iak_sgs_term%i_SGS_m_flux .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'sel_product_model_coefs MF'
        call sel_product_model_coefs                                    &
     &     (SGS_param%SGS_momentum%SGS_factor, sph_rtp, sph_d_grp,      &
     &      n_vector, fg_trns_SGS%i_SGS_inertia,                        &
     &      iak_sgs_term%i_SGS_m_flux, wk_sph_sgs, trns_f_SGS)
      end if
!
      if(iak_sgs_term%i_SGS_Lorentz .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'sel_product_model_coefs LZ'
        call sel_product_model_coefs                                    &
     &    (SGS_param%SGS_mawell_factor, sph_rtp, sph_d_grp,             &
     &     n_vector, fg_trns_SGS%i_SGS_Lorentz,                         &
     &     iak_sgs_term%i_SGS_Lorentz, wk_sph_sgs, trns_f_SGS)
      end if
!
      if(iak_sgs_term%i_SGS_induction .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'sel_product_model_coefs ID'
        call sel_product_model_coefs                                    &
     &     (SGS_param%SGS_uxb_factor, sph_rtp, sph_d_grp,               &
     &      n_vector, fg_trns_SGS%i_SGS_vp_induct,                      &
     &      iak_sgs_term%i_SGS_induction, wk_sph_sgs, trns_f_SGS)
      end if
!
      if(iak_sgs_term%i_SGS_h_flux .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'sel_product_model_coefs HF'
        call sel_product_model_coefs                                    &
     &     (SGS_param%SGS_heat%SGS_factor, sph_rtp, sph_d_grp,          &
     &      n_vector, fg_trns_SGS%i_SGS_h_flux,                         &
     &      iak_sgs_term%i_SGS_h_flux, wk_sph_sgs, trns_f_SGS)
      end if
!
      if(iak_sgs_term%i_SGS_c_flux .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'sel_product_model_coefs CF'
        call sel_product_model_coefs                                    &
     &     (SGS_param%SGS_light%SGS_factor, sph_rtp, sph_d_grp,         &
     &      n_vector, fg_trns_SGS%i_SGS_c_flux,                         &
     &      iak_sgs_term%i_SGS_c_flux, wk_sph_sgs, trns_f_SGS)
      end if
!
      end subroutine product_model_coefs_4_sph
!
! ----------------------------------------------------------------------
!
      subroutine sel_product_model_coefs                                &
     &         (const_Csim, sph_rtp, sph_d_grp, numdir,                 &
     &          irtp_sgs, ifld_sgs, wk_sph_sgs, trns_f_SGS)
!
      use prod_SGS_model_coefs_sph
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_dynamic_model_group), intent(in) :: sph_d_grp
      integer(kind = kint), intent(in) :: numdir
!
      integer(kind = kint), intent(in) :: irtp_sgs
      integer(kind = kint), intent(in) :: ifld_sgs
      real(kind = kreal), intent(in) :: const_Csim
!
      type(SPH_dynamic_model_data), intent(inout) :: wk_sph_sgs
      type(spherical_transform_data), intent(inout) :: trns_f_SGS
!
!
      if(sph_rtp%istep_rtp(3) .eq. 1) then
        call product_model_coefs_pin(const_Csim, sph_rtp,               &
     &     sph_d_grp, wk_sph_sgs%fld_coef(1,ifld_sgs),                  &
     &     numdir, trns_f_SGS%fld_rtp(1,irtp_sgs))
      else
        call product_model_coefs_rin(const_Csim, sph_rtp,               &
     &      sph_d_grp, wk_sph_sgs%fld_coef(1,ifld_sgs),                 &
     &      numdir, trns_f_SGS%fld_rtp(1,irtp_sgs))
      end if
!
      end subroutine sel_product_model_coefs
!
! ----------------------------------------------------------------------
!
      end module product_model_coefs_sph
