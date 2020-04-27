!>@file   nolinear_gradient_terms_sph.f90
!!@brief  module nolinear_gradient_terms_sph
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Evaluate nonlinear terms in spherical coordinate grid
!!
!!@verbatim
!!      subroutine nl_gradient_SGS_terms_rtp(sph, sph_filters, MHD_prop,&
!!     &          b_trns_base, bn_trns_grd, bn_trns_dv, fg_trns_SGS,    &
!!     &          trns_b_MHD, trns_b_NLGD, trns_f_SGS)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_group_data), intent(in) :: sph_grps
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(base_field_address), intent(in) :: b_trns_base
!!        type(gradient_field_address), intent(in) :: bn_trns_grd
!!        type(diff_vector_address), intent(in) :: bn_trns_dv
!!        type(SGS_term_address), intent(in) :: fg_trns_SGS
!!        type(spherical_transform_data), intent(in) :: trns_b_MHD
!!        type(spherical_transform_data), intent(in) :: trns_b_NLGD
!!        type(spherical_transform_data), intent(inout) :: trns_f_SGS
!!
!!      subroutine wider_nl_grad_SGS_rtp(sph, wide_filters, MHD_prop,   &
!!     &          bg_trns_fil, bd_trns_gfs, bd_trns_dfv, bd_trns_wSGS,  &
!!     &          fd_trns_wSGS, trns_b_SGS, trns_b_DYNG, trns_b_DYNS)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_group_data), intent(in) :: sph_grps
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(base_field_address), intent(in) :: bg_trns_fil
!!        type(gradient_field_address), intent(in) :: bd_trns_gfs
!!        type(diff_vector_address), intent(in) :: bd_trns_dfv
!!        type(SGS_term_address), intent(in) :: bd_trns_wSGS
!!        type(spherical_transform_data), intent(in) :: trns_b_SGS
!!        type(spherical_transform_data), intent(in) :: trns_b_DYNG
!!        type(spherical_transform_data), intent(inout) :: trns_b_DYNS
!!@endverbatim
!
      module nolinear_gradient_terms_sph
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      use t_control_parameter
      use t_spheric_parameter
      use t_spheric_rtp_data
      use t_base_field_labels
      use t_grad_field_labels
      use t_diff_vector_labels
      use t_SGS_term_labels
      use t_addresses_sph_transform
      use t_sph_filtering_data
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
!>      Get @f$ e_{ijk} \overline{\tilde{\omega}_{j}\tilde{u}_{k}}
!!             - e_{ijk} \overline{\tilde{\omega}}_{j}
!!                      \overline{\tilde{u}}_{k} @f$,
!!      @f$ e_{ijk}\overline{\tilde{J}_{j} \tilde{B}_{k}}
!!         - e_{ijk}\overline{\tilde{J}}_{j}\overline{\tilde{B}}_{k} @f$,
!!      @f$ e_{ijk}\overline{\tilde{u}_{j} \tilde{B}_{k}}
!!         - e_{ijk}\overline{\tilde{u}}_{j}\overline{\tilde{B}}_{k} @f$,
!!      @f$ e_{ijk}\overline{\tilde{u}_{j} \tilde{T}}
!!         - e_{ijk}\overline{\tilde{u}}_{j}\overline{\tilde{T}} @f$, and
!!      @f$ e_{ijk}\overline{\tilde{u}_{j} \tilde{C}}
!!         - e_{ijk}\overline{\tilde{u}}_{j}\overline{\tilde{C}} @f$,
      subroutine nl_gradient_SGS_terms_rtp(sph, sph_filters, MHD_prop,  &
     &          b_trns_base, bn_trns_grd, bn_trns_dv, fg_trns_SGS,      &
     &          trns_b_MHD, trns_b_NLGD, trns_f_SGS)
!
      use sel_sph_SGS_nl_gradient
!
      type(sph_filters_type), intent(in) :: sph_filters
      type(sph_grids), intent(in) :: sph
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(base_field_address), intent(in) :: b_trns_base
      type(gradient_field_address), intent(in) :: bn_trns_grd
      type(diff_vector_address), intent(in) :: bn_trns_dv
      type(SGS_term_address), intent(in) :: fg_trns_SGS
      type(spherical_transform_data), intent(in) :: trns_b_MHD
      type(spherical_transform_data), intent(in) :: trns_b_NLGD
!
      type(spherical_transform_data), intent(inout) :: trns_f_SGS
!
!
      if(fg_trns_SGS%i_SGS_inertia .gt. 0) then
        call sel_sph_SGS_induct_nl_gradient                             &
     &     (sph%sph_rtp, sph_filters, MHD_prop%fl_prop%coef_velo,       &
     &      b_trns_base%i_vort, b_trns_base%i_velo,                     &
     &      trns_b_MHD%ncomp, trns_b_MHD%fld_rtp,                       &
     &      bn_trns_dv%i_grad_wx, bn_trns_dv%i_grad_wy,                 &
     &      bn_trns_dv%i_grad_wz, bn_trns_dv%i_grad_vx,                 &
     &      bn_trns_dv%i_grad_vy, bn_trns_dv%i_grad_vz,                 &
     &      trns_b_NLGD%ncomp, trns_b_NLGD%fld_rtp,                     &
     &      fg_trns_SGS%i_SGS_inertia, trns_f_SGS%ncomp,                &
     &      trns_f_SGS%fld_rtp)
      end if
!
      if(fg_trns_SGS%i_SGS_Lorentz .gt. 0) then
        call sel_sph_SGS_induct_nl_gradient                             &
     &     (sph%sph_rtp, sph_filters, MHD_prop%fl_prop%coef_lor,        &
     &      b_trns_base%i_current, b_trns_base%i_magne,                 &
     &      trns_b_MHD%ncomp, trns_b_MHD%fld_rtp,                       &
     &      bn_trns_dv%i_grad_jx, bn_trns_dv%i_grad_jy,                 &
     &      bn_trns_dv%i_grad_jz, bn_trns_dv%i_grad_bx,                 &
     &      bn_trns_dv%i_grad_by, bn_trns_dv%i_grad_bz,                 &
     &      trns_b_NLGD%ncomp, trns_b_NLGD%fld_rtp,                     &
     &      fg_trns_SGS%i_SGS_Lorentz, trns_f_SGS%ncomp,                &
     &      trns_f_SGS%fld_rtp)
      end if
!
      if(fg_trns_SGS%i_SGS_vp_induct .gt. 0) then
        call sel_sph_SGS_induct_nl_gradient                             &
     &     (sph%sph_rtp, sph_filters, MHD_prop%cd_prop%coef_induct,     &
     &      b_trns_base%i_velo, b_trns_base%i_magne,                    &
     &      trns_b_MHD%ncomp, trns_b_MHD%fld_rtp,                       &
     &      bn_trns_dv%i_grad_vx, bn_trns_dv%i_grad_vy,                 &
     &      bn_trns_dv%i_grad_vz, bn_trns_dv%i_grad_bx,                 &
     &      bn_trns_dv%i_grad_by, bn_trns_dv%i_grad_bz,                 &
     &      trns_b_NLGD%ncomp, trns_b_NLGD%fld_rtp,                     &
     &      fg_trns_SGS%i_SGS_vp_induct, trns_f_SGS%ncomp,              &
     &      trns_f_SGS%fld_rtp)
      end if
!
      if(fg_trns_SGS%i_SGS_h_flux .gt. 0) then
        call sel_SGS_s_flux_nl_gradient                                 &
     &     (sph%sph_rtp, sph_filters, MHD_prop%ht_prop%coef_advect,     &
     &      b_trns_base%i_velo, trns_b_MHD%ncomp, trns_b_MHD%fld_rtp,   &
     &      bn_trns_dv%i_grad_vx, bn_trns_dv%i_grad_vy,                 &
     &      bn_trns_dv%i_grad_vz, bn_trns_grd%i_grad_temp,              &
     &      trns_b_NLGD%ncomp, trns_b_NLGD%fld_rtp,                     &
     &      fg_trns_SGS%i_SGS_h_flux,                                   &
     &      trns_f_SGS%ncomp, trns_f_SGS%fld_rtp)
      end if
!
      if(fg_trns_SGS%i_SGS_c_flux .gt. 0) then
        call sel_SGS_s_flux_nl_gradient                                 &
     &     (sph%sph_rtp, sph_filters, MHD_prop%cp_prop%coef_advect,     &
     &      b_trns_base%i_velo, trns_b_MHD%ncomp, trns_b_MHD%fld_rtp,   &
     &      bn_trns_dv%i_grad_vx, bn_trns_dv%i_grad_vy,                 &
     &      bn_trns_dv%i_grad_vz, bn_trns_grd%i_grad_composit,          &
     &      trns_b_NLGD%ncomp, trns_b_NLGD%fld_rtp,                     &
     &      fg_trns_SGS%i_SGS_c_flux,                                   &
     &      trns_f_SGS%ncomp, trns_f_SGS%fld_rtp)
      end if
!
      end subroutine nl_gradient_SGS_terms_rtp
!
!-----------------------------------------------------------------------
!
!>      Get @f$ e_{ijk} \overline{\tilde{\omega}_{j}\tilde{u}_{k}}
!!             - e_{ijk} \overline{\tilde{\omega}}_{j}
!!                      \overline{\tilde{u}}_{k} @f$,
!!      @f$ e_{ijk}\overline{\tilde{J}_{j} \tilde{B}_{k}}
!!         - e_{ijk}\overline{\tilde{J}}_{j}\overline{\tilde{B}}_{k} @f$,
!!      @f$ e_{ijk}\overline{\tilde{u}_{j} \tilde{B}_{k}}
!!         - e_{ijk}\overline{\tilde{u}}_{j}\overline{\tilde{B}}_{k} @f$,
!!      @f$ e_{ijk}\overline{\tilde{u}_{j} \tilde{T}}
!!         - e_{ijk}\overline{\tilde{u}}_{j}\overline{\tilde{T}} @f$, and
!!      @f$ e_{ijk}\overline{\tilde{u}_{j} \tilde{C}}
!!         - e_{ijk}\overline{\tilde{u}}_{j}\overline{\tilde{C}} @f$,
      subroutine wider_nl_grad_SGS_rtp(sph, wide_filters, MHD_prop,     &
     &          bg_trns_fil, bd_trns_gfs, bd_trns_dfv, bd_trns_wSGS,    &
     &          trns_b_SGS, trns_b_DYNG, trns_b_DYNS)
!
      use sel_sph_SGS_nl_gradient
!
      type(sph_filters_type), intent(in) :: wide_filters
      type(sph_grids), intent(in) :: sph
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(base_field_address), intent(in) :: bg_trns_fil
      type(gradient_field_address), intent(in) :: bd_trns_gfs
      type(diff_vector_address), intent(in) :: bd_trns_dfv
      type(SGS_term_address), intent(in) :: bd_trns_wSGS
      type(spherical_transform_data), intent(in) :: trns_b_SGS
      type(spherical_transform_data), intent(in) :: trns_b_DYNG
!
      type(spherical_transform_data), intent(inout) :: trns_b_DYNS
!
!
      if(bd_trns_wSGS%i_SGS_inertia .gt. 0) then
        call sel_sph_SGS_induct_nl_gradient                             &
     &     (sph%sph_rtp, wide_filters, MHD_prop%fl_prop%coef_velo,      &
     &      bg_trns_fil%i_vort, bg_trns_fil%i_velo,                     &
     &      trns_b_SGS%ncomp, trns_b_SGS%fld_rtp,                       &
     &      bd_trns_dfv%i_grad_wx, bd_trns_dfv%i_grad_wy,               &
     &      bd_trns_dfv%i_grad_wz, bd_trns_dfv%i_grad_vx,               &
     &      bd_trns_dfv%i_grad_vy, bd_trns_dfv%i_grad_vz,               &
     &      trns_b_DYNG%ncomp, trns_b_DYNG%fld_rtp,                     &
     &      bd_trns_wSGS%i_SGS_inertia, trns_b_DYNS%ncomp,              &
     &      trns_b_DYNS%fld_rtp)
      end if
!
      if(bd_trns_wSGS%i_SGS_Lorentz .gt. 0) then
        call sel_sph_SGS_induct_nl_gradient                             &
     &     (sph%sph_rtp, wide_filters, MHD_prop%fl_prop%coef_lor,       &
     &      bg_trns_fil%i_current, bg_trns_fil%i_magne,                 &
     &      trns_b_SGS%ncomp, trns_b_SGS%fld_rtp,                       &
     &      bd_trns_dfv%i_grad_jx, bd_trns_dfv%i_grad_jy,               &
     &      bd_trns_dfv%i_grad_jz, bd_trns_dfv%i_grad_bx,               &
     &      bd_trns_dfv%i_grad_by, bd_trns_dfv%i_grad_bz,               &
     &      trns_b_DYNG%ncomp, trns_b_DYNG%fld_rtp,                     &
     &      bd_trns_wSGS%i_SGS_Lorentz, trns_b_DYNS%ncomp,              &
     &      trns_b_DYNS%fld_rtp)
      end if
!
      if(bd_trns_wSGS%i_SGS_vp_induct .gt. 0) then
        call sel_sph_SGS_induct_nl_gradient                             &
     &     (sph%sph_rtp, wide_filters, MHD_prop%cd_prop%coef_induct,    &
     &      bg_trns_fil%i_velo, bg_trns_fil%i_magne,                    &
     &      trns_b_SGS%ncomp, trns_b_SGS%fld_rtp,                       &
     &      bd_trns_dfv%i_grad_vx, bd_trns_dfv%i_grad_vy,               &
     &      bd_trns_dfv%i_grad_vz, bd_trns_dfv%i_grad_bx,               &
     &      bd_trns_dfv%i_grad_by, bd_trns_dfv%i_grad_bz,               &
     &      trns_b_DYNG%ncomp, trns_b_DYNG%fld_rtp,                     &
     &      bd_trns_wSGS%i_SGS_vp_induct, trns_b_DYNS%ncomp,            &
     &      trns_b_DYNS%fld_rtp)
      end if
!
      if(bd_trns_wSGS%i_SGS_h_flux .gt. 0) then
        call sel_SGS_s_flux_nl_gradient(sph%sph_rtp, wide_filters,      &
     &      MHD_prop%ht_prop%coef_advect, bg_trns_fil%i_velo,           &
     &      trns_b_SGS%ncomp, trns_b_SGS%fld_rtp,                       &
     &      bd_trns_dfv%i_grad_vx, bd_trns_dfv%i_grad_vy,               &
     &      bd_trns_dfv%i_grad_vz, bd_trns_gfs%i_grad_temp,             &
     &      trns_b_DYNG%ncomp, trns_b_DYNG%fld_rtp,                     &
     &      bd_trns_wSGS%i_SGS_h_flux, trns_b_DYNS%ncomp,               &
     &      trns_b_DYNS%fld_rtp)
      end if
!
      if(bd_trns_wSGS%i_SGS_c_flux .gt. 0) then
        call sel_SGS_s_flux_nl_gradient(sph%sph_rtp, wide_filters,      &
     &      MHD_prop%cp_prop%coef_advect, bg_trns_fil%i_velo,           &
     &      trns_b_SGS%ncomp, trns_b_SGS%fld_rtp,                       &
     &      bd_trns_dfv%i_grad_vx, bd_trns_dfv%i_grad_vy,               &
     &      bd_trns_dfv%i_grad_vz, bd_trns_gfs%i_grad_composit,         &
     &      trns_b_DYNG%ncomp, trns_b_DYNG%fld_rtp,                     &
     &      bd_trns_wSGS%i_SGS_c_flux, trns_b_DYNS%ncomp,               &
     &      trns_b_DYNS%fld_rtp)
      end if
!
      end subroutine wider_nl_grad_SGS_rtp
!
!-----------------------------------------------------------------------
!
      end module nolinear_gradient_terms_sph
