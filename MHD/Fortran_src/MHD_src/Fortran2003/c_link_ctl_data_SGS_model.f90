!>@file   c_link_ctl_data_SGS_model.f90
!!@brief  module c_link_ctl_data_SGS_model
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for SGS_model_control structure
!!@verbatim
!!      type(c_ptr) function c_SGS_model_ctl_block_name(c_ctl)          &
!!     &          bind(C, NAME = 'c_SGS_model_ctl_block_name')
!!      type(c_ptr) function c_SGS_model_ctl_iflag(c_ctl)               &
!!     &          bind(C, NAME = 'c_SGS_model_ctl_iflag')
!!
!!      type(c_ptr) function c_SGS_model_SGS_model_name_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_SGS_model_SGS_model_name_ctl')
!!      type(c_ptr) function c_SGS_model_filter_name_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_SGS_model_filter_name_ctl')
!!      type(c_ptr) function c_SGS_model_DIFF_model_c_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_SGS_model_DIFF_model_c_ctl')
!!      type(c_ptr) function c_SGS_model_negative_clip_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_SGS_model_negative_clip_ctl')
!!      type(c_ptr) function c_SGS_model_SGS_marging_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_SGS_model_SGS_marging_ctl')
!!      type(c_ptr) function c_SGS_model_perturbation_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_SGS_model_perturbation_ctl')
!!      type(c_ptr) function c_SGS_model_m_coef_type_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_SGS_model_m_coef_type_ctl')
!!      type(c_ptr) function c_SGS_model_hflux_csim_type(c_ctl)         &
!!     &          bind(C, NAME = 'c_SGS_model_hflux_csim_type')
!!      type(c_ptr) function c_SGS_model_cflux_csim_type(c_ctl)         &
!!     &          bind(C, NAME = 'c_SGS_model_cflux_csim_type')
!!      type(c_ptr) function c_SGS_model_mflux_csim_type(c_ctl)         &
!!     &          bind(C, NAME = 'c_SGS_model_mflux_csim_type')
!!      type(c_ptr) function c_SGS_model_maxwell_csim_type(c_ctl)       &
!!     &          bind(C, NAME = 'c_SGS_model_maxwell_csim_type')
!!      type(c_ptr) function c_SGS_model_uxb_csim_type_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_SGS_model_uxb_csim_type_ctl')
!!      type(c_ptr) function c_SGS_model_coef_coord_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_SGS_model_coef_coord_ctl')
!!      type(c_ptr) function c_SGS_model_SGS_buo_Csim_usage(c_ctl)      &
!!     &          bind(C, NAME = 'c_SGS_model_SGS_buo_Csim_usage')
!!      type(c_ptr) function c_SGS_model_istep_dynamic_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_SGS_model_istep_dynamic_ctl')
!!      type(c_ptr) function c_SGS_model_min_step_dynamic(c_ctl)        &
!!     &          bind(C, NAME = 'c_SGS_model_min_step_dynamic')
!!      type(c_ptr) function c_SGS_model_max_step_dynamic(c_ctl)        &
!!     &          bind(C, NAME = 'c_SGS_model_max_step_dynamic')
!!      type(c_ptr) function c_SGS_model_stabilize_weight(c_ctl)        &
!!     &          bind(C, NAME = 'c_SGS_model_stabilize_weight')
!!      type(c_ptr) function c_SGS_model_del_shrink_dynamic(c_ctl)      &
!!     &          bind(C, NAME = 'c_SGS_model_del_shrink_dynamic')
!!      type(c_ptr) function c_SGS_model_del_extend_dynamic(c_ctl)      &
!!     &          bind(C, NAME = 'c_SGS_model_del_extend_dynamic')
!!      type(c_ptr) function c_SGS_model_ngrp_radial_ave(c_ctl)         &
!!     &          bind(C, NAME = 'c_SGS_model_ngrp_radial_ave')
!!      type(c_ptr) function c_SGS_model_ngrp_med_ave_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_SGS_model_ngrp_med_ave_ctl')
!!      type(c_ptr) function c_SGS_model_clipping_limit_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_SGS_model_clipping_limit_ctl')
!!!!
!!      type(c_ptr) function c_SGS_model_SGS_hf_factor_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_SGS_model_SGS_hf_factor_ctl')
!!      type(c_ptr) function c_SGS_model_SGS_cf_factor_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_SGS_model_SGS_cf_factor_ctl')
!!      type(c_ptr) function c_SGS_model_SGS_mf_factor_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_SGS_model_SGS_mf_factor_ctl')
!!      type(c_ptr) function c_SGS_model_mxwl_factor_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_SGS_model_mxwl_factor_ctl')
!!      type(c_ptr) function c_SGS_model_SGS_uxb_factor_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_SGS_model_SGS_uxb_factor_ctl')
!!      type(c_ptr) function c_SGS_model_SGS_terms_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_SGS_model_SGS_terms_ctl')
!!      type(c_ptr) function c_SGS_model_commutate_fld_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_SGS_model_commutate_fld_ctl')
!!      type(c_ptr) function c_SGS_model_ffile_ctl(c_ctl)               &
!!     &          bind(C, NAME = 'c_SGS_model_ffile_ctl')
!!      type(c_ptr) function c_SGS_model_elayer_ctl(c_ctl)              &
!!     &          bind(C, NAME = 'c_SGS_model_elayer_ctl')
!!      type(c_ptr) function c_SGS_model_s3df_ctl(c_ctl)                &
!!     &          bind(C, NAME = 'c_SGS_model_s3df_ctl')
!!      type(c_ptr) function c_SGS_model_num_sph_filter_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_SGS_model_num_sph_filter_ctl')
!!      type(c_ptr) function c_SGS_model_sph_filter_ctl(i, c_ctl)       &
!!     &          bind(C, NAME = 'c_SGS_model_sph_filter_ctl')
!!        integer(c_int), value, intent(in) :: i
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
!!
      module c_link_ctl_data_SGS_model
!
      use iso_c_binding
      use t_ctl_data_SGS_model
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_ctl_block_name(c_ctl)            &
     &          bind(C, NAME = 'c_SGS_model_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_SGS_model_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_ctl_iflag(c_ctl)                 &
     &          bind(C, NAME = 'c_SGS_model_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_ctl_iflag = C_loc(f_ctl%i_sgs_ctl)
      end function c_SGS_model_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_SGS_model_name_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_SGS_model_SGS_model_name_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_SGS_model_name_ctl = C_loc(f_ctl%SGS_model_name_ctl)
      end function c_SGS_model_SGS_model_name_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_filter_name_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_SGS_model_filter_name_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_filter_name_ctl = C_loc(f_ctl%SGS_filter_name_ctl)
      end function c_SGS_model_filter_name_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_DIFF_model_c_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_SGS_model_DIFF_model_c_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_DIFF_model_c_ctl = C_loc(f_ctl%DIFF_model_coef_ctl)
      end function c_SGS_model_DIFF_model_c_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_negative_clip_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_SGS_model_negative_clip_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_negative_clip_ctl= C_loc(f_ctl%SGS_negative_clip_ctl)
      end function c_SGS_model_negative_clip_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_SGS_marging_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_SGS_model_SGS_marging_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_SGS_marging_ctl = C_loc(f_ctl%SGS_marging_ctl)
      end function c_SGS_model_SGS_marging_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_perturbation_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_SGS_model_perturbation_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_perturbation_ctl = C_loc(f_ctl%SGS_perturbation_ctl)
      end function c_SGS_model_perturbation_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_m_coef_type_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_SGS_model_m_coef_type_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_m_coef_type_ctl= C_loc(f_ctl%SGS_model_coef_type_ctl)
      end function c_SGS_model_m_coef_type_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_hflux_csim_type(c_ctl)           &
     &          bind(C, NAME = 'c_SGS_model_hflux_csim_type')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_hflux_csim_type= C_loc(f_ctl%heat_flux_csim_type_ctl)
      end function c_SGS_model_hflux_csim_type
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_cflux_csim_type(c_ctl)           &
     &          bind(C, NAME = 'c_SGS_model_cflux_csim_type')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_cflux_csim_type= C_loc(f_ctl%comp_flux_csim_type_ctl)
      end function c_SGS_model_cflux_csim_type
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_mflux_csim_type(c_ctl)           &
     &          bind(C, NAME = 'c_SGS_model_mflux_csim_type')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_mflux_csim_type                                       &
     &                 = C_loc(f_ctl%mom_flux_csim_type_ctl)
      end function c_SGS_model_mflux_csim_type
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_maxwell_csim_type(c_ctl)         &
     &          bind(C, NAME = 'c_SGS_model_maxwell_csim_type')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_maxwell_csim_type                                     &
     &                 = C_loc(f_ctl%maxwell_csim_type_ctl)
      end function c_SGS_model_maxwell_csim_type
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_uxb_csim_type_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_SGS_model_uxb_csim_type_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_uxb_csim_type_ctl                                     &
     &                 = C_loc(f_ctl%uxb_csim_type_ctl)
      end function c_SGS_model_uxb_csim_type_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_coef_coord_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_SGS_model_coef_coord_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_coef_coord_ctl                                        &
     &                 = C_loc(f_ctl%SGS_model_coef_coord_ctl)
      end function c_SGS_model_coef_coord_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_SGS_buo_Csim_usage(c_ctl)        &
     &          bind(C, NAME = 'c_SGS_model_SGS_buo_Csim_usage')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_SGS_buo_Csim_usage                                    &
     &                 = C_loc(f_ctl%SGS_buo_Csim_usage_ctl)
      end function c_SGS_model_SGS_buo_Csim_usage
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_istep_dynamic_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_SGS_model_istep_dynamic_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_istep_dynamic_ctl = C_loc(f_ctl%istep_dynamic_ctl)
      end function c_SGS_model_istep_dynamic_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_min_step_dynamic(c_ctl)          &
     &          bind(C, NAME = 'c_SGS_model_min_step_dynamic')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_min_step_dynamic = C_loc(f_ctl%min_step_dynamic_ctl)
      end function c_SGS_model_min_step_dynamic
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_max_step_dynamic(c_ctl)          &
     &          bind(C, NAME = 'c_SGS_model_max_step_dynamic')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_max_step_dynamic = C_loc(f_ctl%max_step_dynamic_ctl)
      end function c_SGS_model_max_step_dynamic
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_stabilize_weight(c_ctl)          &
     &          bind(C, NAME = 'c_SGS_model_stabilize_weight')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_stabilize_weight = C_loc(f_ctl%stabilize_weight_ctl)
      end function c_SGS_model_stabilize_weight
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_del_shrink_dynamic(c_ctl)        &
     &          bind(C, NAME = 'c_SGS_model_del_shrink_dynamic')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_del_shrink_dynamic                                    &
     &               = C_loc(f_ctl%delta_to_shrink_dynamic_ctl)
      end function c_SGS_model_del_shrink_dynamic
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_del_extend_dynamic(c_ctl)        &
     &          bind(C, NAME = 'c_SGS_model_del_extend_dynamic')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_del_extend_dynamic                                    &
     &                = C_loc(f_ctl%delta_to_extend_dynamic_ctl)
      end function c_SGS_model_del_extend_dynamic
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_ngrp_radial_ave(c_ctl)           &
     &          bind(C, NAME = 'c_SGS_model_ngrp_radial_ave')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_ngrp_radial_ave = C_loc(f_ctl%ngrp_radial_ave_ctl)
      end function c_SGS_model_ngrp_radial_ave
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_ngrp_med_ave_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_SGS_model_ngrp_med_ave_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_ngrp_med_ave_ctl = C_loc(f_ctl%ngrp_med_ave_ctl)
      end function c_SGS_model_ngrp_med_ave_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_clipping_limit_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_SGS_model_clipping_limit_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_clipping_limit_ctl = C_loc(f_ctl%clipping_limit_ctl)
      end function c_SGS_model_clipping_limit_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_SGS_hf_factor_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_SGS_model_SGS_hf_factor_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_SGS_hf_factor_ctl = C_loc(f_ctl%SGS_hf_factor_ctl)
      end function c_SGS_model_SGS_hf_factor_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_SGS_cf_factor_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_SGS_model_SGS_cf_factor_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_SGS_cf_factor_ctl = C_loc(f_ctl%SGS_cf_factor_ctl)
      end function c_SGS_model_SGS_cf_factor_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_SGS_mf_factor_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_SGS_model_SGS_mf_factor_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_SGS_mf_factor_ctl = C_loc(f_ctl%SGS_mf_factor_ctl)
      end function c_SGS_model_SGS_mf_factor_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_mxwl_factor_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_SGS_model_mxwl_factor_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_mxwl_factor_ctl = C_loc(f_ctl%SGS_mxwl_factor_ctl)
      end function c_SGS_model_mxwl_factor_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_SGS_uxb_factor_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_SGS_model_SGS_uxb_factor_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_SGS_uxb_factor_ctl = C_loc(f_ctl%SGS_uxb_factor_ctl)
      end function c_SGS_model_SGS_uxb_factor_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_SGS_terms_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_SGS_model_SGS_terms_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_SGS_terms_ctl = C_loc(f_ctl%SGS_terms_ctl)
      end function c_SGS_model_SGS_terms_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_commutate_fld_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_SGS_model_commutate_fld_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_commutate_fld_ctl = C_loc(f_ctl%commutate_fld_ctl)
      end function c_SGS_model_commutate_fld_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_ffile_ctl(c_ctl)                 &
     &          bind(C, NAME = 'c_SGS_model_ffile_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_ffile_ctl = C_loc(f_ctl%ffile_ctl)
      end function c_SGS_model_ffile_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_elayer_ctl(c_ctl)                &
     &          bind(C, NAME = 'c_SGS_model_elayer_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_elayer_ctl = C_loc(f_ctl%elayer_ctl)
      end function c_SGS_model_elayer_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_s3df_ctl(c_ctl)                  &
     &          bind(C, NAME = 'c_SGS_model_s3df_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_s3df_ctl = C_loc(f_ctl%s3df_ctl)
      end function c_SGS_model_s3df_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_num_sph_filter_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_SGS_model_num_sph_filter_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_num_sph_filter_ctl = C_loc(f_ctl%num_sph_filter_ctl)
      end function c_SGS_model_num_sph_filter_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_sph_filter_ctl(i, c_ctl)         &
     &          bind(C, NAME = 'c_SGS_model_sph_filter_ctl')
      integer(c_int), value, intent(in) :: i
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_sph_filter_ctl = C_loc(f_ctl%sph_filter_ctl(i))
      end function c_SGS_model_sph_filter_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_ctl_data_SGS_model
