!>@file   ctl_data_SGS_model_IO.f90
!!@brief  module ctl_data_SGS_model_IO
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2012
!
!>@brief  Structure for SGS model controls
!!
!!@verbatim
!!      subroutine read_sgs_ctl(id_control, hd_block, sgs_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(SGS_model_control), intent(inout) :: sgs_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_sgs_ctl(id_control, hd_block, sgs_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(SGS_model_control), intent(in) :: sgs_ctl
!!        integer(kind = kint), intent(inout) :: level
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    begin SGS_control
!!      SGS_model_ctl           gradient
!!      filtering_scheme_ctl    line
!!   ??   difference_scheme_ctl   original
!!      diff_coef_mode_ctl      layerd
!!      negative_clip_ctl       save
!!      clipping_limit_ctl      0.2
!!      direction_marging_ctl   lsq
!!
!!      array sph_filter_ctl
!!        ...
!!      end array sph_filter_ctl
!!
!!      SGS_buoyancy_Csim_usage   volume
!!      SGS_hf_factor_ctl        0.5
!!      SGS_cf_factor_ctl        0.5
!!      SGS_mf_factor_ctl        0.8
!!
!!      begin filter_files_def
!!        filter_elength_prefix        'filter_elen'
!!        filter_file_prefix           'filter_node'
!!        wider_filter_prefix          'filter/filter_coef_2'
!!
!!        model_coef_rst_prefix        'model_coefs_ini'
!!        commutel_coef_rst_prefix    'commute_coefs_ini'
!!      end  filter_files_def
!!
!!      istep_dynamic_ctl         10
!!      stabilize_weight_ctl      0.6
!!      num_radial_averaging_area   4
!!      num_med_averaging_area      4
!!
!!      min_step_dynamic_ctl      1
!!      max_step_dynamic_ctl      50
!!      delta_to_shrink_ctl      1.0d-2
!!      delta_to_extend_ctl      1.0d-3
!!
!!      array SGS_terms_ctl
!!        SGS_terms_ctl    heat_advect          (heat)
!!        SGS_terms_ctl    composition_advect   (comp_flux)
!!        SGS_terms_ctl    inertia
!!        SGS_terms_ctl    Lorentz_force        (Lorentz)
!!        SGS_terms_ctl    magnetic_induction   (induction)
!!        SGS_terms_ctl    buoyancy             (gravity)
!!      end array SGS_terms_ctl
!!
!!      array commutation_ctl
!!        commutation_ctl    velocity
!!        commutation_ctl    vector_potential
!!        commutation_ctl    temperature
!!        commutation_ctl    composition
!!
!!        commutation_ctl    inertia
!!        commutation_ctl    Lorentz_force        (Lorentz)
!!        commutation_ctl    magnetic_induction   (induction)
!!        commutation_ctl    heat_advect          (heat)
!!        commutation_ctl    composition_advect   (comp_flux)
!!      end array commutation_ctl
!!
!!      begin 3d_filtering_ctl
!!        ...
!!      end 3d_filtering_ctl
!! 
!!  Define by number and starting group of element group list
!!
!!      begin dynamic_model_layer_ctl
!!        layering_data_ctl     ele_group_list
!!        num_layering_grp_ctl     8
!!        start_layering_grp_name_ctl  fluid_layer_1
!!        num_fl_layer_grp_ctl     8
!!        start_fl_layer_grp_name_ctl  fluid_layer_1
!!      end dynamic_model_layer_ctl
!!
!!  Define by explicit element group list
!!
!!      begin dynamic_model_layer_ctl
!!        layering_data_ctl     explicit
!!        array grp_stack_each_layer_ctl
!!          grp_stack_each_layer_ctl  2
!!          grp_stack_each_layer_ctl  4
!!          grp_stack_each_layer_ctl  6
!!          grp_stack_each_layer_ctl  8
!!        end array grp_stack_each_layer_ctl
!!
!!        array layer_grp_name_ctl
!!          layer_grp_name_ctl    fluid_layer_1
!!          layer_grp_name_ctl    fluid_layer_2
!!          layer_grp_name_ctl    fluid_layer_3
!!          layer_grp_name_ctl    fluid_layer_4
!!          layer_grp_name_ctl    fluid_layer_5
!!          layer_grp_name_ctl    fluid_layer_6
!!          layer_grp_name_ctl    fluid_layer_7
!!          layer_grp_name_ctl    fluid_layer_8
!!        end array layer_grp_name_ctl
!!      end dynamic_model_layer_ctl
!!
!!  Define by start and end element address
!!
!!      begin dynamic_model_layer_ctl
!!        layering_data_ctl     start_end
!!      end dynamic_model_layer_ctl
!!
!!    end SGS_control
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module ctl_data_SGS_model_IO
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use skip_comment_f
      use t_read_control_elements
      use t_ctl_data_SGS_model
      use t_ctl_data_SGS_filter
      use t_ctl_data_filter_files
      use t_ctl_data_ele_layering
!
      implicit  none
!
!    4th level for SGS model
!
      character(len=kchara), parameter, private                         &
     &             :: hd_SGS_model =   'SGS_model_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_SGS_filter =  'filtering_scheme_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_DIFF_coefs =  'diff_coef_mode_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_SGS_clips =   'negative_clip_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_SGS_clip_limit = 'clipping_limit_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_SGS_marging = 'direction_marging_ctl'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_sph_filter =  'sph_filter_ctl'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_SGS_buo_Csim_usage = 'SGS_buoyancy_Csim_usage'
      character(len=kchara), parameter, private                         &
     &             :: hd_SGS_hf_factor = 'SGS_hf_factor_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_SGS_cf_factor = 'SGS_cf_factor_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_SGS_mf_factor = 'SGS_mf_factor_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_SGS_mxwl_factor = 'SGS_mxwl_factor_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_SGS_uxb_factor =  'SGS_uxb_factor_ctl'
!
      character(len=kchara), parameter, private                         &
     &         :: hd_filter_fnames = 'filter_files_def'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_istep_dynamic = 'istep_dynamic_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_stabilize_weight = 'stabilize_weight_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_r_ave_area_ctl = 'num_radial_averaging_area'
      character(len=kchara), parameter, private                         &
     &             :: hd_med_ave_area_ctl = 'num_med_averaging_area'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_min_step_dynamic = 'min_step_dynamic_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_max_step_dynamic = 'max_step_dynamic_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_delta_shrink_dynamic = 'delta_to_shrink_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_delta_extend_dynamic = 'delta_to_extend_ctl'
!
      character(len=kchara), parameter, private                         &
     &         :: hd_SGS_terms =  'SGS_terms_ctl'
      character(len=kchara), parameter, private                         &
     &         :: hd_commutation_fld = 'commutation_ctl'
      character(len=kchara), parameter, private                         &
     &         :: hd_3d_filtering = '3d_filtering_ctl'
      character(len=kchara), parameter, private                         &
     &         :: hd_dynamic_layers = 'dynamic_model_layer_ctl'
!
      character(len=kchara), parameter, private                         &
     &         :: hd_SGS_perturbation_ctl = 'SGS_perturbation_ctl'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_model_coef_type_ctl = 'model_coef_type_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_hf_csim_type_ctl = 'heat_flux_csim_type_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_cf_csim_type_ctl = 'comp_flux_csim_type_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_mf_csim_type_ctl = 'mom_flux_csim_type_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_mxwl_csim_type_ctl = 'maxwell_csim_type_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_uxb_csim_type_ctl = 'uxb_csim_type_ctl'
!
      character(len=kchara), parameter, private                         &
     &         :: hd_model_coef_coord_ctl = 'model_coef_coordinate_ctl'
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_sgs_ctl(id_control, hd_block, sgs_ctl, c_buf)
!
      use t_control_array_real
      use ctl_data_SGS_filters_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(SGS_model_control), intent(inout) :: sgs_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(sgs_ctl%i_sgs_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_3d_filtering_ctl                                      &
     &     (id_control, hd_3d_filtering, sgs_ctl%s3df_ctl, c_buf)
        call read_filter_fnames_control                                 &
     &     (id_control, hd_filter_fnames, sgs_ctl%ffile_ctl, c_buf)
        call read_ele_layers_control(id_control, hd_dynamic_layers,     &
     &      sgs_ctl%elayer_ctl, c_buf)
!
!
        call read_control_4_SGS_filters                                 &
     &     (id_control, hd_sph_filter, sgs_ctl, c_buf)
!
        call read_control_array_c1(id_control,                          &
     &      hd_SGS_terms, sgs_ctl%SGS_terms_ctl, c_buf)
        call read_control_array_c1(id_control,                          &
     &      hd_commutation_fld, sgs_ctl%commutate_fld_ctl, c_buf)
!
        call read_chara_ctl_type(c_buf, hd_SGS_model,                   &
     &      sgs_ctl%SGS_model_name_ctl)
        call read_chara_ctl_type(c_buf, hd_SGS_filter,                  &
     &      sgs_ctl%SGS_filter_name_ctl)
        call read_chara_ctl_type(c_buf, hd_DIFF_coefs,                  &
     &      sgs_ctl%DIFF_model_coef_ctl)
!
        call read_chara_ctl_type(c_buf, hd_SGS_clips,                   &
     &      sgs_ctl%SGS_negative_clip_ctl)
        call read_chara_ctl_type(c_buf, hd_SGS_marging,                 &
     &      sgs_ctl%SGS_marging_ctl)
        call read_chara_ctl_type(c_buf, hd_SGS_perturbation_ctl,        &
     &      sgs_ctl%SGS_perturbation_ctl)
        call read_chara_ctl_type(c_buf, hd_model_coef_type_ctl,         &
     &      sgs_ctl%SGS_model_coef_type_ctl)
!
        call read_chara_ctl_type(c_buf, hd_hf_csim_type_ctl,            &
     &      sgs_ctl%heat_flux_csim_type_ctl)
        call read_chara_ctl_type(c_buf, hd_cf_csim_type_ctl,            &
     &      sgs_ctl%comp_flux_csim_type_ctl)
        call read_chara_ctl_type(c_buf, hd_mf_csim_type_ctl,            &
     &      sgs_ctl%mom_flux_csim_type_ctl)
        call read_chara_ctl_type(c_buf, hd_mxwl_csim_type_ctl,          &
     &      sgs_ctl%maxwell_csim_type_ctl)
        call read_chara_ctl_type(c_buf, hd_uxb_csim_type_ctl,           &
     &      sgs_ctl%uxb_csim_type_ctl)
        call read_chara_ctl_type(c_buf, hd_model_coef_coord_ctl,        &
     &      sgs_ctl%SGS_model_coef_coord_ctl)
        call read_chara_ctl_type(c_buf, hd_SGS_buo_Csim_usage,          &
     &      sgs_ctl%SGS_buo_Csim_usage_ctl)
!
!
        call read_real_ctl_type(c_buf, hd_delta_shrink_dynamic,         &
     &      sgs_ctl%delta_to_shrink_dynamic_ctl)
        call read_real_ctl_type(c_buf, hd_SGS_clip_limit,               &
     &      sgs_ctl%clipping_limit_ctl)
!
        call read_real_ctl_type(c_buf, hd_SGS_hf_factor,                &
     &      sgs_ctl%SGS_hf_factor_ctl)
        call read_real_ctl_type(c_buf, hd_SGS_cf_factor,                &
     &      sgs_ctl%SGS_cf_factor_ctl)
        call read_real_ctl_type(c_buf, hd_SGS_mf_factor,                &
     &      sgs_ctl%SGS_mf_factor_ctl)
        call read_real_ctl_type(c_buf, hd_SGS_mxwl_factor,              &
     &      sgs_ctl%SGS_mxwl_factor_ctl)
        call read_real_ctl_type(c_buf, hd_SGS_uxb_factor,               &
     &      sgs_ctl%SGS_uxb_factor_ctl)
!
        call read_real_ctl_type(c_buf, hd_delta_extend_dynamic,         &
     &      sgs_ctl%delta_to_extend_dynamic_ctl)
        call read_real_ctl_type(c_buf, hd_stabilize_weight,             &
     &      sgs_ctl%stabilize_weight_ctl)
!
        call read_integer_ctl_type(c_buf, hd_istep_dynamic,             &
     &      sgs_ctl%istep_dynamic_ctl)
        call read_integer_ctl_type(c_buf, hd_min_step_dynamic,          &
     &      sgs_ctl%min_step_dynamic_ctl)
        call read_integer_ctl_type(c_buf, hd_max_step_dynamic,          &
     &      sgs_ctl%max_step_dynamic_ctl)
!
        call read_integer_ctl_type(c_buf, hd_r_ave_area_ctl,            &
     &      sgs_ctl%ngrp_radial_ave_ctl)
        call read_integer_ctl_type(c_buf, hd_med_ave_area_ctl,          &
     &      sgs_ctl%ngrp_med_ave_ctl)
      end do
      sgs_ctl%i_sgs_ctl = 1
!
      end subroutine read_sgs_ctl
!
!   --------------------------------------------------------------------
!
      subroutine write_sgs_ctl(id_control, hd_block, sgs_ctl, level)
!
      use t_control_array_real
      use ctl_data_SGS_filters_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(SGS_model_control), intent(in) :: sgs_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(sgs_ctl%i_sgs_ctl .le. 0) return
!
      maxlen = len_trim(hd_SGS_model)
      maxlen = max(maxlen, len_trim(hd_SGS_filter))
      maxlen = max(maxlen, len_trim(hd_DIFF_coefs))
      maxlen = max(maxlen, len_trim(hd_SGS_clips))
      maxlen = max(maxlen, len_trim(hd_SGS_marging))
      maxlen = max(maxlen, len_trim(hd_SGS_perturbation_ctl))
      maxlen = max(maxlen, len_trim(hd_model_coef_type_ctl))
      maxlen = max(maxlen, len_trim(hd_hf_csim_type_ctl))
      maxlen = max(maxlen, len_trim(hd_cf_csim_type_ctl))
      maxlen = max(maxlen, len_trim(hd_mf_csim_type_ctl))
      maxlen = max(maxlen, len_trim(hd_mxwl_csim_type_ctl))
      maxlen = max(maxlen, len_trim(hd_uxb_csim_type_ctl))
      maxlen = max(maxlen, len_trim(hd_model_coef_coord_ctl))
      maxlen = max(maxlen, len_trim(hd_SGS_buo_Csim_usage))
      maxlen = max(maxlen, len_trim(hd_delta_shrink_dynamic))
      maxlen = max(maxlen, len_trim(hd_SGS_clip_limit))
      maxlen = max(maxlen, len_trim(hd_SGS_hf_factor))
      maxlen = max(maxlen, len_trim(hd_SGS_cf_factor))
      maxlen = max(maxlen, len_trim(hd_SGS_mf_factor))
      maxlen = max(maxlen, len_trim(hd_SGS_mxwl_factor))
      maxlen = max(maxlen, len_trim(hd_SGS_uxb_factor))
      maxlen = max(maxlen, len_trim(hd_delta_extend_dynamic))
      maxlen = max(maxlen, len_trim(hd_stabilize_weight))
      maxlen = max(maxlen, len_trim(hd_istep_dynamic))
      maxlen = max(maxlen, len_trim(hd_min_step_dynamic))
      maxlen = max(maxlen, len_trim(hd_max_step_dynamic))
      maxlen = max(maxlen, len_trim(hd_r_ave_area_ctl))
      maxlen = max(maxlen, len_trim(hd_med_ave_area_ctl))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_SGS_model, sgs_ctl%SGS_model_name_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_SGS_filter, sgs_ctl%SGS_filter_name_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_DIFF_coefs, sgs_ctl%DIFF_model_coef_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_SGS_clips, sgs_ctl%SGS_negative_clip_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_SGS_clip_limit, sgs_ctl%clipping_limit_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_SGS_marging, sgs_ctl%SGS_marging_ctl)
!
      call write_control_4_SGS_filters                                  &
     &   (id_control, hd_sph_filter, sgs_ctl, level)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_SGS_buo_Csim_usage, sgs_ctl%SGS_buo_Csim_usage_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_SGS_hf_factor, sgs_ctl%SGS_hf_factor_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_SGS_cf_factor, sgs_ctl%SGS_cf_factor_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_SGS_mf_factor, sgs_ctl%SGS_mf_factor_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_SGS_mxwl_factor, sgs_ctl%SGS_mxwl_factor_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_SGS_uxb_factor, sgs_ctl%SGS_uxb_factor_ctl)
!
      call write_filter_fnames_control                                  &
     &   (id_control, hd_filter_fnames, sgs_ctl%ffile_ctl, level)
!
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    hd_istep_dynamic, sgs_ctl%istep_dynamic_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_stabilize_weight, sgs_ctl%stabilize_weight_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    hd_r_ave_area_ctl, sgs_ctl%ngrp_radial_ave_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    hd_med_ave_area_ctl, sgs_ctl%ngrp_med_ave_ctl)
!
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    hd_min_step_dynamic, sgs_ctl%min_step_dynamic_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    hd_max_step_dynamic, sgs_ctl%max_step_dynamic_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_delta_shrink_dynamic,                                      &
     &    sgs_ctl%delta_to_shrink_dynamic_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_delta_extend_dynamic, sgs_ctl%delta_to_extend_dynamic_ctl)
!
      call write_control_array_c1(id_control, level,                    &
     &    hd_SGS_terms, sgs_ctl%SGS_terms_ctl)
      call write_control_array_c1(id_control, level,                    &
     &    hd_commutation_fld, sgs_ctl%commutate_fld_ctl)
      call write_3d_filtering_ctl                                       &
     &   (id_control, hd_3d_filtering, sgs_ctl%s3df_ctl, level)
      call write_ele_layers_control(id_control, hd_dynamic_layers,      &
     &                              sgs_ctl%elayer_ctl, level)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_SGS_perturbation_ctl, sgs_ctl%SGS_perturbation_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_model_coef_type_ctl, sgs_ctl%SGS_model_coef_type_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_hf_csim_type_ctl, sgs_ctl%heat_flux_csim_type_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_cf_csim_type_ctl, sgs_ctl%comp_flux_csim_type_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_mf_csim_type_ctl, sgs_ctl%mom_flux_csim_type_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_mxwl_csim_type_ctl, sgs_ctl%maxwell_csim_type_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_uxb_csim_type_ctl, sgs_ctl%uxb_csim_type_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_model_coef_coord_ctl, sgs_ctl%SGS_model_coef_coord_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_sgs_ctl
!
!   --------------------------------------------------------------------
!
      end module ctl_data_SGS_model_IO
