!>@file   read_ctl_data_SGS_model.f90
!!@brief  module read_ctl_data_SGS_model
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2012
!
!>@brief  Structure for SGS model controls
!!
!!@verbatim
!!      subroutine read_sgs_ctl                                         &
!!     &         (id_control, hd_block, sgs_ctl, c_buf)
!!      subroutine bcast_sgs_ctl(sgs_ctl)
!!      subroutine reset_sgs_ctl(sgs_ctl)
!!        type(SGS_model_control), intent(inout) :: sgs_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    begin SGS_control
!!      SGS_model_ctl           gradient
!!      filtering_scheme_ctl    line
!!      difference_scheme_ctl   original
!!      diff_coef_mode_ctl      layerd
!!      negative_clip_ctl       save
!!      clipping_limit_ctl      0.2
!!      direction_marging_ctl   lsq
!!
!!      SGS_buoyancy_Csim_usage   volume
!!
!!      array sph_filter_ctl
!!        ...
!!      end array sph_filter_ctl
!!
!!
!!      SGS_hf_factor_ctl        0.5
!!      SGS_cf_factor_ctl        0.5
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
!!        SGS_terms_ctl    heat
!!        SGS_terms_ctl    parturbation_heat
!!        SGS_terms_ctl    inertia
!!        SGS_terms_ctl    gravity
!!        SGS_terms_ctl    Lorentz
!!        SGS_terms_ctl    induction
!!      end array SGS_terms_ctl
!!
!!      array commutation_ctl
!!        commutation_ctl    velocity
!!        commutation_ctl    vector_potential
!!        commutation_ctl    temperature
!!        commutation_ctl    dumnmy_scalar
!!
!!        commutation_ctl    heat
!!        commutation_ctl    inertia
!!        commutation_ctl    Lorentz
!!        commutation_ctl    induction
!!        commutation_ctl    composit_flux
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
      module read_ctl_data_SGS_model
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
      character(len=kchara), parameter                                  &
     &             :: hd_SGS_model =   'SGS_model_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_SGS_filter =  'filtering_scheme_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_SGS_clips =   'negative_clip_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_SGS_clip_limit = 'clipping_limit_ctl'
!
      character(len=kchara), parameter                                  &
     &             :: hd_SGS_hf_factor = 'SGS_hf_factor_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_SGS_cf_factor = 'SGS_cf_factor_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_SGS_mf_factor = 'SGS_mf_factor_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_SGS_mxwl_factor = 'SGS_mxwl_factor_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_SGS_uxb_factor =  'SGS_uxb_factor_ctl'
!
!
      character(len=kchara), parameter                                  &
     &             :: hd_SGS_marging = 'direction_marging_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_DIFF_coefs =  'diff_coef_mode_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_3d_filtering = '3d_filtering_ctl'
      character(len=kchara), parameter :: hd_istep_dynamic              &
     &                        = 'istep_dynamic_ctl'
      character(len=kchara), parameter :: hd_stabilize_weight           &
     &                        = 'stabilize_weight_ctl'
      character(len=kchara), parameter :: hd_min_step_dynamic           &
     &                        = 'min_step_dynamic_ctl'
      character(len=kchara), parameter :: hd_max_step_dynamic           &
     &                        = 'max_step_dynamic_ctl'
      character(len=kchara), parameter :: hd_delta_shrink_dynamic       &
     &                        = 'delta_to_shrink_ctl'
      character(len=kchara), parameter :: hd_delta_extend_dynamic       &
     &                        = 'delta_to_extend_ctl'
      character(len=kchara), parameter :: hd_SGS_perturbation_ctl       &
     &                        = 'SGS_perturbation_ctl'
!
      character(len=kchara), parameter :: hd_r_ave_area_ctl             &
     &                        = 'num_radial_averaging_area'
      character(len=kchara), parameter :: hd_med_ave_area_ctl           &
     &                        = 'num_med_averaging_area'
!
      character(len=kchara), parameter :: hd_model_coef_type_ctl        &
     &                        = 'model_coef_type_ctl'
      character(len=kchara), parameter :: hd_hf_csim_type_ctl           &
     &                        = 'heat_flux_csim_type_ctl'
      character(len=kchara), parameter :: hd_cf_csim_type_ctl           &
     &                        = 'comp_flux_csim_type_ctl'
      character(len=kchara), parameter :: hd_mf_csim_type_ctl           &
     &                        = 'mom_flux_csim_type_ctl'
      character(len=kchara), parameter :: hd_mxwl_csim_type_ctl         &
     &                        = 'maxwell_csim_type_ctl'
      character(len=kchara), parameter :: hd_uxb_csim_type_ctl          &
     &                        = 'uxb_csim_type_ctl'
!
      character(len=kchara), parameter :: hd_model_coef_coord_ctl       &
     &                        = 'model_coef_coordinate_ctl'
      character(len=kchara), parameter :: hd_SGS_buo_Csim_usage         &
     &                        = 'SGS_buoyancy_Csim_usage'
      character(len=kchara), parameter :: hd_commutation_fld            &
     &                        = 'commutation_ctl'
!
      integer (kind=kint) :: i_3d_filtering =   0
!
      character(len=kchara) :: hd_SGS_terms =  'SGS_terms_ctl'
      character(len=kchara) :: hd_sph_filter =  'sph_filter_ctl'
!
!    5th level for 3d filtering
!
      character(len=kchara), parameter :: hd_dynamic_layers             &
     &                        = 'dynamic_model_layer_ctl'
!
      character(len=kchara), parameter :: hd_filter_fnames              &
     &                        = 'filter_files_def'
!
      private :: hd_SGS_filter, hd_SGS_model
      private :: hd_SGS_clips, hd_SGS_clip_limit
      private :: hd_SGS_mf_factor, hd_SGS_mxwl_factor
      private :: hd_SGS_uxb_factor, hd_SGS_hf_factor
      private :: hd_SGS_marging, hd_DIFF_coefs, hd_3d_filtering
      private :: hd_istep_dynamic, hd_stabilize_weight
      private :: hd_min_step_dynamic, hd_max_step_dynamic
      private :: hd_r_ave_area_ctl, hd_med_ave_area_ctl
      private :: hd_delta_shrink_dynamic, hd_delta_extend_dynamic
      private :: hd_SGS_terms, hd_SGS_perturbation_ctl, hd_sph_filter
      private :: hd_model_coef_type_ctl, hd_model_coef_coord_ctl
      private :: hd_commutation_fld, hd_SGS_buo_Csim_usage
      private :: hd_hf_csim_type_ctl, hd_mf_csim_type_ctl
      private :: hd_mxwl_csim_type_ctl, hd_uxb_csim_type_ctl
!
      private :: read_control_4_SGS_filters
      private :: hd_dynamic_layers
!
      private :: hd_filter_fnames
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_sgs_ctl                                           &
     &         (id_control, hd_block, sgs_ctl, c_buf)
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
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_3d_filtering_ctl(id_control, hd_3d_filtering,         &
     &      i_3d_filtering, sgs_ctl%s3df_ctl, c_buf)
        call read_filter_fnames_control                                 &
     &     (id_control, hd_filter_fnames, sgs_ctl%ffile_ctl, c_buf)
        call read_ele_layers_control(id_control, hd_dynamic_layers,     &
     &      sgs_ctl%elayer_ctl, c_buf)
!
!
        if(check_array_flag(c_buf, hd_sph_filter)) then
          call read_control_4_SGS_filters                               &
     &       (id_control, hd_sph_filter, sgs_ctl, c_buf)
        end if
!
        call read_control_array_c1(id_control,                          &
     &      hd_SGS_terms, sgs_ctl%SGS_terms_ctl, c_buf)
        call read_control_array_c1(id_control,                          &
     &      hd_commutation_fld, sgs_ctl%commutate_fld_ctl, c_buf)
!
        call read_chara_ctl_type(c_buf, hd_SGS_model,                  &
     &      sgs_ctl%SGS_model_name_ctl)
        call read_chara_ctl_type(c_buf, hd_SGS_filter,                 &
     &      sgs_ctl%SGS_filter_name_ctl)
        call read_chara_ctl_type(c_buf, hd_DIFF_coefs,                 &
     &      sgs_ctl%DIFF_model_coef_ctl)
!
        call read_chara_ctl_type(c_buf, hd_SGS_clips,                  &
     &      sgs_ctl%SGS_negative_clip_ctl)
        call read_chara_ctl_type(c_buf, hd_SGS_marging,                &
     &      sgs_ctl%SGS_marging_ctl)
        call read_chara_ctl_type(c_buf, hd_SGS_perturbation_ctl,       &
     &      sgs_ctl%SGS_perturbation_ctl)
        call read_chara_ctl_type(c_buf, hd_model_coef_type_ctl,        &
     &      sgs_ctl%SGS_model_coef_type_ctl)
!
        call read_chara_ctl_type(c_buf, hd_hf_csim_type_ctl,           &
     &      sgs_ctl%heat_flux_csim_type_ctl)
        call read_chara_ctl_type(c_buf, hd_cf_csim_type_ctl,           &
     &      sgs_ctl%comp_flux_csim_type_ctl)
        call read_chara_ctl_type(c_buf, hd_mf_csim_type_ctl,           &
     &      sgs_ctl%mom_flux_csim_type_ctl)
        call read_chara_ctl_type(c_buf, hd_mxwl_csim_type_ctl,         &
     &      sgs_ctl%maxwell_csim_type_ctl)
        call read_chara_ctl_type(c_buf, hd_uxb_csim_type_ctl,          &
     &      sgs_ctl%uxb_csim_type_ctl)
        call read_chara_ctl_type(c_buf, hd_model_coef_coord_ctl,       &
     &      sgs_ctl%SGS_model_coef_coord_ctl)
        call read_chara_ctl_type(c_buf, hd_SGS_buo_Csim_usage,         &
     &      sgs_ctl%SGS_buo_Csim_usage_ctl)
!
!
        call read_real_ctl_type(c_buf, hd_delta_shrink_dynamic,        &
     &      sgs_ctl%delta_to_shrink_dynamic_ctl)
        call read_real_ctl_type(c_buf, hd_SGS_clip_limit,              &
     &      sgs_ctl%clipping_limit_ctl)
!
        call read_real_ctl_type(c_buf, hd_SGS_hf_factor,               &
     &      sgs_ctl%SGS_hf_factor_ctl)
        call read_real_ctl_type(c_buf, hd_SGS_cf_factor,               &
     &      sgs_ctl%SGS_cf_factor_ctl)
        call read_real_ctl_type(c_buf, hd_SGS_mf_factor,               &
     &      sgs_ctl%SGS_mf_factor_ctl)
        call read_real_ctl_type(c_buf, hd_SGS_mxwl_factor,             &
     &      sgs_ctl%SGS_mxwl_factor_ctl)
        call read_real_ctl_type(c_buf, hd_SGS_uxb_factor,              &
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
      subroutine bcast_sgs_ctl(sgs_ctl)
!
      use bcast_4_filter_files_ctl
!
      type(SGS_model_control), intent(inout) :: sgs_ctl
!
!
      call bcast_filter_fnames_control(sgs_ctl%ffile_ctl)
!
      end subroutine bcast_sgs_ctl
!
!   --------------------------------------------------------------------
!
      subroutine reset_sgs_ctl(sgs_ctl)
!
      type(SGS_model_control), intent(inout) :: sgs_ctl
!
!
      call reset_filter_fnames_control(sgs_ctl%ffile_ctl)
!
      end subroutine reset_sgs_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_control_4_SGS_filters                             &
     &         (id_control, hd_block, sgs_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(SGS_model_control), intent(inout) :: sgs_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
      integer(kind = kint) :: iflag = 0
      type(sph_filter_ctl_type) :: read_sfil_c
!
!
      if(sgs_ctl%num_sph_filter_ctl .gt. 0) return
      iflag = 0
      sgs_ctl%num_sph_filter_ctl = 0
      call alloc_sph_filter_ctl(sgs_ctl)
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_array_flag(c_buf, hd_block)) exit
!
        call read_control_4_SGS_filter(id_control, hd_block,            &
     &      iflag, read_sfil_c, c_buf)
        if(iflag .gt. 0) then
          call append_SGS_filter_ctls(read_sfil_c, sgs_ctl)
          iflag = 0
        end if
      end do
!
      end subroutine read_control_4_SGS_filters
!
!   --------------------------------------------------------------------
!
      end module read_ctl_data_SGS_model
