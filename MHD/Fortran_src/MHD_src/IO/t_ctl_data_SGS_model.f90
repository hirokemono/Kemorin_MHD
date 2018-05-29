!t_ctl_data_SGS_model.f90
!      module t_ctl_data_SGS_model
!
!        programmed by H.Matsui on March. 2006
!
!!      subroutine read_sgs_ctl(hd_block, iflag, sgs_ctl)
!!      subroutine bcast_sgs_ctl(sgs_ctl)
!!        type(SGS_model_control), intent(inout) :: sgs_ctl
!!      subroutine dealloc_sph_filter_ctl(sgs_ctl)
!!
!!!!!!!!!  SGS Model !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!     SGS_model_ctl: gradient.........nonlinear gradient model
!!                    similarity.......Similarity model
!!                    dynamic..........Dynamic model
!!                    dynamic_similarity..Dynamic similarity model
!!     filtering_scheme_ctl:   line....filtering along each directrion
!!                             3d......filtering using 3-dimensional table
!!                             3d-smp..filtering using 3-dimensional on SMP model
!!     diff_coef_mode_ctl:     whole_domain.....lead one constant for whole domain
!!                             layerd...........lead one constant for each layer
!!     negative_clip_ctl:      none.............use negative model coefficient
!!                             zero.............set model coefs to 0 if it is neagative
!!                             keep.............keep previous coefs if it is neagative
!!     direction_marging_ctl:  lsq........... ..taking LSQ over directions
!!                             average..........average over directions
!!                             weighting........weighting average by correlation
!!     SGS_perturbation_ctl:   perturbation model for temperature and composition
!!                             none...use temperature
!!                             reference...use given reference temperature
!!                             average.....use average of aphere or plane
!!     model_coef_type_ctl:       Selection of model coefficient type
!!                             field:      Intergrate model coefficients all directions
!!                             components: Use model coefficients for each components
!!     model_coef_coordinate_ctl: Selection of Cooredinate system for dynamic model
!!                             Certecian:   use Certecian   coordinate
!!                             Spherical:   use Spherical   coordinate
!!                             Cylindrical: use Cylindrical coordinate
!!
!!     SGS_buoyancy_Csim_usage::   
!!                             volume: use volume average
!!                             zonal: use zonal average
!!                             sphere: use sphere average
!!
!!    filter_file_prefix:  header name for filter data
!!
!!
!!    3d_filtering_ctl
!!      whole_area:   filtering groups for whole domain
!!      fluid_area:   filtering groups for fluid region
!!
!!    filter_4_eqs_ctl:   filtering area for each equation
!!                      (whole_filtering of fluid filtering)
!!          momentum_filter_ctl:     momentum equation
!!          heat_filter_ctl:         heat equation
!!          induction_filter_ctl:    inducition equation
!!
!!    layering_data_ctl:  layering type for dynamic model
!!                      start_end... indicate start and end element ID by group
!!                      explicit...  set elements by element groups explicitly
!!                      ele_group_list...  set elements by element group list
!!    num_layer_grp_ctl:    num. of group for layering
!!        grp_stack_each_layer_ctl:  end layer group for each coefs
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
!!      array sph_filter_ctl    2
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
!!      array SGS_terms_ctl      5
!!        SGS_terms_ctl    heat
!!        SGS_terms_ctl    parturbation_heat
!!        SGS_terms_ctl    inertia
!!        SGS_terms_ctl    gravity
!!        SGS_terms_ctl    Lorentz
!!        SGS_terms_ctl    induction
!!      end array SGS_terms_ctl
!!
!!      array commutation_ctl    9
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
!!        array grp_stack_each_layer_ctl    4
!!          grp_stack_each_layer_ctl  2
!!          grp_stack_each_layer_ctl  4
!!          grp_stack_each_layer_ctl  6
!!          grp_stack_each_layer_ctl  8
!!        end array grp_stack_each_layer_ctl
!!
!!        array layer_grp_name_ctl    8
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
!!
!
      module t_ctl_data_SGS_model
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
      use t_control_elements
      use t_read_control_arrays
      use t_ctl_data_SGS_filter
      use t_ctl_data_filter_files
      use t_ctl_data_ele_layering
!
      implicit  none
!
!
      type SGS_model_control
        type(read_character_item) :: SGS_model_name_ctl
        type(read_character_item) :: SGS_filter_name_ctl
        type(read_character_item) :: DIFF_model_coef_ctl
        type(read_character_item) :: SGS_negative_clip_ctl
        type(read_character_item) :: SGS_marging_ctl
        type(read_character_item) :: SGS_perturbation_ctl
!
        type(read_character_item) :: SGS_model_coef_type_ctl
!
        type(read_character_item) :: heat_flux_csim_type_ctl
        type(read_character_item) :: comp_flux_csim_type_ctl
        type(read_character_item) :: mom_flux_csim_type_ctl
        type(read_character_item) :: maxwell_csim_type_ctl
        type(read_character_item) :: uxb_csim_type_ctl
!
        type(read_character_item) :: SGS_model_coef_coord_ctl
        type(read_character_item) :: SGS_buo_Csim_usage_ctl
! 
        type(read_integer_item) :: istep_dynamic_ctl
        type(read_integer_item) :: min_step_dynamic_ctl
        type(read_integer_item) :: max_step_dynamic_ctl
        type(read_real_item) :: stabilize_weight_ctl
        type(read_real_item) :: delta_to_shrink_dynamic_ctl
        type(read_real_item) :: delta_to_extend_dynamic_ctl
!
        type(read_integer_item) :: ngrp_radial_ave_ctl
        type(read_integer_item) :: ngrp_med_ave_ctl
!
        type(read_real_item) :: clipping_limit_ctl
!
        type(read_real_item) :: SGS_hf_factor_ctl
        type(read_real_item) :: SGS_cf_factor_ctl
        type(read_real_item) :: SGS_mf_factor_ctl
        type(read_real_item) :: SGS_mxwl_factor_ctl
        type(read_real_item) :: SGS_uxb_factor_ctl
!
!>        Structure for field list of SGS terms
!!@n        SGS_terms_ctl%c_tbl: name of SGS terms
        type(ctl_array_chara) :: SGS_terms_ctl
!>        Structure for field list of commutaion error correction
!!@n        commutate_fld_ctl%c_tbl: field name
!!                                  for commutaion error correction
        type(ctl_array_chara) :: commutate_fld_ctl
!
        type(filter_file_control) :: ffile_ctl
!>        Structure for element layering
        type(layering_control) :: elayer_ctl
!
        type(SGS_3d_filter_control) :: s3df_ctl
!
        integer(kind = kint) :: num_sph_filter_ctl
        type(sph_filter_ctl_type), allocatable :: sph_filter_ctl(:)
      end type SGS_model_control
!
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
      integer (kind=kint) :: i_sph_filter_ctl = 0
!
      character(len=kchara) :: hd_SGS_terms =  'SGS_terms_ctl'
      character(len=kchara) :: hd_sph_filter =  'sph_filter_ctl'
!
!    5th level for 3d filtering
!
      character(len=kchara), parameter :: hd_dynamic_layers             &
     &                        = 'dynamic_model_layer_ctl'
      integer (kind=kint) :: i_dynamic_layers = 0
!
      character(len=kchara), parameter :: hd_filter_fnames              &
     &                        = 'filter_files_def'
      integer (kind=kint) :: i_filter_fnames = 0
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
      private :: read_control_4_SGS_filters, alloc_sph_filter_ctl
      private :: hd_dynamic_layers, i_dynamic_layers
!
      private :: hd_filter_fnames, i_filter_fnames
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_sph_filter_ctl(sgs_ctl)
!
      type(SGS_model_control), intent(inout) :: sgs_ctl
!
      allocate(sgs_ctl%sph_filter_ctl(sgs_ctl%num_sph_filter_ctl))
!
      end subroutine alloc_sph_filter_ctl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_sph_filter_ctl(sgs_ctl)
!
      type(SGS_model_control), intent(inout) :: sgs_ctl
!
      deallocate(sgs_ctl%sph_filter_ctl)
      sgs_ctl%num_sph_filter_ctl = 0
!
      end subroutine dealloc_sph_filter_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_sgs_ctl(hd_block, iflag, sgs_ctl)
!
      character(len=kchara), intent(in) :: hd_block
      integer(kind = kint), intent(inout) :: iflag
!
      type(SGS_model_control), intent(inout) :: sgs_ctl
!
!
      if(right_begin_flag(hd_block) .eq. 0) return
      if(iflag .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_block, iflag)
        if(iflag .gt. 0) exit
!
!
        call read_3d_filtering_ctl                                      &
     &     (hd_3d_filtering, i_3d_filtering, sgs_ctl%s3df_ctl)
        call read_filter_fnames_control                                 &
     &     (hd_filter_fnames, i_filter_fnames, sgs_ctl%ffile_ctl)
        call read_ele_layers_control                                    &
     &     (hd_dynamic_layers, i_dynamic_layers, sgs_ctl%elayer_ctl)
!
!
        call find_control_array_flag                                    &
     &     (hd_sph_filter, sgs_ctl%num_sph_filter_ctl)
        if(sgs_ctl%num_sph_filter_ctl .gt. 0) then
          call read_control_4_SGS_filters                               &
     &       (hd_sph_filter, i_sph_filter_ctl, sgs_ctl)
        end if
!
        call read_control_array_c1(hd_SGS_terms, sgs_ctl%SGS_terms_ctl)
        call read_control_array_c1                                      &
     &     (hd_commutation_fld, sgs_ctl%commutate_fld_ctl)
!
        call read_chara_ctl_type(hd_SGS_model,                          &
     &      sgs_ctl%SGS_model_name_ctl)
        call read_chara_ctl_type(hd_SGS_filter,                         &
     &      sgs_ctl%SGS_filter_name_ctl)
        call read_chara_ctl_type(hd_DIFF_coefs,                         &
     &      sgs_ctl%DIFF_model_coef_ctl)
!
        call read_chara_ctl_type(hd_SGS_clips,                          &
     &      sgs_ctl%SGS_negative_clip_ctl)
        call read_chara_ctl_type(hd_SGS_marging,                        &
     &      sgs_ctl%SGS_marging_ctl)
        call read_chara_ctl_type(hd_SGS_perturbation_ctl,               &
     &      sgs_ctl%SGS_perturbation_ctl)
        call read_chara_ctl_type(hd_model_coef_type_ctl,                &
     &      sgs_ctl%SGS_model_coef_type_ctl)
!
        call read_chara_ctl_type(hd_hf_csim_type_ctl,                   &
     &      sgs_ctl%heat_flux_csim_type_ctl)
        call read_chara_ctl_type(hd_cf_csim_type_ctl,                   &
     &      sgs_ctl%comp_flux_csim_type_ctl)
        call read_chara_ctl_type(hd_mf_csim_type_ctl,                   &
     &      sgs_ctl%mom_flux_csim_type_ctl)
        call read_chara_ctl_type(hd_mxwl_csim_type_ctl,                 &
     &      sgs_ctl%maxwell_csim_type_ctl)
        call read_chara_ctl_type(hd_uxb_csim_type_ctl,                  &
     &      sgs_ctl%uxb_csim_type_ctl)
        call read_chara_ctl_type(hd_model_coef_coord_ctl,               &
     &      sgs_ctl%SGS_model_coef_coord_ctl)
        call read_chara_ctl_type(hd_SGS_buo_Csim_usage,                 &
     &      sgs_ctl%SGS_buo_Csim_usage_ctl)
!
!
        call read_real_ctl_type(hd_delta_shrink_dynamic,                &
     &      sgs_ctl%delta_to_shrink_dynamic_ctl)
        call read_real_ctl_type(hd_SGS_clip_limit,                      &
     &      sgs_ctl%clipping_limit_ctl)
!
        call read_real_ctl_type(hd_SGS_hf_factor,                       &
     &      sgs_ctl%SGS_hf_factor_ctl)
        call read_real_ctl_type(hd_SGS_cf_factor,                       &
     &      sgs_ctl%SGS_cf_factor_ctl)
        call read_real_ctl_type(hd_SGS_mf_factor,                       &
     &      sgs_ctl%SGS_mf_factor_ctl)
        call read_real_ctl_type(hd_SGS_mxwl_factor,                     &
     &      sgs_ctl%SGS_mxwl_factor_ctl)
        call read_real_ctl_type(hd_SGS_uxb_factor,                      &
     &      sgs_ctl%SGS_uxb_factor_ctl)
!
        call read_real_ctl_type(hd_delta_extend_dynamic,                &
     &      sgs_ctl%delta_to_extend_dynamic_ctl)
        call read_real_ctl_type(hd_stabilize_weight,                    &
     &      sgs_ctl%stabilize_weight_ctl)
!
        call read_integer_ctl_type(hd_istep_dynamic,                    &
     &      sgs_ctl%istep_dynamic_ctl)
        call read_integer_ctl_type(hd_min_step_dynamic,                 &
     &      sgs_ctl%min_step_dynamic_ctl)
        call read_integer_ctl_type(hd_max_step_dynamic,                 &
     &      sgs_ctl%max_step_dynamic_ctl)
!
        call read_integer_ctl_type(hd_r_ave_area_ctl,                   &
     &      sgs_ctl%ngrp_radial_ave_ctl)
        call read_integer_ctl_type(hd_med_ave_area_ctl,                 &
     &      sgs_ctl%ngrp_med_ave_ctl)
      end do
!
      end subroutine read_sgs_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_control_4_SGS_filters(hd_block, iflag, sgs_ctl)
!
      character(len=kchara), intent(in) :: hd_block
      integer(kind = kint), intent(inout) :: iflag
!
      type(SGS_model_control), intent(inout) :: sgs_ctl
!
!
      if (iflag .gt. 0) return
!
      call alloc_sph_filter_ctl(sgs_ctl)
      do
        call load_ctl_label_and_line
!
        call find_control_end_array_flag(hd_block,                      &
     &      sgs_ctl%num_sph_filter_ctl, iflag)
        if(iflag .ge. sgs_ctl%num_sph_filter_ctl) exit
!
        if(right_begin_flag(hd_block) .gt. 0) then
          iflag = iflag + 1
          call read_control_4_SGS_filter(hd_block,                      &
     &        sgs_ctl%sph_filter_ctl(iflag))
        end if
      end do
!
      end subroutine read_control_4_SGS_filters
!
!   --------------------------------------------------------------------
!
      subroutine bcast_sgs_ctl(sgs_ctl)
!
      use calypso_mpi
      use bcast_4_filter_files_ctl
      use bcast_control_arrays
!
      type(SGS_model_control), intent(inout) :: sgs_ctl
!
      integer(kind = kint) :: i
!
!
      call bcast_3d_filtering_ctl(sgs_ctl%s3df_ctl)
      call bcast_ctl_data_4_filter_files(sgs_ctl%ffile_ctl)
      call bcast_ele_layers_control(sgs_ctl%elayer_ctl)
!
!
      call MPI_BCAST(sgs_ctl%num_sph_filter_ctl, ione,                  &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      if(my_rank .gt. 0 .and. sgs_ctl%num_sph_filter_ctl.gt. 0) then
        call alloc_sph_filter_ctl(sgs_ctl)
      end if
      do i = 1, sgs_ctl%num_sph_filter_ctl
        call bcast_control_4_SGS_filter(sgs_ctl%sph_filter_ctl(i))
      end do
!
      call bcast_ctl_array_c1(sgs_ctl%SGS_terms_ctl)
      call bcast_ctl_array_c1(sgs_ctl%commutate_fld_ctl)
!
      call bcast_ctl_type_c1(sgs_ctl%SGS_model_name_ctl)
      call bcast_ctl_type_c1(sgs_ctl%SGS_filter_name_ctl)
      call bcast_ctl_type_c1(sgs_ctl%DIFF_model_coef_ctl)
!
      call bcast_ctl_type_c1(sgs_ctl%SGS_negative_clip_ctl)
      call bcast_ctl_type_c1(sgs_ctl%SGS_marging_ctl)
      call bcast_ctl_type_c1(sgs_ctl%SGS_perturbation_ctl)
      call bcast_ctl_type_c1(sgs_ctl%SGS_model_coef_type_ctl)
!
      call bcast_ctl_type_c1(sgs_ctl%heat_flux_csim_type_ctl)
      call bcast_ctl_type_c1(sgs_ctl%comp_flux_csim_type_ctl)
      call bcast_ctl_type_c1(sgs_ctl%mom_flux_csim_type_ctl)
      call bcast_ctl_type_c1(sgs_ctl%maxwell_csim_type_ctl)
      call bcast_ctl_type_c1(sgs_ctl%uxb_csim_type_ctl)
      call bcast_ctl_type_c1(sgs_ctl%SGS_model_coef_coord_ctl)
      call bcast_ctl_type_c1(sgs_ctl%SGS_buo_Csim_usage_ctl)
!
!
      call bcast_ctl_type_r1(sgs_ctl%delta_to_shrink_dynamic_ctl)
      call bcast_ctl_type_r1(sgs_ctl%clipping_limit_ctl)
!
      call bcast_ctl_type_r1(sgs_ctl%SGS_hf_factor_ctl)
      call bcast_ctl_type_r1(sgs_ctl%SGS_cf_factor_ctl)
      call bcast_ctl_type_r1(sgs_ctl%SGS_mf_factor_ctl)
      call bcast_ctl_type_r1(sgs_ctl%SGS_mxwl_factor_ctl)
      call bcast_ctl_type_r1(sgs_ctl%SGS_uxb_factor_ctl)
!
      call bcast_ctl_type_r1(sgs_ctl%delta_to_extend_dynamic_ctl)
      call bcast_ctl_type_r1(sgs_ctl%stabilize_weight_ctl)
!
      call bcast_ctl_type_i1(sgs_ctl%istep_dynamic_ctl)
      call bcast_ctl_type_i1(sgs_ctl%min_step_dynamic_ctl)
      call bcast_ctl_type_i1(sgs_ctl%max_step_dynamic_ctl)
!
      call bcast_ctl_type_i1(sgs_ctl%ngrp_radial_ave_ctl)
      call bcast_ctl_type_i1(sgs_ctl%ngrp_med_ave_ctl)
!
      end subroutine bcast_sgs_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_SGS_model
