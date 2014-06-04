!m_ctl_data_SGS_model.f90
!      module m_ctl_data_SGS_model
!
!        programmed by H.Matsui on March. 2006
!      subroutine dealloc_SGS_term_name_ctl
!      subroutine dealloc_whole_filter_grp_ctl
!      subroutine dealloc_fluid_filter_grp_ctl
!      subroutine dealloc_commutation_fld_ctl
!
!      subroutine read_sgs_ctl
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
!!    filter_file_header:  header name for filter data
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
!!      SGS_hf_factor_ctl        0.5
!!
!!      begin filter_files_def
!!        filter_elength_header   'filter_elen'
!!        filter_file_header      'filter_node'
!!        model_coef_ini_header    'model_coefs_ini'
!!!        wider_filter_header     'filter/filter_coef_2'
!!      end  filter_files_def
!!
!!      min_step_dynamic_ctl      1
!!      max_step_dynamic_ctl      50
!!      delta_to_shrink_ctl      1.0d-2
!!      delta_to_extend_ctl      1.0d-3
!!
!!      array SGS_terms_ctl      5
!!        SGS_terms_ctl    heat              end
!!        SGS_terms_ctl    parturbation_heat end
!!        SGS_terms_ctl    inertia           end
!!        SGS_terms_ctl    gravity           end
!!        SGS_terms_ctl    Lorentz           end
!!        SGS_terms_ctl    induction         end
!!      end array SGS_terms_ctl
!!
!!      array commutation_ctl    9
!!        commutation_ctl    velocity          end
!!        commutation_ctl    vector_potential  end
!!        commutation_ctl    temperature       end
!!        commutation_ctl    dumnmy_scalar     end
!!
!!        commutation_ctl    heat              end
!!        commutation_ctl    inertia           end
!!        commutation_ctl    Lorentz           end
!!        commutation_ctl    induction         end
!!        commutation_ctl    composit_flux     end
!!      end array commutation_ctl
!!
!!      begin 3d_filtering_ctl
!!        array whole_filtering_grp_ctl  2
!!          whole_filtering_grp_ctl  Both   end
!!          whole_filtering_grp_ctl  whole  end
!!        end array 3d_filtering_ctl
!!
!!        array fluid_filtering_grp_ctl  2
!!            fluid_filtering_grp_ctl  Both   end
!!            fluid_filtering_grp_ctl  fluid  end
!!        end array fluid_filtering_grp_ctl
!!
!!        momentum_filter_ctl      fluid_filtering
!!        heat_filter_ctl          fluid_filtering
!!        induction_filter_ctl     whole_filtering
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
!!          layer_grp_name_ctl    fluid_layer_1   end
!!          layer_grp_name_ctl    fluid_layer_2   end
!!          layer_grp_name_ctl    fluid_layer_3   end
!!          layer_grp_name_ctl    fluid_layer_4   end
!!          layer_grp_name_ctl    fluid_layer_5   end
!!          layer_grp_name_ctl    fluid_layer_6   end
!!          layer_grp_name_ctl    fluid_layer_7   end
!!          layer_grp_name_ctl    fluid_layer_8   end
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
      module m_ctl_data_SGS_model
!
      use m_precision
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
      implicit  none
!
!
      character (len=kchara) :: SGS_model_name_ctl
      character (len=kchara) :: SGS_filter_name_ctl
      character (len=kchara) :: DIFF_model_coef_ctl
      character (len=kchara) :: SGS_negative_clip_ctl
      character (len=kchara) :: SGS_marging_ctl
      character (len=kchara) :: SGS_perturbation_ctl
!
      character (len=kchara) :: SGS_model_coef_type_ctl
!
      character (len=kchara) :: heat_flux_csim_type_ctl
      character (len=kchara) :: mom_flux_csim_type_ctl
      character (len=kchara) :: maxwell_csim_type_ctl
      character (len=kchara) :: uxb_csim_type_ctl
!
      character (len=kchara) :: SGS_model_coef_coord_ctl
! 
      integer (kind=kint) :: min_step_dynamic_ctl = 1
      integer (kind=kint) :: max_step_dynamic_ctl = 50
      real (kind = kreal) :: delta_to_shrink_dynamic_ctl
      real (kind = kreal) :: delta_to_extend_dynamic_ctl
!
      real (kind = kreal) :: clipping_limit_ctl
!
      real(kind = kreal) :: SGS_hf_factor_ctl
      real(kind = kreal) :: SGS_mf_factor_ctl
      real(kind = kreal) :: SGS_mxwl_factor_ctl
      real(kind = kreal) :: SGS_uxb_factor_ctl
!
      integer (kind=kint)   :: num_SGS_term_ctl
      integer (kind=kint)   :: num_SGS_bc_neighbour_ctl
! 
      character (len=kchara), allocatable :: SGS_term_name_ctl(:)
!
      integer (kind=kint)   :: num_whole_filter_grp_ctl
      integer (kind=kint)   :: num_fluid_filter_grp_ctl
      character (len=kchara), allocatable :: whole_filter_grp_ctl(:)
      character (len=kchara), allocatable :: fluid_filter_grp_ctl(:)
!
      character (len=kchara)   :: momentum_filter_ctl
      character (len=kchara)   :: heat_filter_ctl
      character (len=kchara)   :: induction_filter_ctl
!
      integer (kind=kint)   :: num_commutation_fld_ctl
      character (len=kchara), allocatable :: commutation_fld_ctl(:)
!
!    label for entry of group
!
      character(len=kchara), parameter :: hd_sgs_ctl = 'SGS_control'
      integer (kind=kint) :: i_sgs_ctl =       0
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
      integer (kind=kint) :: i_SGS_model =            0
      integer (kind=kint) :: i_SGS_filter =           0
      integer (kind=kint) :: i_SGS_clips =            0
      integer (kind=kint) :: i_SGS_clip_limit =       0
!
      character(len=kchara), parameter                                  &
     &             :: hd_SGS_hf_factor = 'SGS_hf_factor_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_SGS_mf_factor = 'SGS_mf_factor_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_SGS_mxwl_factor = 'SGS_mxwl_factor_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_SGS_uxb_factor =  'SGS_uxb_factor_ctl'
      integer (kind=kint) :: i_SGS_hf_factor =        0
      integer (kind=kint) :: i_SGS_mf_factor =        0
      integer (kind=kint) :: i_SGS_mxwl_factor =      0
      integer (kind=kint) :: i_SGS_uxb_factor =       0
!
!
      character(len=kchara), parameter                                  &
     &             :: hd_SGS_marging = 'direction_marging_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_DIFF_coefs =  'diff_coef_mode_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_3d_filtering = '3d_filtering_ctl'
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
      character(len=kchara), parameter :: hd_model_coef_type_ctl        &
     &                        = 'model_coef_type_ctl'
      character(len=kchara), parameter :: hd_hf_csim_type_ctl           &
     &                        = 'heat_flux_csim_type_ctl'
      character(len=kchara), parameter :: hd_mf_csim_type_ctl           &
     &                        = 'mom_flux_csim_type_ctl'
      character(len=kchara), parameter :: hd_mxwl_csim_type_ctl         &
     &                        = 'maxwell_csim_type_ctl'
      character(len=kchara), parameter :: hd_uxb_csim_type_ctl          &
     &                        = 'uxb_csim_type_ctl'
!
      character(len=kchara), parameter :: hd_model_coef_coord_ctl       &
     &                        = 'model_coef_coordinate_ctl'
      character(len=kchara), parameter :: hd_commutation_fld            &
     &                        = 'commutation_ctl'
!
      integer (kind=kint) :: i_SGS_marging =          0
      integer (kind=kint) :: i_DIFF_coefs =           0
      integer (kind=kint) :: i_3d_filtering =         0
      integer (kind=kint) :: i_min_step_dynamic =     0
      integer (kind=kint) :: i_max_step_dynamic =     0
      integer (kind=kint) :: i_delta_shrink_dynamic = 0
      integer (kind=kint) :: i_delta_extend_dynamic = 0
      integer (kind=kint) :: i_SGS_perturbation_ctl = 0
      integer (kind=kint) :: i_model_coef_type_ctl =  0
      integer (kind=kint) :: i_model_coef_coord_ctl = 0
      integer (kind=kint) :: i_commutation_fld =      0
!
      integer (kind=kint) :: i_hf_csim_type_ctl =   0
      integer (kind=kint) :: i_mf_csim_type_ctl =   0
      integer (kind=kint) :: i_mxwl_csim_type_ctl = 0
      integer (kind=kint) :: i_uxb_csim_type_ctl =  0
!
      character(len=kchara) :: hd_n_SGS_terms =  'SGS_terms_ctl'
      integer (kind=kint) :: i_n_SGS_terms = 0
!
!    5th level for 3d filtering
!
      character(len=kchara) :: hd_num_whole_filter_grp                  &
     &                        = 'whole_filtering_grp_ctl'
      character(len=kchara) :: hd_num_fluid_filter_grp                  &
     &                        = 'fluid_filtering_grp_ctl'
      integer (kind=kint) :: i_num_fluid_filter_grp = 0
      integer (kind=kint) :: i_num_whole_filter_grp = 0
!
      character(len=kchara) :: hd_momentum_filter_ctl                   &
     &                        = 'momentum_filter_ctl'
      character(len=kchara) :: hd_heat_filter_ctl                       &
     &                        =  'heat_filter_ctl'
      character(len=kchara) :: hd_induction_filter_ctl                  &
     &                        =  'induction_filter_ctl'
      integer (kind=kint) :: i_momentum_filter_ctl =  0
      integer (kind=kint) :: i_heat_filter_ctl =      0
      integer (kind=kint) :: i_induction_filter_ctl = 0
!
      private :: hd_sgs_ctl, i_sgs_ctl
      private :: hd_SGS_filter, hd_SGS_model
      private :: hd_SGS_clips, hd_SGS_clip_limit
      private :: hd_SGS_mf_factor, hd_SGS_mxwl_factor
      private :: hd_SGS_uxb_factor, hd_SGS_hf_factor
      private :: hd_SGS_marging, hd_DIFF_coefs, hd_3d_filtering
      private :: hd_min_step_dynamic, hd_max_step_dynamic
      private :: hd_delta_shrink_dynamic, hd_delta_extend_dynamic
      private :: hd_n_SGS_terms, hd_SGS_perturbation_ctl
      private :: hd_model_coef_type_ctl, hd_model_coef_coord_ctl
      private :: hd_num_whole_filter_grp, hd_num_fluid_filter_grp
      private :: hd_momentum_filter_ctl, hd_heat_filter_ctl
      private :: hd_induction_filter_ctl, hd_commutation_fld
      private :: hd_hf_csim_type_ctl, hd_mf_csim_type_ctl
      private :: hd_mxwl_csim_type_ctl, hd_uxb_csim_type_ctl
!
      private :: alloc_SGS_term_name_ctl, alloc_whole_filter_grp_ctl
      private :: alloc_fluid_filter_grp_ctl, read_3d_filtering_ctl
      private :: alloc_commutation_fld_ctl

!
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_SGS_term_name_ctl
!
      allocate(SGS_term_name_ctl(num_SGS_term_ctl))
!
      end subroutine alloc_SGS_term_name_ctl
!
!   --------------------------------------------------------------------
!
      subroutine alloc_whole_filter_grp_ctl
!
      allocate(whole_filter_grp_ctl(num_whole_filter_grp_ctl))
!
      end subroutine alloc_whole_filter_grp_ctl
!
!   --------------------------------------------------------------------
!
      subroutine alloc_fluid_filter_grp_ctl
!
      allocate(fluid_filter_grp_ctl(num_fluid_filter_grp_ctl))
!
      end subroutine alloc_fluid_filter_grp_ctl
!
!   --------------------------------------------------------------------
!
      subroutine alloc_commutation_fld_ctl
!
      allocate(commutation_fld_ctl(num_commutation_fld_ctl))
!
      end subroutine alloc_commutation_fld_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_SGS_term_name_ctl
!
      deallocate(SGS_term_name_ctl)
!
      end subroutine dealloc_SGS_term_name_ctl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_whole_filter_grp_ctl
!
      deallocate(whole_filter_grp_ctl)
!
      end subroutine dealloc_whole_filter_grp_ctl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_fluid_filter_grp_ctl
!
      deallocate(fluid_filter_grp_ctl)
!
      end subroutine dealloc_fluid_filter_grp_ctl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_commutation_fld_ctl
!
      deallocate(commutation_fld_ctl)
!
      end subroutine dealloc_commutation_fld_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_sgs_ctl
!
      use m_ctl_data_ele_layering
      use m_ctl_data_filter_files
!
!
      if(right_begin_flag(hd_sgs_ctl) .eq. 0) return
      if (i_sgs_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_sgs_ctl, i_sgs_ctl)
        if(i_sgs_ctl .gt. 0) exit
!
!
        call read_3d_filtering_ctl
        call read_filter_fnames_ctl
        call read_ele_layers_grp_ctl
!
!
        call find_control_array_flag(hd_n_SGS_terms, num_SGS_term_ctl)
        if(num_SGS_term_ctl.gt.0 .and. i_n_SGS_terms.eq.0) then
          call alloc_SGS_term_name_ctl
          call read_control_array_chara_list(hd_n_SGS_terms,            &
     &        num_SGS_term_ctl, i_n_SGS_terms, SGS_term_name_ctl)
        end if
!
        call find_control_array_flag(hd_commutation_fld,                &
     &      num_commutation_fld_ctl)
        if(num_commutation_fld_ctl.gt.0                                 &
     &       .and. i_commutation_fld.eq.0) then
          call alloc_commutation_fld_ctl
          call read_control_array_chara_list(hd_commutation_fld,        &
     &        num_commutation_fld_ctl, i_commutation_fld,               &
     &        commutation_fld_ctl)
        end if
!
!
        call read_character_ctl_item(hd_SGS_model,                      &
     &        i_SGS_model, SGS_model_name_ctl)
        call read_character_ctl_item(hd_SGS_filter,                     &
     &        i_SGS_filter, SGS_filter_name_ctl)
        call read_character_ctl_item(hd_DIFF_coefs,                     &
     &        i_DIFF_coefs, DIFF_model_coef_ctl)
!
        call read_character_ctl_item(hd_SGS_clips,                      &
     &        i_SGS_clips, SGS_negative_clip_ctl)
        call read_character_ctl_item(hd_SGS_marging,                    &
     &        i_SGS_marging, SGS_marging_ctl)
        call read_character_ctl_item(hd_SGS_perturbation_ctl,           &
     &        i_SGS_perturbation_ctl, SGS_perturbation_ctl)
        call read_character_ctl_item(hd_model_coef_type_ctl,            &
     &        i_model_coef_type_ctl, SGS_model_coef_type_ctl)
        call read_character_ctl_item(hd_hf_csim_type_ctl,               &
     &        i_hf_csim_type_ctl, heat_flux_csim_type_ctl)
        call read_character_ctl_item(hd_mf_csim_type_ctl,               &
     &        i_mf_csim_type_ctl, mom_flux_csim_type_ctl)
        call read_character_ctl_item(hd_mxwl_csim_type_ctl,             &
     &        i_mxwl_csim_type_ctl, maxwell_csim_type_ctl)
        call read_character_ctl_item(hd_uxb_csim_type_ctl,              &
     &        i_uxb_csim_type_ctl, uxb_csim_type_ctl)
        call read_character_ctl_item(hd_model_coef_coord_ctl,           &
     &        i_model_coef_coord_ctl, SGS_model_coef_coord_ctl)
!
!
        call read_real_ctl_item(hd_delta_shrink_dynamic,                &
     &        i_delta_shrink_dynamic, delta_to_shrink_dynamic_ctl)
        call read_real_ctl_item(hd_SGS_clip_limit,                      &
     &        i_SGS_clip_limit, clipping_limit_ctl)
!
        call read_real_ctl_item(hd_SGS_hf_factor,                       &
     &        i_SGS_hf_factor, SGS_hf_factor_ctl)
        call read_real_ctl_item(hd_SGS_mf_factor,                       &
     &        i_SGS_mf_factor, SGS_mf_factor_ctl)
        call read_real_ctl_item(hd_SGS_mxwl_factor,                     &
     &        i_SGS_mxwl_factor, SGS_mxwl_factor_ctl)
        call read_real_ctl_item(hd_SGS_uxb_factor,                      &
     &        i_SGS_uxb_factor, SGS_uxb_factor_ctl)
!
        call read_real_ctl_item(hd_delta_extend_dynamic,                &
     &        i_delta_extend_dynamic, delta_to_extend_dynamic_ctl)
!
!
        call read_integer_ctl_item(hd_min_step_dynamic,                 &
     &        i_min_step_dynamic, min_step_dynamic_ctl)
        call read_integer_ctl_item(hd_max_step_dynamic,                 &
     &        i_max_step_dynamic, max_step_dynamic_ctl)
      end do
!
      end subroutine read_sgs_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_3d_filtering_ctl
!
!
      if(right_begin_flag(hd_3d_filtering) .eq. 0) return
      if (i_3d_filtering .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_3d_filtering, i_3d_filtering)
        if(i_3d_filtering .gt. 0) exit
!
!
        call find_control_array_flag(hd_num_whole_filter_grp,           &
     &      num_whole_filter_grp_ctl)
        if(num_whole_filter_grp_ctl.gt.0                                &
     &       .and. i_num_whole_filter_grp.eq.0) then
          allocate(whole_filter_grp_ctl(num_whole_filter_grp_ctl))
          call read_control_array_chara_list(hd_num_whole_filter_grp,   &
     &        num_whole_filter_grp_ctl, i_num_whole_filter_grp,         &
     &        whole_filter_grp_ctl)
        end if
!
        call find_control_array_flag(hd_num_fluid_filter_grp,           &
     &      num_fluid_filter_grp_ctl)
        if(num_fluid_filter_grp_ctl.gt.0                                &
     &       .and. i_num_fluid_filter_grp.eq.0) then
          call alloc_fluid_filter_grp_ctl
          call read_control_array_chara_list(hd_num_fluid_filter_grp,   &
     &        num_fluid_filter_grp_ctl, i_num_fluid_filter_grp,         &
     &        fluid_filter_grp_ctl)
        end if
!
!
        call read_character_ctl_item(hd_momentum_filter_ctl,            &
     &        i_momentum_filter_ctl, momentum_filter_ctl)
        call read_character_ctl_item(hd_heat_filter_ctl,                &
     &        i_heat_filter_ctl, heat_filter_ctl)
        call read_character_ctl_item(hd_induction_filter_ctl,           &
     &        i_induction_filter_ctl, induction_filter_ctl)
      end do
!
      end subroutine read_3d_filtering_ctl
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_SGS_model
