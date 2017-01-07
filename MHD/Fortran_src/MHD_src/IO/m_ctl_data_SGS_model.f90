!m_ctl_data_SGS_model.f90
!      module m_ctl_data_SGS_model
!
!        programmed by H.Matsui on March. 2006
!
!!      subroutine read_sgs_control
!!      subroutine deallocate_sph_filter_ctl
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
!!      array sph_filter_ctl    2
!!        begin sph_filter_ctl
!!          number_of_moments   5
!!          radial_filter_width     1.0
!!          sphere_filter_width     1.0
!!        end   sph_filter_ctl
!!        begin sph_filter_ctl
!!          number_of_moments   7
!!          radial_filter_width     2.0
!!          sphere_filter_width     2.0
!!        end   sph_filter_ctl
!!      end array sph_filter_ctl
!!
!!
!!      SGS_hf_factor_ctl        0.5
!!
!!      begin filter_files_def
!!        filter_elength_header   'filter_elen'
!!        filter_file_header      'filter_node'
!!        model_coef_ini_header    'model_coefs_ini'
!!        wider_filter_header     'filter/filter_coef_2'
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
      use t_ctl_data_SGS_model
!
      implicit  none
!
!
      type(SGS_model_control), save :: sgs_ctl1
!
!    label for entry of group
!
      character(len=kchara), parameter :: hd_sgs_ctl = 'SGS_control'
      integer (kind=kint) :: i_sgs_ctl =       0
!
      private :: hd_sgs_ctl, i_sgs_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_sgs_control
!
!
      call read_sgs_ctl(hd_sgs_ctl, i_sgs_ctl, sgs_ctl1)
!
      end subroutine read_sgs_control
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_SGS_model
