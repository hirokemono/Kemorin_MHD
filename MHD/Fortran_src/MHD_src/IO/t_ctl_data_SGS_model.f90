!>@file   t_ctl_data_SGS_model.f90
!!@brief  module t_ctl_data_SGS_model
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2012
!
!>@brief  Structure for SGS model controls
!!
!!@verbatim
!!      subroutine alloc_sph_filter_ctl(sgs_ctl)
!!      subroutine dealloc_sgs_ctl(sgs_ctl)
!!      subroutine dealloc_sph_filter_ctl(sgs_ctl)
!!        type(SGS_model_control), intent(inout) :: sgs_ctl
!!      subroutine append_SGS_filter_ctls(add_sfil_c, sgs_ctl)
!!        type(sph_filter_ctl_type), intent(inout) :: add_sfil_c
!!        type(SGS_model_control), intent(inout) :: sgs_ctl
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_ctl_data_SGS_model
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use skip_comment_f
      use t_control_elements
      use t_control_array_character
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
!
        integer (kind=kint) :: i_sgs_ctl =       0
      end type SGS_model_control
!
      private :: copy_SGS_filter_ctls
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
      integer(kind = kint) :: i
!
!
      allocate(sgs_ctl%sph_filter_ctl(sgs_ctl%num_sph_filter_ctl))
!
      do i = 1, sgs_ctl%num_sph_filter_ctl
        sgs_ctl%sph_filter_ctl(i)%maximum_moments_ctl%intvalue = 0
        sgs_ctl%sph_filter_ctl(i)%sphere_filter_width_ctl%realvalue     &
     &     = 0.0d0
        sgs_ctl%sph_filter_ctl(i)%radial_filter_width_ctl%realvalue     &
     &     = 0.0d0
        sgs_ctl%sph_filter_ctl(i)%first_reference_ctl%intvalue = 0
        sgs_ctl%sph_filter_ctl(i)%second_reference_ctl%intvalue = 0
      end do
!
      end subroutine alloc_sph_filter_ctl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_sgs_ctl(sgs_ctl)
!
      type(SGS_model_control), intent(inout) :: sgs_ctl
!
!
      call dealloc_sph_filter_ctl(sgs_ctl)
!
      call dealloc_3d_filtering_ctl(sgs_ctl%s3df_ctl)
!
      call dealloc_control_array_chara(sgs_ctl%SGS_terms_ctl)
      call dealloc_control_array_chara(sgs_ctl%commutate_fld_ctl)
!
      call dealloc_ctl_data_ele_layering(sgs_ctl%elayer_ctl)
!
      sgs_ctl%SGS_model_name_ctl%iflag =  0
      sgs_ctl%SGS_filter_name_ctl%iflag = 0
      sgs_ctl%DIFF_model_coef_ctl%iflag = 0
!
      sgs_ctl%SGS_negative_clip_ctl%iflag =   0
      sgs_ctl%SGS_marging_ctl%iflag =         0
      sgs_ctl%SGS_perturbation_ctl%iflag =    0
      sgs_ctl%SGS_model_coef_type_ctl%iflag = 0
!
      sgs_ctl%heat_flux_csim_type_ctl%iflag =  0
      sgs_ctl%comp_flux_csim_type_ctl%iflag =  0
      sgs_ctl%mom_flux_csim_type_ctl%iflag =   0
      sgs_ctl%maxwell_csim_type_ctl%iflag =    0
      sgs_ctl%uxb_csim_type_ctl%iflag =        0
      sgs_ctl%SGS_model_coef_coord_ctl%iflag = 0
      sgs_ctl%SGS_buo_Csim_usage_ctl%iflag =   0
!
!
      sgs_ctl%delta_to_shrink_dynamic_ctl%iflag = 0
      sgs_ctl%clipping_limit_ctl%iflag =          0
!
      sgs_ctl%SGS_hf_factor_ctl%iflag =   0
      sgs_ctl%SGS_cf_factor_ctl%iflag =   0
      sgs_ctl%SGS_mf_factor_ctl%iflag =   0
      sgs_ctl%SGS_mxwl_factor_ctl%iflag = 0
      sgs_ctl%SGS_uxb_factor_ctl%iflag =  0
!
      sgs_ctl%delta_to_extend_dynamic_ctl%iflag = 0
      sgs_ctl%stabilize_weight_ctl%iflag =        0
!
      sgs_ctl%istep_dynamic_ctl%iflag =    0
      sgs_ctl%min_step_dynamic_ctl%iflag = 0
      sgs_ctl%max_step_dynamic_ctl%iflag = 0
!
      sgs_ctl%ngrp_radial_ave_ctl%iflag =  0
      sgs_ctl%ngrp_med_ave_ctl%iflag =     0
!
      sgs_ctl%i_sgs_ctl = 0
!
      end subroutine dealloc_sgs_ctl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_sph_filter_ctl(sgs_ctl)
!
      type(SGS_model_control), intent(inout) :: sgs_ctl
!
      integer(kind = kint) :: i
!
!
      do i = 1, sgs_ctl%num_sph_filter_ctl
        call reset_control_4_SGS_filter(sgs_ctl%sph_filter_ctl(i))
      end do
!
      if(allocated(sgs_ctl%sph_filter_ctl)) then
        deallocate(sgs_ctl%sph_filter_ctl)
      end if
      sgs_ctl%num_sph_filter_ctl = 0
!
      end subroutine dealloc_sph_filter_ctl
!
!   --------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine append_SGS_filter_ctls(add_sfil_c, sgs_ctl)
!
      type(sph_filter_ctl_type), intent(inout) :: add_sfil_c
      type(SGS_model_control), intent(inout) :: sgs_ctl
!
      integer(kind = kint) :: num_tmp = 0
      type(sph_filter_ctl_type), allocatable :: tmp_sfil_c(:)
!
!
      num_tmp = sgs_ctl%num_sph_filter_ctl
      allocate(tmp_sfil_c(num_tmp))
      call copy_SGS_filter_ctls                                         &
     &   (num_tmp, sgs_ctl%sph_filter_ctl, tmp_sfil_c)
      call dealloc_sph_filter_ctl(sgs_ctl)
!
      sgs_ctl%num_sph_filter_ctl = num_tmp + 1
      call alloc_sph_filter_ctl(sgs_ctl)
!
      call copy_SGS_filter_ctls                                         &
     &   (num_tmp, tmp_sfil_c, sgs_ctl%sph_filter_ctl(1))
      deallocate(tmp_sfil_c)
!
      call copy_control_4_SGS_filter(add_sfil_c,                        &
     &    sgs_ctl%sph_filter_ctl(sgs_ctl%num_sph_filter_ctl))
      call reset_control_4_SGS_filter(add_sfil_c)
!
      end subroutine append_SGS_filter_ctls
!
! -----------------------------------------------------------------------
!
      subroutine copy_SGS_filter_ctls(num_ctl, org_sfil_c, new_sfil_c)
!
      integer(kind = kint), intent(in) :: num_ctl
      type(sph_filter_ctl_type), intent(in) :: org_sfil_c(num_ctl)
      type(sph_filter_ctl_type), intent(inout) :: new_sfil_c(num_ctl)
!
      integer(kind = kint) :: i
!
      do i = 1, num_ctl
        call copy_control_4_SGS_filter(org_sfil_c(i), new_sfil_c(i))
      end do
!
      end subroutine copy_SGS_filter_ctls
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_SGS_model
