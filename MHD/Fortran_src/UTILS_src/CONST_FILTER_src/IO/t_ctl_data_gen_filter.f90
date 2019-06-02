!
!      module t_ctl_data_gen_filter
!
!      Written by H. Matsui on July, 2006
!
!!      subroutine read_filter_param_ctl(gen_f_ctl)
!!      subroutine dealloc_filter_param_ctl(gen_f_ctl)
!!        type(ctl_data_gen_filter), intent(inout) :: gen_f_ctl
!
      module t_ctl_data_gen_filter
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use m_read_control_elements
      use t_control_elements
      use t_read_control_arrays
      use t_control_array_charareal
      use t_control_array_intchrreal
      use t_ctl_data_4_solvers
!
      implicit  none
!
!
      type ctl_data_gen_filter
        type(read_integer_item) :: num_int_points_ctl
        type(read_integer_item) :: minimum_comp_ctl
        type(read_integer_item) :: num_ele_4_filter_ctl
        type(read_real_item) :: omitted_ratio_ctl
        type(read_real_item) :: minimum_det_ctl
        type(read_real_item) :: maximum_rms_ctl
        type(read_character_item) :: ordering_list_ctl
        type(read_character_item) :: tgt_filter_type_ctl
        type(read_character_item) :: momentum_type_ctl
        type(read_character_item) :: filter_correction_ctl
        type(read_character_item) :: filter_fixed_point_ctl
        type(read_character_item) :: negative_center_ctl
!
        type(read_integer_item) :: maximum_neighbour_ctl
        type(read_integer_item) :: ilevel_filter_error_info
!
        type(read_integer_item) :: start_node_ctl
        type(read_integer_item) :: end_node_ctl
        type(read_integer_item) :: ist_num_free_ctl
        type(read_integer_item) :: ied_num_free_ctl
!
!>        Structure for list of reference filter mode
!!@n        reference_filter_ctl%c_tbl: list of filter type
!!@n        reference_filter_ctl%vect:  list of filter width
        type(ctl_array_cr)  :: reference_filter_ctl
!
!>        Structure for list of horizontal filter mode
!!@n        horizontal_filter_ctl%c_tbl: list of filter type
!!@n        horizontal_filter_ctl%vect:  list of filter width
        type(ctl_array_cr)  :: horizontal_filter_ctl
!
!>        Structure for reference moments for filter
!!@n        ref_filter_mom_ctl%ivec:  Order of reference filter moments
!!@n        ref_filter_mom_ctl%c_tbl: Type of reference filter moments
!!@n        ref_filter_mom_ctl%vect:  Value of filter moments
        type(ctl_array_icr) :: ref_filter_mom_ctl
!
!>        Structure for CG solver control
        type(solver_control) :: CG_filter_ctl
!
        type(read_character_item) :: f_solver_type_ctl
      end type ctl_data_gen_filter
!
!
!     label for entry
!
      character(len=kchara), parameter                                  &
     &         :: hd_filter_param_ctl = 'filter_control'
      integer (kind=kint) :: i_filter_param_ctl =   0
!
!     3rd level for filter_control
!
      character(len=kchara), parameter                                  &
     &         :: hd_num_int_points =   'num_int_points'
      character(len=kchara), parameter                                  &
     &         :: hd_minimum_comp =     'minimum_components'
      character(len=kchara), parameter                                  &
     &         :: hd_omitted_ratio =    'omitted_ratio'
      character(len=kchara), parameter                                  &
     &         :: hd_ordering_list =    'ordering_list_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_start_node_ctl =    'start_node_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_end_node_ctl =      'end_node_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_start_nfree_mat =   'start_mat_freedom_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_end_nfree_mat =     'end_mat_freedom_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_minimum_det =       'minimum_det_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_maximum_rms =       'maximum_rms_coef_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_nele_filtering =    'num_element_4_filter_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_order_moments =     'moments_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_ref_filter =        'ref_filter_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_maximum_neighbour = 'maximum_neighbour'
      character(len=kchara), parameter                                  &
     &         :: hd_tgt_filter_type =   'target_filter_type'
      character(len=kchara), parameter                                  &
     &         :: hd_filter_corection =  'filter_correction_mode'
      character(len=kchara), parameter                                  &
     &         :: hd_filter_fixed_point = 'fixed_points_mode'
      character(len=kchara), parameter                                  &
     &         :: hd_filter_negative_center = 'allow_negative_center'
      character(len=kchara), parameter                                  &
     &         :: hd_err_level_commute =  'filter_error_info_level'
      character(len=kchara), parameter                                  &
     &         :: hd_momentum_type =     'momentum_type'
      character(len=kchara), parameter                                  &
     &         :: hd_horiz_filter =  'num_horiz_filter'
      character(len=kchara), parameter                                  &
     &         :: hd_solver_type =       'solver_type'
!
      character(len=kchara), parameter                                  &
     &       :: hd_solver_ctl =     'solver_ctl'
      integer (kind=kint) :: i_solver_ctl =     0
!
      private :: hd_filter_param_ctl, i_filter_param_ctl
      private :: hd_solver_ctl, i_solver_ctl
!
      private :: hd_num_int_points, hd_minimum_comp, hd_omitted_ratio
      private :: hd_ordering_list, hd_start_node_ctl, hd_end_node_ctl
      private :: hd_start_nfree_mat, hd_end_nfree_mat, hd_minimum_det
      private :: hd_maximum_rms, hd_nele_filtering, hd_order_moments
      private :: hd_ref_filter, hd_maximum_neighbour
      private :: hd_tgt_filter_type, hd_filter_corection
      private :: hd_filter_fixed_point, hd_filter_negative_center
      private :: hd_err_level_commute, hd_momentum_type
      private :: hd_horiz_filter, hd_solver_type
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_filter_param_ctl(gen_f_ctl)
!
      use m_read_control_elements
      use skip_comment_f
!
      type(ctl_data_gen_filter), intent(inout) :: gen_f_ctl
!
!
      if(right_begin_flag(hd_filter_param_ctl) .eq. 0) return
      if (i_filter_param_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_filter_param_ctl = find_control_end_flag(hd_filter_param_ctl)
        if(i_filter_param_ctl .gt. 0) exit
!
!
        call read_CG_solver_param_ctl                                   &
     &     (hd_solver_ctl, i_solver_ctl, gen_f_ctl%CG_filter_ctl)
!
        call read_control_array_i_c_r(ctl_file_code,                    &
     &      hd_order_moments, gen_f_ctl%ref_filter_mom_ctl, c_buf1)
!
        call read_control_array_c_r(ctl_file_code,                      &
     &      hd_ref_filter, gen_f_ctl%reference_filter_ctl, c_buf1)
        call read_control_array_c_r(ctl_file_code,                      &
     &      hd_horiz_filter, gen_f_ctl%horizontal_filter_ctl, c_buf1)
!
!
        call read_chara_ctl_type                                        &
     &     (hd_solver_type, gen_f_ctl%f_solver_type_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_ordering_list, gen_f_ctl%ordering_list_ctl)
        call read_chara_ctl_type(hd_tgt_filter_type,                    &
     &      gen_f_ctl%tgt_filter_type_ctl)
        call read_chara_ctl_type(hd_momentum_type,                      &
     &      gen_f_ctl%momentum_type_ctl)
        call read_chara_ctl_type(hd_filter_corection,                   &
     &      gen_f_ctl%filter_correction_ctl)
        call read_chara_ctl_type(hd_filter_fixed_point,                 &
     &      gen_f_ctl%filter_fixed_point_ctl)
        call read_chara_ctl_type(hd_filter_negative_center,             &
     &      gen_f_ctl%negative_center_ctl)
!
!
        call read_real_ctl_type                                         &
     &     (hd_omitted_ratio, gen_f_ctl%omitted_ratio_ctl)
        call read_real_ctl_type                                         &
     &     (hd_minimum_det, gen_f_ctl%minimum_det_ctl)
        call read_real_ctl_type                                         &
     &     (hd_maximum_rms, gen_f_ctl%maximum_rms_ctl)
!
!
        call read_integer_ctl_type(hd_num_int_points,                   &
     &      gen_f_ctl%num_int_points_ctl)
        call read_integer_ctl_type(hd_minimum_comp,                     &
     &      gen_f_ctl%minimum_comp_ctl)
        call read_integer_ctl_type(hd_nele_filtering,                   &
     &      gen_f_ctl%num_ele_4_filter_ctl)
        call read_integer_ctl_type(hd_maximum_neighbour,                &
     &      gen_f_ctl%maximum_neighbour_ctl)
!
        call read_integer_ctl_type                                      &
     &     (hd_start_node_ctl, gen_f_ctl%start_node_ctl)
        call read_integer_ctl_type                                      &
     &     (hd_end_node_ctl, gen_f_ctl%end_node_ctl)
        call read_integer_ctl_type(hd_start_nfree_mat,                  &
     &      gen_f_ctl%ist_num_free_ctl)
        call read_integer_ctl_type(hd_end_nfree_mat,                    &
     &      gen_f_ctl%ied_num_free_ctl)
        call read_integer_ctl_type(hd_err_level_commute,                &
     &      gen_f_ctl%ilevel_filter_error_info)
      end do
!
      end subroutine read_filter_param_ctl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_filter_param_ctl(gen_f_ctl)
!
      type(ctl_data_gen_filter), intent(inout) :: gen_f_ctl
!
!
      call dealloc_control_array_c_r(gen_f_ctl%reference_filter_ctl)
      call dealloc_control_array_c_r(gen_f_ctl%horizontal_filter_ctl)
!
      call dealloc_control_array_i_c_r(gen_f_ctl%ref_filter_mom_ctl)
!
      gen_f_ctl%num_int_points_ctl%iflag =     0
      gen_f_ctl%minimum_comp_ctl%iflag =       0
      gen_f_ctl%num_ele_4_filter_ctl%iflag =   0
      gen_f_ctl%omitted_ratio_ctl%iflag =      0
      gen_f_ctl%minimum_det_ctl%iflag =        0
      gen_f_ctl%maximum_rms_ctl%iflag =        0
      gen_f_ctl%ordering_list_ctl%iflag =      0
      gen_f_ctl%tgt_filter_type_ctl%iflag =    0
      gen_f_ctl%momentum_type_ctl%iflag =      0
      gen_f_ctl%filter_correction_ctl%iflag =  0
      gen_f_ctl%filter_fixed_point_ctl%iflag = 0
      gen_f_ctl%negative_center_ctl%iflag =    0
!
      gen_f_ctl%maximum_neighbour_ctl%iflag =    0
      gen_f_ctl%ilevel_filter_error_info%iflag = 0
!
      gen_f_ctl%start_node_ctl%iflag =   0
      gen_f_ctl%end_node_ctl%iflag =     0
      gen_f_ctl%ist_num_free_ctl%iflag = 0
      gen_f_ctl%ied_num_free_ctl%iflag = 0
!
      gen_f_ctl%f_solver_type_ctl%iflag = 0
!
      end subroutine dealloc_filter_param_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_gen_filter
