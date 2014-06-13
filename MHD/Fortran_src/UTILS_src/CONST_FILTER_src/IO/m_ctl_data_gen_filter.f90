!
!      module m_ctl_data_gen_filter
!
!      Written by H. Matsui on July, 2006
!
!      subroutine deallocate_mom_param_ctl
!      subroutine deallocate_ref_filter_ctl
!      subroutine deallocate_horiz_filter_ctl
!
!      subroutine read_filter_param_ctl
!
      module m_ctl_data_gen_filter
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use m_ctl_data_4_solvers
      use t_read_control_arrays
!
      implicit  none
!
!
      integer(kind = kint) :: num_int_points_ctl =   3
      integer(kind = kint) :: minimum_comp_ctl =    11
      integer(kind = kint) :: num_ele_4_filter_ctl = 2
      real(kind = kreal) :: omitted_ratio_ctl = 1.0d-30
      real(kind = kreal) :: minimum_det_ctl =   1.0d+01
      real(kind = kreal) :: maximum_rms_ctl =   2.0d+00
      character(len=kchara) :: ordering_list_ctl =   'connection'
      character(len=kchara) :: tgt_filter_type_ctl = 'Commutative'
      character(len=kchara) :: momentum_type_ctl =   'Normal'
      character(len=kchara) :: filter_correction_ctl =  'OFF'
      character(len=kchara) :: filter_fixed_point_ctl = 'OFF'
      character(len=kchara) :: negative_center_ctl =    'OFF'
!
      integer(kind = kint) :: maximum_neighbour_ctl = 2
      integer(kind = kint) :: ilevel_filter_error_info = 0
!
      integer(kind = kint) :: start_node_ctl = 1
      integer(kind = kint) :: end_node_ctl =  -1
      integer(kind = kint) :: ist_num_free_ctl =  -1
      integer(kind = kint) :: ied_num_free_ctl =  -1
!
!>      Structure for list of reference filter mode
!!@n      reference_filter_ctl%c_tbl: list of filter type
!!@n      reference_filter_ctl%vect:  list of filter width
      type(ctl_array_cr), save  :: reference_filter_ctl
!
!>      Structure for list of horizontal filter mode
!!@n      horizontal_filter_ctl%c_tbl: list of filter type
!!@n      horizontal_filter_ctl%vect:  list of filter width
      type(ctl_array_cr), save  :: horizontal_filter_ctl
!
!>      Structure for reference moments for filter
!!@n      ref_filter_mom_ctl%ivec:  Order of reference filter moments
!!@n      ref_filter_mom_ctl%c_tbl: Type of reference filter moments
!!@n      ref_filter_mom_ctl%vect:  Value of filter moments
      type(ctl_array_icr), save :: ref_filter_mom_ctl
!
      character(len=kchara) :: solver_type_ctl = 'CRS'
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
      integer (kind=kint) :: i_num_int_points =     0
      integer (kind=kint) :: i_minimum_comp =       0
      integer (kind=kint) :: i_omitted_ratio =      0
      integer (kind=kint) :: i_ordering_list =      0
      integer (kind=kint) :: i_start_node_ctl =     0
      integer (kind=kint) :: i_end_node_ctl =       0
      integer (kind=kint) :: i_start_nfree_mat =    0
      integer (kind=kint) :: i_end_nfree_mat =      0
      integer (kind=kint) :: i_minimum_det =        0
      integer (kind=kint) :: i_maximum_rms =        0
      integer (kind=kint) :: i_nele_filtering =     0
      integer (kind=kint) :: i_maximum_neighbour =  0
      integer (kind=kint) :: i_tgt_filter_type =    0
      integer (kind=kint) :: i_filter_corection =   0
      integer (kind=kint) :: i_filter_fixed_point = 0
      integer (kind=kint) :: i_filter_negative_center = 0
      integer (kind=kint) :: i_err_level_commute =  0
      integer (kind=kint) :: i_momentum_type =      0
      integer (kind=kint) :: i_solver_type =        0
!
      private :: hd_filter_param_ctl, i_filter_param_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_ref_filter_ctl
!
      call dealloc_control_array_c_r(reference_filter_ctl)
!
      end subroutine deallocate_ref_filter_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_horiz_filter_ctl
!
      call dealloc_control_array_c_r(horizontal_filter_ctl)
!
      end subroutine deallocate_horiz_filter_ctl
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_filter_param_ctl
!
      use m_read_control_elements
      use skip_comment_f
!
      integer(kind = kint) :: ierr
!
!
      if(right_begin_flag(hd_filter_param_ctl) .eq. 0) return
      if (i_filter_param_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_filter_param_ctl,                 &
     &      i_filter_param_ctl)
        if(i_filter_param_ctl .gt. 0) exit
!
!
        call read_crs_solver_param_ctl(ierr)
        if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
        call read_DJDS_solver_param_ctl
!
!
        call read_control_array_i_c_r                                   &
     &     (hd_order_moments, ref_filter_mom_ctl)
!
        call read_control_array_c_r                                     &
     &     (hd_ref_filter, reference_filter_ctl)
        call read_control_array_c_r                                     &
     &     (hd_horiz_filter, horizontal_filter_ctl)
!
!
        call read_character_ctl_item(hd_solver_type,                    &
     &          i_solver_type, solver_type_ctl)
        call read_character_ctl_item(hd_ordering_list,                  &
     &          i_ordering_list, ordering_list_ctl)
        call read_character_ctl_item(hd_tgt_filter_type,                &
     &          i_tgt_filter_type, tgt_filter_type_ctl)
        call read_character_ctl_item(hd_momentum_type,                  &
     &          i_momentum_type, momentum_type_ctl)
        call read_character_ctl_item(hd_filter_corection,               &
     &          i_filter_corection, filter_correction_ctl)
        call read_character_ctl_item(hd_filter_fixed_point,             &
     &          i_filter_fixed_point, filter_fixed_point_ctl)
        call read_character_ctl_item(hd_filter_negative_center,         &
     &          i_filter_negative_center, negative_center_ctl)
!
!
        call read_real_ctl_item(hd_omitted_ratio,                       &
     &          i_omitted_ratio, omitted_ratio_ctl)
        call read_real_ctl_item(hd_minimum_det,                         &
     &          i_minimum_det, minimum_det_ctl)
        call read_real_ctl_item(hd_maximum_rms,                         &
     &          i_maximum_rms, maximum_rms_ctl)
!
!
        call read_integer_ctl_item(hd_num_int_points,                   &
     &          i_num_int_points, num_int_points_ctl)
        call read_integer_ctl_item(hd_minimum_comp,                     &
     &          i_minimum_comp, minimum_comp_ctl)
        call read_integer_ctl_item(hd_nele_filtering,                   &
     &          i_nele_filtering, num_ele_4_filter_ctl)
        call read_integer_ctl_item(hd_maximum_neighbour,                &
     &          i_maximum_neighbour, maximum_neighbour_ctl)
!
        call read_integer_ctl_item(hd_start_node_ctl,                   &
     &          i_start_node_ctl, start_node_ctl)
        call read_integer_ctl_item(hd_end_node_ctl,                     &
     &          i_end_node_ctl, end_node_ctl)
        call read_integer_ctl_item(hd_start_nfree_mat,                  &
     &          i_start_nfree_mat, ist_num_free_ctl)
        call read_integer_ctl_item(hd_end_nfree_mat,                    &
     &          i_end_nfree_mat, ied_num_free_ctl)
        call read_integer_ctl_item(hd_err_level_commute,                &
     &          i_err_level_commute, ilevel_filter_error_info)
      end do
!
      end subroutine read_filter_param_ctl
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_gen_filter
