!>@file   t_ctl_data_gen_filter.f90
!!@brief  module t_ctl_data_gen_filter
!!
!!@author H. Matsui
!!@date Programmed in March, 2004
!
!> @brief  filter control data structure
!!
!!@verbatim
!!      subroutine dealloc_filter_param_ctl(gen_f_ctl)
!!        type(ctl_data_gen_filter), intent(inout) :: gen_f_ctl
!!@endverbatim
!
      module t_ctl_data_gen_filter
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
      use t_control_array_charareal
      use t_control_array_intcharreal
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
!
        integer (kind=kint) :: i_filter_param_ctl =   0
      end type ctl_data_gen_filter
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
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
      gen_f_ctl%i_filter_param_ctl = 0
!
      end subroutine dealloc_filter_param_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_gen_filter
