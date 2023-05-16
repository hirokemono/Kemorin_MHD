!>@file   bcast_ctl_data_gen_filter.f90
!!@brief  module bcast_ctl_data_gen_filter
!!
!!@author H. Matsui
!!@date Programmed in March, 2004
!
!> @brief Distibute filter control data
!!
!!@verbatim
!!      subroutine bcast_filter_param_ctl(gen_f_ctl)
!!        type(ctl_data_gen_filter), intent(inout) :: gen_f_ctl
!!@endverbatim
!
      module bcast_ctl_data_gen_filter
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use t_ctl_data_gen_filter
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_filter_param_ctl(gen_f_ctl)
!
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(ctl_data_gen_filter), intent(inout) :: gen_f_ctl
!
!
      call bcast_ctl_array_cr(gen_f_ctl%reference_filter_ctl)
      call bcast_ctl_array_cr(gen_f_ctl%horizontal_filter_ctl)
!
      call bcast_ctl_array_icr(gen_f_ctl%ref_filter_mom_ctl)
!
      call bcast_ctl_type_i1(gen_f_ctl%num_int_points_ctl)
      call bcast_ctl_type_i1(gen_f_ctl%minimum_comp_ctl)
      call bcast_ctl_type_i1(gen_f_ctl%num_ele_4_filter_ctl)
      call bcast_ctl_type_r1(gen_f_ctl%omitted_ratio_ctl)
      call bcast_ctl_type_r1(gen_f_ctl%minimum_det_ctl)
      call bcast_ctl_type_r1(gen_f_ctl%maximum_rms_ctl)
      call bcast_ctl_type_c1(gen_f_ctl%ordering_list_ctl)
      call bcast_ctl_type_c1(gen_f_ctl%tgt_filter_type_ctl)
      call bcast_ctl_type_c1(gen_f_ctl%momentum_type_ctl)
      call bcast_ctl_type_c1(gen_f_ctl%filter_correction_ctl)
      call bcast_ctl_type_c1(gen_f_ctl%filter_fixed_point_ctl)
      call bcast_ctl_type_c1(gen_f_ctl%negative_center_ctl)
!
      call bcast_ctl_type_i1(gen_f_ctl%maximum_neighbour_ctl)
      call bcast_ctl_type_i1(gen_f_ctl%ilevel_filter_error_info)
!
      call bcast_ctl_type_i1(gen_f_ctl%start_node_ctl)
      call bcast_ctl_type_i1(gen_f_ctl%end_node_ctl)
      call bcast_ctl_type_i1(gen_f_ctl%ist_num_free_ctl)
      call bcast_ctl_type_i1(gen_f_ctl%ied_num_free_ctl)
!
      call bcast_ctl_type_c1(gen_f_ctl%f_solver_type_ctl)
!
      call calypso_mpi_bcast_one_int(gen_f_ctl%i_filter_param_ctl, 0)
!
      end subroutine bcast_filter_param_ctl
!
!   --------------------------------------------------------------------
!
      end module bcast_ctl_data_gen_filter
