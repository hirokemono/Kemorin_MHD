!>@file   t_control_param_repartition.f90
!!@brief  module t_control_param_repartition
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine input_control_new_partition(ctl_file_name,           &
!!     &                                       part_prog_p)
!!      subroutine input_control_field_to_repart(ctl_file_name,         &
!!     &                                       part_prog_p, t_viz_param)
!!        type(vol_partion_prog_param), intent(inout) :: part_prog_p
!!        type(time_step_param_w_viz), intent(inout) :: t_viz_param
!!@endverbatim
!
      module t_control_param_repartition
!
      use m_precision
      use calypso_mpi
      use t_file_IO_parameter
      use t_control_param_vol_grping
      use t_ctl_file_volume_grouping
      use t_read_control_elements
!
      implicit none
!
      type vol_partion_prog_param
!>        Structure of mesh file IO paramters
        type(field_IO_params) :: mesh_file
!>        Structure for original field file  paramters
        type(field_IO_params) :: org_ucd_file
!
!>        Structure for volume repartitiong paramteres
        type(volume_partioning_param) :: repart_p
      end type vol_partion_prog_param
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine input_control_new_partition(ctl_file_name,             &
     &                                       part_prog_p)
!
      character(len = kchara), intent(in) :: ctl_file_name
      type(vol_partion_prog_param), intent(inout) :: part_prog_p
!
      type(new_patition_test_control) :: part_tctl
      type(buffer_for_control) :: c_buf1
!
!
      c_buf1%level = 0
      if(my_rank .eq. 0) then
        call read_ctl_file_new_partition(ctl_file_name,                 &
     &                                   part_tctl, c_buf1)
      end if
      call bcast_control_new_partition(part_tctl)
!
      if(c_buf1%iend .gt. 0) then
        call calypso_MPI_abort(part_tctl%i_mesh_test_ctl,               &
     &                             'control file is broken')
      end if
!
      call set_control_param_repartition(part_tctl, part_prog_p)
      call dealloc_control_new_partition(part_tctl)
!
      end subroutine input_control_new_partition
!
!   --------------------------------------------------------------------
!
      subroutine input_control_field_to_repart(ctl_file_name,           &
     &                                       part_prog_p, t_viz_param)
!
      use t_VIZ_only_step_parameter
!
      character(len = kchara), intent(in) :: ctl_file_name
      type(vol_partion_prog_param), intent(inout) :: part_prog_p
      type(time_step_param_w_viz), intent(inout) :: t_viz_param
!
      type(new_patition_test_control) :: part_tctl
      integer(kind = kint) :: ierr
      type(buffer_for_control) :: c_buf1
!
!
      c_buf1%level = 0
      if(my_rank .eq. 0) then
        call read_ctl_file_new_partition(ctl_file_name,                 &
     &                                    part_tctl, c_buf1)
      end if
      call bcast_control_new_partition(part_tctl)
!
      if(c_buf1%iend .gt. 0) then
        call calypso_MPI_abort(part_tctl%i_mesh_test_ctl,               &
     &                             'control file is broken')
      end if
!
      call set_control_param_repartition(part_tctl, part_prog_p)
!
      call set_fixed_t_step_params_w_viz                                &
     &   (part_tctl%t_viz_ctl, t_viz_param, ierr, e_message)
      call copy_delta_t(t_viz_param%init_d, t_viz_param%time_d)
      call dealloc_control_new_partition(part_tctl)
!
      end subroutine input_control_field_to_repart
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_control_new_partition(part_tctl)
!
      use calypso_mpi_int
      use calypso_mpi_char
      use bcast_control_arrays
      use transfer_to_long_integers
      use bcast_4_platform_ctl
      use bcast_4_time_step_ctl
      use bcast_ctl_data_vol_repart
!
      type(new_patition_test_control), intent(inout) :: part_tctl
!
!
      call bcast_ctl_data_4_platform(part_tctl%plt)
      call bcast_ctl_data_4_time_step(part_tctl%t_viz_ctl)
!
      call bcast_control_vol_repart(part_tctl%viz_repart_c)
!
      call calypso_mpi_bcast_one_int(part_tctl%i_mesh_test_ctl, 0)
      call calypso_mpi_bcast_character(part_tctl%fname_vol_repart_ctl,  &
     &                                 cast_long(kchara), 0)
!
      end subroutine bcast_control_new_partition
!
!   --------------------------------------------------------------------
!
      subroutine set_control_param_repartition(part_tctl, part_prog_p)
!
      use m_error_IDs
      use m_machine_parameter
!
      use m_machine_parameter
      use m_file_format_switch
      use set_control_platform_item
      use set_control_platform_data
      use set_ctl_parallel_platform
      use parallel_ucd_IO_select
!
      type(new_patition_test_control), intent(inout) :: part_tctl
      type(vol_partion_prog_param), intent(inout) :: part_prog_p
!
!
      call turn_off_debug_flag_by_ctl(my_rank, part_tctl%plt)
      call check_control_num_domains(part_tctl%plt)
      call set_control_parallel_mesh(part_tctl%plt,                     &
     &                               part_prog_p%mesh_file)
      call set_control_smp_def(my_rank, part_tctl%plt)
!
      call set_merged_ucd_file_define(part_tctl%plt,                    &
     &                                part_prog_p%org_ucd_file)
!
      call set_ctl_param_vol_repart(part_tctl%viz_repart_c,             &
     &                              part_prog_p%repart_p)
!
      if(iflag_debug.gt.0) write(*,*)                                   &
     &   'mesh_file_head:  ', trim(part_prog_p%mesh_file%file_prefix)
!
      end subroutine set_control_param_repartition
!
!  ---------------------------------------------------------------------
!
      end module t_control_param_repartition
