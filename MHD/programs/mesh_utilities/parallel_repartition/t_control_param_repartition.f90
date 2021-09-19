!>@file   t_control_param_repartition.f90
!!@brief  module t_control_param_repartition
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine set_control_param_repartition(part_tctl, part_prog_p)
!!        type(mesh_test_control), intent(in) :: part_tctl
!!        type(vol_partion_prog_param), intent(inout) :: part_prog_p
!!@endverbatim
!
      module t_control_param_repartition
!
      use m_precision
      use t_file_IO_parameter
      use t_control_param_vol_grping
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
      subroutine set_control_param_repartition(part_tctl, part_prog_p)
!
      use calypso_mpi
      use m_error_IDs
      use m_machine_parameter
!
      use t_ctl_file_volume_grouping
      use m_machine_parameter
      use m_file_format_switch
      use set_control_platform_item
      use set_control_platform_data
      use set_ctl_parallel_platform
      use parallel_ucd_IO_select
      use mpi_abort_by_missing_zlib
!
      type(new_patition_test_control), intent(inout) :: part_tctl
      type(vol_partion_prog_param), intent(inout) :: part_prog_p
!
!
      call turn_off_debug_flag_by_ctl(my_rank, part_tctl%plt)
      call check_control_num_domains(part_tctl%plt)
      call set_control_parallel_mesh_def(part_tctl%plt,                 &
     &                                   part_prog_p%mesh_file)
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
