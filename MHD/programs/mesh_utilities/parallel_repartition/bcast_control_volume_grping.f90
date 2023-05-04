!>@file   bcast_control_volume_grping.f90
!!@brief  module bcast_control_volume_grping
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine read_ctl_file_new_partition(part_tctl)
!!      subroutine dealloc_control_new_partition(part_tctl)
!!        type(new_patition_test_control), intent(inout) :: part_tctl
!!
!!   --------------------------------------------------------------------
!!    Example of control block
!!
!!  begin mesh_test
!!    begin data_files_def
!!      num_subdomain_ctl    96
!!      mesh_file_prefix         'mesh/in'
!!      mesh_file_fmt_ctl    'merged_bin'
!!    end data_files_def
!!
!!    begin viz_repartition_ctl
!!      ...
!!    end viz_repartition_ctl
!!
!!    begin time_step_ctl
!!      ...
!!    end time_step_ctl
!!  end  mesh_test
!! -------------------------------------------------------------------
!!@endverbatim
      module bcast_control_volume_grping
!
      use m_precision
      use m_machine_parameter
!
      use calypso_mpi
      use t_ctl_file_volume_grouping
!
      implicit  none
!
      character(len = kchara), parameter                                &
     &                        :: fname_new_part_ctl = "ctl_repartition"
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine load_ctl_file_new_partition(part_tctl)
!
      type(new_patition_test_control), intent(inout) :: part_tctl
!
!
      if(my_rank .eq. 0) then
        call read_ctl_file_new_partition(fname_new_part_ctl, part_tctl)
      end if
!
      call bcast_control_new_partition(part_tctl)
!
      end subroutine load_ctl_file_new_partition
!
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
      end module bcast_control_volume_grping
