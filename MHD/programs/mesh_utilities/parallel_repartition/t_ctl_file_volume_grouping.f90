!>@file   t_ctl_file_volume_grouping.f90
!!@brief  module t_ctl_file_volume_grouping
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine read_control_new_partition(part_tctl)
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
!!    begin viz_data_files_def
!!      num_subdomain_ctl    96
!!      mesh_file_prefix         'mesh_new/in'
!!      mesh_file_fmt_ctl    'merged_bin_gz'
!!    end viz_data_files_def
!!
!!    begin FEM_mesh_ctl
!!      ...
!!    end FEM_mesh_ctl
!!
!!    begin new_partitioning_ctl
!!      ...
!!    end new_partitioning_ctl
!!
!!    begin time_step_ctl
!!      ...
!!    end time_step_ctl
!!  end  mesh_test
!! -------------------------------------------------------------------
!!@endverbatim
      module t_ctl_file_volume_grouping
!
      use m_precision
      use m_machine_parameter
      use t_ctl_data_4_platforms
      use t_ctl_data_4_FEM_mesh
!
      use t_read_control_elements
      use t_ctl_data_volume_repart
      use t_ctl_data_4_time_steps
!
      implicit  none
!
      integer(kind = kint), parameter :: part_ctl_file_code = 11
      character(len = kchara), parameter                                &
     &                        :: fname_new_part_ctl = "ctl_repartition"
!
!     Top level
!
      character(len=kchara), parameter, private                         &
     &         :: hd_repartition_test_ctl = 'repartition_test'
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_control_new_partition(part_tctl)
!
      use calypso_mpi
      use skip_comment_f
      use bcast_4_platform_ctl
      use t_read_control_elements
!
      type(new_patition_test_control), intent(inout) :: part_tctl
!
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
        open(part_ctl_file_code, file=fname_new_part_ctl,status='old')
!
        do
          call load_one_line_from_control(part_ctl_file_code, c_buf1)
          call t_ctl_data_volume_repart                                 &
     &       (part_ctl_file_code, hd_repartition_test_ctl,              &
     &        part_tctl, c_buf1)
          if(part_tctl%i_mesh_test_ctl .gt. 0) exit
        end do
        close(part_ctl_file_code)
      end if
!
      call bcast_control_vol_repart(part_tctl)
!
      end subroutine read_control_new_partition
!
!   --------------------------------------------------------------------
!
      end module t_ctl_file_volume_grouping
