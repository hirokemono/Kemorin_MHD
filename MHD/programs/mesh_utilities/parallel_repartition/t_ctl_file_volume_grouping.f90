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
!!    begin new_data_files_def
!!      num_subdomain_ctl    96
!!      mesh_file_prefix         'mesh_new/in'
!!      mesh_file_fmt_ctl    'merged_bin_gz'
!!    end new_data_files_def
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
      use t_control_array_integer
      use t_control_array_character
      use t_control_array_charaint
      use t_ctl_data_4_time_steps
      use t_ctl_data_volume_grouping
      use skip_comment_f
!
      implicit  none
!
      integer(kind = kint), parameter :: part_ctl_file_code = 11
      character(len = kchara), parameter                                &
     &                        :: fname_new_part_ctl = "ctl_repartition"
!
      type new_patition_test_control
!>        Structure for file controls
        type(platform_data_control) :: plt
!>        Structure for new file controls
        type(platform_data_control) :: new_plt
!
!>        Structure of mesh IO controls and sleeve informations
        type(FEM_mesh_control) :: Fmesh_ctl
!
!>        Structure for new partitioning controls
        type(new_patition_control) :: new_part_ctl
!
!>        Structure for time stepping control
        type(time_data_control) :: t_viz_ctl
!
        integer(kind = kint) :: i_mesh_test_ctl = 0
      end type new_patition_test_control
!
!     Top level
!
      character(len=kchara), parameter, private                         &
     &         :: hd_repartition_test_ctl = 'repartition_test'
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_platform =      'data_files_def'
      character(len=kchara), parameter, private                         &
     &                    :: hd_new_platform =  'new_data_files_def'
      character(len=kchara), parameter, private                         &
     &                    :: hd_FEM_mesh =      'FEM_mesh_ctl'
      character(len=kchara), parameter, private                         &
     &                    :: hd_new_partition = 'new_partitioning_ctl'
      character(len=kchara), parameter, private                         &
     &                    :: hd_time_step = 'time_step_ctl'
!
!
      private :: read_new_parition_data_ctl
      private :: bcast_control_new_partition
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
          call read_new_parition_data_ctl                               &
     &       (part_ctl_file_code, hd_repartition_test_ctl,              &
     &        part_tctl, c_buf1)
          if(part_tctl%i_mesh_test_ctl .gt. 0) exit
        end do
        close(part_ctl_file_code)
      end if
!
      call bcast_control_new_partition(part_tctl)
!
      end subroutine read_control_new_partition
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_new_partition(part_tctl)
!
      type(new_patition_test_control), intent(inout) :: part_tctl
!
!
      call reset_control_platforms(part_tctl%plt)
      call reset_control_platforms(part_tctl%new_plt)
      call reset_FEM_mesh_control(part_tctl%Fmesh_ctl)
      call dealloc_ctl_data_new_decomp(part_tctl%new_part_ctl)
!
      part_tctl%t_viz_ctl%i_tstep = 0
      part_tctl%i_mesh_test_ctl =   0
!
      end subroutine dealloc_control_new_partition
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_new_parition_data_ctl                             &
     &         (id_control, hd_block, part_tctl, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(new_patition_test_control), intent(inout) :: part_tctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(part_tctl%i_mesh_test_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, part_tctl%plt, c_buf)
        call read_control_platforms                                     &
     &     (id_control, hd_new_platform, part_tctl%new_plt, c_buf)
!
        call read_control_time_step_data                                &
     &     (id_control, hd_time_step, part_tctl%t_viz_ctl, c_buf)
!
        call read_FEM_mesh_control                                      &
     &     (id_control, hd_FEM_mesh, part_tctl%Fmesh_ctl, c_buf)
        call read_ctl_data_new_partition(id_control, hd_new_partition,  &
     &      part_tctl%new_part_ctl, c_buf)
!
      end do
      part_tctl%i_mesh_test_ctl = 1
!
      end subroutine read_new_parition_data_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_control_new_partition(part_tctl)
!
      use calypso_mpi
      use calypso_mpi_int
      use bcast_4_platform_ctl
      use bcast_4_time_step_ctl
!
      type(new_patition_test_control), intent(inout) :: part_tctl
!
!
      call bcast_ctl_data_4_platform(part_tctl%plt)
      call bcast_ctl_data_4_platform(part_tctl%new_plt)
      call bcast_ctl_data_4_time_step(part_tctl%t_viz_ctl)
!
      call bcast_FEM_mesh_control(part_tctl%Fmesh_ctl)
      call bcast_ctl_data_new_decomp(part_tctl%new_part_ctl)
!
      call calypso_mpi_bcast_one_int(part_tctl%i_mesh_test_ctl, 0)
!
      end subroutine bcast_control_new_partition
!
!   --------------------------------------------------------------------
!
      end module t_ctl_file_volume_grouping
