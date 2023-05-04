!bcast_ctl_data_comm_test.f90
!      module bcast_ctl_data_comm_test
!
!      Written by H. Matsui on Sep., 2007
!
!!      subroutine load_control_4_comm_test(comm_tctl)
!!        type(comm_test_control), intent(inout) :: comm_tctl
!!
!!   --------------------------------------------------------------------
!!    Example of control block
!!
!!  begin mesh_test
!!    begin data_files_def
!!      num_smp_ctl            1
!!      mesh_file_prefix         'mesh/in'
!!    end data_files_def
!!    begin FEM_mesh_ctl
!!      FEM_surface_output_switch      'NO'
!!      FEM_viewer_mesh_output_switch  'NO'
!!    end FEM_mesh_ctl
!!  end  mesh_test
!!
!!    -------------------------------------------------------------------
!
      module bcast_ctl_data_comm_test
!
      use m_precision
      use calypso_mpi
      use t_ctl_data_comm_test
!
      implicit  none
!
      character(len = kchara), parameter                                &
     &                        :: fname_test_mesh_ctl = "ctl_mesh"
!
      private :: bcast_test_comm_ctl_data
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine load_control_4_comm_test(comm_tctl)
!
      use skip_comment_f
!
      type(comm_test_control), intent(inout) :: comm_tctl
!
      if(my_rank .eq. 0) then
        call read_control_4_comm_test(fname_test_mesh_ctl, comm_tctl)
      end if
!
      call bcast_test_comm_ctl_data(comm_tctl)
!
      end subroutine load_control_4_comm_test
!
!   --------------------------------------------------------------------
!
      subroutine bcast_test_comm_ctl_data(comm_tctl)
!
      use calypso_mpi_int
      use bcast_4_platform_ctl
!
      type(comm_test_control), intent(inout) :: comm_tctl
!
!
      call bcast_ctl_data_4_platform(comm_tctl%plt)
      call bcast_FEM_mesh_control(comm_tctl%Fmesh_ctl)
!
      call calypso_mpi_bcast_one_int(comm_tctl%i_mesh_test_ctl, 0)
!
      end subroutine bcast_test_comm_ctl_data
!
!   --------------------------------------------------------------------
!
      end module bcast_ctl_data_comm_test
