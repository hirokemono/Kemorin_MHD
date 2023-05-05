!bcast_ctl_data_comm_test.f90
!      module bcast_ctl_data_comm_test
!
!      Written by H. Matsui on Sep., 2007
!
!!      subroutine bcast_test_comm_ctl_data(comm_tctl)
!!        type(comm_test_control), intent(inout) :: comm_tctl
!
      module bcast_ctl_data_comm_test
!
      use m_precision
      use calypso_mpi
      use t_ctl_data_comm_test
!
      implicit  none
!
!   --------------------------------------------------------------------
!
      contains
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
