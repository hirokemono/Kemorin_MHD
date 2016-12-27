!
!      module input_control_comm_test
!
!     Written by H. Matsui on July, 2006
!
!!     subroutine s_input_control_comm_test(mesh_file)
!!      type(field_IO_params), intent(inout) ::  mesh_file
!
      module input_control_comm_test
!
      use m_precision
      use t_file_IO_parameter
!
      implicit none
!
      private :: set_ctl_params_4_comm_test
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_input_control_comm_test(mesh_file)
!
      use calypso_mpi
      use m_machine_parameter
      use m_ctl_data_comm_test
!
      use set_surface_data_4_IO
      use set_edge_data_4_IO
!
      type(field_IO_params), intent(inout) ::  mesh_file
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_comm_test'
      call read_control_4_comm_test
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_4_comm_test'
      call set_ctl_params_4_comm_test(mesh_file)
!
      end subroutine s_input_control_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_ctl_params_4_comm_test(mesh_file)
!
      use calypso_mpi
      use set_control_platform_data
      use m_ctl_data_4_platforms
!
      type(field_IO_params), intent(inout) ::  mesh_file
!
      call turn_off_debug_flag_by_ctl(my_rank, plt1)
      call set_control_smp_def(my_rank, plt1)
      call set_control_mesh_def(plt1, mesh_file)
!
      end subroutine set_ctl_params_4_comm_test
!
!   --------------------------------------------------------------------
!
      end module input_control_comm_test
