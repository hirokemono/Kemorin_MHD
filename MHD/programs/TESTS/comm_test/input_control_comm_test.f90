!
!      module input_control_comm_test
!
!     Written by H. Matsui on July, 2006
!
!     subroutine s_input_control_comm_test
!
      module input_control_comm_test
!
      use m_precision
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
      subroutine s_input_control_comm_test
!
      use calypso_mpi
      use m_machine_parameter
      use m_read_mesh_data
!
      use m_ctl_data_comm_test
      use load_mesh_data
      use element_IO_select
      use surface_IO_select
      use edge_IO_select
      use set_surface_geometry_4_IO
      use set_edge_geometry_4_IO
!
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_comm_test'
      call read_control_4_comm_test
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_4_comm_test'
      call set_ctl_params_4_comm_test
!
!  --  read geometry
!
      if (iflag_debug.eq.1) write(*,*) 'input_mesh'
      call input_mesh(my_rank)
!
      end subroutine s_input_control_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_ctl_params_4_comm_test
!
      use calypso_mpi
      use m_read_mesh_data
      use set_control_platform_data
!
!
      call turn_off_debug_flag_by_ctl(my_rank)
      call set_control_smp_def(my_rank)
      call set_control_mesh_def
!
      end subroutine set_ctl_params_4_comm_test
!
!   --------------------------------------------------------------------
!
      end module input_control_comm_test
