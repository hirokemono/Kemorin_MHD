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
      use m_parallel_var_dof
      use m_machine_parameter
      use m_read_mesh_data
!
      use m_ctl_data_comm_test
      use load_mesh_data
      use element_IO_select
      use surface_IO_select
      use edge_IO_select
      use set_ele_comm_tbl_4_IO
      use set_surf_comm_tbl_4_IO
      use set_edge_comm_tbl_4_IO
      use set_surface_geometry_4_IO
      use set_edge_geometry_4_IO
!
!
      if (iflag_debug.eq.1) write(*,*) 'time_prog_barrier'
      call time_prog_barrier
!
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_comm_test'
      call read_control_4_comm_test
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_4_comm_test'
      call set_ctl_params_4_comm_test
!
      call time_prog_barrier
!
!  --  read geometry
!
      if (iflag_debug.eq.1) write(*,*) 'input_mesh'
      call input_mesh(my_rank)
!
!  --  read element commumication table
!
      if (iflag_ele_file_name .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'sel_input_element_comm_table'
        call sel_input_element_comm_table(my_rank)
        call copy_ele_comm_tbl_from_IO
      end if
!
!  --  read surface connection
!
      if (iflag_surf_file_name .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'sel_input_surface_connect'
        call sel_input_surface_connect(my_rank)
        call copy_surf_comm_table_from_IO
        call copy_surf_connect_from_IO
      end if
!
!
!  --  read edge connection
!
      if (iflag_edge_file_name .gt. 0) then
        if (iflag_debug.eq.1) write(*,*) 'sel_input_edge_connect'
        call sel_input_edge_connect(my_rank)
        call copy_edge_comm_tbl_from_IO
        call copy_edge_connect_from_IO
      end if
!
      end subroutine s_input_control_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_ctl_params_4_comm_test
!
      use m_parallel_var_dof
      use m_read_mesh_data
      use set_control_platform_data
      use set_control_surface_mesh
!
!
      call turn_off_debug_flag_by_ctl(my_rank)
      call set_control_smp_def(my_rank)
      call set_control_mesh_def
      call set_control_surf_mesh_def
!
      end subroutine set_ctl_params_4_comm_test
!
!   --------------------------------------------------------------------
!
      end module input_control_comm_test
