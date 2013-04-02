!
!      module input_control_gen_table
!
      module input_control_gen_table
!
!     Written by H. Matsui on July, 2006
!
      use m_precision
!
      implicit none
!
!
!     subroutine s_input_control_generate_table
!
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_input_control_generate_table
!
      use m_parallel_var_dof
      use m_machine_parameter
      use m_read_mesh_data
      use m_ctl_params_4_gen_table
      use m_ctl_data_gen_table
!
      use load_mesh_data
!
!
      if (iflag_debug.eq.1) write(*,*) 'time_prog_barrier'
      call time_prog_barrier
!
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_gen_itp_table'
      call read_control_4_gen_itp_table
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_4_gen_table'
      call set_ctl_params_4_gen_table
!
      call time_prog_barrier
!
!  --  read geometry
!
      mesh_file_head = dest_mesh_head
      iflag_mesh_file_fmt = ifmt_itp_mesh_file
!
      if (iflag_debug.eq.1) write(*,*) 'input_mesh'
      call input_mesh(my_rank)
!
      end subroutine s_input_control_generate_table
!
! ----------------------------------------------------------------------
!
      end module input_control_gen_table
