!
!      module input_control_gen_filter
!
!     Written by H. Matsui on July, 2006
!
!     subroutine input_control_3d_commute
!
      module input_control_gen_filter
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine input_control_3d_commute
!
      use m_machine_parameter
      use m_parallel_var_dof
!
      use read_ctl_gen_filter
      use set_ctl_gen_filter
      use load_mesh_data
!
!
      if (iflag_debug.eq.1) write(*,*) 'time_prog_barrier'
      call time_prog_barrier
!
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_gen_filter'
      call read_control_4_gen_filter
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_gen_filter'
      call set_ctl_params_gen_filter
!
      call time_prog_barrier
!
!  --  read geometry
!
      if (iflag_debug.eq.1) write(*,*) 'input_mesh'
      call input_mesh(my_rank)
!
      end subroutine input_control_3d_commute
!
! ----------------------------------------------------------------------
!
      end module input_control_gen_filter
