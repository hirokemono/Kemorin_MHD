!
!      module input_control_gen_table
!
!     Written by H. Matsui on July, 2006
!
!!     subroutine s_input_control_generate_table
!
      module input_control_gen_table
!
      use m_precision
      use t_mesh_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_input_control_generate_table
!
      use calypso_mpi
      use m_machine_parameter
      use m_ctl_params_4_gen_table
      use m_ctl_data_gen_table
!
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_gen_itp_table'
      call read_control_4_gen_itp_table
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_4_gen_table'
      call set_ctl_params_4_gen_table
!
      end subroutine s_input_control_generate_table
!
! ----------------------------------------------------------------------
!
      end module input_control_gen_table
