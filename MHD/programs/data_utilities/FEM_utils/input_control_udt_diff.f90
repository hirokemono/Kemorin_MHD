!
!      module input_control_udt_diff
!
!     Written by H. Matsui on July, 2006
!
!      subroutine s_input_control_udt_diff
!      subroutine s_input_control_ave_udt
!      subroutine s_input_control_corr_udt
!      subroutine s_input_control_grp_patch
!
!      subroutine s_input_mesh_udt_diff
!
      module input_control_udt_diff
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_input_control_udt_diff
!
      use m_ctl_params_4_diff_udt
      use m_ctl_data_diff_udt
!
      use set_ctl_diff_udt
      use set_control_nodal_data
!
      use m_parallel_var_dof
!
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_diff_udt'
      call read_control_4_diff_udt
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_diff_udt'
      call set_ctl_params_diff_udt
!
      if (iflag_debug.eq.1) write(*,*) 's_set_control_nodal_data'
      call s_set_control_nodal_data(ierr)
      if (ierr .ne. 0) call parallel_abort(ierr, e_message)
!
      if (iflag_debug.eq.1) write(*,*) 's_set_ctl_4_diff_udt_steps'
      call s_set_ctl_4_diff_udt_steps
!
      end subroutine s_input_control_udt_diff
!
! ----------------------------------------------------------------------
!
      subroutine s_input_control_ave_udt
!
      use m_ctl_params_4_diff_udt
      use m_ctl_data_diff_udt
!
      use set_ctl_diff_udt
      use set_control_nodal_data
!
      use m_parallel_var_dof
!
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_ave_udt'
      call read_control_4_ave_udt
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_diff_udt'
      call set_ctl_params_diff_udt
!
      if (iflag_debug.eq.1) write(*,*) 's_set_control_nodal_data'
      call s_set_control_nodal_data(ierr)
      if (ierr .ne. 0) call parallel_abort(ierr, e_message)
!
      if (iflag_debug.eq.1) write(*,*) 's_set_ctl_4_diff_udt_steps'
      call s_set_ctl_4_diff_udt_steps
!
      end subroutine s_input_control_ave_udt
!
! ----------------------------------------------------------------------
!
      subroutine s_input_control_corr_udt
!
      use m_ctl_params_4_diff_udt
      use m_ctl_data_diff_udt
!
      use set_ctl_diff_udt
!
      use m_parallel_var_dof
!
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_corr_udt'
      call read_control_4_corr_udt
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_correlate_udt'
      call set_ctl_params_correlate_udt
!
      end subroutine s_input_control_corr_udt
!
! ----------------------------------------------------------------------
!
      subroutine s_input_control_grp_patch
!
      use m_ctl_params_4_diff_udt
      use m_ctl_data_diff_udt
!
      use set_ctl_diff_udt
!
      use m_parallel_var_dof
!
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_med_grp_patch'
      call read_control_med_grp_patch
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_diff_udt'
      call set_ctl_params_diff_udt
!
      end subroutine s_input_control_grp_patch
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine s_input_mesh_udt_diff
!
      use m_parallel_var_dof
      use m_ctl_params_4_diff_udt
      use load_mesh_data
!
!
!  --  read mesh for target
!
      if (iflag_debug.eq.1) write(*,*) 'input_mesh'
      call input_mesh(my_rank)
!
      end subroutine s_input_mesh_udt_diff
!
! ----------------------------------------------------------------------
!
      end module input_control_udt_diff
