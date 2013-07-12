!set_control_visualizer.f90
!      module set_control_visualizer
!
!     Written by H. Matsui on May., 2006
!
!      subroutine set_control_params_4_viz(my_rank, ierr)
!
      module set_control_visualizer
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_control_params_4_viz(my_rank, ierr)
!
      use m_read_mesh_data
      use m_ucd_data
      use m_ctl_data_4_platforms
      use m_ctl_data_4_time_steps
      use m_file_format_switch
      use m_t_step_parameter
      use set_control_platform_data
      use set_fixed_time_step_params
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(inout) :: ierr
!
!
      call turn_off_debug_flag_by_ctl(my_rank)
      call set_control_smp_def
      call set_control_mesh_def
!
      call s_set_fixed_time_step_params(ierr, e_message)
      if(ierr .gt. 0) return
!
      if (i_udt_header .eq. 1) org_ucd_header = udt_file_head_ctl
!
      call choose_ucd_file_format(udt_file_fmt_ctl, i_udt_files_fmt,    &
     &    fem_ucd%ifmt_file)
!
      end subroutine set_control_params_4_viz
!
!  ---------------------------------------------------------------------
!
      end module set_control_visualizer
