!set_control_visualizer.f90
!      module set_control_visualizer
!
!     Written by H. Matsui on May., 2006
!
!!      subroutine set_control_params_4_viz                             &
!!     &         (my_rank, tctl, plt, mesh_file, ucd, ierr)
!!        type(time_data_control), intent(in) :: tctl
!!        type(platform_data_control), intent(inout) :: plt
!!        type(field_IO_params), intent(inout) :: mesh_file
!!        type(ucd_data), intent(inout) :: ucd
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
      subroutine set_control_params_4_viz                               &
     &         (my_rank, tctl, plt, mesh_file, ucd, ierr)
!
      use t_ucd_data
      use t_file_IO_parameter
      use t_ctl_data_4_platforms
      use t_ctl_data_4_time_steps
!
      use m_file_format_switch
      use m_t_step_parameter
      use m_default_file_prefix
      use set_control_platform_data
      use set_fixed_time_step_params
      use ucd_IO_select
!
      integer(kind = kint), intent(in) :: my_rank
      type(time_data_control), intent(in) :: tctl
!
      integer(kind = kint), intent(inout) :: ierr
      type(platform_data_control), intent(inout) :: plt
      type(field_IO_params), intent(inout) :: mesh_file
      type(ucd_data), intent(inout) :: ucd
!
!
      call turn_off_debug_flag_by_ctl(my_rank, plt)
      call set_control_smp_def(my_rank, plt)
      call set_control_mesh_def(plt, mesh_file)
      call set_ucd_file_define(plt, ucd)
!
      call s_set_fixed_time_step_params(tctl, ierr, e_message)
      if(ierr .gt. 0) return
!
      end subroutine set_control_params_4_viz
!
!  ---------------------------------------------------------------------
!
      end module set_control_visualizer
