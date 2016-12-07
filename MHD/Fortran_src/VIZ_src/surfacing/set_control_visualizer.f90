!set_control_visualizer.f90
!      module set_control_visualizer
!
!     Written by H. Matsui on May., 2006
!
!!      subroutine set_control_params_4_viz                             &
!!     &          (my_rank, ierr, rst_org_param, udt_org_param, ucd)
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
     &          (my_rank, ierr, rst_org_param, udt_org_param, ucd)
!
      use t_ucd_data
      use t_file_IO_parameter
!
      use m_read_mesh_data
      use m_ctl_data_4_platforms
      use m_ctl_data_4_time_steps
      use m_file_format_switch
      use m_t_step_parameter
      use set_control_platform_data
      use set_fixed_time_step_params
      use set_ctl_params_2nd_files
      use ucd_IO_select
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(inout) :: ierr
      type(field_IO_params), intent(inout) :: rst_org_param
      type(field_IO_params), intent(inout) :: udt_org_param
      type(ucd_data), intent(inout) :: ucd
!
!
      call turn_off_debug_flag_by_ctl(my_rank)
      call set_control_smp_def(my_rank)
      call set_control_mesh_def(mesh1_file)
      call set_ucd_file_define(ucd)
      call set_control_org_rst_file_def(rst_org_param)
      call set_control_org_udt_file_def(udt_org_param)
!
      call s_set_fixed_time_step_params(ierr, e_message)
      if(ierr .gt. 0) return
!
      end subroutine set_control_params_4_viz
!
!  ---------------------------------------------------------------------
!
      end module set_control_visualizer
