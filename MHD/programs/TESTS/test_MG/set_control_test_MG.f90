!
!      module set_control_test_MG
!
!
!     Written by H. Matsui on July, 2006
!
!!      subroutine set_ctl_test_MG(plt, mesh_file)
!!        type(platform_data_control), intent(in) :: plt
!!        type(field_IO_params), intent(inout) :: mesh_file
!
      module set_control_test_MG(mesh_file)
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_ctl_test_MG(plt, mesh_file)
!
      use t_file_IO_parameter
      use t_ctl_data_4_platforms
      use set_solver_MG_control
      use set_control_platform_data
!
      type(platform_data_control), intent(in) :: plt
      type(field_IO_params), intent(inout) :: mesh_file
!
!
      call set_control_mesh_def(plt, mesh_file)
!
      np_smp = ione
      if(plt%num_smp_ctl%iflag .eq. 1) np_smp = num_smp_ctl%intvalue
!
!
      call s_set_solver_MG_control
!
      end subroutine set_ctl_test_MG
!
!  ---------------------------------------------------------------------
!
      end module set_control_test_MG
