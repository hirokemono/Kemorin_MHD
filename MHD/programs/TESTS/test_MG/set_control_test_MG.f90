!
!      module set_control_test_MG
!
!
!     Written by H. Matsui on July, 2006
!
!      subroutine set_ctl_test_MG
!
      module set_control_test_MG(mesh_file)
!
      use m_precision
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_ctl_test_MG(mesh_file)
!
      use t_file_IO_parameter
      use m_machine_parameter
      use m_ctl_data_4_platforms
      use set_solver_MG_control
      use set_control_platform_data
!
      type(field_IO_params), intent(inout) :: mesh_file
!
!
      call set_control_mesh_def(mesh_file)
!
      np_smp = ione
      if (num_smp_ctl%iflag .eq. 1) np_smp = num_smp_ctl%intvalue
!
!
      call s_set_solver_MG_control
!
      end subroutine set_ctl_test_MG
!
!  ---------------------------------------------------------------------
!
      end module set_control_test_MG
