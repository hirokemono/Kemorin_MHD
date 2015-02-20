!
!      module set_control_test_MG
!
      module set_control_test_MG
!
!     Written by H. Matsui on July, 2006
!
      use m_precision
!
      implicit none
!
!      subroutine set_ctl_test_MG
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_ctl_test_MG
!
      use m_machine_parameter
      use m_ctl_data_4_platforms
      use m_read_mesh_data
      use set_solver_MG_control
!
!
      if (mesh_file_prefix%iflag .gt. 0) then
        mesh_file_head = mesh_file_prefix%charavalue
      else
        mesh_file_head = def_mesh_file_head
      end if
!
      np_smp = 1
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
