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
      if (i_mesh_header .eq. 1) then
        mesh_file_head = mesh_file_prefix
      else
        mesh_file_head = def_mesh_file_head
      end if
!
      if (i_num_smp .eq. 1) then
        np_smp = num_smp_ctl
      else
        np_smp = 1
      end if
!
!
      call s_set_solver_MG_control
!
      end subroutine set_ctl_test_MG
!
!  ---------------------------------------------------------------------
!
      end module set_control_test_MG
