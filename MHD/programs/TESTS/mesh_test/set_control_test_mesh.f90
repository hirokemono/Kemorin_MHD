!
!      module set_control_test_mesh
!
!     Written by H. Matsui on July, 2006
!
!      subroutine set_ctl_params_4_test_mesh
!
      module set_control_test_mesh
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
      subroutine set_ctl_params_4_test_mesh
!
      use calypso_mpi
      use m_machine_parameter
      use m_file_format_switch
      use m_read_mesh_data
      use m_ctl_data_4_platforms
!
!
      if (i_mesh_header .ne. 0) then
        mesh_file_head = mesh_file_prefix
      else
        mesh_file_head = def_mesh_file_head
      end if
      if(iflag_debug.gt.0) write(*,*) 'mesh_file_head ', mesh_file_head
!
      np_smp = num_smp_ctl
      if (iflag_debug.gt.0) write(*,*) 'np_smp', np_smp
!
      call choose_file_format(mesh_file_fmt_ctl, i_mesh_file_fmt,       &
     &          iflag_mesh_file_fmt)
!
      end subroutine set_ctl_params_4_test_mesh
!
!  ---------------------------------------------------------------------
!
      end module set_control_test_mesh
