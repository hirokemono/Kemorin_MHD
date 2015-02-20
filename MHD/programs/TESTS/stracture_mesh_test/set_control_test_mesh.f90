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
      use m_read_mesh_data
      use m_ctl_data_4_platforms
!
!
      if (mesh_file_prefix%iflag .gt. 0) then
        mesh_file_head = mesh_file_prefix%charavalue
      else
        mesh_file_head = def_mesh_file_head
      end if
      if(iflag_debug.gt.0) write(*,*) 'mesh_file_head ', mesh_file_head
!
      np_smp = 1
      if(num_smp_ctl%iflag .gt. 0) np_smp = num_smp_ctl%intvalue
      if (iflag_debug.gt.0) write(*,*) 'np_smp', np_smp
!
!
      end subroutine set_ctl_params_4_test_mesh
!
!  ---------------------------------------------------------------------
!
      end module set_control_test_mesh
