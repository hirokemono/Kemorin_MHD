!
!      module set_control_test_mesh
!
!     Written by H. Matsui on July, 2006
!
!      subroutine set_ctl_params_4_test_mesh(mesh_file)
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
      subroutine set_ctl_params_4_test_mesh(mesh_file)
!
      use calypso_mpi
      use t_file_IO_parameter
      use m_machine_parameter
      use m_file_format_switch
      use m_ctl_data_4_platforms
      use set_control_platform_data
      use set_ctl_parallel_platform
!
      type(field_IO_params), intent(inout) :: mesh_file
!
!
      call turn_off_debug_flag_by_ctl(my_rank, plt1)
      call set_control_mesh_def(plt1, mesh_file)
      if(iflag_debug.gt.0) write(*,*)                                   &
     &       'mesh_file_head:  ', trim(mesh_file%file_prefix)
!
      np_smp = 1
      if (plt1%num_smp_ctl%iflag .gt. 0) then
        np_smp = plt1%num_smp_ctl%intvalue
      end if
      if (iflag_debug.gt.0) write(*,*) 'np_smp', np_smp
!
      end subroutine set_ctl_params_4_test_mesh
!
!  ---------------------------------------------------------------------
!
      end module set_control_test_mesh
