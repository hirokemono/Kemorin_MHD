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
      use set_control_platform_data
      use set_ctl_parallel_platform
!
!
      call turn_off_debug_flag_by_ctl(my_rank)
      call check_control_num_domains
      if (mesh_file_prefix%iflag .gt. 0) then
        mesh1_file%file_prefix = mesh_file_prefix%charavalue
      else
        mesh1_file%file_prefix = def_mesh_file_head
      end if
      if(iflag_debug.gt.0) write(*,*)                                   &
     &       'mesh_file_head:  ', trim(mesh1_file%file_prefix)
!
      np_smp = 1
      if (num_smp_ctl%iflag .gt. 0) np_smp = num_smp_ctl%intvalue
      if (iflag_debug.gt.0) write(*,*) 'np_smp', np_smp
!
      call choose_file_format                                           &
     &   (mesh_file_fmt_ctl, mesh1_file%iflag_format)
!
      end subroutine set_ctl_params_4_test_mesh
!
!  ---------------------------------------------------------------------
!
      end module set_control_test_mesh
