!
!      module m_ctl_data_comm_test
!
!      Written by H. Matsui on Sep., 2007
!
!      subroutine read_control_4_comm_test
!
!!   --------------------------------------------------------------------
!!    Example of control block
!!
!!  begin mesh_test
!!    begin data_files_def
!!      num_smp_ctl            1
!!      mesh_file_prefix         'mesh/in'
!!    end data_files_def
!!  end  mesh_test
!!
!!    -------------------------------------------------------------------
!
      module m_ctl_data_comm_test
!
      use m_precision
!
      use m_read_control_elements
      use skip_comment_f
!
      implicit  none
!
!
      integer(kind = kint), parameter :: test_mest_ctl_file_code = 11
      character(len = kchara), parameter                                &
     &                        :: fname_test_mesh_ctl = "ctl_mesh"
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_control_4_comm_test
!
      use m_machine_parameter
      use m_ctl_data_4_platforms
!
!
      ctl_file_code = test_mest_ctl_file_code
!
      open(ctl_file_code, file = fname_test_mesh_ctl, status='old')
!
      call load_ctl_label_and_line
      call read_ctl_data_4_platform(plt1)
!
      close(ctl_file_code)
!
      end subroutine read_control_4_comm_test
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_comm_test
