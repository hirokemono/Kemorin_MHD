!
!      module m_ctl_data_test_mesh
!
!      Written by H. Matsui on July, 2006
!      Mmodified by H. Matsui on June, 2007
!
!      subroutine read_control_4_mesh_test
!
!!   --------------------------------------------------------------------
!!    Example of control block
!!
!!  begin mesh_test
!!    begin data_files_def
!!      num_smp_ctl            1
!!      mesh_file_prefix         'mesh/in'
!!      elem_file_prefix         'mesh/element'
!!      surf_file_prefix         'mesh/surface'
!!      edge_file_prefix         'mesh/edge'
!!    end data_files_def
!!  end  mesh_test
!!
!!    -------------------------------------------------------------------
!
      module m_ctl_data_test_mesh
!
      use m_precision
!
      implicit  none
!
      integer(kind = kint), parameter :: test_mest_ctl_file_code = 11
      character(len = kchara), parameter                                &
     &                        :: fname_test_mesh_ctl = "ctl_mesh"
!
!     Top level
!
      character(len=kchara), parameter                                  &
     &         :: hd_mesh_test_ctl = 'mesh_test'
      integer (kind=kint) :: i_mesh_test_ctl = 0
!
      private :: read_test_mesh_ctl_data
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_control_4_mesh_test
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
      use m_read_control_elements
!
!
      ctl_file_code = test_mest_ctl_file_code
!
      open(ctl_file_code, file = fname_test_mesh_ctl,  status='old')
!
      call load_ctl_label_and_line
      call read_test_mesh_ctl_data
!
      close(ctl_file_code)
!
      end subroutine read_control_4_mesh_test
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_test_mesh_ctl_data
!
      use m_machine_parameter
      use m_ctl_data_4_platforms
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_mesh_test_ctl) .eq. 0) return
      if (i_mesh_test_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_mesh_test_ctl, i_mesh_test_ctl)
        if(i_mesh_test_ctl .gt. 0) exit
!
        call read_ctl_data_4_platform
      end do
!
      end subroutine read_test_mesh_ctl_data
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_test_mesh
