!m_ctl_data_filter_comm_test.f90
!      module m_ctl_data_filter_comm_test
!
!      Written by H. Matsui on May, 2008
!
!      subroutine read_control_4_comm_test
!
!!   --------------------------------------------------------------------
!!    Example of control block
!!
!!  begin filter_comm_test
!!  
!!    begin data_files_def
!!      num_smp_ctl            1
!!      mesh_file_prefix         'mesh/in'
!!    end data_files_def
!!
!!    begin filter_filtes_ctl
!!      filter_file_header      'filter/filter_node'
!!    end filter_files_def
!!  end filter_comm_test
!!
!!    -------------------------------------------------------------------
!
      module m_ctl_data_filter_comm_test
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
     &               :: fname_test_mesh_ctl = "ctl_filter_comm_test"
!
!     Top level
!
      character(len=kchara), parameter                                  &
     &         :: hd_filter_test_ctl = 'filter_comm_test'
      integer (kind=kint) :: i_filter_test_ctl = 0
!
      private :: test_mest_ctl_file_code, fname_test_mesh_ctl
      private :: hd_filter_test_ctl, i_filter_test_ctl
      private :: read_filter_comm_test_data
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_control_filter_comm_test
!
!
      ctl_file_code = test_mest_ctl_file_code
!
      open(ctl_file_code, file = fname_test_mesh_ctl, status='old')
!
      call load_ctl_label_and_line
      call read_filter_comm_test_data
!
      close(ctl_file_code)
!
      end subroutine read_control_filter_comm_test
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_filter_comm_test_data
!
      use m_machine_parameter
      use m_ctl_data_4_platforms
      use m_ctl_data_filter_files
!
!
      if(right_begin_flag(hd_filter_test_ctl) .eq. 0) return
      if (i_filter_test_ctl.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_filter_test_ctl,                  &
     &      i_filter_test_ctl)
        if(i_filter_test_ctl .gt. 0) exit
!
        call read_ctl_data_4_platform(plt1)
        call read_filter_fnames_ctl
      end do
!
      end subroutine read_filter_comm_test_data
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_filter_comm_test
