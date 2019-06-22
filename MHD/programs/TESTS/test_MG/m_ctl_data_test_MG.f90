!
!      module m_ctl_data_test_MG
!
!      Written by H. Matsui on Apr., 2008
!
!      subroutine read_control_4_MG_test
!
!!   --------------------------------------------------------------------
!!    Example of control block
!!
!!  begin solver_test_ctl
!!    begin data_files_def
!!      ...
!!    end data_files_def
!!
!!    begin MG_grids_ctl
!!      ...
!!    end MG_grids_ctl
!!  end solver_test_ctl
!!
!!    -------------------------------------------------------------------
!
      module m_ctl_data_test_MG
!
      use m_precision
!
      use m_machine_parameter
      use t_ctl_data_4_platforms
!
      implicit  none
!
!
      integer(kind = kint), parameter :: id_ctl_4_MG_test = 11
      character(len = kchara), parameter                                &
     &                        :: fname_MG_test_ctl = "ctl_MG_test"
!
!
      type(platform_data_control), save :: MGtest_plt
!
!     Top level
!
      character(len=kchara), parameter                                  &
     &         :: hd_MG_test_ctl = 'MG_test'
      integer (kind=kint) :: i_MG_test_ctl = 0
!
!     2nd level for const_filter
!
      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter                                  &
     &         :: hd_MG_params =    'MG_grids_ctl'
      integer (kind=kint) :: i_MG_params =  0
!
      private :: id_ctl_4_MG_test, fname_MG_test_ctl
      private :: hd_MG_test_ctl, i_MG_test_ctl
      private :: hd_platform, hd_mesh_head_ctl
!
      private :: read_MG_test_data_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_control_4_MG_test
!
      use m_read_control_elements
      use skip_comment_f
!
      use m_read_control_elements
!
!
      ctl_file_code = id_ctl_4_MG_test
      open(ctl_file_code, file = fname_MG_test_ctl, status='old')
!
!
      call load_ctl_label_and_line
      call read_MG_test_data_ctl
!
      close(ctl_file_code)
!
      end subroutine read_control_4_MG_test
!
!   --------------------------------------------------------------------
!
      subroutine read_MG_test_data_ctl
!
      use m_ctl_data_4_MG
!
!
      if(right_begin_flag(hd_MG_test_ctl) .eq. 0) return
      if (i_MG_test_ctl.gt.0) return
      do
        call load_ctl_label_and_line
!
        i_MG_test_ctl = find_control_end_flag(hd_MG_test_ctl)
        if(i_MG_test_ctl .gt. 0) exit
!
!
        call read_control_platforms                                     &
     &     (ctl_file_code, hd_platform, MGtest_plt, c_buf1)
        call read_MG_param_ctl
      end do
!
      end subroutine read_MG_test_data_ctl
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_test_MG
