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
      use t_ctl_data_4_platforms
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
!>      Structure for file settings
      type(platform_data_control), save :: comm_test_plt
!
!     Label for the entry
!
      character(len=kchara), parameter                                  &
     &         :: hd_mesh_test_ctl = 'mesh_test'
      integer (kind=kint) :: i_mesh_test_ctl = 0
!
      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
      integer (kind=kint) :: i_platform =   0
!
      private :: hd_mesh_test_ctl, i_mesh_test_ctl
      private :: hd_platform, i_platform
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_control_4_comm_test
!
      use calypso_mpi
      use m_machine_parameter
      use bcast_4_platform_ctl
!
!
      if(my_rank .eq. 0) then
        ctl_file_code = test_mest_ctl_file_code
        open(ctl_file_code, file = fname_test_mesh_ctl, status='old')
!
        call load_ctl_label_and_line
        call read_test_comm_ctl_data
!
        close(ctl_file_code)
      end if
!
      call calypso_mpi_barrier
      call bcast_ctl_data_4_platform(comm_test_plt)
!
      end subroutine read_control_4_comm_test
!
!   --------------------------------------------------------------------
!
      subroutine read_test_comm_ctl_data
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_mesh_test_ctl) .eq. 0) return
      if (i_mesh_test_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_mesh_test_ctl = find_control_end_flag(hd_mesh_test_ctl)
        if(i_mesh_test_ctl .gt. 0) exit
!
        call read_control_platforms                                     &
     &     (hd_platform, i_platform, comm_test_plt)
      end do
!
      end subroutine read_test_comm_ctl_data
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_comm_test
