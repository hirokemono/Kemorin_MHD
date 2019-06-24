!
!      module t_ctl_data_test_MG
!
!      Written by H. Matsui on Apr., 2008
!
!!      subroutine read_control_4_MG_test(test_MG_ctl)
!!      subroutine reset_MG_test_data_ctl(test_MG_ctl)
!!        type(ctl_data_test_MG), intent(inout) :: test_MG_ctl
!!
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
      module t_ctl_data_test_MG
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use t_read_control_elements
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
      type ctl_data_test_MG
        type(platform_data_control), save :: MGtest_plt
!
        integer (kind=kint) :: i_MG_test_ctl = 0
      end type ctl_data_test_MG
!
!     Top level
!
      character(len=kchara), parameter                                  &
     &         :: hd_MG_test_ctl = 'MG_test'
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
      private :: hd_MG_test_ctl
      private :: hd_platform, hd_mesh_head_ctl
!
      private :: read_MG_test_data_ctl, bcast_MG_test_data_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_control_4_MG_test(test_MG_ctl)
!
      use skip_comment_f
!
      type(ctl_data_test_MG), intent(inout) :: test_MG_ctl
!
!
      if(my_rank .eq. 0) then
        open(id_ctl_4_MG_test, file = fname_MG_test_ctl, status='old')
        do
          call load_one_line_from_control(id_ctl_4_MG_test, c_buf1)
          call read_MG_test_data_ctl   &
     &       (id_ctl_4_MG_test, hd_MG_test_ctl, test_MG_ctl, c_buf1)
          if(test_MG_ctl%i_MG_test_ctl.gt.0) exit
        end do
        close(id_ctl_4_MG_test)
      end if
!
      call bcast_MG_test_data_ctl(test_MG_ctl)
!
      end subroutine read_control_4_MG_test
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_MG_test_data_ctl                                  &
     &         (id_control, hd_block, test_MG_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_test_MG), intent(inout) :: test_MG_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(test_MG_ctl%i_MG_test_ctl.gt.0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, MGtest_plt%MGtest_plt, c_buf)
      end do
      test_MG_ctl%i_MG_test_ctl = 1
!
      end subroutine read_MG_test_data_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_MG_test_data_ctl(test_MG_ctl)
!
      use bcast_4_platform_ctl
!
      type(ctl_data_test_MG), intent(inout) :: test_MG_ctl
!
!
      call bcast_ctl_data_4_platform(test_MG_ctl%MGtest_plt)
!
      call MPI_BCAST(test_MG_ctl%i_MG_test_ctl, 1,                      &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_MG_test_data_ctl
!
!   --------------------------------------------------------------------
!
      subroutine reset_MG_test_data_ctl(test_MG_ctl)
!
      type(ctl_data_test_MG), intent(inout) :: test_MG_ctl
!
!
      call reset_control_platforms(test_MG_ctl%MGtest_plt)
      test_MG_ctl%i_MG_test_ctl = 0
!
      end subroutine reset_MG_test_data_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_test_MG
