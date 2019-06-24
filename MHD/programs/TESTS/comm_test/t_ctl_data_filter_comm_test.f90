!t_ctl_data_filter_comm_test.f90
!      module t_ctl_data_filter_comm_test
!
!      Written by H. Matsui on May, 2008
!
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
      module t_ctl_data_filter_comm_test
!
      use m_precision
!
      use calypso_mpi
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_filter_files
      use skip_comment_f
!
      implicit  none
!
!
      integer(kind = kint), parameter :: test_mest_ctl_file_code = 11
      character(len = kchara), parameter                                &
     &               :: fname_test_mesh_ctl = "ctl_filter_comm_test"
!
      type ctl_data_filter_comm_test
        type(platform_data_control) :: f_comm_plt
!>        Structure for filtering files
        type(filter_file_control) :: ffile_ctest_ctl
!
        integer (kind=kint) :: i_filter_test_ctl = 0
      end type ctl_data_filter_comm_test
!
!     Top level
!
      character(len=kchara), parameter                                  &
     &         :: hd_filter_test_ctl = 'filter_comm_test'
!
!
      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter :: hd_filter_fnames              &
     &                        = 'filter_files_def'
!
      private :: hd_filter_test_ctl, hd_platform, hd_filter_fnames
!
      private :: test_mest_ctl_file_code, fname_test_mesh_ctl
      private :: read_filter_comm_test_data
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_control_filter_comm_test(fc_test_ctl)
!
      type(ctl_data_filter_comm_test), intent(inout) :: fc_test_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
       open(test_mest_ctl_file_code, file = fname_test_mesh_ctl,        &
     &      status='old')
!
       do
          call load_one_line_from_control                               &
     &       (test_mest_ctl_file_code, c_buf1)
          call read_filter_comm_test_data(test_mest_ctl_file_code,      &
     &        hd_filter_test_ctl, fc_test_ctl, c_buf1)
          if(fc_test_ctl%i_filter_test_ctl.gt.0) exit
        end do
        close(test_mest_ctl_file_code)
      end if
!
      call bcast_filter_comm_test_data(fc_test_ctl)
!
      end subroutine read_control_filter_comm_test
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_filter_comm_test_data                             &
     &         (id_control, hd_block, fc_test_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_filter_comm_test), intent(inout) :: fc_test_ctl
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(fc_test_ctl%i_filter_test_ctl.gt.0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, fc_test_ctl%f_comm_plt, c_buf)
        call read_filter_fnames_control(id_control, hd_filter_fnames,   &
     &      fc_test_ctl%ffile_ctest_ctl, c_buf)
      end do
      fc_test_ctl%i_filter_test_ctl = 1
!
      end subroutine read_filter_comm_test_data
!
!   --------------------------------------------------------------------
!
      subroutine bcast_filter_comm_test_data(fc_test_ctl)
!
      use bcast_4_platform_ctl
      use bcast_4_filter_files_ctl
!
      type(ctl_data_filter_comm_test), intent(inout) :: fc_test_ctl
!
!
      call bcast_ctl_data_4_platform(fc_test_ctl%f_comm_plt)
      call bcast_filter_fnames_control(fc_test_ctl%ffile_ctest_ctl)
!
      call MPI_BCAST(fc_test_ctl%i_filter_test_ctl, 1,                  &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_filter_comm_test_data
!
!   --------------------------------------------------------------------
!
      subroutine reset_filter_comm_test_data(fc_test_ctl)
!
      type(ctl_data_filter_comm_test), intent(inout) :: fc_test_ctl
!
!
      call reset_control_platforms(fc_test_ctl%f_comm_plt)
      call reset_filter_fnames_control(fc_test_ctl%ffile_ctest_ctl)
!
      fc_test_ctl%i_filter_test_ctl = 0
!
      end subroutine reset_filter_comm_test_data
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_filter_comm_test
