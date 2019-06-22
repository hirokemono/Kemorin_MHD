!
!      module t_ctl_data_comm_test
!
!      Written by H. Matsui on Sep., 2007
!
!      subroutine read_control_4_comm_test(comm_tctl)
!
!!   --------------------------------------------------------------------
!!    Example of control block
!!
!!  begin mesh_test
!!    begin data_files_def
!!      num_smp_ctl            1
!!      mesh_file_prefix         'mesh/in'
!!    end data_files_def
!!    begin FEM_mesh_ctl
!!      FEM_surface_output_switch      'NO'
!!      FEM_viewer_mesh_output_switch  'NO'
!!    end FEM_mesh_ctl
!!  end  mesh_test
!!
!!    -------------------------------------------------------------------
!
      module t_ctl_data_comm_test
!
      use m_precision
!
      use t_ctl_data_4_platforms
      use t_ctl_data_4_FEM_mesh
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
      type comm_test_control
!>        Structure for file settings
        type(platform_data_control) :: plt
!>        Structure of mesh IO controls and sleeve informations
        type(FEM_mesh_control) :: Fmesh_ctl
      end type comm_test_control
!
!     Label for the entry
!
      character(len=kchara), parameter, private                         &
     &         :: hd_mesh_test_ctl = 'mesh_test'
      integer (kind=kint), private :: i_mesh_test_ctl = 0
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_platform = 'data_files_def'
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_FEM_mesh = 'FEM_mesh_ctl'
!
      private :: read_test_comm_ctl_data
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_control_4_comm_test(comm_tctl)
!
      use calypso_mpi
      use m_read_control_elements
      use skip_comment_f
      use bcast_4_platform_ctl
      use m_read_control_elements
!
      type(comm_test_control), intent(inout) :: comm_tctl
!
!
      ctl_file_code = test_mest_ctl_file_code
!
      open(ctl_file_code, file = fname_test_mesh_ctl,  status='old')
!
      call load_ctl_label_and_line
      call read_test_comm_ctl_data(comm_tctl)
!
      close(ctl_file_code)
!
      call calypso_mpi_barrier
      call bcast_ctl_data_4_platform(comm_tctl%plt)
      call bcast_FEM_mesh_control(comm_tctl%Fmesh_ctl)
!
      end subroutine read_control_4_comm_test
!
!   --------------------------------------------------------------------
!
      subroutine read_test_comm_ctl_data(comm_tctl)
!
      use m_read_control_elements
      use skip_comment_f
!
      type(comm_test_control), intent(inout) :: comm_tctl
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
     &     (ctl_file_code, hd_platform, comm_tctl%plt, c_buf1)
        call read_FEM_mesh_control                                      &
     &     (ctl_file_code, hd_FEM_mesh, comm_tctl%Fmesh_ctl, c_buf1)
      end do
!
      end subroutine read_test_comm_ctl_data
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_comm_test
