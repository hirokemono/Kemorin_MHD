!
!      module t_ctl_data_comm_test
!
!      Written by H. Matsui on Sep., 2007
!
!!      subroutine read_control_4_comm_test(comm_tctl, c_buf)
!!      subroutine reset_test_comm_ctl_data(comm_tctl)
!!        type(comm_test_control), intent(inout) :: comm_tctl
!!
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
      use t_read_control_elements
      use skip_comment_f
!
      implicit  none
!
!
      integer(kind = kint), parameter :: test_mest_ctl_file_code = 11
!
!>      Structure for file settings
      type comm_test_control
!>        Structure for file settings
        type(platform_data_control) :: plt
!>        Structure of mesh IO controls and sleeve informations
        type(FEM_mesh_control) :: Fmesh_ctl
!
        integer (kind=kint) :: i_mesh_test_ctl = 0
      end type comm_test_control
!
!     Label for the entry
!
      character(len=kchara), parameter, private                         &
     &         :: hd_mesh_test_ctl = 'mesh_test'
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
      subroutine read_control_4_comm_test(file_name, comm_tctl, c_buf)
!
      use skip_comment_f
!
      character(len=kchara), intent(in) :: file_name
      type(comm_test_control), intent(inout) :: comm_tctl
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      c_buf%level = c_buf%level + 1
      open(test_mest_ctl_file_code, file = file_name, status='old')
      do
        call load_one_line_from_control                                 &
     &     (test_mest_ctl_file_code, hd_mesh_test_ctl, c_buf)
        if(c_buf%iend .gt. 0) exit
!
        call read_test_comm_ctl_data(test_mest_ctl_file_code,         &
     &      hd_mesh_test_ctl, comm_tctl, c_buf)
        if(comm_tctl%i_mesh_test_ctl .gt. 0) exit
      end do
      close(test_mest_ctl_file_code)
!
      c_buf%level = c_buf%level - 1
      if(c_buf%iend .gt. 0) return
!
      end subroutine read_control_4_comm_test
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_test_comm_ctl_data                                &
     &         (id_control, hd_block, comm_tctl, c_buf)
!
      use ctl_data_platforms_IO
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(comm_test_control), intent(inout) :: comm_tctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(comm_tctl%i_mesh_test_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, comm_tctl%plt, c_buf)
        call read_FEM_mesh_control                                      &
     &     (id_control, hd_FEM_mesh, comm_tctl%Fmesh_ctl, c_buf)
      end do
      comm_tctl%i_mesh_test_ctl = 1
!
      end subroutine read_test_comm_ctl_data
!
!   --------------------------------------------------------------------
!
      subroutine reset_test_comm_ctl_data(comm_tctl)
!
      type(comm_test_control), intent(inout) :: comm_tctl
!
!
      call reset_control_platforms(comm_tctl%plt)
      call reset_FEM_mesh_control(comm_tctl%Fmesh_ctl)
      comm_tctl%i_mesh_test_ctl = 0
!
      end subroutine reset_test_comm_ctl_data
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_comm_test
