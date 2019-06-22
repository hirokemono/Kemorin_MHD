!
!      module t_ctl_data_mesh_test
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
!!    end data_files_def
!!    begin FEM_mesh_ctl
!!      FEM_surface_output_switch      'NO'
!!      FEM_viewer_mesh_output_switch  'NO'
!!    end FEM_mesh_ctl
!!  end  mesh_test
!!
!!    -------------------------------------------------------------------
!
      module t_ctl_data_mesh_test
!
      use m_precision
      use m_machine_parameter
      use t_ctl_data_4_platforms
      use t_ctl_data_4_FEM_mesh
!
      implicit  none
!
      integer(kind = kint), parameter :: mest_ctl_file_code = 11
      character(len = kchara), parameter                                &
     &                        :: fname_test_mesh_ctl = "ctl_mesh"
!
      type mesh_test_control
!>        Structure for file settings
        type(platform_data_control) :: plt
!>        Structure of mesh IO controls and sleeve informations
        type(FEM_mesh_control) :: Fmesh_ctl
!
        integer(kind = kint), private :: i_mesh_test_ctl = 0
      end type mesh_test_control
!
!     Top level
!
      character(len=kchara), parameter, private                         &
     &         :: hd_mesh_test_ctl = 'mesh_test'
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter, private                         &
     &                    :: hd_FEM_mesh = 'FEM_mesh_ctl'
!
      private :: read_test_mesh_ctl_data
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_control_4_mesh_test(mesh_tctl)
!
      use calypso_mpi
      use skip_comment_f
      use bcast_4_platform_ctl
      use t_read_control_elements
!
      type(mesh_test_control), intent(inout) :: mesh_tctl
!
      type(buffer_for_control) :: c_buf1
!
!
      open(mest_ctl_file_code, file = fname_test_mesh_ctl,status='old')
!
      do
        call load_one_line_from_control(mest_ctl_file_code, c_buf1)
        call read_test_mesh_ctl_data                                    &
     &     (mest_ctl_file_code, hd_mesh_test_ctl, mesh_tctl, c_buf1)
        if(mesh_tctl%i_mesh_test_ctl .gt. 0) exit
      end do
      close(mest_ctl_file_code)
!
      call bcast_ctl_data_4_platform(mesh_tctl%plt)
      call bcast_FEM_mesh_control(mesh_tctl%Fmesh_ctl)
!
      end subroutine read_control_4_mesh_test
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_test_mesh_ctl_data                                &
     &         (id_control, hd_block, mesh_tctl, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(mesh_test_control), intent(inout) :: mesh_tctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(mesh_tctl%i_mesh_test_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, mesh_tctl%plt, c_buf)
        call read_FEM_mesh_control                                      &
     &     (id_control, hd_FEM_mesh, mesh_tctl%Fmesh_ctl, c_buf)
      end do
      mesh_tctl%i_mesh_test_ctl = 1
!
      end subroutine read_test_mesh_ctl_data
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_mesh_test
