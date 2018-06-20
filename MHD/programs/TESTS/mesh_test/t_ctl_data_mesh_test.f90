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
      integer(kind = kint), parameter :: test_mest_ctl_file_code = 11
      character(len = kchara), parameter                                &
     &                        :: fname_test_mesh_ctl = "ctl_mesh"
!
      type mesh_test_control
!>        Structure for file settings
        type(platform_data_control) :: plt
!>        Structure of mesh IO controls and sleeve informations
        type(FEM_mesh_control) :: Fmesh_ctl
      end type mesh_test_control
!
!     Top level
!
      character(len=kchara), parameter, private                         &
     &         :: hd_mesh_test_ctl = 'mesh_test'
      integer (kind=kint), private :: i_mesh_test_ctl = 0
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_platform = 'data_files_def'
      integer (kind=kint), private :: i_platform =   0
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_FEM_mesh = 'FEM_mesh_ctl'
      integer(kind=kint), private :: i_FEM_mesh =   0
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
      use m_read_control_elements
      use skip_comment_f
      use bcast_4_platform_ctl
      use m_read_control_elements
!
      type(mesh_test_control), intent(inout) :: mesh_tctl
!
!
      ctl_file_code = test_mest_ctl_file_code
!
      open(ctl_file_code, file = fname_test_mesh_ctl,  status='old')
!
      call load_ctl_label_and_line
      call read_test_mesh_ctl_data(mesh_tctl)
!
      close(ctl_file_code)
!
      call calypso_mpi_barrier
      call bcast_ctl_data_4_platform(mesh_tctl%plt)
      call bcast_FEM_mesh_control(mesh_tctl%Fmesh_ctl)
!
      end subroutine read_control_4_mesh_test
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_test_mesh_ctl_data(mesh_tctl)
!
      use m_read_control_elements
      use skip_comment_f
!
      type(mesh_test_control), intent(inout) :: mesh_tctl
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
        call read_control_platforms                                     &
     &     (hd_platform, i_platform, mesh_tctl%plt)
        call read_FEM_mesh_control                                      &
     &     (hd_FEM_mesh, i_FEM_mesh, mesh_tctl%Fmesh_ctl)
      end do
!
      end subroutine read_test_mesh_ctl_data
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_mesh_test
