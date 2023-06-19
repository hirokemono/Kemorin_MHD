!
!      module t_control_param_comm_test
!
!     Written by H. Matsui on July, 2006
!
!!     subroutine s_input_control_comm_test(comm_tctl, T_files)
!!       type(comm_test_control), intent(inout) :: comm_tctl
!!       type(comm_test_files_param), intent(inout) ::  T_files
!
      module t_control_param_comm_test
!
      use m_precision
      use t_file_IO_parameter
!
      implicit none
!
      character(len = kchara), parameter, private                       &
     &                        :: fname_test_mesh_ctl = "ctl_mesh"
!
      type comm_test_files_param
!>        Logical flag to output surface data
        logical :: flag_output_SURF = .FALSE.
!>        Structure of mesh file IO paramters
        type(field_IO_params) :: mesh_file_IO
      end type comm_test_files_param
!
      private :: set_ctl_params_4_comm_test
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_input_control_comm_test(comm_tctl, T_files)
!
      use calypso_mpi
      use m_machine_parameter
      use t_ctl_data_comm_test
!
      use bcast_ctl_data_comm_test
      use set_surface_data_4_IO
      use set_edge_data_4_IO
!
      type(comm_test_control), intent(inout) :: comm_tctl
      type(comm_test_files_param), intent(inout) ::  T_files
!
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_comm_test'
      if(my_rank .eq. 0) then
        call read_control_4_comm_test(fname_test_mesh_ctl, comm_tctl)
      end if
      call bcast_test_comm_ctl_data(comm_tctl)
!
      if(comm_tctl%i_mesh_test_ctl .ne. 1) then
        call calypso_MPI_abort(comm_tctl%i_mesh_test_ctl,               &
     &                             'control file is broken')
      end if
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_params_4_comm_test'
      call set_ctl_params_4_comm_test                                   &
     &  (comm_tctl%plt, comm_tctl%Fmesh_ctl, T_files)
!
      end subroutine s_input_control_comm_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_ctl_params_4_comm_test(plt, Fmesh_ctl, T_files)
!
      use calypso_mpi
      use t_ctl_data_4_platforms
      use t_ctl_data_4_FEM_mesh
      use set_control_platform_item
      use set_control_platform_data
!
      type(platform_data_control), intent(in) :: plt
      type(FEM_mesh_control), intent(in) :: Fmesh_ctl
      type(comm_test_files_param), intent(inout) :: T_files
!
      call turn_off_debug_flag_by_ctl(my_rank, plt)
      call set_control_smp_def(my_rank, plt)
      call set_control_parallel_mesh(plt, T_files%mesh_file_IO)
      T_files%flag_output_SURF = FEM_surface_output_switch(Fmesh_ctl)
!
      end subroutine set_ctl_params_4_comm_test
!
!   --------------------------------------------------------------------
!
      end module t_control_param_comm_test
