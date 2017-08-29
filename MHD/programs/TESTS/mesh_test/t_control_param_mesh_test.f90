!t_control_param_mesh_test.f90
!      module t_control_param_mesh_test
!
!     Written by H. Matsui on July, 2006
!
!!      subroutine set_ctl_params_4_test_mesh(plt, T_files)
!!        type(platform_data_control), intent(in) :: plt
!!        type(mesh_test_files_param), intent(inout) :: T_files
!
      module t_control_param_mesh_test
!
      use m_precision
      use t_file_IO_parameter
!
      implicit none
!
      type mesh_test_files_param
!>        Integer flag to output surface data
        integer(kind = kint) :: iflag_output_SURF = 0
!>        Structure of mesh file IO paramters
        type(field_IO_params) :: mesh_file_IO
      end type mesh_test_files_param
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_ctl_params_4_test_mesh(plt, T_files)
!
      use calypso_mpi
      use t_ctl_data_4_platforms
      use m_machine_parameter
      use m_file_format_switch
      use set_control_platform_data
      use set_ctl_parallel_platform
!
      type(platform_data_control), intent(in) :: plt
      type(mesh_test_files_param), intent(inout) :: T_files
!
!
      call turn_off_debug_flag_by_ctl(my_rank, plt)
      call set_control_mesh_def(plt, T_files%mesh_file_IO)
      call set_FEM_surface_output_flag(plt, T_files%iflag_output_SURF)
      if(iflag_debug.gt.0) write(*,*)                                   &
     &   'mesh_file_head:  ', trim(T_files%mesh_file_IO%file_prefix)
!
      np_smp = 1
      if (plt%num_smp_ctl%iflag .gt. 0) then
        np_smp = plt%num_smp_ctl%intvalue
      end if
      if (iflag_debug.gt.0) write(*,*) 'np_smp', np_smp
!
      end subroutine set_ctl_params_4_test_mesh
!
!  ---------------------------------------------------------------------
!
      end module t_control_param_mesh_test
