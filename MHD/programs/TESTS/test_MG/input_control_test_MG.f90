!
!      module input_control_test_MG
!     Written by H. Matsui on Apr., 2008
!
!!     subroutine s_input_control_test_MG(fem)
!!        type(mesh_data), intent(inout) :: fem
!
!
!
      module input_control_test_MG
!
      use m_precision
!
      use m_machine_parameter
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_edge_data
!
      implicit none
!
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_input_control_test_MG(fem)
!
      use calypso_mpi
      use m_machine_parameter
!
      use m_geometry_param_MG
      use t_ctl_data_test_MG
      use set_control_test_MG
      use mpi_load_mesh_data
      use set_MG_mesh_data
!
      type(mesh_data), intent(inout) :: fem
!
      type(ctl_data_test_MG), save :: test_MG_ctl1
      type(field_IO_params), save ::  mesh_file_test
!
!  --  read control data
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_mesh_test'
      call read_control_4_MG_test(test_MG_ctl1)
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_test_MG'
      call set_ctl_test_MG(test_MG_ctl1%MGtest_plt, mesh_file_test)
!
!  --  read geometry
!
      if (iflag_debug.eq.1) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(mesh_file_test, nprocs, fem)
!
!  --  read geometry data for MG
!
      call set_MG_mesh(my_rank, ifile_type,  MG_mesh_head)
!
      end subroutine s_input_control_test_MG
!
! ----------------------------------------------------------------------
!
      end module input_control_test_MG
