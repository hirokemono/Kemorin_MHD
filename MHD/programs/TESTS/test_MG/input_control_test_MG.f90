!
!      module input_control_test_MG
!     Written by H. Matsui on Apr., 2008
!
!     subroutine s_input_control_test_MG(mesh, group, surf, edge)
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
      subroutine s_input_control_test_MG(mesh, group, surf, edge)
!
        use calypso_mpi
        use m_machine_parameter
!
        use m_ctl_data_test_MG
        use m_geometry_param_MG
        use set_control_test_MG
        use load_mesh_data
        use set_MG_mesh_data
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) :: group
      type(surface_data), intent(inout) :: surf
      type(edge_data), intent(inout) :: edge
!
!
!  --  read control data
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_mesh_test'
      call read_control_4_MG_test
!
      if (iflag_debug.eq.1) write(*,*) 'set_ctl_test_MG'
      call set_ctl_test_MG
!
!  --  read geometry
!
      iflag_mesh_file_fmt = ifile_type
      if (iflag_debug.eq.1) write(*,*) 'input_mesh'
      call input_mesh(my_rank, mesh, group,                             &
     &    surf%nnod_4_surf, edge%nnod_4_edge)
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
