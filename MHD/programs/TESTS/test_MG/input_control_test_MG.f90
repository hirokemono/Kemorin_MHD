!
!      module input_control_test_MG
!
      module input_control_test_MG
!
!     Written by H. Matsui on Apr., 2008
!
      use m_precision
!
      use m_machine_parameter
!
      implicit none
!
!
!     subroutine s_input_control_test_MG
!
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_input_control_test_MG
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
      call input_mesh                                                   &
     &   (my_rank, nod_comm, node1, ele1, nod_grp1, ele_grp1, sf_grp1,  &
     &    surf1%nnod_4_surf, edge1%nnod_4_edge)
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
