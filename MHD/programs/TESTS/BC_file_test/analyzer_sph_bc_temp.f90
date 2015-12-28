!analyzer_sph_bc_temp.f90
!
!      module analyzer_sph_bc_temp
!
!      modified by H. Matsui on Aug., 2006 
!
!      subroutine initilize_bc_temp
!      subroutine analyze_bc_temp
!
!..................................................
!
      module analyzer_sph_bc_temp
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initilize_bc_temp
!
      use m_nod_comm_table
      use m_geometry_data
      use m_group_data
      use m_ctl_data_test_bc_temp
      use m_ctl_params_test_bc_temp
      use load_mesh_data
      use const_mesh_information
!
!
!     ----- read control data
!
      if (iflag_debug.gt.0) write(*,*) 'read_control_4_bc_temp'
      call read_control_4_bc_temp
!
      if (iflag_debug.gt.0) write(*,*) 'set_ctl_params_sph_bc_temp'
      call set_ctl_params_sph_bc_temp
!
!  --  read geometry
!
      if (iflag_debug.gt.0) write(*,*) 'input_mesh'
      call input_mesh                                                   &
     &   (my_rank, nod_comm, node1, ele1, nod_grp1, ele_grp1, sf_grp1,  &
     &    surf1%nnod_4_surf, edge1%nnod_4_edge)
!
!     ---------------------
!
      if (iflag_debug.eq.1) write(*,*) 'const_mesh_infos'
      call const_mesh_infos(my_rank,                                    &
     &    node1, ele1, surf1, edge1, nod_grp1, ele_grp1, sf_grp1,       &
     &    ele_grp_tbl1, sf_grp_tbl1, sf_grp_nod1)
!
       end subroutine initilize_bc_temp
!
! ----------------------------------------------------------------------
!
      subroutine analyze_bc_temp
!
      use m_group_data
      use const_sph_boundary_temp
!
      if (iflag_debug.gt.0) write(*,*) 'const_sph_temp_bc'
      call const_sph_temp_bc(nod_grp1)
!
      end subroutine analyze_bc_temp
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_bc_temp
