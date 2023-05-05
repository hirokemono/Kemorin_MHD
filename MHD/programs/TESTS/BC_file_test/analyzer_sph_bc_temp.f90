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
      use m_machine_parameter
!
      use calypso_mpi
      use t_mesh_data
      use t_boundary_field_IO
      use t_file_IO_parameter
      use t_ctl_data_test_bc_temp
      use t_solver_SR
      use t_solver_SR_int
!
      implicit none
!
      character(len = kchara), parameter, private                       &
     &                        :: fname_test_mesh_ctl = "ctl_bc_temp"
!
      type(field_IO_params), save ::  mesh_file_TEC
      type(mesh_data), save :: femmesh
      type(IO_boundary), save :: IO_bc_t
!
!>        Structure of communication flags
        type(send_recv_status) :: SR_sig1
!>        Structure of communication buffer for 4-byte integer
        type(send_recv_int_buffer) :: SR_i1
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initilize_bc_temp
!
      use m_ctl_params_test_bc_temp
      use mpi_load_mesh_data
      use const_mesh_information
      use parallel_edge_information
!
!
!     ----- read control data
!
      if (iflag_debug.gt.0) write(*,*) 'input_control_bc_temp'
      call input_control_bc_temp(fname_test_mesh_ctl, mesh_file_TEC)
!
!  --  read geometry
!
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(mesh_file_TEC, nprocs, femmesh)
!
      if (iflag_debug.gt.0) write(*,*) 'const_nod_ele_infos'
      call const_nod_ele_infos(my_rank, femmesh%mesh, femmesh%group)
      if (iflag_debug.eq.1) write(*,*) 'const_surface_infos'
      call const_surface_infos                                          &
     &   (my_rank, femmesh%mesh%node, femmesh%mesh%ele,                 &
     &    femmesh%group%surf_grp, femmesh%mesh%surf,                    &
     &    femmesh%group%surf_nod_grp)
      if (iflag_debug.gt.0) write(*,*) 'const_para_edge_infos'
      call const_para_edge_infos                                        &
     &   (femmesh%mesh%nod_comm, femmesh%mesh%node, femmesh%mesh%ele,   &
     &    femmesh%mesh%surf, femmesh%mesh%edge, SR_sig1, SR_i1)
!
       end subroutine initilize_bc_temp
!
! ----------------------------------------------------------------------
!
      subroutine analyze_bc_temp
!
      use const_sph_boundary_temp
!
      if (iflag_debug.gt.0) write(*,*) 'const_sph_temp_bc'
      call const_sph_temp_bc                                            &
     &   (femmesh%mesh%node, femmesh%group%nod_grp, IO_bc_t)
!
      end subroutine analyze_bc_temp
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_bc_temp
