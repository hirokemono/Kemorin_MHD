!analyzer_mesh_test.f90
!
!      module analyzer_mesh_test
!
!      modified by H. Matsui on Aug., 2006 
!
!      subroutine initialize_mesh_test
!      subroutine analyze_mesh_test
!
!..................................................
!
      module analyzer_mesh_test
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_mesh_data
      use t_comm_table
      use t_geometry_data
      use t_group_data
      use t_surface_data
      use t_edge_data
!
      implicit none
!
      type(mesh_geometry), save :: mesh
      type(mesh_groups), save :: group
      type(element_geometry), save :: ele_mesh
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_mesh_test
!
      use m_array_for_send_recv
      use m_default_file_prefix
!
      use const_mesh_information
      use copy_mesh_structures
      use set_element_data_4_IO
      use set_surface_data_4_IO
      use set_edge_data_4_IO
      use element_file_IO
      use check_jacobians
      use int_volume_of_domain
      use set_surf_grp_vectors
      use check_surface_groups
      use set_normal_vectors
      use set_edge_vectors
      use mesh_file_IO
      use nod_phys_send_recv
      use sum_normal_4_surf_group
      use set_parallel_file_name
      use set_comm_table_4_IO
!
      use m_ctl_data_test_mesh
      use set_control_test_mesh
      use mpi_load_mesh_data
      use const_jacobians_3d
      use const_element_comm_tables
!
      use t_file_IO_parameter
      use t_mesh_data
      use t_read_mesh_data
      use t_jacobians
!
!>     Stracture for Jacobians
      type(jacobians_type), save :: jacobians1
!
      type(field_IO_params) ::  tested_mesh_file
      type(mesh_geometry) :: mesh_IO
      type(surf_edge_IO_file) :: ele_mesh_IO
      character(len=kchara) :: file_prefix
!
!     --------------------- 
!
      if (my_rank.eq.0) then
        write(*,*) 'Construct commutation filter'
        write(*,*) 'Input file: mesh data'
      end if
!
!     ----- read control data
!
      if (iflag_debug.gt.0) write(*,*) 'read_control_4_mesh_test'
      call read_control_4_mesh_test
!
      if (iflag_debug.gt.0) write(*,*) 'set_ctl_params_4_test_mesh'
      call set_ctl_params_4_test_mesh(mesh_test_plt, tested_mesh_file)
!
!  --  read geometry
!
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(tested_mesh_file, mesh, group,                &
     &    ele_mesh%surf%nnod_4_surf, ele_mesh%edge%nnod_4_edge)
!
      if (iflag_debug.eq.1) write(*,*) 'const_mesh_infos'
      call const_mesh_infos(my_rank, mesh, group, ele_mesh)
!
!  -------------------------------
!
      if (iflag_debug.gt.0 ) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver(isix, mesh%node%numnod)
!
      if(iflag_debug.gt.0) write(*,*)' init_send_recv'
      call init_send_recv(mesh%nod_comm)
!
!  -----    construct geometry informations
!
      if(iflag_debug.gt.0) write(*,*)' const_element_comm_tbls'
      call const_element_comm_tbls(mesh, ele_mesh)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'pick_surface_group_geometry'
      call pick_surface_group_geometry(ele_mesh%surf,                   &
     &   group%surf_grp, group%tbls_surf_grp, group%surf_grp_geom)
!
!  -------------------------------
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'const_jacobian_volume_normals'
      call max_int_point_by_etype(mesh%ele%nnod_4_ele)
      call const_jacobian_volume_normals(my_rank, nprocs,               &
     &    mesh, ele_mesh%surf, group, jacobians1)
!
!      call check_jacobians_trilinear                                   &
!     &   (my_rank, mesh%ele, jacobians1%jac_3d_l)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 's_cal_normal_vector_spherical'
      call s_cal_normal_vector_spherical(ele_mesh%surf)
      if (iflag_debug.gt.0) write(*,*) 's_cal_normal_vector_cylindrical'
      call s_cal_normal_vector_cylindrical(ele_mesh%surf)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'const_edge_vector'
      call const_edge_vector(my_rank, nprocs,                           &
     &    mesh%node, ele_mesh%edge, jacobians1)
!
      if (iflag_debug.gt.0) write(*,*) 's_cal_edge_vector_spherical'
      call s_cal_edge_vector_spherical(ele_mesh%edge)
      if (iflag_debug.gt.0) write(*,*) 's_cal_edge_vector_cylindrical'
      call s_cal_edge_vector_cylindrical(ele_mesh%edge)
!
!  ---------------------------------------------
!     output node data
!      spherical and cylindrical coordinate
!  ---------------------------------------------
!
      mesh_IO%nod_comm%num_neib = 0
      call allocate_type_neib_id(mesh_IO%nod_comm)
      call copy_node_sph_to_xx(mesh%node, mesh_IO%node)
!
      call write_node_position_sph(my_rank, def_sph_mesh_head, mesh_IO)
!
      mesh_IO%nod_comm%num_neib = 0
      call allocate_type_neib_id(mesh_IO%nod_comm)
      call copy_node_cyl_to_xx(mesh%node, mesh_IO%node)
!
      call write_node_position_cyl(my_rank, def_cyl_mesh_head, mesh_IO)
!
!  -------------------------------
!     output element data
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'copy_ele_geometry_to_IO'
      call copy_comm_tbl_type(ele_mesh%ele_comm, ele_mesh_IO%comm)
      call calypso_mpi_barrier
      file_prefix = def_ele_mesh_head
!
      call copy_ele_geometry_to_IO                                      &
     &   (mesh%ele, ele_mesh_IO%node, ele_mesh_IO%sfed)
      call calypso_mpi_barrier
!
      call output_element_file(my_rank, file_prefix, ele_mesh_IO)
      call calypso_mpi_barrier
!
!  -------------------------------
!     output surface data
!  -------------------------------
!
      file_prefix = def_surf_mesh_head
      call copy_comm_tbl_type(ele_mesh%surf_comm, ele_mesh_IO%comm)
      call calypso_mpi_barrier
      if (iflag_debug.gt.0) write(*,*) 'copy_surf_geometry_to_IO'
      call copy_surf_connect_to_IO(ele_mesh%surf, mesh%ele%numele,      &
     &    ele_mesh_IO%ele, ele_mesh_IO%sfed)
      call calypso_mpi_barrier
      call copy_surf_geometry_to_IO(ele_mesh%surf,                      &
     &    ele_mesh_IO%node, ele_mesh_IO%sfed)
      call calypso_mpi_barrier
!
      if (iflag_debug.gt.0) write(*,*) 'output_surface_file'
      call output_surface_file(my_rank, file_prefix, ele_mesh_IO)
      call calypso_mpi_barrier
!
!  -------------------------------
!     output edge data
!  -------------------------------
!
      file_prefix = def_edge_mesh_head
      call copy_comm_tbl_type(ele_mesh%edge_comm, ele_mesh_IO%comm)
      if (iflag_debug.gt.0) write(*,*) 'copy_edge_geometry_to_IO'
      call calypso_mpi_barrier
      call copy_edge_connect_to_IO                                      &
     &   (ele_mesh%edge, mesh%ele%numele, ele_mesh%surf%numsurf,        &
     &    ele_mesh_IO%ele, ele_mesh_IO%sfed)
      call calypso_mpi_barrier
      call copy_edge_geometry_to_IO(ele_mesh%edge,                      &
     &    ele_mesh_IO%node, ele_mesh_IO%sfed)
      call calypso_mpi_barrier
!
      if (iflag_debug.gt.0) write(*,*) 'output_edge_geometries'
      call output_edge_geometries(my_rank, file_prefix, ele_mesh_IO)
      call calypso_mpi_barrier
!
      end subroutine initialize_mesh_test
!
! ----------------------------------------------------------------------
!
        subroutine analyze_mesh_test
!
!
      if (iflag_debug.gt.0) write(*,*) 'exit analyze'
!
        end subroutine analyze_mesh_test
!
! ----------------------------------------------------------------------
!
      end module analyzer_mesh_test
