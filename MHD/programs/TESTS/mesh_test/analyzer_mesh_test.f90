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
      use t_mesh_data
      use t_read_mesh_data
      use t_jacobian_3d
!
!>     Stracture for Jacobians for linear element
      type(jacobians_3d), save :: jac_3d_l
!>     Stracture for Jacobians for quad element
      type(jacobians_3d), save :: jac_3d_q
!
      type(mesh_geometry) :: mesh_IO
      type(surf_edge_IO_file) :: ele_mesh_IO
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
      call set_ctl_params_4_test_mesh
!
!  --  read geometry
!
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh(mesh, group,                                  &
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
      if (iflag_debug.gt.0) write(*,*) 'const_jacobian_and_volume'
      call max_int_point_by_etype(mesh%ele%nnod_4_ele)
      call const_jacobian_and_volume                                    &
     &   (mesh%node, group%surf_grp, group%infty_grp, mesh%ele,         &
     &    jac_3d_l, jac_3d_q)
!
!      call check_jacobians_trilinear(my_rank, mesh%ele, jac_3d_l)
!
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*)  'const_normal_vector'
      call const_normal_vector(mesh%node, ele_mesh%surf)
!
      if (iflag_debug.gt.0) write(*,*) 's_cal_normal_vector_spherical'
      call s_cal_normal_vector_spherical(ele_mesh%surf)
      if (iflag_debug.gt.0) write(*,*) 's_cal_normal_vector_cylindrical'
      call s_cal_normal_vector_cylindrical(ele_mesh%surf)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'const_edge_vector'
      call const_edge_vector(mesh%node, ele_mesh%edge)
!
      if (iflag_debug.gt.0) write(*,*) 's_cal_edge_vector_spherical'
      call s_cal_edge_vector_spherical(ele_mesh%edge)
      if (iflag_debug.gt.0) write(*,*) 's_cal_edge_vector_cylindrical'
      call s_cal_edge_vector_cylindrical(ele_mesh%edge)
!
!  -------------------------------
!
      if (iflag_debug.gt.0)  write(*,*) 'pick_normal_of_surf_group'
      call pick_normal_of_surf_group(ele_mesh%surf,                     &
     &   group%surf_grp, group%tbls_surf_grp, group%surf_grp_geom)
!
      if (iflag_debug.gt.0)  write(*,*) 's_sum_normal_4_surf_group'
      call s_sum_normal_4_surf_group                                    &
     &   (mesh%ele, group%surf_grp, group%surf_grp_geom)
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
      call add_int_suffix(my_rank, mesh_sph_file_head, mesh_file_name)
      call write_node_position_sph(my_rank, mesh_IO)
!
      mesh_IO%nod_comm%num_neib = 0
      call allocate_type_neib_id(mesh_IO%nod_comm)
      call copy_node_cyl_to_xx(mesh%node, mesh_IO%node)
!
      call write_node_position_cyl(my_rank, mesh_IO)
!
!  -------------------------------
!     output element data
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'copy_ele_geometry_to_IO'
      call copy_comm_tbl_type(ele_mesh%ele_comm, ele_mesh_IO%comm)
      call calypso_mpi_barrier
      mesh_ele_file_head = mesh_ele_def_head
!
      call copy_ele_geometry_to_IO                                      &
     &   (mesh%ele, ele_mesh_IO%node, ele_mesh_IO%sfed)
      call calypso_mpi_barrier
!
      call output_element_file(my_rank, ele_mesh_IO)
      call calypso_mpi_barrier
!
!  -------------------------------
!     output surface data
!  -------------------------------
!
      mesh_surf_file_head = mesh_def_surf_head
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
      call output_surface_file(my_rank, ele_mesh_IO)
      call calypso_mpi_barrier
!
!  -------------------------------
!     output edge data
!  -------------------------------
!
      mesh_edge_file_head = mesh_def_edge_head
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
      call output_edge_geometries(my_rank, ele_mesh_IO)
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
