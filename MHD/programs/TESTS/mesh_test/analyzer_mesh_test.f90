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
      use m_work_time
!
      implicit none
!
      type(mesh_data), save :: fem_T
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
      use t_ctl_data_mesh_test
      use t_control_param_mesh_test
!
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
!
      use mpi_load_mesh_data
      use const_jacobians_3d
      use parallel_FEM_mesh_init
      use load_element_mesh_data
      use output_test_mesh
      use const_element_comm_table
!
      use t_file_IO_parameter
      use t_mesh_data
      use t_read_mesh_data
      use t_shape_functions
      use t_jacobians
!
!>     Stracture for Jacobians
      type(jacobians_type), save :: jacobians_T
      type(shape_finctions_at_points), save :: spfs_T
!
      type(mesh_test_control), save :: mesh_tctl1
      type(mesh_test_files_param) ::  T_meshes
      type(mesh_geometry) :: mesh_IO
      type(surf_edge_IO_file) :: ele_mesh_IO
!
!
      num_elapsed = 1
      call allocate_elapsed_times
!
      elapse_labels(1) = 'Total time                  '
      call elapsed_label_4_ele_comm_tbl
!
!     --------------------- 
!
      if (my_rank.eq.0) then
        write(*,*) 'Test mesh commnucations'
        write(*,*) 'Input file: mesh data'
      end if
!
!     ----- read control data
!
      call read_control_4_mesh_test(mesh_tctl1)
!
      call set_ctl_params_4_test_mesh(mesh_tctl1, T_meshes)
!
!  --  read geometry
!
      if (iflag_debug.gt.0) write(*,*) 'mpi_input_mesh'
      call mpi_input_mesh                                               &
     &   (T_meshes%mesh_file_IO, nprocs, fem_T, ele_mesh)
!
!  -------------------------------
!
      call start_elapsed_time(1)
      if (iflag_debug.gt.0 ) write(*,*) 'FEM_mesh_init_with_IO'
      call FEM_mesh_init_with_IO(T_meshes%iflag_output_SURF,            &
     &    T_meshes%mesh_file_IO, fem_T%mesh, fem_T%group, ele_mesh)
      call end_elapsed_time(1)
      call calypso_MPI_barrier
      return
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'pick_surface_group_geometry'
      call pick_surface_group_geometry(ele_mesh%surf,                   &
     &   fem_T%group%surf_grp, fem_T%group%tbls_surf_grp,               &
     &   fem_T%group%surf_grp_geom)
!
!  -------------------------------
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'const_jacobian_volume_normals'
      allocate(jacobians_T%g_FEM)
      call sel_max_int_point_by_etype                                   &
     &   (fem_T%mesh%ele%nnod_4_ele, jacobians_T%g_FEM)
      call const_jacobian_volume_normals(my_rank, nprocs,               &
     &    fem_T%mesh, ele_mesh%surf, fem_T%group, spfs_T, jacobians_T)
!
      if (iflag_debug.gt.0) write(*,*) 'const_edge_vector'
      call const_edge_vector(my_rank, nprocs,                           &
     &    fem_T%mesh%node, ele_mesh%edge, spfs_T%spf_1d, jacobians_T)
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
      if (iflag_debug.gt.0) write(*,*) 's_cal_edge_vector_spherical'
      call s_cal_edge_vector_spherical(ele_mesh%edge)
      if (iflag_debug.gt.0) write(*,*) 's_cal_edge_vector_cylindrical'
      call s_cal_edge_vector_cylindrical(ele_mesh%edge)
!
!  ---------------------------------------------
!     output element, surface, edge data
!  ---------------------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'output_test_mesh_informations'
      call output_test_mesh_informations                               &
     &   (my_rank, fem_T%mesh, ele_mesh, mesh_IO, ele_mesh_IO)
!
      end subroutine initialize_mesh_test
!
! ----------------------------------------------------------------------
!
      subroutine analyze_mesh_test
!
!
      call output_elapsed_times
      call calypso_MPI_barrier
!
      if (iflag_debug.gt.0) write(*,*) 'exit analyze'
!
      end subroutine analyze_mesh_test
!
! ----------------------------------------------------------------------
!
      end module analyzer_mesh_test
