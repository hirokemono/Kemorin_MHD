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
      num_elapsed = 11
      call allocate_elapsed_times
!
      elapse_labels(1) = 'Total time                  '
      elapse_labels(2) = 'const_element_comm_tbls2'
!
      elapse_labels(3) = 'const_ele_comm_tbl2'
      elapse_labels(4) = 'const_global_element_id'
      elapse_labels(5) = 'const_surf_comm_table'
      elapse_labels(6) = 'start_elapsed_time'
      elapse_labels(7) = 'const_edge_comm_table'
      elapse_labels(8) = 'const_global_edge_id'
!
      elapse_labels(9) = 'const_comm_table_by_connenct2'
      elapse_labels(10) = 's_set_element_export_item'
      elapse_labels(11) = 'search_target_element3'
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
      if (iflag_debug.gt.0 ) write(*,*) 'FEM_mesh_init_with_IO2'
      call FEM_mesh_init_with_IO2(T_meshes%iflag_output_SURF,           &
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
!-----------------------------------------------------------------------
!
      subroutine FEM_mesh_init_with_IO2                                 &
     &         (iflag_output_SURF, mesh_file, mesh, group, ele_mesh)
!
      use t_file_IO_parameter
      use t_read_mesh_data
!
      use m_array_for_send_recv
      use m_phys_constants
!
      use nod_phys_send_recv
      use node_monitor_IO
      use const_mesh_information
      use const_element_comm_tables
      use mesh_file_name_by_param
      use parallel_FEM_mesh_init
!
      integer(kind = kint), intent(in) :: iflag_output_SURF
      type(field_io_params), intent(in) :: mesh_file
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
!
      type(surf_edge_IO_file) :: ele_mesh_IO
      integer(kind = kint) :: iflag_ele_mesh
!
!
      iflag_ele_mesh =  check_exist_ele_mesh(mesh_file, izero)          &
     &                + check_exist_surf_mesh(mesh_file, izero)         &
     &                + check_exist_edge_mesh(mesh_file, izero)
      if(iflag_ele_mesh .eq. 0) then
        if(iflag_debug.gt.0) write(*,*) 'mpi_load_element_surface_edge'
        call mpi_load_element_surface_edge                              &
     &     (mesh_file, mesh, ele_mesh, ele_mesh_IO)
      end if
      call calypso_mpi_barrier
!
!  -------------------------------
!      if (iflag_debug.gt.0) write(*,*) 'set_local_nod_4_monitor'
!      call set_local_nod_4_monitor(mesh, group)
!
!  ------  In itialize data communication for FEM data
!
      if (iflag_debug.gt.0 ) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver(n_sym_tensor, mesh%node%numnod)
!
      if(iflag_debug.gt.0) write(*,*)' init_nod_send_recv'
      call init_nod_send_recv(mesh)
!
!  -----    construct geometry informations
!
      if (iflag_debug .gt. 0) write(*,*) 'const_mesh_infos'
      call const_mesh_infos(my_rank, mesh, group, ele_mesh)
!
      if(iflag_ele_mesh .eq. 0) return
!
      call start_elapsed_time(2)
      if(iflag_debug.gt.0) write(*,*)' const_element_comm_tbls2'
      call const_element_comm_tbls2(mesh, ele_mesh)
      call end_elapsed_time(2)
!
      if(i_debug .eq. iflag_full_msg) then
        call check_whole_num_of_elements(mesh%ele)
      end if
!
      if(iflag_ele_mesh .ne. 0 .and. iflag_output_SURF .gt. 0) then
        call mpi_output_element_surface_edge                            &
     &         (mesh_file, mesh, ele_mesh, ele_mesh_IO)
      end if
!
!      call deallocate_surface_geom_type(ele_mesh%surf)
!      call dealloc_edge_geometory(ele_mesh%edge)
!
      end subroutine FEM_mesh_init_with_IO2
!
!-----------------------------------------------------------------------
!
      subroutine const_element_comm_tbls2(mesh, ele_mesh)
!
      use set_ele_id_4_node_type
      use t_belonged_element_4_node
      use const_element_comm_tables
!
      type(mesh_geometry), intent(inout) :: mesh
      type(element_geometry), intent(inout) :: ele_mesh
!
      type(belonged_table) :: blng_tbl
!
!
      if(iflag_debug.gt.0) write(*,*) 'const_global_numnod_list'
      call const_global_numnod_list(mesh%node)
!
      if(iflag_debug.gt.0) write(*,*) 'find_position_range'
      call find_position_range(mesh%node)
!
      if(iflag_debug.gt.0) write(*,*) 'const_ele_comm_tbl2'
      call start_elapsed_time(3)
      call const_ele_comm_tbl2(mesh%node, mesh%ele, mesh%nod_comm,      &
     &    blng_tbl, ele_mesh%ele_comm)
      call end_elapsed_time(3)
      call calypso_mpi_barrier
!
      call start_elapsed_time(4)
      if(i_debug.gt.0) write(*,*)' const_global_element_id', my_rank
      call const_global_element_id(mesh%ele, ele_mesh%ele_comm)
      call end_elapsed_time(4)
      call calypso_mpi_barrier
!
      if(iflag_debug.gt.0) write(*,*)' const_surf_comm_table'
      call start_elapsed_time(5)
      call const_surf_comm_table(mesh%node, mesh%nod_comm,              &
     &    ele_mesh%surf, blng_tbl, ele_mesh%surf_comm)
      call end_elapsed_time(5)
!
      if(iflag_debug.gt.0) write(*,*)' const_global_surface_id'
      call start_elapsed_time(6)
      call const_global_surface_id(ele_mesh%surf, ele_mesh%surf_comm)
      call end_elapsed_time(6)
!
      if(iflag_debug.gt.0) write(*,*)' const_edge_comm_table'
      call start_elapsed_time(7)
      call const_edge_comm_table(mesh%node, mesh%nod_comm,              &
     &    ele_mesh%edge, blng_tbl, ele_mesh%edge_comm)
      call end_elapsed_time(7)
!
      if(iflag_debug.gt.0) write(*,*)' const_global_edge_id'
      call start_elapsed_time(8)
      call const_global_edge_id(ele_mesh%edge, ele_mesh%edge_comm)
      call end_elapsed_time(8)
!
      end subroutine const_element_comm_tbls2
!
!-----------------------------------------------------------------------
!
      subroutine const_ele_comm_tbl2                                    &
     &         (node, ele, nod_comm, belongs, ele_comm)
!
      use set_ele_id_4_node_type
      use const_element_comm_table
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: nod_comm
      type(belonged_table), intent(inout) :: belongs
      type(communication_table), intent(inout) :: ele_comm
!
      character(len=kchara), parameter :: txt = 'element'
!
!
      call set_ele_id_4_node(node, ele, belongs%blng_ele)
      call alloc_x_ref_ele(node, belongs)
      call sort_inod_4_ele_by_position(ione, ele%numele, ele%x_ele,     &
     &    node, belongs%blng_ele, belongs%x_ref_ele)
!
      call belonged_ele_id_4_node(node, ele, belongs%host_ele)
      call calypso_mpi_barrier
!
      if(i_debug.gt.0) write(*,*)' const_comm_table_by_connenct2',      &
     &                            my_rank
      call start_elapsed_time(9)
      call const_comm_table_by_connenct                                 &
     &   (txt, ele%numele, ele%nnod_4_ele, ele%ie,                      &
     &    ele%interior_ele, ele%x_ele, node, nod_comm,                  &
     &    belongs%blng_ele, belongs%x_ref_ele, belongs%host_ele,        &
     &    ele_comm)
      call end_elapsed_time(9)
      call calypso_mpi_barrier
!
      call dealloc_iele_belonged(belongs%host_ele)
      call dealloc_x_ref_ele(belongs)
      call dealloc_iele_belonged(belongs%blng_ele)
!
      end subroutine const_ele_comm_tbl2
!
!-----------------------------------------------------------------------
!
      end module analyzer_mesh_test
